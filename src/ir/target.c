#include "ir/target.h"
#include "ir/machine.h"
#include "sema.h"

#include <assert.h>

const TargetDescriptor *getTargetDescriptor(enum Arch arch) {
  switch (arch) {
  case X86_64:
    return &targetX86_64;
  case RISCV64:
    return &targetRiscv64;
  default:
    unreachable("Unknown target architecture");
  }

  return NULL;
}

const char *physRegName(const TargetDescriptor *target, uint32_t reg) {
  if (target == NULL || reg >= target->numPhysRegs) {
    return NULL;
  }

  return target->regName[reg];
}

const char *targetOpcodeName(const TargetDescriptor *target, uint32_t opcode) {
  if (target == NULL || target->opcodeName == NULL || opcode < MOP_TARGET_FIRST) {
    return NULL;
  }

  uint32_t idx = opcode - MOP_TARGET_FIRST;
  return idx < target->numOpcodes ? target->opcodeName[idx] : NULL;
}

enum MachineFlagsEffect targetOpcodeFlagsEffect(const TargetDescriptor *target, uint32_t opcode) {
  if (opcode < MOP_TARGET_FIRST) {
    return MFE_NONE;
  }

  if (target == NULL || target->opcodeFlagsEffect == NULL) {
    return MFE_UNKNOWN;
  }

  uint32_t idx = opcode - MOP_TARGET_FIRST;
  return idx < target->numOpcodes ? (enum MachineFlagsEffect)target->opcodeFlagsEffect[idx]
                                  : MFE_UNKNOWN;
}

Boolean returnsThroughHiddenPointer(const TypeRef *returnType) {
  return isCompositeType(returnType) &&
         computeTypeSize(returnType) > sizeof(intptr_t);
}

void classifyParametersGeneric(const TargetDescriptor *target,
                               AstFunctionDeclaration *declaration,
                               ParamtersABIInfo *infos, size_t numberOfParams,
                               ParametersABISummary *summary) {

  uint32_t intRegParams = 0;
  uint32_t fpRegParams = 0;

  // The buffer a large composite return is written through arrives in the
  // first integer argument register, ahead of every declared parameter - so
  // the first of those starts one register along. It is not in the parameter
  // list, which is why this is a counter rather than another iteration.
  if (returnsThroughHiddenPointer(declaration->returnType)) {
    intRegParams += 1;
  }

  // The first stack parameter sits above the saved frame pointer and the
  // return address that the call itself pushed.
  int32_t stackParamOffset = sizeof(intptr_t) + sizeof(intptr_t);

  uint32_t idx = 0;
  for (AstValueDeclaration *param = declaration->parameters; param != NULL;
       param = param->next, ++idx) {
    TypeRef *paramType = param->type;
    assert(idx < numberOfParams);

    ParamtersABIInfo *pi = &infos[idx];
    pi->idx = idx;
    pi->declaration = param;

    size_t size = max(computeTypeSize(paramType), sizeof(intptr_t));
    size_t align = max(typeAlignment(paramType), sizeof(intptr_t));

    Boolean inRegister = FALSE;

    if (isEmptyCompositeType(paramType)) {
      // Passed in nothing: no register is used up and no stack space is
      // reserved, so the parameter behind it arrives where it would have with
      // this one absent. The address still has to be somewhere - the argument
      // area is where a stack parameter's is - but it names zero bytes, so
      // nothing reads or writes through it.
      pi->isRegister = FALSE;
      pi->loc.stackOffset = ALIGN_SIZE(stackParamOffset, sizeof(intptr_t));
      continue;
    }

    if (isCompositeType(paramType) && size > sizeof(intptr_t)) {
      // TODO: SysV splits an aggregate of <= 16 bytes into two eightbytes and
      // passes those in registers; riscv64 LP64D has its own rules. Both are
      // approximated here by passing everything oversized on the stack, which
      // is ABI-incompatible for small structs. This is the point where the
      // two targets will need separate classifyParameters implementations.
      inRegister = FALSE;
    } else if (isRealType(paramType)) {
      inRegister = fpRegParams < target->fpArgRegCount && size <= sizeof(intptr_t);
      if (inRegister) {
        pi->loc.physReg = target->fpArgRegs[fpRegParams++];
      }
    } else {
      inRegister = intRegParams < target->intArgRegCount;
      if (inRegister) {
        pi->loc.physReg = target->intArgRegs[intRegParams++];
      }
    }

    pi->isRegister = inRegister;

    if (!inRegister) {
      int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
      pi->loc.stackOffset = alignedOffset;
      // Every stack parameter has to advance the cursor, aggregates included -
      // otherwise two of them are handed the same slot.
      stackParamOffset = alignedOffset + size;
    }
  }

  assert(idx == numberOfParams);

  if (summary != NULL) {
    summary->intRegParams = intRegParams;
    summary->fpRegParams = fpRegParams;
    summary->stackParamOffset = ALIGN_SIZE(stackParamOffset, sizeof(intptr_t));
  }
}
