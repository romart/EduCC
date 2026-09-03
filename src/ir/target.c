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

// SysV classifies an aggregate one eightbyte at a time by what lands in each:
// an eightbyte holding nothing but 'float' and 'double' is class SSE and
// travels in an xmm register, and anything else in it - an integer, a pointer,
// a bitfield - makes it INTEGER. Two eightbytes is the whole of it: sixteen
// bytes is where the rule stops and the aggregate goes to memory instead.
//
// Marks the eightbytes one scalar leaf spans, at 'offset' from the start of the
// aggregate being classified. FALSE means the whole aggregate is class MEMORY,
// which is what a long double in it says: SysV gives that class X87, which has
// no argument register, and an aggregate containing one is passed on the stack.
static Boolean classifyLeaf(const TypeRef *type, size_t offset, enum EightbyteClass classes[2]);

static Boolean classifyMembers(const TypeRef *type, size_t offset, enum EightbyteClass classes[2]) {
  TypeDefiniton *definition = type->descriptorDesc->typeDefinition;

  if (definition == NULL) {
    return FALSE;  // an incomplete type never gets this far, but say so anyway
  }

  for (StructualMember *member = definition->members; member != NULL; member = member->next) {
    if (member->type == NULL) continue;

    // A union's members all start where the union does, and 'offset' already
    // carries that; StructualMember.offset is zero for each of them.
    if (!classifyLeaf(member->type, offset + (size_t)member->offset, classes)) {
      return FALSE;
    }
  }

  return TRUE;
}

static Boolean classifyLeaf(const TypeRef *type, size_t offset, enum EightbyteClass classes[2]) {
  if (offset >= 2 * sizeof(intptr_t)) {
    return FALSE;  // past the second eightbyte: the size test should have caught it
  }

  if (type->kind == TR_ARRAY) {
    const TypeRef *element = type->arrayTypeDesc.elementType;
    size_t elementSize = (size_t)computeTypeSize(element);
    int count = type->arrayTypeDesc.size;

    if (elementSize == 0 || count <= 0) {
      return TRUE;  // nothing occupies an eightbyte, so nothing to classify
    }

    for (int idx = 0; idx < count; ++idx) {
      if (!classifyLeaf(element, offset + idx * elementSize, classes)) {
        return FALSE;
      }
    }

    return TRUE;
  }

  if (isCompositeType(type)) {
    return classifyMembers(type, offset, classes);
  }

  enum EightbyteClass leaf = EB_INTEGER;

  if (type->kind == TR_BITFIELD) {
    leaf = EB_INTEGER;  // its storage is an integer whatever it is declared as
  } else if (type->kind == TR_VALUE) {
    TypeId id = type->descriptorDesc->typeId;
    if (id == T_F10) return FALSE;  // class X87: the aggregate goes to memory
    if (id == T_F4 || id == T_F8) leaf = EB_SSE;
  }

  // The leaf's own bytes decide which eightbytes it touches; one that straddles
  // the boundary marks both. INTEGER wins wherever the two meet, which is the
  // whole of SysV's merge rule once MEMORY has been dealt with above.
  size_t size = (size_t)computeTypeSize(type);
  size_t last = size == 0 ? offset : offset + size - 1;

  if (last >= 2 * sizeof(intptr_t)) {
    return FALSE;
  }

  for (size_t eb = offset / sizeof(intptr_t); eb <= last / sizeof(intptr_t); ++eb) {
    classes[eb] = (classes[eb] == EB_SSE || classes[eb] == EB_NONE) && leaf == EB_SSE
                      ? EB_SSE : EB_INTEGER;
  }

  return TRUE;
}

uint32_t classifyComposite(const TypeRef *type, enum EightbyteClass classes[2]) {
  classes[0] = classes[1] = EB_NONE;

  if (!isCompositeType(type)) {
    return 0;
  }

  size_t size = (size_t)computeTypeSize(type);

  if (size == 0 || size > 2 * sizeof(intptr_t)) {
    return 0;
  }

  if (!classifyMembers(type, 0, classes)) {
    return 0;
  }

  // An eightbyte no member reached - a struct that is all padding, or one whose
  // only member is a zero-sized array - still has to travel as something, and
  // INTEGER is what SysV makes of a NO_CLASS eightbyte that is not MEMORY.
  uint32_t eightbytes = (uint32_t)((size + sizeof(intptr_t) - 1) / sizeof(intptr_t));

  for (uint32_t eb = 0; eb < eightbytes; ++eb) {
    if (classes[eb] == EB_NONE) classes[eb] = EB_INTEGER;
  }

  return eightbytes;
}

// Which register file a composite small enough to travel in one register uses.
// The size test is the caller's; this only picks the class.
//
// The IR backend alone reads this. The legacy backend evaluates a composite
// argument down a separate address-based path that never reaches a register
// class at all, so it still passes these in the integer file - which is why
// the two backends disagree here and the crossabi fixtures below say so.
Boolean isCompositeInSSERegister(const TypeRef *type) {
  enum EightbyteClass classes[2];
  return classifyComposite(type, classes) == 1 && classes[0] == EB_SSE;
}

Boolean returnsThroughHiddenPointer(const TypeRef *returnType) {
  enum EightbyteClass classes[2];
  return isCompositeType(returnType) && !isEmptyCompositeType(returnType) &&
         classifyComposite(returnType, classes) == 0;
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

    pi->regCount = 1;
    pi->physReg2 = IR_NO_PHYS_REG;
    pi->classes[0] = pi->classes[1] = EB_NONE;

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

    if (isCompositeType(paramType)) {
      // The eightbyte rule below is SysV's; riscv64 LP64D has its own, and this
      // is the point the hook comment in target.h means when it says the two
      // targets will stop sharing an implementation.
      uint32_t eightbytes = classifyComposite(paramType, pi->classes);

      uint32_t needInt = 0, needFp = 0;
      for (uint32_t eb = 0; eb < eightbytes; ++eb) {
        if (pi->classes[eb] == EB_SSE) needFp += 1; else needInt += 1;
      }

      // All in registers or all on the stack. An aggregate that would take the
      // last integer register and one that is not there does not get half of
      // what it needs - it goes to memory entire, and the argument behind it
      // may still find a register.
      inRegister = eightbytes != 0 &&
                   intRegParams + needInt <= target->intArgRegCount &&
                   fpRegParams + needFp <= target->fpArgRegCount;

      if (inRegister) {
        pi->regCount = eightbytes;
        for (uint32_t eb = 0; eb < eightbytes; ++eb) {
          uint32_t reg = pi->classes[eb] == EB_SSE ? target->fpArgRegs[fpRegParams++]
                                                   : target->intArgRegs[intRegParams++];
          if (eb == 0) pi->loc.physReg = reg; else pi->physReg2 = reg;
        }
      }
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
