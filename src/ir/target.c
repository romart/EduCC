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
static Boolean classifyLeaf(const TargetDescriptor *target, const TypeRef *type, size_t offset,
                            enum EightbyteClass classes[2]);

static Boolean classifyMembers(const TargetDescriptor *target, const TypeRef *type, size_t offset,
                               enum EightbyteClass classes[2]) {
  TypeDefiniton *definition = type->descriptorDesc->typeDefinition;

  if (definition == NULL) {
    return FALSE;  // an incomplete type never gets this far, but say so anyway
  }

  for (StructualMember *member = definition->members; member != NULL; member = member->next) {
    if (member->type == NULL) continue;

    // A union's members all start where the union does, and 'offset' already
    // carries that; StructualMember.offset is zero for each of them.
    if (!classifyLeaf(target, member->type, offset + (size_t)member->offset, classes)) {
      return FALSE;
    }
  }

  return TRUE;
}

static Boolean classifyLeaf(const TargetDescriptor *target, const TypeRef *type, size_t offset,
                            enum EightbyteClass classes[2]) {
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
      if (!classifyLeaf(target, element, offset + idx * elementSize, classes)) {
        return FALSE;
      }
    }

    return TRUE;
  }

  if (isCompositeType(type)) {
    return classifyMembers(target, type, offset, classes);
  }

  enum EightbyteClass leaf = EB_INTEGER;

  if (type->kind == TR_BITFIELD) {
    leaf = EB_INTEGER;  // its storage is an integer whatever it is declared as
  } else if (type->kind == TR_VALUE) {
    TypeId id = type->descriptorDesc->typeId;
    // SysV gives a long double class X87, which has no argument register, so
    // an aggregate holding one is passed in memory entire. LP64D's is
    // binary128 and travels in integer registers like any other sixteen bytes,
    // so there it is nothing special - which is why this is a target's answer.
    if (id == T_F10 && target->longDoubleFormat != LDF_BINARY128) return FALSE;
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

// Fills the pieces in with the eightbytes 'size' bytes divide into - the offset
// and the width a SysV piece always has, and the shape the integer convention
// gives an aggregate on either target.
static uint32_t eightbyteParts(size_t size, AggregatePart parts[2]) {
  uint32_t count = (uint32_t)((size + sizeof(intptr_t) - 1) / sizeof(intptr_t));

  for (uint32_t eb = 0; eb < count; ++eb) {
    parts[eb].offset = (uint32_t)(eb * sizeof(intptr_t));
    parts[eb].size = (uint32_t)min(sizeof(intptr_t), size - eb * sizeof(intptr_t));
  }

  return count;
}

uint32_t classifyCompositeIntegers(const TargetDescriptor *target, const TypeRef *type,
                                   AggregatePart parts[2]) {
  parts[0].cls = parts[1].cls = EB_NONE;
  parts[0].offset = parts[1].offset = 0;
  parts[0].size = parts[1].size = 0;

  if (!isCompositeType(type)) {
    return 0;
  }

  size_t size = (size_t)computeTypeSize(type);

  if (size == 0 || size > 2 * sizeof(intptr_t)) {
    return 0;
  }

  uint32_t count = eightbyteParts(size, parts);

  for (uint32_t eb = 0; eb < count; ++eb) {
    parts[eb].cls = EB_INTEGER;
  }

  return count;
}

// LP64D's rule, which asks what the *fields* are rather than what each
// eightbyte holds. The struct is flattened recursively - a nested struct or a
// one-element array contributes its own leaves - and the answer is a piece per
// leaf, in declaration order.
//
// It applies to a struct of at most two leaves, each no wider than a register,
// at least one of which is a floating-point real. Anything else - three leaves,
// a union anywhere, a long double, an array of three floats - is not flattened
// and travels by the integer convention; the oracle confirms each of those.
// FALSE means exactly that, and is not the same as MEMORY.
#define MAX_FLAT_LEAVES 2

typedef struct {
  AggregatePart leaves[MAX_FLAT_LEAVES];
  uint32_t count;
  Boolean ok;
} FlatFields;

static void flattenLeaf(const TypeRef *type, size_t offset, FlatFields *flat) {
  if (!flat->ok) {
    return;
  }

  size_t size = (size_t)computeTypeSize(type);

  if (type->kind == TR_ARRAY) {
    const TypeRef *element = type->arrayTypeDesc.elementType;
    int count = type->arrayTypeDesc.size;
    size_t elementSize = (size_t)computeTypeSize(element);

    // A zero-length array occupies nothing and contributes no leaf, which is
    // what lets struct { double a; int z[0]; } still go in one fa register.
    for (int idx = 0; idx < count; ++idx) {
      flattenLeaf(element, offset + idx * elementSize, flat);
    }

    return;
  }

  if (isCompositeType(type)) {
    TypeDefiniton *definition = type->descriptorDesc->typeDefinition;

    // A union is never flattened: its members share an offset, so "one piece
    // per field" has no answer. gcc passes one by the integer convention.
    if (definition == NULL || definition->kind != TDK_STRUCT) {
      flat->ok = FALSE;
      return;
    }

    for (StructualMember *member = definition->members; member != NULL; member = member->next) {
      if (member->type == NULL) continue;
      flattenLeaf(member->type, offset + (size_t)member->offset, flat);
    }

    return;
  }

  if (size == 0) {
    return;
  }

  if (flat->count >= MAX_FLAT_LEAVES || size > sizeof(intptr_t)) {
    flat->ok = FALSE;
    return;
  }

  enum EightbyteClass cls = EB_INTEGER;

  // A bitfield's storage is an integer whatever it is declared as, and gcc
  // flattens a struct holding one exactly as if it were that integer.
  if (type->kind == TR_VALUE) {
    TypeId id = type->descriptorDesc->typeId;
    if (id == T_F4 || id == T_F8) cls = EB_SSE;
  }

  flat->leaves[flat->count].cls = cls;
  flat->leaves[flat->count].offset = (uint32_t)offset;
  flat->leaves[flat->count].size = (uint32_t)size;
  flat->count += 1;
}

// How many pieces the flattened form has, or 0 when this type does not flatten.
static uint32_t flattenFloatFields(const TypeRef *type, AggregatePart parts[2]) {
  FlatFields flat;
  flat.count = 0;
  flat.ok = TRUE;

  flattenLeaf(type, 0, &flat);

  if (!flat.ok || flat.count == 0) {
    return 0;
  }

  Boolean anyFloat = FALSE;
  for (uint32_t idx = 0; idx < flat.count; ++idx) {
    if (flat.leaves[idx].cls == EB_SSE) anyFloat = TRUE;
  }

  if (!anyFloat) {
    return 0;  // all integer: the integer convention says the same thing
  }

  for (uint32_t idx = 0; idx < flat.count; ++idx) {
    parts[idx] = flat.leaves[idx];
  }

  return flat.count;
}

uint32_t classifyComposite(const TargetDescriptor *target, const TypeRef *type,
                           AggregatePart parts[2]) {
  enum EightbyteClass classes[2] = {EB_NONE, EB_NONE};

  parts[0].cls = parts[1].cls = EB_NONE;
  parts[0].offset = parts[1].offset = 0;
  parts[0].size = parts[1].size = 0;

  if (!isCompositeType(type)) {
    return 0;
  }

  size_t size = (size_t)computeTypeSize(type);

  if (size == 0 || size > 2 * sizeof(intptr_t)) {
    return 0;
  }

  if (target->flattenFloatAggregates) {
    uint32_t flat = flattenFloatFields(type, parts);
    return flat != 0 ? flat : classifyCompositeIntegers(target, type, parts);
  }

  if (!classifyMembers(target, type, 0, classes)) {
    return 0;
  }

  uint32_t eightbytes = eightbyteParts(size, parts);

  // An eightbyte no member reached - a struct that is all padding, or one whose
  // only member is a zero-sized array - still has to travel as something, and
  // INTEGER is what SysV makes of a NO_CLASS eightbyte that is not MEMORY.
  for (uint32_t eb = 0; eb < eightbytes; ++eb) {
    parts[eb].cls = classes[eb] == EB_NONE ? EB_INTEGER : classes[eb];
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
Boolean isCompositeInSSERegister(const TargetDescriptor *target, const TypeRef *type) {
  AggregatePart parts[2];
  return classifyComposite(target, type, parts) == 1 && parts[0].cls == EB_SSE;
}

Boolean longDoubleIsSoft(const TargetDescriptor *target) {
  return target->longDoubleFormat == LDF_BINARY128;
}

Boolean isRegisterPairScalar(const TargetDescriptor *target, const TypeRef *type) {
  return target->longDoubleFormat == LDF_BINARY128 && type->kind == TR_VALUE &&
         type->descriptorDesc->typeId == T_F10;
}

Boolean returnsThroughHiddenPointer(const TargetDescriptor *target, const TypeRef *returnType) {
  AggregatePart parts[2];
  return isCompositeType(returnType) && !isEmptyCompositeType(returnType) &&
         classifyComposite(target, returnType, parts) == 0;
}

// All of it or none of it: an aggregate that would take the last integer
// register and one that is not there does not get half of what it needs. The
// argument behind it may still find a register, which is why this asks rather
// than stopping the walk.
static Boolean partsFitInRegisters(const TargetDescriptor *target, const AggregatePart parts[2],
                                   uint32_t count, uint32_t usedInt, uint32_t usedFp) {
  uint32_t needInt = 0, needFp = 0;

  for (uint32_t idx = 0; idx < count; ++idx) {
    if (parts[idx].cls == EB_SSE) needFp += 1; else needInt += 1;
  }

  return count != 0 && usedInt + needInt <= target->intArgRegCount &&
         usedFp + needFp <= target->fpArgRegCount;
}

// The half-and-half case: two integer words with exactly one integer argument
// register left. LP64D puts the low word in it and the high word on the stack;
// SysV has no such rule, so this is FALSE there and the whole thing goes to
// memory.
//
// It can only ever be the first stack argument - the stack does not start
// taking integers until the registers are gone - which is why the high word
// needs no alignment of its own.
static Boolean splitsAcrossStack(const TargetDescriptor *target, const AggregatePart parts[2],
                                 uint32_t count, uint32_t usedInt) {
  return target->splitsArgumentsAcrossStack && count == 2 &&
         parts[0].cls == EB_INTEGER && parts[1].cls == EB_INTEGER &&
         usedInt + 1 == target->intArgRegCount;
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
  if (returnsThroughHiddenPointer(target, declaration->returnType)) {
    intRegParams += 1;
  }

  // Where the first stack parameter sits relative to the frame pointer - above
  // the saved frame pointer and the pushed return address on x86-64, and at the
  // frame pointer itself on a target that pushes neither. The target's answer,
  // because it is not the same constant on both.
  int32_t stackParamOffset = target->incomingStackParamOffset;

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
    pi->parts[0].cls = pi->parts[1].cls = EB_NONE;
    pi->parts[0].offset = pi->parts[1].offset = 0;
    pi->parts[0].size = pi->parts[1].size = 0;
    pi->byReference = FALSE;
    pi->split = FALSE;
    pi->splitStackOffset = 0;

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
      uint32_t eightbytes = classifyComposite(target, paramType, pi->parts);

      // Too wide for two registers, on a target that passes such a thing by
      // reference: it is a pointer argument from here on, and one word is all
      // it takes wherever it lands.
      if (eightbytes == 0 && target->largeAggregatesByReference) {
        pi->byReference = TRUE;
        pi->parts[0].cls = pi->parts[1].cls = EB_NONE;
        size = align = sizeof(intptr_t);

        inRegister = intRegParams < target->intArgRegCount;
        if (inRegister) {
          pi->loc.physReg = target->intArgRegs[intRegParams++];
        }

        pi->isRegister = inRegister;

        if (!inRegister) {
          int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
          pi->loc.stackOffset = alignedOffset;
          stackParamOffset = alignedOffset + size;
        }

        continue;
      }

      inRegister = partsFitInRegisters(target, pi->parts, eightbytes,
                                       intRegParams, fpRegParams);

      // LP64D's flattened form wants a floating-point register per float
      // field, and when the arguments ahead have used them up the *whole*
      // argument falls back to the integer convention rather than to the
      // stack - struct { float a, b; } behind eight doubles arrives in a0, not
      // in fa0 and fa1 and not in memory. Confirmed against the oracle.
      if (!inRegister && target->flattenFloatAggregates) {
        eightbytes = classifyCompositeIntegers(target, paramType, pi->parts);
        inRegister = partsFitInRegisters(target, pi->parts, eightbytes,
                                         intRegParams, fpRegParams);
      }

      if (inRegister) {
        pi->regCount = eightbytes;
        for (uint32_t eb = 0; eb < eightbytes; ++eb) {
          uint32_t reg = pi->parts[eb].cls == EB_SSE ? target->fpArgRegs[fpRegParams++]
                                                     : target->intArgRegs[intRegParams++];
          if (eb == 0) pi->loc.physReg = reg; else pi->physReg2 = reg;
        }
      } else if (splitsAcrossStack(target, pi->parts, eightbytes, intRegParams)) {
        pi->split = TRUE;
        pi->regCount = 2;
        pi->loc.physReg = target->intArgRegs[intRegParams++];
        pi->splitStackOffset = ALIGN_SIZE(stackParamOffset, sizeof(intptr_t));
        stackParamOffset = pi->splitStackOffset + (int32_t)sizeof(intptr_t);
        pi->isRegister = TRUE;
        continue;
      }
    } else if (isRegisterPairScalar(target, paramType)) {
      // Twice a register wide, so it travels in two of them, low half first -
      // the same shape a two-eightbyte aggregate takes and for the same reason,
      // which is why the eightbyte classes say INTEGER and the rest of the
      // translation needs to know nothing else about it.
      //
      pi->parts[0].cls = pi->parts[1].cls = EB_INTEGER;
      pi->parts[0].offset = 0;
      pi->parts[1].offset = (uint32_t)sizeof(intptr_t);
      pi->parts[0].size = pi->parts[1].size = (uint32_t)sizeof(intptr_t);
      inRegister = intRegParams + 2 <= target->intArgRegCount;

      if (inRegister) {
        pi->regCount = 2;
        pi->loc.physReg = target->intArgRegs[intRegParams++];
        pi->physReg2 = target->intArgRegs[intRegParams++];
      } else if (splitsAcrossStack(target, pi->parts, 2, intRegParams)) {
        // The same split the aggregate takes, and the oracle confirms it is
        // one rule: a long double is a two-word argument like any other.
        pi->split = TRUE;
        pi->regCount = 2;
        pi->loc.physReg = target->intArgRegs[intRegParams++];
        pi->splitStackOffset = ALIGN_SIZE(stackParamOffset, sizeof(intptr_t));
        stackParamOffset = pi->splitStackOffset + (int32_t)sizeof(intptr_t);
        pi->isRegister = TRUE;
        continue;
      }
    } else if (isRealType(paramType)) {
      inRegister = fpRegParams < target->fpArgRegCount && size <= sizeof(intptr_t);
      if (inRegister) {
        pi->loc.physReg = target->fpArgRegs[fpRegParams++];
      } else if (target->floatsUseIntegerRegsWhenFull && size <= sizeof(intptr_t) &&
                 intRegParams < target->intArgRegCount) {
        // The floating-point argument registers are gone, so this one arrives
        // in an integer register holding its bits - which is the one place
        // loc.physReg is not in the file the type says. selectPhysReg is what
        // notices, and moves across the banks instead of copying.
        pi->loc.physReg = target->intArgRegs[intRegParams++];
        inRegister = TRUE;
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
