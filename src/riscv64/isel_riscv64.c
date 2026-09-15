#include <assert.h>
#include <string.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "sema.h"
#include "instructions_riscv64.h"
#include "machine_riscv64.h"

// -============================ Stage 1: instruction selection ============-
//
// The riscv64 half of the seam described in include/ir/isel.h. Everything
// target-independent - block layout, the walk, constant and address folding,
// the operand bookkeeping - is in src/ir/codegen/isel.c and is shared with
// x86-64 unchanged; what is here is what an IR instruction becomes.
//
// Three things shape this file, and all three are RV64 being a load/store
// three-address machine with no flags:
//
//   * No instruction reads its own destination, so there is no 'dst <- lhs'
//     copy before an arithmetic instruction the way x86 needs one.
//   * A comparison's answer is an ordinary register (slt) or is the branch
//     itself (blt), so there is no setcc/jcc pair and nothing to keep live
//     between a compare and its reader.
//   * Arithmetic is either 64-bit or 32-bit-sign-extending ('w' forms), and
//     the second is what a C 'int' uses. Keeping every 32-bit value
//     sign-extended in its register is what makes one representation of it,
//     and it is why a narrower type is widened at its definition rather than
//     masked at each use.

static uint8_t valueSize(const IrInstruction *i) {
  return irTypeMachineSize(i->type);
}

static const IrInstruction *inputAt(const IrInstruction *i, size_t idx) {
  return getInstructionFromVector(&i->inputs, idx);
}

// Whether this width uses the 32-bit 'w' instructions. Anything narrower than
// a word is computed at word width too - RV64 has no byte or halfword
// arithmetic - and the integer promotions mean sema has already made almost
// every arithmetic operand an int or wider.
static Boolean isWordOp(uint8_t size) {
  return size <= 4;
}

static Boolean isAddressableIrType(enum IrTypeKind t) {
  return t != IR_VOID;
}

// -============================ Addresses =================================-
//
// One base register plus a signed twelve-bit displacement, and nothing else:
// no index, no scale. isLegalAddressMode says so, which makes the shared GEP
// walk stop one term earlier here than it does on x86-64, so an index term
// arrives as arithmetic that has already been selected.

static void setFrameAddressOperandAt(MachineInstr *mi, uint16_t idx, int32_t frameIdx,
                                     int32_t disp) {
  MachineAddress addr = { MAK_FRAME, NO_REG, NO_REG, 0, disp };
  addr.anchor.frameIdx = frameIdx;
  setMemoryOperand(mi, idx, &addr);
}

static void setFrameAddressOperand(MachineInstr *mi, uint16_t idx, int32_t frameIdx) {
  setFrameAddressOperandAt(mi, idx, frameIdx, 0);
}

static MachineAddress addressFor(MachineBuilder *b, const IrInstruction *ptr, int32_t disp) {
  const AddressFold *f = machineBuilderAddressFold(b, ptr);
  MachineAddress addr = { MAK_REG, NO_REG, NO_REG, 0, disp };

  if (f == NULL) {
    addr.base = machineBuilderVreg(b, ptr);
    return addr;
  }

  addr.disp = disp + f->disp;
  assert(f->index == NULL && "riscv64 has no index register in an address");

  if (f->base != NULL) {
    addr.base = machineBuilderVreg(b, f->base);
  } else {
    addr.kind = MAK_FRAME;
    addr.anchor.frameIdx = f->frameIdx;
  }

  return addr;
}

// -============================ Canonical register form ===================-
//
// The extension a value of this type carries in its register. This is the
// invariant the whole integer side rests on: a 32-bit value is kept *sign*-
// extended whatever its own signedness, because that is what lw produces and
// what every 'w' instruction produces, so there is one representation of it
// and slt/sltu compare two of them correctly. Narrower values follow their own
// signedness, which is what lb/lbu and lh/lhu produce.
static Boolean canonicalIsUnsigned(enum IrTypeKind t, uint8_t size) {
  return size < 4 && isUnsignedIrOperand(t);
}

// A constant in that same form. Without this a folded 0xffffffffu is
// materialized as the 64-bit 4294967295 while the identical value loaded out
// of memory is -1, and the two compare unequal - which is the one way an
// invariant about representations can be got wrong.
static int64_t canonicalValue(int64_t v, uint8_t size, Boolean isUnsigned) {
  if (size >= (uint8_t)sizeof(intptr_t)) {
    return v;
  }

  uint32_t bits = (uint32_t)size * 8;
  uint64_t masked = (uint64_t)v & ((UINT64_C(1) << bits) - 1);

  if (isUnsigned) {
    return (int64_t)masked;
  }

  return (int64_t)(masked << (64 - bits)) >> (64 - bits);
}

static int64_t canonicalConstant(const IrInstruction *cnst, uint8_t size) {
  return canonicalValue(cnst->info.constant.data.i, size,
                        canonicalIsUnsigned(cnst->type, size));
}

// 'li dst, <constant>', in the canonical form for the constant's own type.
// Every place a folded constant needs a register goes through here rather than
// through setValueOperand, which would spell the value as the IR holds it.
static void buildConstantInto(MachineBuilder *b, uint32_t dst, const IrInstruction *cnst,
                              uint8_t size) {
  MachineInstr *mi = buildMachineInstr(b, RISCV_LI, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setImmediateOperand(mi, 1, canonicalConstant(cnst, valueSize(cnst)));
  mi->opSize = size;
}

// -============================ Moves and widths ==========================-

static void selectLoadInto(MachineBuilder *b, uint32_t dst, const IrInstruction *value,
                           uint8_t size) {
  if (machineBuilderIsFolded(b, value)) {
    buildConstantInto(b, dst, value, size);
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
  mi->opSize = size;
}

static uint32_t widenRegister(MachineBuilder *b, uint32_t src, uint8_t srcSize, uint8_t size,
                              Boolean isUnsigned) {
  uint32_t dst = createVirtualRegister(b->mf, RC_GP, size);

  MachineInstr *mi = buildMachineInstr(b, isUnsigned ? RISCV_ZEXT : RISCV_SEXT, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  mi->opSize = size;
  mi->srcSize = srcSize;

  machineNoteRegisterExtension(b->mf, dst, size, isUnsigned);
  return dst;
}

static uint32_t selectWidened(MachineBuilder *b, const IrInstruction *value, uint8_t size) {
  uint32_t src = machineBuilderVreg(b, value);
  uint8_t srcSize = valueSize(value);

  if (srcSize >= size || isFloatIrType(value->type)) {
    return src;
  }

  const Boolean isUnsigned = isUnsignedIrOperand(value->type);

  if (machineTakeRegisterExtension(b->mf, src, size, isUnsigned)) {
    return src;
  }

  return widenRegister(b, src, srcSize, size, isUnsigned);
}

// -============================ Arithmetic ================================-

// Which of a pair of opcodes this width wants: the 'w' form for a 32-bit
// operation, the full-width one for a 64-bit.
static uint32_t widthOpcode(uint8_t size, uint32_t wordOp, uint32_t wideOp) {
  return isWordOp(size) ? wordOp : wideOp;
}

// RV64 has no instruction narrower than a word, so an operation on a one- or
// two-byte value - which is what the bit-field lowering builds - computes in
// all 64 bits and can leave some above the value's own width. Every register
// here is canonically extended, so those bits have to be put back.
//
// and/or/xor and the two right shifts are the exception and need nothing: they
// take canonically extended operands to a canonically extended answer, an
// unsigned one staying zero above the value and a signed one staying its sign.
static Boolean narrowAnswerEscapes(const IrInstruction *i) {
  if (valueSize(i) >= 4) {
    return FALSE; // a 'w' form sign-extends its answer, which is canonical
  }

  switch (i->kind) {
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_SHR:
    return FALSE;
  default:
    return TRUE;
  }
}

// Where such an operation writes: a temporary, so that the extension below is
// what defines the value's own register.
static uint32_t narrowAnswerRegister(MachineBuilder *b, const IrInstruction *i,
                                     Boolean escapes) {
  return escapes ? createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t))
                 : machineBuilderVreg(b, i);
}

static void canonicalizeNarrowAnswer(MachineBuilder *b, const IrInstruction *i,
                                     uint32_t answer, uint8_t size) {
  Boolean isUnsigned = canonicalIsUnsigned(i->type, size);
  uint32_t dst = machineBuilderVreg(b, i);

  MachineInstr *mi = buildMachineInstr(b, isUnsigned ? RISCV_ZEXT : RISCV_SEXT, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, answer);
  mi->opSize = sizeof(intptr_t);
  mi->srcSize = size;

  machineNoteRegisterExtension(b->mf, dst, sizeof(intptr_t), isUnsigned);
}

static void selectBinaryWith(MachineBuilder *b, const IrInstruction *i, uint32_t opcode,
                             Boolean rhsMayBeImmediate) {
  uint8_t size = valueSize(i);
  Boolean escapes = narrowAnswerEscapes(i);
  uint32_t dst = narrowAnswerRegister(b, i, escapes);

  const IrInstruction *lhs = inputAt(i, 0);
  const IrInstruction *rhs = inputAt(i, 1);

  uint32_t lhsReg = selectWidened(b, lhs, size);
  Boolean folded = rhsMayBeImmediate && machineBuilderIsFolded(b, rhs);
  uint32_t rhsReg = folded ? NO_REG : selectWidened(b, rhs, size);

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, lhsReg);
  if (folded) {
    setValueOperand(b, mi, 2, rhs);
  } else {
    setRegisterOperand(mi, 2, rhsReg);
  }
  mi->opSize = escapes ? (uint8_t)sizeof(intptr_t) : size;

  if (escapes) {
    canonicalizeNarrowAnswer(b, i, dst, size);
  }
}

// add/sub/and/or/xor and the multiply-divide family, picking the 'w' form for
// a 32-bit result. 'sub' has no immediate form on RV64 and neither has any of
// the M extension, which is what the flag says.
static void selectBinary(MachineBuilder *b, const IrInstruction *i, uint32_t wordOp,
                         uint32_t wideOp, Boolean rhsMayBeImmediate) {
  selectBinaryWith(b, i, widthOpcode(valueSize(i), wordOp, wideOp), rhsMayBeImmediate);
}

static void selectShift(MachineBuilder *b, const IrInstruction *i, uint32_t wordOp,
                        uint32_t wideOp) {
  // A shift's count is its own operand and not widened to the result's width:
  // RV64 reads the low five or six bits of it and C has already made anything
  // wider undefined.
  uint8_t size = valueSize(i);
  Boolean escapes = narrowAnswerEscapes(i);
  uint32_t dst = narrowAnswerRegister(b, i, escapes);

  uint32_t lhsReg = selectWidened(b, inputAt(i, 0), size);
  const IrInstruction *rhs = inputAt(i, 1);
  Boolean folded = machineBuilderIsFolded(b, rhs);
  uint32_t rhsReg = folded ? NO_REG : machineBuilderVreg(b, rhs);

  MachineInstr *mi = buildMachineInstr(b, widthOpcode(size, wordOp, wideOp), 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, lhsReg);
  if (folded) {
    setValueOperand(b, mi, 2, rhs);
  } else {
    setRegisterOperand(mi, 2, rhsReg);
  }
  mi->opSize = escapes ? (uint8_t)sizeof(intptr_t) : size;

  if (escapes) {
    canonicalizeNarrowAnswer(b, i, dst, size);
  }
}

// '-x' is 'sub rd, x0, x', and '~x' is 'xori rd, x, -1'. Both are pseudo-
// instructions the assembler spells out; here they are opcodes of their own so
// that the operand count says one input rather than two.
static void selectUnary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  Boolean escapes = narrowAnswerEscapes(i);
  uint32_t dst = narrowAnswerRegister(b, i, escapes);

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, selectWidened(b, inputAt(i, 0), size));
  mi->opSize = escapes ? (uint8_t)sizeof(intptr_t) : size;

  if (escapes) {
    canonicalizeNarrowAnswer(b, i, dst, size);
  }
}

// '!x' is 'x == 0', which is 'sltiu rd, x, 1' - one instruction, and the only
// use of an unsigned compare against a constant this backend makes.
//
// '!' answers int whatever it was applied to, so the operand's width is not
// the answer's. sltiu writes the whole register either way, so opSize says the
// register and srcSize the width the test is made at.
static void selectLogicalNot(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *operand = inputAt(i, 0);
  uint8_t size = valueSize(operand);

  MachineInstr *mi = buildMachineInstr(b, RISCV_SLTU, 1, 2);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, selectWidened(b, operand, size));
  setImmediateOperand(mi, 2, 1);
  mi->opSize = sizeof(intptr_t);
  mi->srcSize = size;
}

// -============================ Floats ====================================-

// A float constant's bits, at the width it is stored in. The IR keeps every
// float constant at long-double precision, so this is where it is rounded to
// the one it actually has.
static int64_t floatConstantBits(const IrInstruction *i) {
  float80_const_t v = i->info.constant.data.f;

  if (i->type == IR_F32) {
    float f = (float)v;
    uint32_t bits = 0;
    memcpy(&bits, &f, sizeof(bits));
    // Sign-extended, because that is what 'li' will build and what fmv.w.x
    // reads the low half of.
    return (int64_t)(int32_t)bits;
  }

  double d = (double)v;
  uint64_t bits = 0;
  memcpy(&bits, &d, sizeof(bits));
  return (int64_t)bits;
}

static void selectFloatBinary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, machineBuilderVreg(b, inputAt(i, 0)));
  setRegisterOperand(mi, 2, machineBuilderVreg(b, inputAt(i, 1)));
  mi->opSize = size;
}

// feq/flt/fle and nothing else, so > and >= swap their operands and != is the
// negation of ==. All three are quiet on a quiet NaN and answer false, which
// is what C says every one of these comparisons does - and it is why != is a
// negation rather than a comparison of its own: 'a != b' is true when either
// is a NaN, which is exactly '!(a == b)'.
static void selectFloatCompare(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *lhs = inputAt(i, 0);
  const IrInstruction *rhs = inputAt(i, 1);
  uint8_t size = valueSize(lhs);
  uint32_t dst = machineBuilderVreg(b, i);

  uint32_t opcode = RISCV_FEQ;
  Boolean swap = FALSE, invert = FALSE;

  switch (i->kind) {
  case IR_E_FEQ: opcode = RISCV_FEQ; break;
  case IR_E_FNE: opcode = RISCV_FEQ; invert = TRUE; break;
  case IR_E_FLT: opcode = RISCV_FLT; break;
  case IR_E_FLE: opcode = RISCV_FLE; break;
  case IR_E_FGT: opcode = RISCV_FLT; swap = TRUE; break;
  case IR_E_FGE: opcode = RISCV_FLE; swap = TRUE; break;
  default: unreachable("not a float comparison");
  }

  uint32_t l = machineBuilderVreg(b, lhs);
  uint32_t r = machineBuilderVreg(b, rhs);
  uint32_t answer = invert ? createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t)) : dst;

  // The answer lands in an *integer* register, so opSize is that register's
  // width and srcSize carries the precision the comparison is made at. Using
  // opSize for the precision would say this def is four bytes of an eight-byte
  // register, which is a partial def and is not what happens.
  MachineInstr *cmp = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(cmp, 0, answer);
  setRegisterOperand(cmp, 1, swap ? r : l);
  setRegisterOperand(cmp, 2, swap ? l : r);
  cmp->opSize = sizeof(intptr_t);
  cmp->srcSize = size;

  if (invert) {
    MachineInstr *flip = buildMachineInstr(b, RISCV_XOR, 1, 2);
    setRegisterOperand(flip, 0, dst);
    setRegisterOperand(flip, 1, answer);
    setImmediateOperand(flip, 2, 1);
    flip->opSize = sizeof(intptr_t);
  }
}

// A conversion with a float on one side or both.
static void selectFloatConversion(MachineBuilder *b, const IrInstruction *i,
                                  enum IrTypeKind fromType, enum IrTypeKind toType) {
  uint8_t toSize = valueSize(i);
  const IrInstruction *value = inputAt(i, 0);
  uint8_t fromSize = valueSize(value);
  uint32_t dst = machineBuilderVreg(b, i);

  Boolean fromFloat = isFloatIrType(fromType);
  Boolean toFloat = isFloatIrType(toType);

  if (fromFloat && toFloat) {
    // Same width is a move; float <-> double changes the bits completely and
    // is its own instruction.
    MachineInstr *mi = buildMachineInstr(b, fromSize == toSize ? MOP_COPY : RISCV_FCVT_F2F, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
    mi->opSize = toSize;
    mi->srcSize = fromSize;
    return;
  }

  if (toType == IR_BOOL) {
    // 'x != 0.0', which is a compare against a zero this has to make: there is
    // no float register hardwired to it the way x0 is.
    uint32_t zeroBits = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));
    uint32_t zero = createVirtualRegister(b->mf, RC_FP, fromSize);
    uint32_t equal = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

    MachineInstr *li = buildMachineInstr(b, RISCV_LI, 1, 1);
    setRegisterOperand(li, 0, zeroBits);
    setImmediateOperand(li, 1, 0);
    li->opSize = sizeof(intptr_t);

    MachineInstr *mv = buildMachineInstr(b, RISCV_FMV_F, 1, 1);
    setRegisterOperand(mv, 0, zero);
    setRegisterOperand(mv, 1, zeroBits);
    mv->opSize = fromSize;

    MachineInstr *cmp = buildMachineInstr(b, RISCV_FEQ, 1, 2);
    setRegisterOperand(cmp, 0, equal);
    setRegisterOperand(cmp, 1, machineBuilderVreg(b, value));
    setRegisterOperand(cmp, 2, zero);
    cmp->opSize = sizeof(intptr_t);
    cmp->srcSize = fromSize;

    MachineInstr *flip = buildMachineInstr(b, RISCV_XOR, 1, 2);
    setRegisterOperand(flip, 0, dst);
    setRegisterOperand(flip, 1, equal);
    setImmediateOperand(flip, 2, 1);
    flip->opSize = sizeof(intptr_t);
    return;
  }

  if (toFloat) {
    // integer -> float. The integer forms are four and eight bytes only, so a
    // narrower source is widened by its own signedness first - which is exact,
    // and is what C says the conversion does.
    uint8_t srcSize = fromSize < 4 ? 4 : fromSize;
    uint32_t src = selectWidened(b, value, srcSize);

    MachineInstr *mi = buildMachineInstr(
        b, isUnsignedIrOperand(fromType) ? RISCV_FCVT_U2F : RISCV_FCVT_I2F, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, src);
    mi->opSize = toSize;
    mi->srcSize = srcSize;
    return;
  }

  // float -> integer, truncating toward zero, which is what a C cast does. The
  // instruction converts to four or eight bytes; a narrower destination takes
  // the low ones, and the value is in range by C's rules whenever the result is
  // defined at all. The destination register is written whole, so opSize says
  // the register and srcSize the float.
  Boolean isUnsigned = isUnsignedIrOperand(toType);
  uint8_t intSize = toSize < 4 ? 4 : toSize;

  MachineInstr *mi = buildMachineInstr(
      b, isUnsigned ? RISCV_FCVT_F2U : RISCV_FCVT_F2I, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
  mi->opSize = intSize;
  mi->srcSize = fromSize;
}

// -============================ Conversions ===============================-
//
static void selectConversion(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind fromType = i->info.fromCastType;
  enum IrTypeKind toType = i->type;
  uint8_t toSize = valueSize(i);
  const IrInstruction *value = inputAt(i, 0);
  uint32_t dst = machineBuilderVreg(b, i);

  assert(fromType != IR_P_AGG && toType != IR_P_AGG &&
         "a conversion to or from an aggregate");

  if (isFloatIrType(fromType) || isFloatIrType(toType)) {
    selectFloatConversion(b, i, fromType, toType);
    return;
  }

  // A conversion to _Bool is the one that is not a change of width: C defines
  // it as 'x != 0', so (_Bool)0x100 is 1 rather than the low byte of 0x100.
  if (toType == IR_BOOL) {
    MachineInstr *mi = buildMachineInstr(b, RISCV_SLTU, 1, 2);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, X_ZERO);
    setRegisterOperand(mi, 2, machineBuilderVreg(b, value));
    mi->opSize = sizeof(intptr_t);
    return;
  }

  // Nothing sits above a 64-bit value, so a conversion to one is a move; a
  // narrower source is widened by its own signedness on the way, which is what
  // C says a conversion does.
  if (toSize >= sizeof(intptr_t)) {
    uint32_t src = selectWidened(b, value, toSize);

    MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, src);
    mi->opSize = toSize;
    return;
  }

  // Everything else re-establishes the destination type's canonical form. That
  // covers narrowing - where the bits above the result are the source's and
  // would otherwise be read by the next compare - and a same-width change of
  // signedness, where 'char' and 'unsigned char' disagree about what the bits
  // above the byte should be.
  Boolean isUnsigned = canonicalIsUnsigned(toType, toSize);
  uint32_t src = machineBuilderIsFolded(b, value) ? NO_REG : machineBuilderVreg(b, value);

  if (src == NO_REG) {
    // A folded constant is already the value, and buildConstantInto puts it in
    // the destination type's canonical form, so nothing extends it afterwards.
    buildConstantInto(b, dst, value, toSize);
    return;
  }

  if (machineTakeRegisterExtension(b->mf, src, toSize, isUnsigned)) {
    MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, src);
    mi->opSize = toSize;
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, isUnsigned ? RISCV_ZEXT : RISCV_SEXT, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  mi->opSize = toSize;
  mi->srcSize = toSize;

  machineNoteRegisterExtension(b->mf, dst, toSize, isUnsigned);
}

// -============================ Comparisons ===============================-
//
// RV64 has two of the six orderings - 'less than', signed and unsigned - and
// everything else comes from swapping the operands or inverting the answer.
// Equality has neither form and is a subtraction tested against zero.

typedef struct _CompareForm {
  uint32_t opcode;   // RISCV_SLT or RISCV_SLTU
  Boolean swap;      // compare the operands the other way round
  Boolean invert;    // and then flip the answer
} CompareForm;

static CompareForm compareFormFor(enum IrIntructionKind kind, Boolean isUnsigned) {
  uint32_t slt = isUnsigned ? RISCV_SLTU : RISCV_SLT;
  CompareForm f = { slt, FALSE, FALSE };

  switch (kind) {
  case IR_E_LT: break;
  case IR_E_GT: f.swap = TRUE; break;
  case IR_E_LE: f.swap = TRUE; f.invert = TRUE; break;
  case IR_E_GE: f.invert = TRUE; break;
  default: unreachable("not an ordered integer comparison");
  }

  return f;
}

// The width both operands are compared at. A 32-bit value is kept sign-
// extended in its register, and sign extension preserves unsigned order among
// 32-bit values as well as signed order, so the comparison is done at 64 bits
// either way and needs no masking.
static uint8_t compareOperandSize(const IrInstruction *i) {
  uint8_t size = valueSize(inputAt(i, 0));
  uint8_t rhsSize = valueSize(inputAt(i, 1));
  return size > rhsSize ? size : rhsSize;
}

static void selectCompare(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *lhs = inputAt(i, 0);
  const IrInstruction *rhs = inputAt(i, 1);
  uint8_t size = compareOperandSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  uint32_t l = selectWidened(b, lhs, size);
  uint32_t r = selectWidened(b, rhs, size);

  if (i->kind == IR_E_EQ || i->kind == IR_E_NE) {
    // The difference is zero exactly when they are equal, so 'sltiu d, 1'
    // answers == and 'sltu d, x0, d' answers !=.
    uint32_t diff = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

    MachineInstr *sub = buildMachineInstr(b, RISCV_SUB, 1, 2);
    setRegisterOperand(sub, 0, diff);
    setRegisterOperand(sub, 1, l);
    setRegisterOperand(sub, 2, r);
    sub->opSize = sizeof(intptr_t);

    MachineInstr *test = buildMachineInstr(b, RISCV_SLTU, 1, 2);
    setRegisterOperand(test, 0, dst);
    if (i->kind == IR_E_EQ) {
      setRegisterOperand(test, 1, diff);
      setImmediateOperand(test, 2, 1);
    } else {
      setRegisterOperand(test, 1, X_ZERO);
      setRegisterOperand(test, 2, diff);
    }
    test->opSize = sizeof(intptr_t);
    return;
  }

  CompareForm f = compareFormFor(i->kind, isUnsignedIrOperand(lhs->type));
  uint32_t answer = f.invert ? createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t)) : dst;

  MachineInstr *slt = buildMachineInstr(b, f.opcode, 1, 2);
  setRegisterOperand(slt, 0, answer);
  setRegisterOperand(slt, 1, f.swap ? r : l);
  setRegisterOperand(slt, 2, f.swap ? l : r);
  // A comparison answers int however narrow its operands are, and slt writes
  // the whole register - so the operand width is srcSize, not opSize.
  slt->opSize = sizeof(intptr_t);
  slt->srcSize = size;

  if (f.invert) {
    MachineInstr *flip = buildMachineInstr(b, RISCV_XOR, 1, 2);
    setRegisterOperand(flip, 0, dst);
    setRegisterOperand(flip, 1, answer);
    setImmediateOperand(flip, 2, 1);
    flip->opSize = sizeof(intptr_t);
  }
}

// -============================ Branches ==================================-

typedef struct _BranchForm {
  uint32_t opcode;
  Boolean swap;
} BranchForm;

static uint32_t invertedBranch(uint32_t opcode) {
  switch (opcode) {
  case RISCV_BEQ: return RISCV_BNE;
  case RISCV_BNE: return RISCV_BEQ;
  case RISCV_BLT: return RISCV_BGE;
  case RISCV_BGE: return RISCV_BLT;
  case RISCV_BLTU: return RISCV_BGEU;
  case RISCV_BGEU: return RISCV_BLTU;
  default: unreachable("not a conditional branch");
  }
}

static BranchForm branchFormFor(enum IrIntructionKind kind, Boolean isUnsigned) {
  BranchForm f = { RISCV_BEQ, FALSE };

  switch (kind) {
  case IR_E_EQ: f.opcode = RISCV_BEQ; break;
  case IR_E_NE: f.opcode = RISCV_BNE; break;
  case IR_E_LT: f.opcode = isUnsigned ? RISCV_BLTU : RISCV_BLT; break;
  case IR_E_GE: f.opcode = isUnsigned ? RISCV_BGEU : RISCV_BGE; break;
  // No 'greater' branch exists, so the operands go the other way round.
  case IR_E_GT: f.opcode = isUnsigned ? RISCV_BLTU : RISCV_BLT; f.swap = TRUE; break;
  case IR_E_LE: f.opcode = isUnsigned ? RISCV_BGEU : RISCV_BGE; f.swap = TRUE; break;
  default: unreachable("not an integer comparison");
  }

  return f;
}

static Boolean isIntegerComparison(const IrInstruction *i) {
  switch (i->kind) {
  case IR_E_EQ: case IR_E_NE: case IR_E_LT:
  case IR_E_LE: case IR_E_GT: case IR_E_GE:
    return TRUE;
  default:
    return FALSE;
  }
}

// The two registers a conditional branch compares, and which branch to use.
// When the condition is a comparison the driver placed for folding, this is
// the comparison's own operands and no boolean is ever materialized; otherwise
// it is 'the value against x0', which is what C means by a condition.
static BranchForm branchOperands(MachineBuilder *b, const IrInstruction *cond,
                                 uint32_t *lhs, uint32_t *rhs) {
  if (machineBuilderIsAbsorbed(b, cond)) {
    uint8_t size = compareOperandSize(cond);
    uint32_t l = selectWidened(b, inputAt(cond, 0), size);
    uint32_t r = selectWidened(b, inputAt(cond, 1), size);

    BranchForm f = branchFormFor(cond->kind, isUnsignedIrOperand(inputAt(cond, 0)->type));
    *lhs = f.swap ? r : l;
    *rhs = f.swap ? l : r;
    return f;
  }

  BranchForm f = { RISCV_BNE, FALSE };
  *lhs = machineBuilderVreg(b, cond);
  *rhs = X_ZERO;
  return f;
}

static void selectJumpTo(MachineBuilder *b, const IrBasicBlock *target) {
  // A jump to the block that comes next is not a jump.
  if (machineBuilderFallsThroughTo(b, target)) {
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, RISCV_J, 0, 1);
  setBlockOperand(mi, 0, machineBuilderBlock(b, target));
}

static void buildBranch(MachineBuilder *b, uint32_t opcode, uint32_t lhs, uint32_t rhs,
                        const IrBasicBlock *target) {
  MachineInstr *mi = buildMachineInstr(b, opcode, 0, 3);
  setRegisterOperand(mi, 0, lhs);
  setRegisterOperand(mi, 1, rhs);
  setBlockOperand(mi, 2, machineBuilderBlock(b, target));
}

static void selectCondBranch(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *cond = inputAt(i, 0);
  const IrBasicBlock *taken = i->info.branch.taken;
  const IrBasicBlock *notTaken = i->info.branch.notTaken;

  uint32_t lhs = NO_REG, rhs = NO_REG;
  BranchForm f = branchOperands(b, cond, &lhs, &rhs);

  if (machineBuilderFallsThroughTo(b, notTaken)) {
    buildBranch(b, f.opcode, lhs, rhs, taken);
    return;
  }

  if (machineBuilderFallsThroughTo(b, taken)) {
    // Branch on the condition being false and fall into the taken block.
    // Inverting is free; jumping over a jump is not.
    buildBranch(b, invertedBranch(f.opcode), lhs, rhs, notTaken);
    return;
  }

  buildBranch(b, f.opcode, lhs, rhs, taken);

  MachineInstr *jmp = buildMachineInstr(b, RISCV_J, 0, 1);
  setBlockOperand(jmp, 0, machineBuilderBlock(b, notTaken));
}

// -============================ Switches ==================================-
//
// A chain of compare-and-branch, and no jump table. A table would want the
// addresses of this function's own blocks in .rodata, an indirect jump through
// one of them, and a bounds test around the pair; the chain is correct for
// every switch and costs only time, so the table is a quality item rather than
// a missing rule. x86 builds one when the cases are dense enough - see
// switchTableRange there.
static void selectCaseTest(MachineBuilder *b, uint32_t condReg, uint8_t size,
                           Boolean isUnsigned, int64_t caseConst,
                           const IrBasicBlock *target) {
  // RV64 branches compare two registers and no immediate, so the case value
  // needs one - in the same canonical form the switch operand is in, or a
  // negative int would compare unequal to itself.
  uint32_t k = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  MachineInstr *li = buildMachineInstr(b, RISCV_LI, 1, 1);
  setRegisterOperand(li, 0, k);
  setImmediateOperand(li, 1, canonicalValue(caseConst, size, isUnsigned));
  li->opSize = sizeof(intptr_t);

  buildBranch(b, RISCV_BEQ, condReg, k, target);
}

static void selectTableBranch(MachineBuilder *b, const IrInstruction *i) {
  const SwitchTable *table = i->info.switchTable;
  const IrInstruction *cond = inputAt(i, 0);
  uint8_t size = valueSize(cond);
  Boolean isUnsigned = canonicalIsUnsigned(cond->type, size);
  uint32_t condReg = machineBuilderVreg(b, cond);

  for (uint32_t c = 0; c < table->caseCount; ++c) {
    selectCaseTest(b, condReg, size, isUnsigned, table->caseBlocks[c].caseConst,
                   table->caseBlocks[c].block);
  }

  selectJumpTo(b, table->defaultBB);
}

// -============================ Memory ====================================-

static void selectFrameAddress(MachineBuilder *b, const IrInstruction *i, int32_t frameIdx) {
  const MachineFrameObject *obj = machineFrameObjectAt(b->mf, frameIdx);
  assert(!obj->isDynamic && "a dynamic alloca needs its own rule");

  // The slot's address, not what is in it - 'addi rd, s0, off'.
  MachineInstr *mi = buildMachineInstr(b, RISCV_LA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setFrameAddressOperand(mi, 1, frameIdx);
  mi->opSize = sizeof(intptr_t);
}

// A register holding a pointer's value.
//
// Not simply its virtual register: a pointer whose address folded into an
// addressing mode may have been *absorbed*, and an absorbed value computes
// nothing and defines no register - every use was supposed to have taken it
// over. On a target with a scaled-index mode every use can, which is why this
// question does not arise on x86-64. Here a GEP with a variable offset cannot,
// so the address it was going to fold has to be computed after all.
static uint32_t addressIntoRegister(MachineBuilder *b, const IrInstruction *ptr) {
  const AddressFold *f = machineBuilderAddressFold(b, ptr);

  if (f == NULL) {
    return machineBuilderVreg(b, ptr);
  }

  MachineAddress addr = addressFor(b, ptr, 0);
  uint32_t dst = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  MachineInstr *la = buildMachineInstr(b, RISCV_LA, 1, 1);
  setRegisterOperand(la, 0, dst);
  setMemoryOperand(la, 1, &addr);
  la->opSize = sizeof(intptr_t);

  return dst;
}

static void selectGep(MachineBuilder *b, const IrInstruction *i) {
  // A GEP the driver could not turn into an addressing mode - a variable
  // offset, or a constant too big for a displacement - is arithmetic, and on
  // this target that is the common case rather than the exception: there is no
  // index register for the walk to put a term in. See foldValue in
  // src/ir/codegen/isel.c for where it gives up.
  if (machineBuilderAddressFold(b, i) == NULL) {
    const IrInstruction *base = inputAt(i, 0);
    const IrInstruction *offset = inputAt(i, 1);

    uint32_t baseReg = addressIntoRegister(b, base);
    Boolean folded = machineBuilderIsFolded(b, offset);
    uint32_t offsetReg = folded ? NO_REG : selectWidened(b, offset, sizeof(intptr_t));

    MachineInstr *add = buildMachineInstr(b, RISCV_ADD, 1, 2);
    setRegisterOperand(add, 0, machineBuilderVreg(b, i));
    setRegisterOperand(add, 1, baseReg);
    if (folded) {
      setValueOperand(b, add, 2, offset);
    } else {
      setRegisterOperand(add, 2, offsetReg);
    }
    add->opSize = sizeof(intptr_t);
    return;
  }

  MachineAddress addr = addressFor(b, i, 0);

  MachineInstr *mi = buildMachineInstr(b, RISCV_LA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = sizeof(intptr_t);
}

static void selectMemoryLoad(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;
  assert(isAddressableIrType(t) && "a load of a value with no register class");

  MachineAddress addr = addressFor(b, inputAt(i, 0), 0);
  const uint8_t size = irTypeMachineSize(t);
  const uint32_t dst = machineBuilderVreg(b, i);

  if (isFloatIrType(t)) {
    MachineInstr *mi = buildMachineInstr(b, RISCV_FLOAD, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setMemoryOperand(mi, 1, &addr);
    mi->opSize = size;
    return;
  }

  // A narrow load extends as it loads - lb/lh sign-extend and lbu/lhu do not -
  // so the register holds the value at full width and nothing has to widen it
  // afterwards. lw always sign-extends, which is the canonical form for a
  // 32-bit value; an unsigned int wanted at 64 bits is widened by the cast
  // the IR spells out.
  //
  // The two widths are therefore different things and both are said: srcSize is
  // how many bytes come out of memory, and opSize is the whole register,
  // because that is what the load writes. Saying the value's own width instead
  // would make this a *partial* def the moment a later use accepted the
  // extension noted below - machineTakeRegisterExtension grows the register to
  // the width it was extended to, and the def would then be narrower than the
  // register it wholly wrote. On x86 the same load really is partial unless it
  // is spelled movzx/movsx; here there is no such form because there is no
  // such thing.
  // Only a byte or a halfword uses the zero-extending form. A four-byte load
  // is 'lw', which sign-extends, *whatever* the value's signedness: that is
  // the canonical form for a 32-bit value here (see canonicalIsUnsigned), it
  // is what every 'w' instruction produces, and it is what makes one
  // representation of one value. An unsigned int wanted at sixty-four bits is
  // zero-extended by the cast the IR spells out.
  //
  // Using 'lwu' instead and noting the register as sign-extended - which is
  // what this did first - is the two halves disagreeing: the note is what a
  // later use skips its own extension on the strength of.
  Boolean extendsWithZero = isUnsignedIrOperand(t) && size < 4;
  MachineInstr *mi = buildMachineInstr(b, extendsWithZero ? RISCV_LOADU : RISCV_LOAD, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = sizeof(intptr_t);
  mi->srcSize = size;

  if (size < sizeof(intptr_t)) {
    machineNoteRegisterExtension(b->mf, dst, sizeof(intptr_t), extendsWithZero);
  }
}

static void selectMemoryStore(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;
  assert(isAddressableIrType(t) && "a store of a value with no register class");

  uint8_t size = irTypeMachineSize(t);
  const IrInstruction *value = inputAt(i, 1);
  MachineAddress addr;

  if (isFloatIrType(t)) {
    uint32_t src = machineBuilderVreg(b, value);
    addr = addressFor(b, inputAt(i, 0), 0);

    MachineInstr *mi = buildMachineInstr(b, RISCV_FSTORE, 0, 2);
    setMemoryOperand(mi, 0, &addr);
    setRegisterOperand(mi, 1, src);
    mi->opSize = size;
    return;
  }

  // A store writes exactly its own width, so nothing has to be widened first -
  // unlike x86, where the same is true but the value may arrive in a register
  // whose upper bytes are the previous value's.
  uint32_t src = machineBuilderIsFolded(b, value)
                     ? NO_REG
                     : machineBuilderVreg(b, value);

  if (src == NO_REG) {
    // A folded constant has no register and a store has no immediate form, so
    // it is materialized here rather than at every use.
    src = createVirtualRegister(b->mf, RC_GP, size);
    buildConstantInto(b, src, value, size);
  }

  addr = addressFor(b, inputAt(i, 0), 0);

  MachineInstr *mi = buildMachineInstr(b, RISCV_STORE, 0, 2);
  setMemoryOperand(mi, 0, &addr);
  setRegisterOperand(mi, 1, src);
  mi->opSize = size;
}

// -============================ Block copy ================================-

// An aggregate assignment, unrolled into load/store pairs. RV64 has no string
// move to fall back on - x86's 'rep movsb' has no counterpart - and selection
// may not invent the basic blocks a loop would need, so the count has to be
// known here. It always is: C has no assignment of an object whose size is not
// known, a VLA cannot be assigned and a flexible array member is not part of
// its struct's size, which is the same reasoning selectMemoryCopy states on
// the other target.
static void selectMemoryCopy(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *count = inputAt(i, 2);

  assert(count->kind == IR_DEF_CONST && count->info.constant.kind == IR_CK_INTEGER &&
         count->info.constant.data.i >= 0 &&
         "a block copy whose size is not known needs a call to memcpy");

  int32_t bytes = (int32_t)count->info.constant.data.i;

  // Both addresses once, outside the loop: the chunks differ only in their
  // displacement, and an address built by arithmetic would otherwise be built
  // again for every chunk.
  MachineAddress from = addressFor(b, inputAt(i, 1), 0);
  MachineAddress to = addressFor(b, inputAt(i, 0), 0);

  // One register per chunk width rather than one for all of them: a register
  // whose width is the load's is a register the spill of it writes whole.
  uint32_t tmp[4] = { NO_REG, NO_REG, NO_REG, NO_REG };

  for (int32_t done = 0; done < bytes;) {
    int32_t left = bytes - done;
    uint8_t chunk = left >= 8 ? 8 : left >= 4 ? 4 : left >= 2 ? 2 : 1;
    uint32_t *slot = &tmp[chunk == 8 ? 3 : chunk == 4 ? 2 : chunk == 2 ? 1 : 0];

    if (*slot == NO_REG) {
      *slot = createVirtualRegister(b->mf, RC_GP, chunk);
    }

    MachineAddress fromChunk = from;
    MachineAddress toChunk = to;
    fromChunk.disp += done;
    toChunk.disp += done;

    // Unsigned, so that a byte or halfword of somebody else's bytes is not
    // sign-extended into the register and then stored back narrowed - the
    // store would write the same bytes either way, but the load is what the
    // allocator sees and a zero-extended one is what it is.
    MachineInstr *load = buildMachineInstr(b, RISCV_LOADU, 1, 1);
    setRegisterOperand(load, 0, *slot);
    setMemoryOperand(load, 1, &fromChunk);
    load->opSize = chunk;
    load->srcSize = chunk;

    MachineInstr *store = buildMachineInstr(b, RISCV_STORE, 0, 2);
    setMemoryOperand(store, 0, &toChunk);
    setRegisterOperand(store, 1, *slot);
    store->opSize = chunk;

    done += chunk;
  }
}

// -============================ Constants and registers ===================-

static void selectConstant(MachineBuilder *b, const IrInstruction *i) {
  // Only the constants no use could take as an immediate reach here; the
  // driver dropped the rest (see decideConstants in src/ir/codegen/isel.c).
  if (i->info.constant.kind == IR_CK_SYMBOL) {
    // A global's address. Under medlow that is an absolute lui/addi pair, and
    // the linker fills both halves in - see RK_RISCV_HI20 in instructions.h.
    MachineAddress addr = { MAK_SYMBOL, NO_REG, NO_REG, 0, 0,
                            { i->info.constant.data.s } };
    MachineInstr *mi = buildMachineInstr(b, RISCV_LA, 1, 1);
    setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
    setMemoryOperand(mi, 1, &addr);
    mi->opSize = sizeof(intptr_t);
    return;
  }

  if (i->info.constant.kind == IR_CK_LITERAL) {
    uint32_t constantIdx = addMachineConstant(b->mf, MCK_BYTES, i->info.constant.data.l.s,
                                              i->info.constant.data.l.length, 1);

    MachineAddress addr = { MAK_CONSTANT, NO_REG, NO_REG, 0, 0 };
    addr.anchor.constantIdx = constantIdx;

    MachineInstr *mi = buildMachineInstr(b, RISCV_LA, 1, 1);
    setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
    setMemoryOperand(mi, 1, &addr);
    mi->opSize = sizeof(intptr_t);
    return;
  }

  if (i->info.constant.kind == IR_CK_FLOAT) {
    // RV64 has no float immediate and no need of a constant pool for one: the
    // bits go into an integer register, which 'li' can build to any width, and
    // fmv moves them across unchanged. x86 does the same thing for the same
    // reason - there is no 'mov xmm, imm' either.
    uint8_t size = valueSize(i);
    uint32_t bits = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

    MachineInstr *li = buildMachineInstr(b, RISCV_LI, 1, 1);
    setRegisterOperand(li, 0, bits);
    setImmediateOperand(li, 1, floatConstantBits(i));
    li->opSize = sizeof(intptr_t);

    MachineInstr *mv = buildMachineInstr(b, RISCV_FMV_F, 1, 1);
    setRegisterOperand(mv, 0, machineBuilderVreg(b, i));
    setRegisterOperand(mv, 1, bits);
    mv->opSize = size;
    return;
  }

  assert(i->info.constant.kind == IR_CK_INTEGER && "no rule for this constant kind");

  buildConstantInto(b, machineBuilderVreg(b, i), i, valueSize(i));
}

static void selectPhysReg(MachineBuilder *b, const IrInstruction *i) {
  // Read out of the fixed register the ABI put the value in, into one the
  // allocator is free to move.
  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, i->info.physReg);
  mi->opSize = valueSize(i);
}

// a0 then a1, fa0 then fa1. The two files are counted separately, so an
// aggregate of a double and a long comes back in fa0 and a0.
static uint32_t retRegOf(const TargetDescriptor *target, enum RegClass rc, uint32_t idx) {
  assert(idx < 2 && "no aggregate comes back in more than two registers");

  if (rc == RC_FP) {
    return idx == 0 ? target->fpRetReg : target->fpRetReg2;
  }

  return idx == 0 ? target->intRetReg : target->intRetReg2;
}

// -============================ Calls =====================================-
//
// LP64D: a0..a7 and fa0..fa7, then the stack, with each stack argument
// occupying a whole eightbyte. Two things differ from SysV and both are here
// rather than in the shared classification:
//
//   * A *variadic* call passes floating-point arguments in integer registers.
//     That is the ABI, and it is why a float argument to one goes through
//     fmv.x.d on its way. This treats every argument of a variadic call that
//     way, where the ABI says only the unnamed ones - the named parameters of
//     a variadic function are a prototype this IR does not carry, and nothing
//     in the corpus declares a variadic function whose named parameter is a
//     float.
//   * There is no register holding a count of float arguments; nothing
//     corresponds to x86's 'mov al, N'.

static size_t firstCallArgIndex(void) {
  // Input 0 is the callee; everything after it is an argument.
  return 1;
}

// Which register file an argument travels in. A variadic call sends everything
// through the integer one - see above.
static enum RegClass callArgClass(const IrInstruction *call, const IrInstruction *arg) {
  if (call->info.call.isVariadic) {
    return RC_GP;
  }

  return isFloatIrType(arg->type) ? RC_FP : RC_GP;
}

static uint32_t argRegCountOf(const TargetDescriptor *target, enum RegClass rc) {
  return rc == RC_FP ? target->fpArgRegCount : target->intArgRegCount;
}

static uint32_t argRegOf(const TargetDescriptor *target, enum RegClass rc, uint32_t idx) {
  return rc == RC_FP ? target->fpArgRegs[idx] : target->intArgRegs[idx];
}

// What an aggregate passed as bytes takes in the outgoing area: its size
// rounded to a whole number of eightbytes, and the alignment the callee will
// read it at. Both come from the callee's view - classifyParametersGeneric -
// because only the caller can put the bytes where the callee looks.
static void memArgShape(const IrInstruction *arg, uint32_t *size, uint32_t *align) {
  assert(arg->astType != NULL && isPointerLikeType(arg->astType));

  const TypeRef *type = arg->astType->pointed;

  *size = (uint32_t)ALIGN_SIZE(computeTypeSize(type), sizeof(intptr_t));
  *align = (uint32_t)max((int32_t)sizeof(intptr_t), typeAlignment(type));
}

static uint32_t memArgStackSlots(const IrInstruction *arg) {
  uint32_t size = 0, align = 0;
  memArgShape(arg, &size, &align);
  return size / (uint32_t)sizeof(intptr_t);
}

// Where one argument goes, or NO_REG when it goes on the stack. Walked from
// the start each time rather than tabulated: a call has few enough arguments
// that walking again is cheaper than the array, which is the same reasoning
// §6.8 gives for x86.
static uint32_t callArgLocation(const TargetDescriptor *target, const IrInstruction *call,
                                size_t inputIdx) {
  uint32_t used[RC_CLASS_COUNT] = {0};

  for (size_t idx = firstCallArgIndex(); idx < call->inputs.size;) {
    // How many inputs this argument is: two when the one behind it is the
    // second eightbyte of the same aggregate, which the ABI places together.
    size_t parts = 1;
    if (idx + 1 < call->inputs.size && isCallPairedArg(call, idx + 1)) {
      parts = 2;
    }

    uint32_t reg[2] = { NO_REG, NO_REG };

    // An aggregate passed as bytes consumes no register of either class: it is
    // on the stack because of what it is rather than because a class ran out,
    // so the arguments behind it are unaffected.
    if (!isCallMemoryArg(call, idx)) {
      uint32_t want[RC_CLASS_COUNT] = {0};
      for (size_t p = 0; p < parts; ++p) {
        want[callArgClass(call, inputAt(call, idx + p))] += 1;
      }

      // All of it or none of it, the way classifyParametersGeneric decides it
      // on the callee's side.
      Boolean fits = TRUE;
      for (uint32_t rc = 0; rc < RC_CLASS_COUNT; ++rc) {
        if (used[rc] + want[rc] > argRegCountOf(target, (enum RegClass)rc)) fits = FALSE;
      }

      if (fits) {
        for (size_t p = 0; p < parts; ++p) {
          enum RegClass rc = callArgClass(call, inputAt(call, idx + p));
          reg[p] = argRegOf(target, rc, used[rc]++);
        }
      }
    }

    if (inputIdx < idx + parts) {
      return reg[inputIdx - idx];
    }

    idx += parts;
  }

  unreachable("every input past the callee is an argument");
  return NO_REG;
}

// The outgoing area this call needs, and where one stack argument sits in it.
// Pass SIZE_MAX for wantedIdx to ask only for the total.
static uint32_t callStackArea(const TargetDescriptor *target, const IrInstruction *call,
                              size_t wantedIdx, int32_t *wantedOffset) {
  int32_t offset = 0;

  for (size_t idx = firstCallArgIndex(); idx < call->inputs.size; ++idx) {
    if (callArgLocation(target, call, idx) != NO_REG) {
      continue;
    }

    // A stack argument occupies a whole eightbyte however narrow it is; an
    // aggregate passed as bytes occupies as many as it is wide.
    uint32_t size = sizeof(intptr_t), align = sizeof(intptr_t);

    if (isCallMemoryArg(call, idx)) {
      memArgShape(inputAt(call, idx), &size, &align);
    }

    offset = (int32_t)ALIGN_SIZE(offset, align);

    if (idx == wantedIdx) {
      *wantedOffset = offset;
    }

    offset += (int32_t)size;
  }

  // LP64D wants sp sixteen-byte aligned at the call, and the frame size stage
  // 3 subtracts is rounded to 16, so rounding the area is the whole of it.
  return (uint32_t)ALIGN_SIZE(offset, 2 * sizeof(intptr_t));
}

// The classes of the eightbytes a call's result comes back in, or zero when it
// does not come back in registers at all. The call's astType is the return
// type; the IR type says only IR_P_AGG and cannot answer.
static uint32_t callResultEightbytes(const IrInstruction *call, enum EightbyteClass classes[2]) {
  classes[0] = classes[1] = EB_NONE;

  if (call->type != IR_P_AGG || call->info.call.returnBuffer != NULL || call->astType == NULL) {
    return 0;
  }

  return classifyComposite(call->astType, classes);
}

static void riscvReserveFrame(MachineFunction *mf) {
  uint32_t widest = 0;

  for (const MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (const IrInstruction *i = mbb->ir->instrunctions.head; i != NULL; i = i->next) {
      if (i->kind != IR_CALL && i->kind != IR_ICALL) {
        continue;
      }

      uint32_t bytes = callStackArea(mf->target, i, SIZE_MAX, NULL);

      if (bytes > widest) {
        widest = bytes;
      }
    }
  }

  mf->frame.outgoingSize = widest;
}

// The bits of a float in an integer register, for a variadic call.
static uint32_t floatBitsInGpRegister(MachineBuilder *b, const IrInstruction *arg) {
  uint8_t size = valueSize(arg);
  uint32_t bits = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  MachineInstr *mv = buildMachineInstr(b, RISCV_FMV_X, 1, 1);
  setRegisterOperand(mv, 0, bits);
  setRegisterOperand(mv, 1, machineBuilderVreg(b, arg));
  mv->opSize = sizeof(intptr_t);
  mv->srcSize = size;

  return bits;
}

// The value an argument contributes, in the register file it travels in.
static uint32_t callArgRegister(MachineBuilder *b, const IrInstruction *call,
                                const IrInstruction *arg) {
  if (call->info.call.isVariadic && isFloatIrType(arg->type)) {
    return floatBitsInGpRegister(b, arg);
  }

  uint8_t size = valueSize(arg);

  // Widened to the full register: LP64D says a narrow argument arrives
  // sign- or zero-extended to the register's width according to its own type,
  // which is stricter than SysV and is why this is not the argument's own
  // width the way x86 passes it.
  if (!isFloatIrType(arg->type) && size < sizeof(intptr_t)) {
    return selectWidened(b, arg, sizeof(intptr_t));
  }

  return machineBuilderVreg(b, arg);
}

// A slot of the outgoing area, which sits at the bottom of the frame and is
// therefore addressed from the stack pointer.
static MachineAddress outgoingArgAddress(const MachineFunction *mf, int32_t offset) {
  MachineAddress addr = { MAK_REG, mf->target->sp, NO_REG, 0, offset };
  return addr;
}

// An aggregate the ABI passes as bytes, copied into the outgoing area one
// eightbyte at a time - which is what the ABI asks for, the callee reading the
// bytes where the call left them.
//
// Reading up to seven bytes past the end is deliberate and safe: the source is
// the temporary translateCall copied into, and a slot is a whole eightbyte.
static void selectMemoryArgument(MachineBuilder *b, const IrInstruction *arg, int32_t offset) {
  uint32_t slots = memArgStackSlots(arg);

  // Once, outside the loop: an address that had to be computed would otherwise
  // be computed again per eightbyte.
  MachineAddress from = addressFor(b, arg, 0);
  uint32_t tmp = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  for (uint32_t slot = 0; slot < slots; ++slot) {
    int32_t disp = (int32_t)(slot * sizeof(intptr_t));

    MachineAddress chunk = from;
    chunk.disp += disp;

    MachineInstr *load = buildMachineInstr(b, RISCV_LOAD, 1, 1);
    setRegisterOperand(load, 0, tmp);
    setMemoryOperand(load, 1, &chunk);
    load->opSize = sizeof(intptr_t);
    load->srcSize = sizeof(intptr_t);

    MachineAddress to = outgoingArgAddress(b->mf, offset + disp);

    MachineInstr *store = buildMachineInstr(b, RISCV_STORE, 0, 2);
    setMemoryOperand(store, 0, &to);
    setRegisterOperand(store, 1, tmp);
    store->opSize = sizeof(intptr_t);
  }
}

// Give an aggregate returned in a0 (and a1, or the float pair) somewhere to
// live, and produce its address. Everything downstream of the call reads a
// composite as an address - that is what IR_P_AGG means - and the registers
// the callee handed back are not one, so this spends a frame slot on it. The
// buffered case needs none of it: there the callee has already written the
// aggregate into the slot the IR allocated.
static void selectRegisterReturnedStruct(MachineBuilder *b, const IrInstruction *i) {
  MachineFunction *mf = b->mf;

  enum EightbyteClass classes[2];
  uint32_t eightbytes = callResultEightbytes(i, classes);

  // Whole eightbytes, whatever the aggregate's own size: a register is stored
  // in full, and the bytes above the value are ours either way. Zero of them
  // means an empty struct, which still needs an address and no bytes.
  uint32_t bytes = (eightbytes == 0 ? 1u : eightbytes) * (uint32_t)sizeof(intptr_t);
  int32_t frameIdx = addMachineFrameObject(mf, MFO_CALL_RESULT, bytes, sizeof(intptr_t));

  mf->frame.size = (uint32_t)ALIGN_SIZE(
      placeMachineFrameObject(mf, (int32_t)mf->frame.size, frameIdx), 2 * sizeof(intptr_t));

  uint32_t used[RC_CLASS_COUNT] = {0};

  for (uint32_t eb = 0; eb < eightbytes; ++eb) {
    enum RegClass rc = classes[eb] == EB_SSE ? RC_FP : RC_GP;

    MachineInstr *store = buildMachineInstr(b, rc == RC_FP ? RISCV_FSTORE : RISCV_STORE, 0, 2);
    setFrameAddressOperandAt(store, 0, frameIdx, (int32_t)(eb * sizeof(intptr_t)));
    setRegisterOperand(store, 1, retRegOf(mf->target, rc, used[rc]++));
    store->opSize = sizeof(intptr_t);
  }

  MachineInstr *addr = buildMachineInstr(b, RISCV_LA, 1, 1);
  setRegisterOperand(addr, 0, machineBuilderVreg(b, i));
  setFrameAddressOperand(addr, 1, frameIdx);
  addr->opSize = sizeof(intptr_t);
}

static void selectCall(MachineBuilder *b, const IrInstruction *i) {
  const TargetDescriptor *target = b->mf->target;

  uint32_t numRegArgs = 0;

  // Stack arguments first, in the list's own order. The area was reserved for
  // the whole function before anything was selected, so nothing here moves the
  // stack pointer and there is nothing to give back afterwards.
  for (size_t idx = firstCallArgIndex(); idx < i->inputs.size; ++idx) {
    const IrInstruction *arg = inputAt(i, idx);

    if (callArgLocation(target, i, idx) != NO_REG) {
      numRegArgs += 1;
      continue;
    }

    int32_t offset = 0;
    callStackArea(target, i, idx, &offset);

    if (isCallMemoryArg(i, idx)) {
      selectMemoryArgument(b, arg, offset);
      continue;
    }

    Boolean asFloat = !i->info.call.isVariadic && isFloatIrType(arg->type);
    uint32_t src = callArgRegister(b, i, arg);

    MachineAddress to = outgoingArgAddress(b->mf, offset);

    MachineInstr *store = buildMachineInstr(b, asFloat ? RISCV_FSTORE : RISCV_STORE, 0, 2);
    setMemoryOperand(store, 0, &to);
    setRegisterOperand(store, 1, src);
    store->opSize = asFloat ? valueSize(arg) : (uint8_t)sizeof(intptr_t);
  }

  for (size_t idx = firstCallArgIndex(); idx < i->inputs.size; ++idx) {
    uint32_t reg = callArgLocation(target, i, idx);

    if (reg == NO_REG) {
      continue;
    }

    const IrInstruction *arg = inputAt(i, idx);
    uint32_t src = callArgRegister(b, i, arg);

    MachineInstr *copy = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(copy, 0, reg);
    setRegisterOperand(copy, 1, src);
    copy->opSize = isFloatIrType(arg->type) && !i->info.call.isVariadic
                       ? valueSize(arg)
                       : (uint8_t)sizeof(intptr_t);
  }

  // Defs: the return register, when the call produces something. Uses: the
  // callee, then every argument register the copies above filled in - implicit
  // because the instruction does not name them, but in the operand list all the
  // same, or nothing connects the copies to the call and liveness is free to
  // conclude they are dead.
  Boolean hasResult = i->type != IR_VOID;

  // A small aggregate is IR_P_AGG whichever file it comes back in and however
  // many registers that takes, so the return type has the answer where the IR
  // type does not.
  enum EightbyteClass resultClasses[2];
  uint32_t resultEightbytes = callResultEightbytes(i, resultClasses);
  uint16_t numDefs = hasResult ? (uint16_t)(resultEightbytes == 0 ? 1u : resultEightbytes) : 0;

  MachineInstr *call = buildMachineInstr(b, RISCV_CALL, numDefs, 1 + (uint16_t)numRegArgs);
  uint16_t op = 0;

  if (hasResult) {
    uint32_t used[RC_CLASS_COUNT] = {0};

    for (uint16_t def = 0; def < numDefs; ++def) {
      enum RegClass rc = resultEightbytes != 0
                             ? (resultClasses[def] == EB_SSE ? RC_FP : RC_GP)
                             : (isFloatIrType(i->type) ? RC_FP : RC_GP);
      setRegisterOperand(call, op, retRegOf(target, rc, used[rc]++));
      machineOperandAt(call, op)->flags.isImplicit = 1;
      op += 1;
    }
  }

  // A folded symbol constant becomes the relocated call target; anything else
  // is a register holding the address, and the call goes through it.
  setValueOperand(b, call, op++, inputAt(i, 0));

  for (size_t idx = firstCallArgIndex(); idx < i->inputs.size; ++idx) {
    uint32_t reg = callArgLocation(target, i, idx);

    if (reg != NO_REG) {
      setRegisterOperand(call, op, reg);
      machineOperandAt(call, op)->flags.isImplicit = 1;
      op += 1;
    }
  }

  assert(op == call->numOperands);
  call->flags.isCall = 1;

  if (i->type == IR_P_AGG && i->info.call.returnBuffer == NULL) {
    selectRegisterReturnedStruct(b, i);
  } else if (hasResult) {
    // Out of the fixed return register into one the allocator can move.
    MachineInstr *copy = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(copy, 0, machineBuilderVreg(b, i));
    setRegisterOperand(copy, 1, retRegOf(target, isFloatIrType(i->type) ? RC_FP : RC_GP, 0));
    copy->opSize = valueSize(i);
  }
}

// -============================ Returns ===================================-

static void selectReturn(MachineBuilder *b, const IrInstruction *i) {
  if (i->inputs.size != 0) {
    // One input per eightbyte, each into the return register of its own class.
    // A scalar is the one-input case of the same walk.
    uint32_t used[RC_CLASS_COUNT] = {0};

    for (size_t idx = 0; idx < i->inputs.size; ++idx) {
      const IrInstruction *part = inputAt(i, idx);
      enum RegClass rc = isFloatIrType(part->type) ? RC_FP : RC_GP;

      selectLoadInto(b, retRegOf(b->mf->target, rc, used[rc]++), part, valueSize(part));
    }
  }

  buildMachineInstr(b, RISCV_RET, 0, 0);
}

// -============================ The dispatch ==============================-

static void selectInstruction_riscv64(MachineBuilder *b, const IrInstruction *i) {
  // RV64's long double is IEEE binary128 and there is no hardware for it:
  // every operation is a libgcc call, which is a project of its own. Refused
  // where a value of the type is actually computed rather than where the type
  // is named - glibc's headers declare strtold and friends, so a translation
  // unit that includes <stdlib.h> mentions the type without using it.
  assert(i->type != IR_F80 && "long double is not supported on riscv64");

  int32_t frameIdx = machineFrameIndexForValue(b->mf, i);

  if (frameIdx >= 0) {
    selectFrameAddress(b, i, frameIdx);
    return;
  }

  switch (i->kind) {
  case IR_DEF_CONST: selectConstant(b, i); break;
  case IR_P_REG: selectPhysReg(b, i); break;

  case IR_E_ADD: selectBinary(b, i, RISCV_ADDW, RISCV_ADD, TRUE); break;
  case IR_E_SUB: selectBinary(b, i, RISCV_SUBW, RISCV_SUB, FALSE); break;
  case IR_E_MUL: selectBinary(b, i, RISCV_MULW, RISCV_MUL, FALSE); break;
  case IR_E_AND: selectBinaryWith(b, i, RISCV_AND, TRUE); break;
  case IR_E_OR:  selectBinaryWith(b, i, RISCV_OR, TRUE); break;
  case IR_E_XOR: selectBinaryWith(b, i, RISCV_XOR, TRUE); break;

  case IR_E_DIV:
    if (isUnsignedIrOperand(i->type)) {
      selectBinary(b, i, RISCV_DIVUW, RISCV_DIVU, FALSE);
    } else {
      selectBinary(b, i, RISCV_DIVW, RISCV_DIV, FALSE);
    }
    break;

  case IR_E_MOD:
    if (isUnsignedIrOperand(i->type)) {
      selectBinary(b, i, RISCV_REMUW, RISCV_REMU, FALSE);
    } else {
      selectBinary(b, i, RISCV_REMW, RISCV_REM, FALSE);
    }
    break;

  case IR_E_SHL: selectShift(b, i, RISCV_SLLW, RISCV_SLL); break;

  // Right shift is where signedness stops being cosmetic: an arithmetic shift
  // keeps the sign bit, a logical one does not.
  case IR_E_SHR:
    if (isUnsignedIrOperand(i->type)) {
      selectShift(b, i, RISCV_SRLW, RISCV_SRL);
    } else {
      selectShift(b, i, RISCV_SRAW, RISCV_SRA);
    }
    break;

  case IR_U_BNOT: selectUnary(b, i, RISCV_NOT); break;
  case IR_U_NOT: selectLogicalNot(b, i); break;
  case IR_E_BITCAST: selectConversion(b, i); break;

  case IR_E_FADD: selectFloatBinary(b, i, RISCV_FADD); break;
  case IR_E_FSUB: selectFloatBinary(b, i, RISCV_FSUB); break;
  case IR_E_FMUL: selectFloatBinary(b, i, RISCV_FMUL); break;
  case IR_E_FDIV: selectFloatBinary(b, i, RISCV_FDIV); break;

  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    selectFloatCompare(b, i);
    break;

  // Both spellings reach the same rule: what distinguishes a direct call from
  // an indirect one is whether the callee operand came out a symbol or a
  // register, and setValueOperand decides that from the value itself.
  case IR_CALL:
  case IR_ICALL:
    selectCall(b, i);
    break;

  case IR_GET_ELEMENT_PTR: selectGep(b, i); break;
  case IR_M_LOAD: selectMemoryLoad(b, i); break;
  case IR_M_STORE: selectMemoryStore(b, i); break;
  case IR_M_COPY: selectMemoryCopy(b, i); break;

  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    selectCompare(b, i);
    break;

  // The placeholder buildSSA leaves where a promoted local is read on a path
  // that never wrote it. Its value is whatever C says an uninitialized object
  // holds; zero is the one that makes the resulting misbehaviour reproducible.
  case IR_BAD: {
    uint8_t size = valueSize(i);
    uint32_t dst = machineBuilderVreg(b, i);
    Boolean viaGp = isFloatIrType(i->type);
    // 'li' writes an integer register, so a float one takes the zero the way a
    // float constant does - through a general register and across.
    uint32_t zero = viaGp ? createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t)) : dst;

    MachineInstr *mi = buildMachineInstr(b, RISCV_LI, 1, 1);
    setRegisterOperand(mi, 0, zero);
    setImmediateOperand(mi, 1, 0);
    mi->opSize = viaGp ? (uint8_t)sizeof(intptr_t) : size;

    if (viaGp) {
      MachineInstr *across = buildMachineInstr(b, RISCV_FMV_F, 1, 1);
      setRegisterOperand(across, 0, dst);
      setRegisterOperand(across, 1, zero);
      across->opSize = size;
    }
    break;
  }

  default:
    unreachable(irInstructionMnemonic(i->kind));
  }
}

static void selectTerminator_riscv64(MachineBuilder *b, const IrInstruction *term) {
  switch (term->kind) {
  case IR_BRANCH: selectJumpTo(b, term->info.branch.taken); break;
  case IR_CBRANCH: selectCondBranch(b, term); break;
  case IR_RET: selectReturn(b, term); break;
  case IR_TBRANCH: selectTableBranch(b, term); break;

  default:
    unreachable(irInstructionMnemonic(term->kind));
  }
}

// -============================ The target's answers ======================-

// RV64's I-type immediate is twelve bits, signed, and that is the whole of it.
// The operations with no immediate form at all - sub, mul, div, rem - are why
// this is asked per use rather than per constant.
static Boolean riscvIsLegalImmediate(const IrInstruction *use, size_t operandIdx,
                                     const IrInstruction *cnst) {
  Boolean isCall = use->kind == IR_CALL || use->kind == IR_ICALL;

  // The address of a directly called function, which the call encodes as a
  // relocated auipc/jalr pair rather than reading out of a register. Input 0
  // of a call and nowhere else: every other use of a function's address needs
  // it materialized, which selectConstant does.
  if (cnst->info.constant.kind == IR_CK_SYMBOL) {
    return isCall && operandIdx == 0;
  }

  if (cnst->info.constant.kind != IR_CK_INTEGER) {
    return FALSE;
  }

  // Only the right-hand operand of the few instructions that have an 'i' form,
  // and only a shift count that the shift itself can encode.
  if (operandIdx != 1) {
    return FALSE;
  }

  switch (use->kind) {
  case IR_E_ADD:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
    return isInt(cnst->info.constant.data.i, 12);

  case IR_E_SHL:
  case IR_E_SHR: {
    int64_t v = cnst->info.constant.data.i;
    return 0 <= v && v < (isWordOp(irTypeMachineSize(use->type)) ? 32 : 64);
  }

  default:
    return FALSE;
  }
}

// There is no scaled-index addressing mode: an address is one base register
// plus a twelve-bit signed displacement, and anything else is arithmetic the
// selector has to emit. So every non-zero scale is refused, and the GEP walk
// in isel.c stops one term earlier here than it does on x86-64.
static Boolean riscvIsLegalAddressMode(uint32_t scale, int64_t disp) {
  return scale == 0 && isInt(disp, 12);
}

static uint32_t riscvAddressOperands(const IrInstruction *i) {
  switch (i->kind) {
  case IR_M_LOAD: return 1u << 0;        // the pointer
  case IR_M_STORE: return 1u << 0;
  case IR_GET_ELEMENT_PTR: return 1u << 0;
  default: return 0;
  }
}

// A comparison is the branch itself on this target, so every integer one is
// worth folding - unlike x86, where the question is whether the flags survive.
static Boolean riscvFoldsIntoCondition(const IrInstruction *cond) {
  return isIntegerComparison(cond);
}

const ArchSelector riscvSelector = {
  // The outgoing-argument area: how wide it is is a fact about every call in
  // the function at once, and it has to be known before any of them is
  // selected.
  .reserveFrame = &riscvReserveFrame,
  .selectInstruction = &selectInstruction_riscv64,
  .selectTerminator = &selectTerminator_riscv64,
  .isLegalImmediate = &riscvIsLegalImmediate,
  .isLegalAddressMode = &riscvIsLegalAddressMode,
  .addressOperands = &riscvAddressOperands,
  .foldsIntoCondition = &riscvFoldsIntoCondition
};
