#include <assert.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "machine_x86_64.h"
#include "instructions_x86_64.h"

// -============================ x86-64 instruction selection ==============-
//
// One IR instruction at a time, into the two-address form the ISA actually
// has. See docs/ir-codegen-design.md section 6 for the shape and
// machine_x86_64.h for the opcodes.
//
// What is covered so far is the integer subset: constants, values arriving in
// registers, arithmetic, compares and branches. Everything else - memory,
// calls, floats, aggregates - falls through to buildUnselected(), which leaves
// a well-formed placeholder rather than a hole, so the rest of the function
// still selects and can still be read.
//
// Flags are not modelled. x86's compare writes EFLAGS and the setcc or jcc
// after it reads them, and nothing here says so: the two are simply emitted
// adjacent. That holds only because nothing between selection and emission
// reorders instructions, which is true today and is written down in section 10
// as the precondition it is.

static uint8_t valueSize(const IrInstruction *i) {
  return irTypeMachineSize(i->type);
}

static const IrInstruction *inputAt(const IrInstruction *i, size_t idx) {
  return getInstructionFromVector(&i->inputs, idx);
}

// Pointers compare unsigned, and so does anything already narrowed to a
// predicate; everything else follows its own signedness.
static Boolean isUnsignedCompare(enum IrTypeKind type) {
  return isUnsignedIrType(type) || type == IR_PTR || type == IR_REF || type == IR_BOOL;
}

// 'dst <- value', in whichever form the value has. A folded constant has no
// register to copy from and is spelled out as an immediate instead.
//
// The width is the *source's*, not the width of whatever operation is about to
// read it, because that is what the move actually moves. The two differ only
// when the IR asks for an operation on operands narrower than its result -
// pointer arithmetic on an int index is the case that occurs - and there the
// difference is a real gap that belongs in the dump rather than papered over
// with a wider move than the value has bytes. See
// docs/ir-codegen-design.md section 10.
static void selectLoadInto(MachineBuilder *b, uint32_t dst, const IrInstruction *value) {
  Boolean folded = machineBuilderIsFolded(b, value);
  MachineInstr *mi = buildMachineInstr(b, folded ? X86_MOV : MOP_COPY, 1, 1);

  setRegisterOperand(mi, 0, dst);
  setValueOperand(b, mi, 1, value);
  mi->opSize = valueSize(value);
}

// -============================ Leaves ============================-

static void selectConstant(MachineBuilder *b, const IrInstruction *i) {
  // Only the ones no use could take as an immediate reach here; the driver
  // dropped the rest (see decideConstants in src/ir/codegen/isel.c).
  if (i->info.constant.kind != IR_CK_INTEGER) {
    buildUnselected(b, i);
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setImmediateOperand(mi, 1, i->info.constant.data.i);
  mi->opSize = valueSize(i);
}

static void selectPhysReg(MachineBuilder *b, const IrInstruction *i) {
  // Read straight out of the fixed register the ABI put the value in, into one
  // the allocator is free to move. Leaving a parameter sitting in $rdi and
  // hoping nothing wants $rdi is how a backend acquires mysterious bugs.
  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, i->info.physReg);
  mi->opSize = valueSize(i);
}

// -============================ Arithmetic ============================-

// 'dst = lhs op rhs' as x86 spells it: 'dst <- lhs' and then 'dst op= rhs'.
// The leading copy is not waste - it is what makes the destructive form safe
// when lhs is still live afterwards - and the register allocator deletes it
// whenever it can give dst and lhs the same register.
static void selectBinary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, dst, inputAt(i, 0));

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setValueOperand(b, mi, 2, inputAt(i, 1));
  mi->opSize = size;
}

// Same, except that a shift by a value rather than by a constant has to take
// its count from cl and nowhere else.
static void selectShift(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);
  const IrInstruction *count = inputAt(i, 1);

  selectLoadInto(b, dst, inputAt(i, 0));

  if (!machineBuilderIsFolded(b, count)) {
    MachineInstr *toCl = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(toCl, 0, R_ECX);
    setRegisterOperand(toCl, 1, machineBuilderVreg(b, count));
    // The count is one byte however wide the shifted value is.
    toCl->opSize = 1;
  }

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  if (machineBuilderIsFolded(b, count)) {
    setImmediateOperand(mi, 2, count->info.constant.data.i);
  } else {
    setRegisterOperand(mi, 2, R_ECX);
  }
  mi->opSize = size;
}

// Division is the one place the ISA dictates which registers are involved: the
// dividend has to be in rdx:rax, and the instruction leaves the quotient in
// rax and the remainder in rdx whether or not both were wanted. Those are
// written as physical registers here, and the allocator has to treat them as
// pre-coloured - it may not reassign them, and it may not leave anything live
// in them across this.
static void selectDivMod(MachineBuilder *b, const IrInstruction *i, Boolean wantRemainder) {
  uint8_t size = valueSize(i);
  Boolean isSigned = !isUnsignedIrType(i->type);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, R_EAX, inputAt(i, 0));

  if (isSigned) {
    // Sign-extend rax into rdx:rax. One opcode for the whole cwd/cdq/cqo
    // family - opSize is what picks between them at emission.
    MachineInstr *ext = buildMachineInstr(b, X86_CDQ, 1, 1);
    setRegisterOperand(ext, 0, R_EDX);
    setRegisterOperand(ext, 1, R_EAX);
    machineOperandAt(ext, 1)->flags.isImplicit = 1;
    ext->opSize = size;
  } else {
    // An unsigned divide wants the high half zero rather than sign-extended.
    MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(zero, 0, R_EDX);
    setImmediateOperand(zero, 1, 0);
    zero->opSize = size;
  }

  // The divisor is never an immediate on x86, which is why isLegalImmediate
  // refuses to fold a constant into a divide - it arrives here in a register.
  MachineInstr *div = buildMachineInstr(b, isSigned ? X86_IDIV : X86_DIV, 2, 3);
  setRegisterOperand(div, 0, R_EAX);
  setRegisterOperand(div, 1, R_EDX);
  setRegisterOperand(div, 2, machineBuilderVreg(b, inputAt(i, 1)));
  setRegisterOperand(div, 3, R_EAX);
  setRegisterOperand(div, 4, R_EDX);
  // Only the divisor is written down in the instruction; the dividend halves
  // and both results are the ISA's own doing.
  machineOperandAt(div, 0)->flags.isImplicit = 1;
  machineOperandAt(div, 1)->flags.isImplicit = 1;
  machineOperandAt(div, 3)->flags.isImplicit = 1;
  machineOperandAt(div, 4)->flags.isImplicit = 1;
  div->opSize = size;

  MachineInstr *out = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(out, 0, dst);
  setRegisterOperand(out, 1, wantRemainder ? R_EDX : R_EAX);
  out->opSize = size;
}

// '~x' is the destructive one-operand form, so it reads like a binary op with
// only a left-hand side.
static void selectBitwiseNot(MachineBuilder *b, const IrInstruction *i) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, dst, inputAt(i, 0));

  MachineInstr *mi = buildMachineInstr(b, X86_NOT, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  mi->opSize = size;
}

// setcc writes one byte and one byte only. When the value being computed is
// wider than that - and it usually is, since C's comparisons and '!' both
// produce an int, not a predicate - the other bytes would be whatever the
// register happened to hold. Zeroing the destination first is what makes the
// whole width defined.
//
// It has to happen before the compare, not after: a move leaves the flags
// alone, so it is harmless there, whereas afterwards it would overwrite the
// answer the setcc is about to read out of them.
//
// Returns whether it emitted anything, because that is exactly the question
// "does the setcc read its own destination?" - see selectSetcc below.
static Boolean selectZeroExtendedSetup(MachineBuilder *b, uint32_t dst, uint8_t size) {
  if (size <= 1) {
    return FALSE;
  }

  MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(zero, 0, dst);
  setImmediateOperand(zero, 1, 0);
  zero->opSize = size;

  return TRUE;
}

// A setcc writes the low byte of its destination and leaves the rest as it
// found it. That makes it a read-modify-write of the whole register whenever
// the register is wider than a byte, and it is written down as one - operand 0
// defines it, operand 1 reads it - rather than as a plain def.
//
// Saying so is not decoration. The three upper bytes come from the zeroing
// move above, and nothing else in the machine function records that the setcc
// depends on it. A register allocator that believes the def is total will
// happily put the move's result somewhere the setcc never sees: the trivial
// allocator spills the zero to the frame and then reloads a *different* value
// into the scratch register the setcc writes, so the three bytes stored back
// are whatever the comparison's operand left behind. The dependency has to be
// in the operand list for the allocator to keep it.
//
// When the destination is a single byte there is no zeroing move and the def
// really is total, so the use is left off - claiming a read of a register
// nothing has written would be a use before def, and liveness would be right
// to complain about it.
static void selectSetcc(MachineBuilder *b, uint32_t opcode, uint32_t dst, Boolean readsDst) {
  MachineInstr *set = buildMachineInstr(b, opcode, 1, readsDst ? 1 : 0);

  setRegisterOperand(set, 0, dst);
  if (readsDst) {
    setRegisterOperand(set, 1, dst);
  }

  set->opSize = 1;
}

static void selectLogicalNot(MachineBuilder *b, const IrInstruction *i) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);
  const IrInstruction *arg = inputAt(i, 0);

  Boolean zeroed = selectZeroExtendedSetup(b, dst, size);

  MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
  setRegisterOperand(test, 0, machineBuilderVreg(b, arg));
  setRegisterOperand(test, 1, machineBuilderVreg(b, arg));
  test->opSize = valueSize(arg);

  selectSetcc(b, X86_SETE, dst, zeroed);
}

// -============================ Compares ============================-

static uint32_t setOpcodeFor(enum IrIntructionKind kind, Boolean isUnsigned) {
  switch (kind) {
  case IR_E_EQ: return X86_SETE;
  case IR_E_NE: return X86_SETNE;
  case IR_E_LT: return isUnsigned ? X86_SETB : X86_SETL;
  case IR_E_LE: return isUnsigned ? X86_SETBE : X86_SETLE;
  case IR_E_GT: return isUnsigned ? X86_SETA : X86_SETG;
  case IR_E_GE: return isUnsigned ? X86_SETAE : X86_SETGE;
  default: unreachable("not an integer comparison");
  }

  return X86_SETE;
}

// The compare materializes its boolean. When the only thing that boolean is
// for is the branch right after it - much the commonest case - this is a cmp,
// a setcc, and then a test of what the setcc just wrote. Collapsing that into
// cmp + jcc is a folding, and foldings are step 8; correctness does not depend
// on it and the dumps say plainly what is being left on the table.
static void selectCompare(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *lhs = inputAt(i, 0);
  Boolean isUnsigned = isUnsignedCompare(lhs->type);
  uint32_t dst = machineBuilderVreg(b, i);

  Boolean zeroed = selectZeroExtendedSetup(b, dst, valueSize(i));

  MachineInstr *cmp = buildMachineInstr(b, X86_CMP, 0, 2);
  // The left-hand side is never an immediate: x86 encodes the immediate as the
  // source operand, which is what isLegalImmediate's position rule enforces.
  setRegisterOperand(cmp, 0, machineBuilderVreg(b, lhs));
  setValueOperand(b, cmp, 1, inputAt(i, 1));
  cmp->opSize = valueSize(lhs);

  selectSetcc(b, setOpcodeFor(i->kind, isUnsigned), dst, zeroed);
}

// -============================ Terminators ============================-

static void selectBranch(MachineBuilder *b, const IrInstruction *i) {
  const IrBasicBlock *target = i->info.branch.taken;

  // A jump to the block that comes next is not a jump.
  if (machineBuilderFallsThroughTo(b, target)) {
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, X86_JMP, 0, 1);
  setBlockOperand(mi, 0, machineBuilderBlock(b, target));
}

static void selectCondBranch(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *cond = inputAt(i, 0);
  const IrBasicBlock *taken = i->info.branch.taken;
  const IrBasicBlock *notTaken = i->info.branch.notTaken;
  uint32_t condReg = machineBuilderVreg(b, cond);

  MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
  setRegisterOperand(test, 0, condReg);
  setRegisterOperand(test, 1, condReg);
  test->opSize = valueSize(cond);

  // Whichever way the layout fell, one of the two successors is next and needs
  // no branch. Block layout arranges for that to be the not-taken one wherever
  // it can (see layoutBlocks), so the first arm is the usual one.
  if (machineBuilderFallsThroughTo(b, notTaken)) {
    MachineInstr *jcc = buildMachineInstr(b, X86_JNE, 0, 1);
    setBlockOperand(jcc, 0, machineBuilderBlock(b, taken));
    return;
  }

  if (machineBuilderFallsThroughTo(b, taken)) {
    // Branch on the condition being false instead, and fall into the taken
    // block. Inverting is free; jumping over a jump is not.
    MachineInstr *jcc = buildMachineInstr(b, X86_JE, 0, 1);
    setBlockOperand(jcc, 0, machineBuilderBlock(b, notTaken));
    return;
  }

  // Neither is next, so both need a jump. No fixture reaches this and none can
  // today: ast2ir gives every conditional branch two blocks created at the
  // moment the branch is, nothing merges an empty block away afterwards, and a
  // block with a single predecessor cannot have been reached by the layout walk
  // before the branch that dominates it. So the second successor is always
  // still unvisited and always lands next. The arm stays because that is a
  // property of the frontend rather than of this file - merging empty blocks,
  // or ordering by profitability instead of by reverse postorder, reaches it
  // immediately. See docs/ir-codegen-design.md section 10.
  MachineInstr *jcc = buildMachineInstr(b, X86_JNE, 0, 1);
  setBlockOperand(jcc, 0, machineBuilderBlock(b, taken));

  MachineInstr *jmp = buildMachineInstr(b, X86_JMP, 0, 1);
  setBlockOperand(jmp, 0, machineBuilderBlock(b, notTaken));
}

static void selectReturn(MachineBuilder *b, const IrInstruction *i) {
  if (i->inputs.size != 0) {
    const IrInstruction *value = inputAt(i, 0);

    // A wide or aggregate return is an ABI question - a hidden buffer pointer,
    // or two registers - and that is step 7's. A scalar goes in the one
    // register its class returns in.
    if (value->type == IR_P_AGG || value->type == IR_F80) {
      buildUnselected(b, i);
      return;
    }

    uint32_t retReg =
        isFloatIrType(value->type) ? b->mf->target->fpRetReg : b->mf->target->intRetReg;

    MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(mi, 0, retReg);
    setValueOperand(b, mi, 1, value);
    mi->opSize = valueSize(value);
  }

  // Just the return. The prologue and epilogue around it are stage 3's, which
  // is the first point at which the frame size and the callee-saved registers
  // actually used are both known.
  buildMachineInstr(b, X86_RET, 0, 0);
}

// -============================ Dispatch ============================-

static void selectInstruction_x86_64(MachineBuilder *b, const IrInstruction *i) {
  switch (i->kind) {
  case IR_DEF_CONST: selectConstant(b, i); break;
  case IR_P_REG: selectPhysReg(b, i); break;

  case IR_E_ADD: selectBinary(b, i, X86_ADD); break;
  case IR_E_SUB: selectBinary(b, i, X86_SUB); break;
  case IR_E_MUL: selectBinary(b, i, X86_IMUL); break;
  case IR_E_AND: selectBinary(b, i, X86_AND); break;
  case IR_E_OR: selectBinary(b, i, X86_OR); break;
  case IR_E_XOR: selectBinary(b, i, X86_XOR); break;

  case IR_E_SHL: selectShift(b, i, X86_SHL); break;
  // Right shift is where signedness stops being cosmetic: an arithmetic shift
  // keeps the sign bit, a logical one does not.
  case IR_E_SHR:
    selectShift(b, i, isUnsignedIrType(i->type) ? X86_SHR : X86_SAR);
    break;

  case IR_E_DIV: selectDivMod(b, i, FALSE); break;
  case IR_E_MOD: selectDivMod(b, i, TRUE); break;

  case IR_U_BNOT: selectBitwiseNot(b, i); break;
  case IR_U_NOT: selectLogicalNot(b, i); break;

  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    selectCompare(b, i);
    break;

  // Memory, calls, floats, casts, aggregates. Each of these is a step of its
  // own in docs/ir-codegen-design.md section 11, and until then a placeholder
  // is more useful than an abort: the rest of the function still selects, and
  // the dump names exactly what is missing.
  default:
    buildUnselected(b, i);
    break;
  }
}

static void selectTerminator_x86_64(MachineBuilder *b, const IrInstruction *i) {
  switch (i->kind) {
  case IR_BRANCH: selectBranch(b, i); break;
  case IR_CBRANCH: selectCondBranch(b, i); break;
  case IR_RET: selectReturn(b, i); break;

  // A switch table and a computed goto both need a jump through memory, which
  // needs an addressing mode, which is step 8.
  default:
    buildUnselected(b, i);
    break;
  }
}

// -============================ Immediates ============================-

static Boolean x86IsLegalImmediate(const IrInstruction *use, size_t operandIdx,
                                   const IrInstruction *cnst) {
  // The right-hand operand only. x86 encodes an immediate as the source, so
  // 'c - x' has nowhere to put one; gvn already canonicalizes a commutative
  // operation's constant into this position, so the restriction costs almost
  // nothing, and swapping the rest is a peephole for step 8.
  if (operandIdx != 1) {
    return FALSE;
  }

  int64_t v = cnst->info.constant.data.i;

  switch (use->kind) {
  case IR_E_ADD:
  case IR_E_SUB:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    // ALU and compare immediates are 32 bits, sign-extended to the operand
    // width; a 64-bit constant outside that range has to be materialized.
    return v >= INT32_MIN && v <= INT32_MAX;

  case IR_E_SHL:
  case IR_E_SHR:
    // A shift count is 8 bits, and the hardware masks it to the operand width
    // anyway, so anything at or beyond 64 is not a shift this can encode.
    return v >= 0 && v < 64;

  // IR_E_MUL is deliberately absent. imul's immediate form is the
  // three-operand one, which is a different encoding from the two-address
  // shape everything above shares, so folding into it belongs with the rest of
  // step 8 rather than as a special case here.
  //
  // Divides are absent because x86 has no immediate divisor at all, and
  // everything else because it has no rule yet.
  default:
    return FALSE;
  }
}

const ArchSelector x86Selector = {
  .selectInstruction = &selectInstruction_x86_64,
  .selectTerminator = &selectTerminator_x86_64,
  .isLegalImmediate = &x86IsLegalImmediate
};
