#include <assert.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "machine_riscv64.h"

// -============================ Stage 1: instruction selection ============-
//
// The riscv64 half of the seam described in include/ir/isel.h. Everything
// target-independent - block layout, the walk, constant and address folding,
// the operand bookkeeping - is in src/ir/codegen/isel.c and is shared with
// x86-64 unchanged; what is here is what an IR instruction becomes.
//
// Nothing is selected yet. This file exists so that '-march riscv64' fails
// where the gap actually is, naming the instruction it has no rule for, rather
// than by returning NULL from archSelectorFor() and asserting a hundred lines
// later that this target has no selector at all.

static uint8_t valueSize(const IrInstruction *i) {
  return irTypeMachineSize(i->type);
}

static const IrInstruction *inputAt(const IrInstruction *i, size_t idx) {
  return getInstructionFromVector(&i->inputs, idx);
}

static uint32_t retRegOf(const TargetDescriptor *target, enum RegClass rc) {
  return rc == RC_FP ? target->fpRetReg : target->intRetReg;
}

static void selectConstant(MachineBuilder *b, const IrInstruction *i) {
  // Only the constants no use could take as an immediate reach here; the
  // driver dropped the rest (see decideConstants in src/ir/codegen/isel.c).
  assert(i->info.constant.kind == IR_CK_INTEGER && "only integer constants are selected yet");

  MachineInstr *mi = buildMachineInstr(b, RISCV_LI, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setImmediateOperand(mi, 1, i->info.constant.data.i);
  mi->opSize = valueSize(i);
}

// Move a value into a named physical register - the one thing a return needs.
static void selectLoadInto(MachineBuilder *b, uint32_t dst, const IrInstruction *value,
                           uint8_t size) {
  if (machineBuilderIsFolded(b, value)) {
    MachineInstr *mi = buildMachineInstr(b, RISCV_LI, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setValueOperand(b, mi, 1, value);
    mi->opSize = size;
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
  mi->opSize = size;
}

static void selectJumpTo(MachineBuilder *b, const IrBasicBlock *target) {
  // A jump to the block that comes next is not a jump.
  if (machineBuilderFallsThroughTo(b, target)) {
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, RISCV_J, 0, 1);
  setBlockOperand(mi, 0, machineBuilderBlock(b, target));
}

static void selectReturn(MachineBuilder *b, const IrInstruction *i) {
  if (i->inputs.size != 0) {
    assert(i->inputs.size == 1 && "an aggregate return needs the LP64D rules");

    const IrInstruction *value = inputAt(i, 0);
    enum RegClass rc = isFloatIrType(value->type) ? RC_FP : RC_GP;

    selectLoadInto(b, retRegOf(b->mf->target, rc), value, valueSize(value));
  }

  // Just the return. The prologue and epilogue around it are stage 3's, which
  // is the first point at which the frame size and the callee-saved registers
  // actually used are both known.
  buildMachineInstr(b, RISCV_RET, 0, 0);
}

static void selectInstruction_riscv64(MachineBuilder *b, const IrInstruction *i) {
  switch (i->kind) {
  case IR_DEF_CONST: selectConstant(b, i); break;

  default:
    unreachable(irInstructionMnemonic(i->kind));
  }
}

static void selectTerminator_riscv64(MachineBuilder *b, const IrInstruction *term) {
  switch (term->kind) {
  case IR_BRANCH: selectJumpTo(b, term->info.branch.taken); break;
  case IR_RET: selectReturn(b, term); break;

  default:
    unreachable(irInstructionMnemonic(term->kind));
  }
}

// RV64's I-type immediate is twelve bits, signed. Every arithmetic and logical
// 'i' form takes exactly that, so one answer covers all of them; the
// operations with no immediate form at all - mul, div, rem, sub - are the
// reason this is asked per use rather than per constant.
static Boolean riscvIsLegalImmediate(const IrInstruction *use, size_t operandIdx,
                                     const IrInstruction *cnst) {
  return FALSE;
}

// There is no scaled-index addressing mode: an address is one base register
// plus a twelve-bit signed displacement, and anything else is arithmetic the
// selector has to emit. So every non-zero scale is refused, and the GEP walk
// in isel.c stops one term earlier here than it does on x86-64.
static Boolean riscvIsLegalAddressMode(uint32_t scale, int64_t disp) {
  return scale == 0 && -2048 <= disp && disp <= 2047;
}

static uint32_t riscvAddressOperands(const IrInstruction *i) {
  return 0;
}

static Boolean riscvFoldsIntoCondition(const IrInstruction *cond) {
  return FALSE;
}

const ArchSelector riscvSelector = {
  // Nothing to settle about the whole function yet; the outgoing-argument
  // area arrives with call selection.
  .reserveFrame = NULL,
  .selectInstruction = &selectInstruction_riscv64,
  .selectTerminator = &selectTerminator_riscv64,
  .isLegalImmediate = &riscvIsLegalImmediate,
  .isLegalAddressMode = &riscvIsLegalAddressMode,
  .addressOperands = &riscvAddressOperands,
  .foldsIntoCondition = &riscvFoldsIntoCondition
};
