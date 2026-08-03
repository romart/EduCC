#ifndef __IR_ISEL_H__
#define __IR_ISEL_H__ 1

#include "ir/machine.h"

// -============================ Stage 1: instruction selection ============-
//
// Turns each IR instruction into the machine instructions that compute it,
// still over virtual registers. See docs/ir-codegen-design.md section 6.
//
// The shape is macro expansion, not tree tiling: one IR instruction in, one to
// a handful of machine instructions out, with no matching across instruction
// boundaries. That produces worse code than a BURG-style matcher would, and it
// is the deliberate trade - the matcher is a large investment, and the three
// foldings that recover most of the difference (constants, addressing modes,
// compare+branch) are each a local decision a matcher is not needed for.
// Only the first of the three exists today.
//
// The target-independent side owns block layout, the walk, and the bookkeeping
// below; the target owns what an IR instruction becomes. That seam is what
// lets a matcher replace an ArchSelector later without stages 0, 2 or 3
// noticing.

struct _IrInstruction;

typedef struct _MachineBuilder {
  MachineFunction *mf;

  // The block being filled. 'insertBefore' is stage 0's first phi copy in it,
  // or NULL when there is none: those copies carry values out of the block
  // along its outgoing edges, so they have to stay after everything selection
  // emits and before the terminator that follows them.
  MachineBasicBlock *mbb;
  MachineInstr *insertBefore;

  // The IR instruction being selected, so that everything emitted for it is
  // attributed to it without every arch hook having to remember to.
  const struct _IrInstruction *origin;

  // Which integer constants were folded into immediates and so never got a
  // register. Indexed by IrInstruction.id, biased by one exactly like the maps
  // on MachineFunction, so an unwritten entry reads as "not decided".
  Vector foldedConstants;
} MachineBuilder;

typedef struct _ArchSelector {
  // One non-terminator IR instruction. Never called for IR_PHI (stage 0
  // already destroyed those) nor for an instruction the driver folded away.
  void (*selectInstruction)(MachineBuilder *b, const struct _IrInstruction *i);

  // The block's terminator. Responsible for the branch itself *and* for
  // dropping whatever falls through - see machineBuilderFallsThroughTo().
  void (*selectTerminator)(MachineBuilder *b, const struct _IrInstruction *term);

  // Can 'cnst' be an operand of 'use' at input position 'operandIdx', rather
  // than a value some instruction has to put in a register first? Asked once
  // per use before anything is selected, because a constant is materialized or
  // not for the whole function: it is defined in the entry block and would
  // otherwise occupy a register across all of it.
  //
  // Named after the usual answer, an integer immediate. A symbol constant is
  // asked the same question - the callee of a direct call is one, and the call
  // encodes it in place - and setValueOperand then spells it as MO_SYMBOL
  // rather than MO_IMM.
  Boolean (*isLegalImmediate)(const struct _IrInstruction *use, size_t operandIdx,
                              const struct _IrInstruction *cnst);
} ArchSelector;

// ------------- what an ArchSelector is given ------------------------

// Creates a machine instruction, attributes it to the IR instruction being
// selected, and links it into the block at the right place. Operands are left
// unset for the caller to fill in.
MachineInstr *buildMachineInstr(MachineBuilder *b, uint32_t opcode, uint16_t numDefs,
                                uint16_t numUses);

// The register holding an IR value. Fails on a folded constant - that one has
// no register by construction, which is the point of folding it.
uint32_t machineBuilderVreg(MachineBuilder *b, const struct _IrInstruction *value);

Boolean machineBuilderIsFolded(const MachineBuilder *b, const struct _IrInstruction *value);

// Fills operand 'idx' with an IR value: an immediate if it is a folded
// constant, its register otherwise. Every use goes through here, so no arch
// hook has to remember which of its operands might have been folded.
void setValueOperand(MachineBuilder *b, MachineInstr *mi, uint16_t idx,
                     const struct _IrInstruction *value);

// Whether control leaving this block for 'target' needs a jump at all, or
// whether 'target' is simply the next block in layout order.
Boolean machineBuilderFallsThroughTo(const MachineBuilder *b, const struct _IrBasicBlock *target);

// The machine block for an IR block, for use as a branch target operand.
MachineBasicBlock *machineBuilderBlock(MachineBuilder *b, const struct _IrBasicBlock *target);

// The placeholder for something with no selection rule yet, wired up to the IR
// instruction's inputs and result so the machine function stays well formed.
// Arch hooks call this from their default arm rather than asserting.
void buildUnselected(MachineBuilder *b, const struct _IrInstruction *i);

// ------------- entry point ------------------------

// Fixes the block layout, then fills every block. A no-op for a target with no
// selector yet, which leaves the skeleton stage 0 built exactly as it was.
void selectInstructions(MachineFunction *mf);

extern const ArchSelector x86Selector;

#endif // __IR_ISEL_H__
