#include <assert.h>

#include "ir/ir.h"
#include "ir/machine.h"
#include "ir/regalloc.h"

// -============================ Stage 2A: trivial allocation ==============-
//
// See include/ir/regalloc.h for what this is and why it is worth keeping now
// that stage 2B exists and is the default.
//
// The whole algorithm is one rule applied per instruction: every virtual
// register it names gets a scratch register for the duration of that
// instruction and no longer. Read one, and a reload goes in front; write one,
// and a spill goes behind. Nothing is live in a register across an instruction
// boundary, which is what makes the rule sound without any liveness analysis
// at all - there is no interference to compute when nothing has a range.
//
// Two things it does *not* touch, both deliberately:
//
//   physical registers    Selection puts values in fixed registers where the
//                         ISA or the ABI demands it - rdx:rax for a divide, cl
//                         for a shift count, the argument registers a
//                         parameter arrives in. Those are already allocated,
//                         and the scratch set is chosen to be disjoint from
//                         them (see TargetDescriptor.scratchRegs), so leaving
//                         them alone is both correct and the only option.
//
//   instruction order     Reloads and spills are moves, and on both targets a
//                         move leaves the condition flags alone. That is what
//                         makes it safe to drop a reload between a compare and
//                         the setcc reading its result, which this does - see
//                         the EFLAGS note in docs/ir-codegen-design.md section
//                         6.4, which is about *reordering*, not about
//                         interposing.
//
// The scratch registers are this allocator's alone. Stage 2B needs none: a
// spilled value's reload gets an ordinary register from the ordinary pool,
// because the spiller gives it a virtual register and the allocator runs
// again (see the shared spiller in include/ir/regalloc.h).

// The most scratch registers of one class any instruction can ask for. Sized
// by what a target may reserve rather than by what an instruction may want,
// because exceeding it is handled (see fitsScratchBudget) rather than fatal.
#define MAX_SCRATCH_REGS 8

typedef struct _RegAllocContext {
  MachineFunction *mf;
  const TargetDescriptor *target;

  SpillState spill;
} RegAllocContext;

// One virtual register's business with one instruction.
typedef struct _ScratchAssignment {
  uint32_t vreg;
  uint32_t phys;
  Boolean isRead;    // the instruction has it as a use, so it needs a reload
  Boolean isWritten; // ... as a def, so what it leaves has to be stored back
} ScratchAssignment;

// -============================ The scratch budget ============================-

static int findAssignment(const ScratchAssignment *table, size_t count, uint32_t vreg) {
  for (size_t idx = 0; idx < count; ++idx) {
    if (table[idx].vreg == vreg) {
      return (int)idx;
    }
  }

  return -1;
}

// Collects the distinct virtual registers an instruction names, recording for
// each whether the instruction reads it, writes it, or both. A register in
// two operand positions - which two-address form guarantees, since operand 0
// is the destination and operand 1 is the same register again - is one entry
// with both flags set, and so gets one scratch register rather than two.
//
// Returns the number of entries, or -1 when the instruction names more
// distinct registers of some class than the target has scratch for.
static int collectAssignments(const MachineFunction *mf, MachineInstr *mi,
                              ScratchAssignment *table) {
  size_t count = 0;
  uint32_t needed[RC_CLASS_COUNT] = {0};

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    MachineOperand *op = &mi->operands[idx];
    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(op, regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      uint32_t reg = *regs[r];

      if (!isVirtualRegister(reg)) {
        continue;
      }

      int found = findAssignment(table, count, reg);

      if (found < 0) {
        if (count == MAX_SCRATCH_REGS) {
          return -1;
        }

        found = (int)count++;
        table[found].vreg = reg;
        table[found].phys = NO_REG;
        table[found].isRead = FALSE;
        table[found].isWritten = FALSE;

        needed[machineRegisterClass(mf, reg)] += 1;
      }

      // Not an either/or: a partial def is both, and so gets a reload in front
      // of the instruction as well as a spill behind it. A register inside an
      // addressing mode is read however the operand it sits in is flagged -
      // see machineOperandRegisters.
      if (machineOperandIsWritten(op)) {
        table[found].isWritten = TRUE;
      }
      if (machineOperandIsRead(op)) {
        table[found].isRead = TRUE;
      }
    }
  }

  for (size_t rc = 0; rc < RC_CLASS_COUNT; ++rc) {
    if (needed[rc] > mf->target->scratchRegCount[rc]) {
      return -1;
    }
  }

  return (int)count;
}

// Whether the trivial allocator can express this function at all.
//
// It cannot when a single instruction names more distinct virtual registers of
// one class than the target reserves as scratch, because there is then nowhere
// to put them all at once. The widest thing selection builds names three - a
// store through a base and an index, once address operands existed - against a
// budget of three, so it fits, but only just.
//
// Nothing reaches the limit any more, and this is now a check rather than a
// refusal. What used to reach it was MOP_UNSELECTED, which stood in for an IR
// instruction with as many inputs as that instruction had and so read one
// register per argument of a call selection had turned away. Step 7 took the
// common case (an ordinary integer call), step 13 the large aggregate
// argument, step 17 long double, and step 18 the placeholder itself: with
// every instruction genuinely selected, the widest one is three registers by
// construction.
//
// Kept rather than deleted because it is what makes the postcondition a
// statement about every function rather than about the ones tried so far, and
// because the margin is one register and not several. An addressing mode with
// a scaled index *and* a displacement register, or any three-operand form,
// would need a fourth scratch register - so if this ever fires, the answer is
// to widen TargetDescriptor.scratchRegs, not to relax the check.
static Boolean fitsScratchBudget(MachineFunction *mf) {
  ScratchAssignment table[MAX_SCRATCH_REGS];

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      if (collectAssignments(mf, mi, table) < 0) {
        return FALSE;
      }
    }
  }

  return TRUE;
}

// -============================ The walk ============================-

static void allocateInstruction(RegAllocContext *ra, MachineInstr *mi) {
  ScratchAssignment table[MAX_SCRATCH_REGS];

  int count = collectAssignments(ra->mf, mi, table);
  assert(count >= 0 && "fitsScratchBudget said this function was allocatable");

  if (count == 0) {
    return;
  }

  // Handed out in the order the operands mention them, per class, and reset
  // for every instruction - a scratch register holds nothing once the
  // instruction that borrowed it is done.
  uint32_t taken[RC_CLASS_COUNT] = {0};

  for (int idx = 0; idx < count; ++idx) {
    enum RegClass rc = machineRegisterClass(ra->mf, table[idx].vreg);
    table[idx].phys = ra->target->scratchRegs[rc][taken[rc]++];
  }

  // Reloads first, each one immediately ahead of the instruction, so they come
  // out in table order.
  for (int idx = 0; idx < count; ++idx) {
    if (!table[idx].isRead) {
      continue;
    }

    const VRegInfo *info = virtualRegisterInfo(ra->mf, table[idx].vreg);
    int32_t slot = spillSlotForVreg(&ra->spill, table[idx].vreg);
    addMachineInstrBefore(mi, buildReloadInstr(ra->mf, table[idx].phys, slot, info->size));
  }

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      if (!isVirtualRegister(*regs[r])) {
        continue;
      }

      int found = findAssignment(table, (size_t)count, *regs[r]);
      assert(found >= 0);
      *regs[r] = table[found].phys;
    }
  }

  // Spills after, walking a cursor along so that they too keep table order
  // rather than coming out reversed.
  MachineInstr *at = mi;
  for (int idx = 0; idx < count; ++idx) {
    if (!table[idx].isWritten) {
      continue;
    }

    const VRegInfo *info = virtualRegisterInfo(ra->mf, table[idx].vreg);
    int32_t slot = spillSlotForVreg(&ra->spill, table[idx].vreg);
    MachineInstr *spill = buildSpillInstr(ra->mf, slot, table[idx].phys, info->size);

    addMachineInstrAfter(at, spill);
    at = spill;
  }
}

static void allocateRegistersTrivial(MachineFunction *mf) {
  const TargetDescriptor *target = mf->target;

  for (size_t rc = 0; rc < RC_CLASS_COUNT; ++rc) {
    assert(target->scratchRegCount[rc] <= MAX_SCRATCH_REGS);
  }

  if (!fitsScratchBudget(mf)) {
    unreachable("an instruction names more registers than the target has scratch");
  }

  RegAllocContext ra = {0};
  ra.mf = mf;
  ra.target = target;
  initSpillState(&ra.spill, mf);

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    MachineInstr *mi = mbb->instructions.head;

    while (mi != NULL) {
      // Captured before the rewrite, which inserts spills between this
      // instruction and the next one: those are already allocated and walking
      // into them would be pointless work, not a bug.
      MachineInstr *next = mi->next;
      allocateInstruction(&ra, mi);
      mi = next;
    }
  }

  finishSpillFrame(&ra.spill);
  releaseSpillState(&ra.spill);

  mf->allocator = "trivial";
}

void allocateRegisters(MachineFunction *mf, enum RegAllocKind kind) {
  // Only x86_64 reaches this - the IR backend with any other '-march' is
  // refused where the options are read, riscv64 having neither a selector nor
  // a register file to hand out.
  assert(mf->target->scratchRegCount[RC_GP] != 0 && "this target has no IR backend");

  switch (kind) {
  case RA_TRIVIAL: allocateRegistersTrivial(mf); break;
  case RA_LINEAR: allocateRegistersLinear(mf); break;
  case RA_COLOUR: allocateRegistersColour(mf); break;
  default: unreachable("unknown register allocator");
  }

  recordUsedPhysRegs(mf);

  // Spill code placed by liveness is exactly the case the flags model was
  // built for (section 6.4), so the question is asked again here and not only
  // after selection: what stages 2B and 2C insert between a compare and its
  // reader has to be as inert as what stage 2A does.
  verifyFlagsDependencies(mf);
}
