#include <assert.h>
#include <string.h>

#include "ir/ir.h"
#include "ir/machine.h"
#include "ir/regalloc.h"

// -============================ The shared spiller ========================-
//
// See include/ir/regalloc.h for why this belongs to neither allocator.

// The most distinct registers one instruction can name. Sized generously
// rather than by what selection builds today - the widest is a store through a
// base and an index, which is three - because exceeding it here would be a
// silent miscompile rather than a refusal.
#define MAX_SPILLS_PER_INSTR 8

typedef struct _SpillSite {
  uint32_t vreg;  // the spilled register
  uint32_t fresh; // the one-instruction register standing in for it
  Boolean isRead;
  Boolean isWritten;
} SpillSite;

void initSpillState(SpillState *ss, MachineFunction *mf) {
  memset(ss, 0, sizeof *ss);

  ss->mf = mf;
  // Stage 0 rounded the frame it laid out; starting from there rather than
  // from its unrounded depth costs a few bytes of padding and keeps the two
  // areas from having to know anything about each other.
  ss->frameOffset = (int32_t)mf->frame.size;
  ss->firstSpillerVreg = 0;
  initVector(&ss->vregToSlot, INITIAL_VECTOR_CAPACITY);
}

void releaseSpillState(SpillState *ss) {
  releaseVector(&ss->vregToSlot);
}

int32_t spillSlotForVreg(SpillState *ss, uint32_t vreg) {
  size_t idx = vreg - FIRST_VREG;

  if (idx < ss->vregToSlot.size) {
    intptr_t stored = getFromVector(&ss->vregToSlot, idx);
    if (stored != 0) {
      return (int32_t)stored - 1;
    }
  }

  const VRegInfo *info = virtualRegisterInfo(ss->mf, vreg);

  // Aligned to its own size, which for every width a register can hold is the
  // natural alignment as well.
  int32_t frameIdx = addMachineFrameObject(ss->mf, MFO_SPILL, info->size, info->size);
  MachineFrameObject *obj = machineFrameObjectAt(ss->mf, frameIdx);
  obj->vreg = vreg;
  // The IR value this register was holding, when there is one, so the frame
  // and the vreg table in a dump can be read against each other.
  obj->origin = info->origin;

  ss->frameOffset = placeMachineFrameObject(ss->mf, ss->frameOffset, frameIdx);
  putAtVector(&ss->vregToSlot, idx, (intptr_t)frameIdx + 1);

  return frameIdx;
}

Boolean isSpillerVreg(const SpillState *ss, uint32_t vreg) {
  return ss->firstSpillerVreg != 0 && vreg >= ss->firstSpillerVreg ? TRUE : FALSE;
}

void finishSpillFrame(SpillState *ss) {
  ss->mf->frame.size = ALIGN_SIZE(ss->frameOffset, 2 * sizeof(intptr_t));
}

MachineInstr *buildReloadInstr(MachineFunction *mf, uint32_t reg, int32_t frameIdx, uint8_t size) {
  MachineInstr *mi = createMachineInstr(mf, MOP_RELOAD, 1, 1);

  setRegisterOperand(mi, 0, reg);
  setFrameIndexOperand(mi, 1, frameIdx);
  mi->opSize = size;
  // Left without an origin on purpose: a reload stands for no IR instruction,
  // it stands for this pass having nowhere else to keep the value.

  return mi;
}

MachineInstr *buildSpillInstr(MachineFunction *mf, int32_t frameIdx, uint32_t reg, uint8_t size) {
  MachineInstr *mi = createMachineInstr(mf, MOP_SPILL, 0, 2);

  setFrameIndexOperand(mi, 0, frameIdx);
  setRegisterOperand(mi, 1, reg);
  mi->opSize = size;

  return mi;
}

// -============================ Rewriting a site ==========================-

static int findSite(const SpillSite *sites, size_t count, uint32_t vreg) {
  for (size_t idx = 0; idx < count; ++idx) {
    if (sites[idx].vreg == vreg) {
      return (int)idx;
    }
  }

  return -1;
}

// One fresh register per spilled register per instruction, not per operand: a
// two-address form names its destination twice by construction, and giving it
// two registers would break the very form selection built.
static size_t collectSites(SpillState *ss, const BitSet *spilled, MachineInstr *mi,
                           SpillSite *sites) {
  size_t count = 0;

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    MachineOperand *op = &mi->operands[idx];
    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(op, regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      uint32_t reg = *regs[r];

      if (!isVirtualRegister(reg) || !getBit(spilled, reg - FIRST_VREG)) {
        continue;
      }

      assert(!isSpillerVreg(ss, reg) &&
             "a register this spiller invented was chosen to be spilled again");

      int found = findSite(sites, count, reg);

      if (found < 0) {
        assert(count < MAX_SPILLS_PER_INSTR);
        found = (int)count++;
        sites[found].vreg = reg;
        sites[found].fresh = NO_REG;
        sites[found].isRead = FALSE;
        sites[found].isWritten = FALSE;
      }

      // Not an either/or: a partial def is both, and so gets a reload in front
      // of the instruction as well as a spill behind it.
      if (machineOperandIsWritten(op)) {
        sites[found].isWritten = TRUE;
      }
      if (machineOperandIsRead(op)) {
        sites[found].isRead = TRUE;
      }
    }
  }

  return count;
}

static void rewriteInstruction(SpillState *ss, const BitSet *spilled, MachineInstr *mi) {
  SpillSite sites[MAX_SPILLS_PER_INSTR];

  size_t count = collectSites(ss, spilled, mi, sites);
  if (count == 0) {
    return;
  }

  MachineFunction *mf = ss->mf;

  for (size_t idx = 0; idx < count; ++idx) {
    const VRegInfo *info = virtualRegisterInfo(mf, sites[idx].vreg);

    if (ss->firstSpillerVreg == 0) {
      ss->firstSpillerVreg = FIRST_VREG + (uint32_t)mf->vregs.size;
    }

    sites[idx].fresh = createVirtualRegister(mf, info->rc, info->size);
    // The IR value carried over, so that a dump still says which value the
    // traffic around this instruction is about.
    virtualRegisterInfo(mf, sites[idx].fresh)->origin = info->origin;
  }

  // Reloads first, each immediately ahead of the instruction, so they come out
  // in site order.
  for (size_t idx = 0; idx < count; ++idx) {
    if (!sites[idx].isRead) {
      continue;
    }

    const VRegInfo *info = virtualRegisterInfo(mf, sites[idx].vreg);
    int32_t slot = spillSlotForVreg(ss, sites[idx].vreg);
    addMachineInstrBefore(mi, buildReloadInstr(mf, sites[idx].fresh, slot, info->size));
  }

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      int found = isVirtualRegister(*regs[r]) ? findSite(sites, count, *regs[r]) : -1;

      if (found >= 0) {
        *regs[r] = sites[found].fresh;
      }
    }
  }

  // Spills after, walking a cursor along so that they too keep site order
  // rather than coming out reversed.
  MachineInstr *at = mi;
  for (size_t idx = 0; idx < count; ++idx) {
    if (!sites[idx].isWritten) {
      continue;
    }

    const VRegInfo *info = virtualRegisterInfo(mf, sites[idx].vreg);
    int32_t slot = spillSlotForVreg(ss, sites[idx].vreg);
    MachineInstr *spill = buildSpillInstr(mf, slot, sites[idx].fresh, info->size);

    addMachineInstrAfter(at, spill);
    at = spill;
  }
}

void insertSpillCode(SpillState *ss, const BitSet *spilled) {
  for (MachineBasicBlock *mbb = ss->mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    MachineInstr *mi = mbb->instructions.head;

    while (mi != NULL) {
      // Captured before the rewrite, which inserts spills between this
      // instruction and the next one: those name only fresh registers and
      // walking into them would be pointless work, not a bug.
      MachineInstr *next = mi->next;
      rewriteInstruction(ss, spilled, mi);
      mi = next;
    }
  }
}

// -============================ Bookkeeping ============================-

void recordUsedPhysRegs(MachineFunction *mf) {
  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
        uint32_t *regs[MAX_OPERAND_REGS];
        uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

        for (uint16_t r = 0; r < numRegs; ++r) {
          uint32_t reg = *regs[r];

          if (reg == NO_REG) {
            continue;
          }

          assert(isPhysicalRegister(reg) && "a virtual register survived allocation");
          assert(reg < IR_PHYS_REG_MAX);
          mf->usedPhysRegs |= (uint64_t)1 << reg;
        }
      }
    }
  }
}
