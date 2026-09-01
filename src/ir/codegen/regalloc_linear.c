#include <assert.h>
#include <string.h>

#include "mem.h"
#include "utils.h"
#include "ir/ir.h"
#include "ir/liveness.h"
#include "ir/regalloc.h"

// -============================ Stage 2B: linear scan =====================-
//
// Poletto and Sarkar's algorithm over the linearized machine function, on the
// liveness of src/ir/codegen/liveness.c. Intervals are swept in order of their
// start; each one takes a register that nothing else needs over its whole
// range, and when there is none the interval reaching furthest ahead is the
// one that goes to memory - it being the one whose register buys the most.
//
// Three things make it correct here rather than merely plausible:
//
//   physical registers    are in the same liveness as the virtual ones, and
//                         machineLivenessBusyRange() is what an interval asks
//                         before taking one. rdi between an argument copy and
//                         its call is busy, and so is rdx:rax across a divide,
//                         and so is every caller-saved register at a call -
//                         which is what MachineInstr.flags.isCall was put in
//                         the machine model for.
//
//   expiry is strict      An interval is expired only once its end is *before*
//                         the current start, so a def at a position cannot
//                         take the register of a value last read at that same
//                         position. That is what keeps two-address form -
//                         'add dst, src' would otherwise become 'add r, r' -
//                         and an early-clobber def honest, without either
//                         needing a case here.
//
//   spilling iterates     A spilled value's uses still need a register. They
//                         get one from this same pool: the shared spiller
//                         rewrites each site to a fresh virtual register live
//                         across one instruction and the whole allocation runs
//                         again. No registers are reserved, and the loop
//                         terminates because a register the spiller invented
//                         is never spilled - see include/ir/regalloc.h.

typedef struct _LinearScan {
  MachineFunction *mf;
  const TargetDescriptor *target;
  MachineLiveness *lv;
  const SpillState *spill;

  // Which registers each class may hand out, as a mask. Derived once from the
  // target's ordered list, which is what actually decides *which* free
  // register an interval gets.
  uint64_t allocatable[RC_CLASS_COUNT];

  // Intervals holding a register right now, ordered by increasing end point,
  // which is the order the sweep expires and spills them in.
  LiveInterval **active;
  uint32_t numActive;

  // The registers those intervals hold. Kept alongside rather than recomputed,
  // because every allocation asks for it and nothing else changes it.
  uint64_t taken;

  BitSet spilled; // by (vreg - FIRST_VREG)
  Boolean anySpilled;
} LinearScan;

static enum RegClass intervalClass(const LinearScan *ls, const LiveInterval *iv) {
  return machineRegisterClass(ls->mf, iv->vreg);
}

// -============================ The active list ============================-

static void expireOldIntervals(LinearScan *ls, uint32_t start) {
  uint32_t kept = 0;

  for (uint32_t idx = 0; idx < ls->numActive; ++idx) {
    LiveInterval *iv = ls->active[idx];

    // Strict: an interval ending exactly where this one begins still overlaps
    // it, because both are live at that instruction. See the note above.
    if (iv->end < start) {
      ls->taken &= ~((uint64_t)1 << iv->phys);
    } else {
      ls->active[kept++] = iv;
    }
  }

  ls->numActive = kept;
}

static void addActive(LinearScan *ls, LiveInterval *iv) {
  uint32_t idx = ls->numActive;

  while (idx > 0 && ls->active[idx - 1]->end > iv->end) {
    ls->active[idx] = ls->active[idx - 1];
    idx -= 1;
  }

  ls->active[idx] = iv;
  ls->numActive += 1;
  ls->taken |= (uint64_t)1 << iv->phys;
}

static void removeActive(LinearScan *ls, const LiveInterval *iv) {
  uint32_t kept = 0;

  for (uint32_t idx = 0; idx < ls->numActive; ++idx) {
    if (ls->active[idx] == iv) {
      ls->taken &= ~((uint64_t)1 << iv->phys);
    } else {
      ls->active[kept++] = ls->active[idx];
    }
  }

  ls->numActive = kept;
}

// -============================ Choosing ============================-

static void markSpilled(LinearScan *ls, const LiveInterval *iv) {
  assert(!isSpillerVreg(ls->spill, iv->vreg) &&
         "a one-instruction reload interval could not get a register");

  setBit(&ls->spilled, iv->vreg - FIRST_VREG);
  ls->anySpilled = TRUE;
}

// The first free register in the target's own preference order, which is what
// puts a value in a caller-saved register unless it has to survive a call.
static uint32_t pickFreeRegister(const LinearScan *ls, enum RegClass rc, uint64_t busy) {
  const uint64_t candidates = ls->allocatable[rc] & ~busy & ~ls->taken;

  if (candidates == 0) {
    return NO_REG;
  }

  for (uint32_t idx = 0; idx < ls->target->allocatableRegCount[rc]; ++idx) {
    const uint32_t reg = ls->target->allocatableRegs[rc][idx];

    if (candidates & ((uint64_t)1 << reg)) {
      return reg;
    }
  }

  unreachable("a register in the allocatable mask is not in the allocatable list");
}

// No register was free over the whole of 'iv'. Either something already
// holding one gives it up, or 'iv' does.
static void spillAtInterval(LinearScan *ls, LiveInterval *iv, enum RegClass rc, uint64_t busy) {
  LiveInterval *victim = NULL;

  for (uint32_t idx = 0; idx < ls->numActive; ++idx) {
    LiveInterval *cand = ls->active[idx];

    if (intervalClass(ls, cand) != rc) {
      continue;
    }

    // A register 'iv' could not use anyway is not worth taking from anyone.
    if (busy & ((uint64_t)1 << cand->phys)) {
      continue;
    }

    // Never a register the spiller invented: those are the reloads that make
    // this whole scheme terminate, and spilling one would ask for a reload of
    // a reload. The heuristic below would not choose one in any case - a
    // one-instruction interval never reaches furthest - and this says so
    // rather than relying on it.
    if (isSpillerVreg(ls->spill, cand->vreg)) {
      continue;
    }

    if (victim == NULL || cand->end > victim->end) {
      victim = cand;
    }
  }

  if (victim != NULL && victim->end > iv->end) {
    iv->phys = victim->phys;
    removeActive(ls, victim);
    victim->phys = NO_REG;
    markSpilled(ls, victim);
    addActive(ls, iv);
  } else {
    markSpilled(ls, iv);
  }
}

static void sweep(LinearScan *ls) {
  MachineLiveness *lv = ls->lv;

  for (uint32_t idx = 0; idx < lv->numIntervals; ++idx) {
    LiveInterval *iv = &lv->intervals[idx];
    const enum RegClass rc = intervalClass(ls, iv);

    assert(rc == RC_GP || rc == RC_FP);

    expireOldIntervals(ls, iv->start);

    const uint64_t busy = machineLivenessBusyRange(lv, iv->start, iv->end);
    const uint32_t reg = pickFreeRegister(ls, rc, busy);

    if (reg != NO_REG) {
      iv->phys = reg;
      addActive(ls, iv);
    } else {
      spillAtInterval(ls, iv, rc, busy);
    }
  }
}

// -============================ Rewriting ============================-

// A copy whose two halves came out in the same register says nothing. These
// are what the ABI and phi destruction leave behind, and deleting them is the
// only coalescing this allocator does - the real thing needs an interference
// graph, and that is step 35.
static Boolean isRedundantCopy(const MachineInstr *mi) {
  if (mi->opcode != MOP_COPY) {
    return FALSE;
  }

  assert(mi->numOperands == 2);
  return mi->operands[0].kind == MO_REG && mi->operands[1].kind == MO_REG &&
                 mi->operands[0].info.reg == mi->operands[1].info.reg
             ? TRUE
             : FALSE;
}

static void rewriteOperands(LinearScan *ls) {
  for (MachineBasicBlock *mbb = ls->mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    MachineInstr *mi = mbb->instructions.head;

    while (mi != NULL) {
      MachineInstr *next = mi->next;

      for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
        uint32_t *regs[MAX_OPERAND_REGS];
        uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

        for (uint16_t r = 0; r < numRegs; ++r) {
          if (!isVirtualRegister(*regs[r])) {
            continue;
          }

          const LiveInterval *iv = machineLivenessIntervalFor(ls->lv, *regs[r]);
          assert(iv != NULL && iv->phys != NO_REG &&
                 "a virtual register reached rewriting without an allocation");
          *regs[r] = iv->phys;
        }
      }

      if (isRedundantCopy(mi)) {
        eraseMachineInstr(mi);
      }

      mi = next;
    }
  }
}

// -============================ The driver ============================-

static void initLinearScan(LinearScan *ls, MachineFunction *mf, MachineLiveness *lv,
                           const SpillState *spill) {
  memset(ls, 0, sizeof *ls);

  ls->mf = mf;
  ls->target = mf->target;
  ls->lv = lv;
  ls->spill = spill;

  for (size_t rc = 0; rc < RC_CLASS_COUNT; ++rc) {
    for (uint32_t idx = 0; idx < mf->target->allocatableRegCount[rc]; ++idx) {
      ls->allocatable[rc] |= (uint64_t)1 << mf->target->allocatableRegs[rc][idx];
    }
  }

  ls->active = heapAllocate(sizeof(LiveInterval *) * (lv->numIntervals ? lv->numIntervals : 1));
  initBitSet(&ls->spilled, lv->numVregs ? lv->numVregs : 1);
}

static void releaseLinearScan(LinearScan *ls) {
  releaseHeap(ls->active);
  releaseBitSet(&ls->spilled);
}

void allocateRegistersLinear(MachineFunction *mf) {
  assert(mf->target->allocatableRegCount[RC_GP] != 0 &&
         "this target names no registers stage 2B may hand out");

  SpillState spill;
  initSpillState(&spill, mf);

  // Bounded rather than trusted: every round takes at least one register out
  // of circulation for good, so the count is a real bound and not a guess.
  const size_t maxRounds = mf->vregs.size + 2;

  for (size_t round = 0;; ++round) {
    if (round >= maxRounds) {
      unreachable("linear scan did not reach a fixed point");
    }

    MachineLiveness lv;
    computeMachineLiveness(mf, &lv);

    LinearScan ls;
    initLinearScan(&ls, mf, &lv, &spill);

    sweep(&ls);

    if (!ls.anySpilled) {
      rewriteOperands(&ls);
      releaseLinearScan(&ls);
      releaseMachineLiveness(&lv);
      break;
    }

    // The assignments this round made are thrown away with it: what a round
    // that spilled is for is the spill set, and the next one allocates the
    // rewritten function from scratch rather than patching this answer.
    insertSpillCode(&spill, &ls.spilled);

    releaseLinearScan(&ls);
    releaseMachineLiveness(&lv);
  }

  finishSpillFrame(&spill);
  releaseSpillState(&spill);

  mf->allocator = "linear scan";
}
