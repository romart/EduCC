#include <assert.h>
#include <string.h>

#include "mem.h"
#include "utils.h"
#include "ir/ir.h"
#include "ir/liveness.h"

// -============================ Liveness over machine code ================-
//
// See include/ir/liveness.h for what this is for. The dataflow itself is the
// textbook one:
//
//   live_out[B] = union of live_in[S] for S in succ(B)
//   live_in[B]  = transfer of live_out[B] backwards through B's instructions
//
// with the transfer applied instruction by instruction rather than summarized
// into use/def sets first. Summarizing would save a walk per iteration and
// cost a second description of what an instruction does to a register, which
// is the thing most worth having exactly one of: machineOperandIsRead() is
// where a partial def becomes a read, and a use/def summary is one more place
// that could forget to ask it.

// Physical registers occupy the low indices of the dataflow bitset and virtual
// ones follow, so the two are one lattice and a value flowing between them -
// which is what every ABI copy is - needs no translation.
static size_t regBitIndex(const MachineFunction *mf, uint32_t reg) {
  assert(reg != NO_REG);

  if (isVirtualRegister(reg)) {
    return mf->target->numPhysRegs + (size_t)(reg - FIRST_VREG);
  }

  assert(reg < mf->target->numPhysRegs);
  return reg;
}

typedef struct _BlockLiveness {
  MachineBasicBlock *mbb;

  BitSet in;
  BitSet out;

  // The positions this block occupies, when it has any. A block with no
  // instructions is not extended over: anything live through it is live out of
  // a predecessor and live into a successor, and both of those have positions.
  uint32_t first;
  uint32_t last;
  Boolean hasInstructions;
} BlockLiveness;

// One instruction's effect on the set live *after* it, turning it into the set
// live before it. Defs kill and uses revive, in that order, so that a register
// an instruction both writes and reads - two-address form, and every partial
// def - comes out live, which is what it is.
static void transferInstruction(const MachineFunction *mf, MachineInstr *mi, BitSet *live) {
  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    MachineOperand *op = &mi->operands[idx];

    if (!machineOperandIsWritten(op)) {
      continue;
    }

    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(op, regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      clearBit(live, regBitIndex(mf, *regs[r]));
    }
  }

  // A call destroys every caller-saved register whether or not it names one,
  // which is the whole of what MachineInstr.flags.isCall says. Killing them
  // here rather than only marking them busy keeps a physical register's range
  // from running through a call it could not have survived.
  if (mi->flags.isCall) {
    const TargetDescriptor *target = mf->target;

    for (uint32_t idx = 0; idx < target->callerSavedRegCount; ++idx) {
      clearBit(live, regBitIndex(mf, target->callerSavedRegs[idx]));
    }
  }

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    MachineOperand *op = &mi->operands[idx];

    if (!machineOperandIsRead(op)) {
      continue;
    }

    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(op, regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      setBit(live, regBitIndex(mf, *regs[r]));
    }
  }
}

// The physical half of a live set, as a mask. The bitset's word layout would
// give it in a single read and this loop is written instead, because the
// bitset does not promise one - and thirty-odd register ids per instruction is
// not what makes a compile slow.
static uint64_t physMask(const MachineFunction *mf, const BitSet *live) {
  uint64_t mask = 0;

  for (uint32_t reg = 0; reg < mf->target->numPhysRegs; ++reg) {
    if (getBit(live, reg)) {
      mask |= (uint64_t)1 << reg;
    }
  }

  return mask;
}

// Every physical register the instruction names itself, read or written. These
// are busy at its own position even where liveness says they are dead either
// side of it: an idiv naming rdx:rax leaves nowhere for a virtual register to
// sit in them, and the value it puts in rdx may be dead on arrival.
static uint64_t namedPhysMask(MachineInstr *mi) {
  uint64_t mask = 0;

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      if (isPhysicalRegister(*regs[r])) {
        mask |= (uint64_t)1 << *regs[r];
      }
    }
  }

  return mask;
}

static uint64_t callerSavedMask(const TargetDescriptor *target) {
  uint64_t mask = 0;

  for (uint32_t idx = 0; idx < target->callerSavedRegCount; ++idx) {
    mask |= (uint64_t)1 << target->callerSavedRegs[idx];
  }

  return mask;
}

// -============================ The fixed point ============================-

static void runDataflow(const MachineFunction *mf, BlockLiveness *bls, size_t numBlocks,
                        const int32_t *blockIndex, size_t setSize) {
  BitSet live;
  initBitSet(&live, setSize);

  Boolean changed = TRUE;

  // Reverse layout order, which for a reducible CFG walks most edges the right
  // way round and converges in two or three passes rather than in as many as
  // the function is deep.
  while (changed) {
    changed = FALSE;

    for (size_t idx = numBlocks; idx-- > 0;) {
      BlockLiveness *bl = &bls[idx];

      clearAll(&live);
      for (size_t s = 0; s < bl->mbb->succs.size; ++s) {
        const MachineBasicBlock *succ =
            (const MachineBasicBlock *)getFromVector(&bl->mbb->succs, s);
        mergeBitSets(&live, &bls[blockIndex[succ->id]].in, &live);
      }

      copyBitSet(&live, &bl->out);

      for (MachineInstr *mi = bl->mbb->instructions.tail; mi != NULL; mi = mi->prev) {
        transferInstruction(mf, mi, &live);
      }

      if (compareBitSets(&live, &bl->in) != 0) {
        copyBitSet(&live, &bl->in);
        changed = TRUE;
      }
    }
  }

  releaseBitSet(&live);
}

// -============================ Intervals ============================-

static void extendInterval(LiveInterval *iv, uint32_t vreg, uint32_t pos) {
  if (iv->vreg == NO_REG) {
    iv->vreg = vreg;
    iv->start = pos;
    iv->end = pos;
    iv->phys = NO_REG;
    return;
  }

  if (pos < iv->start) {
    iv->start = pos;
  }
  if (pos > iv->end) {
    iv->end = pos;
  }
}

// -============================ The whole answer ============================-

void computeMachineLiveness(MachineFunction *mf, MachineLiveness *lv) {
  const TargetDescriptor *target = mf->target;

  memset(lv, 0, sizeof *lv);
  lv->mf = mf;
  lv->numVregs = (uint32_t)mf->vregs.size;

  size_t numBlocks = 0;
  uint32_t maxBlockId = 0;
  uint32_t numPositions = 0;

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    numBlocks += 1;
    if (mbb->id > maxBlockId) {
      maxBlockId = mbb->id;
    }
    for (const MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      numPositions += 1;
    }
  }

  lv->numPositions = numPositions;
  lv->numChunks = (numPositions + LIVENESS_CHUNK - 1) / LIVENESS_CHUNK;

  if (numPositions == 0) {
    return;
  }

  lv->instrAt = heapAllocate(sizeof(MachineInstr *) * numPositions);
  lv->physBusy = heapAllocate(sizeof(uint64_t) * numPositions);
  lv->chunkBusy = heapAllocate(sizeof(uint64_t) * lv->numChunks);

  BlockLiveness *bls = heapAllocate(sizeof(BlockLiveness) * numBlocks);
  int32_t *blockIndex = heapAllocate(sizeof(int32_t) * (maxBlockId + 1));

  const size_t setSize = target->numPhysRegs + lv->numVregs;

  size_t bIdx = 0;
  uint32_t pos = 0;
  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next, ++bIdx) {
    BlockLiveness *bl = &bls[bIdx];

    bl->mbb = mbb;
    initBitSet(&bl->in, setSize);
    initBitSet(&bl->out, setSize);
    bl->first = pos;
    bl->hasInstructions = mbb->instructions.head != NULL ? TRUE : FALSE;

    for (MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      lv->instrAt[pos++] = mi;
    }

    bl->last = pos != bl->first ? pos - 1 : bl->first;
    blockIndex[mbb->id] = (int32_t)bIdx;
  }

  runDataflow(mf, bls, numBlocks, blockIndex, setSize);

  LiveInterval *byVreg = heapAllocate(sizeof(LiveInterval) * (lv->numVregs ? lv->numVregs : 1));
  for (uint32_t v = 0; v < lv->numVregs; ++v) {
    byVreg[v].vreg = NO_REG;
  }

  const uint64_t callerSaved = callerSavedMask(target);

  BitSet live;
  initBitSet(&live, setSize);

  // Second walk, now that the block boundaries have settled: the busy mask at
  // every position, and the block-level half of every hull.
  for (size_t idx = 0; idx < numBlocks; ++idx) {
    BlockLiveness *bl = &bls[idx];

    if (!bl->hasInstructions) {
      continue;
    }

    copyBitSet(&bl->out, &live);

    uint32_t p = bl->last;
    for (MachineInstr *mi = bl->mbb->instructions.tail; mi != NULL; mi = mi->prev, --p) {
      const uint64_t after = physMask(mf, &live);
      transferInstruction(mf, mi, &live);
      const uint64_t before = physMask(mf, &live);

      lv->physBusy[p] = after | before | namedPhysMask(mi) |
                        (mi->flags.isCall ? callerSaved : 0);
    }

    for (uint32_t v = 0; v < lv->numVregs; ++v) {
      const size_t bit = target->numPhysRegs + v;

      if (getBit(&bl->in, bit)) {
        extendInterval(&byVreg[v], FIRST_VREG + v, bl->first);
      }
      if (getBit(&bl->out, bit)) {
        extendInterval(&byVreg[v], FIRST_VREG + v, bl->last);
      }
    }
  }

  releaseBitSet(&live);

  // And the instruction-level half: every position a register is named at is a
  // position it is live at, whether or not the dataflow carried it across a
  // block boundary.
  for (uint32_t p = 0; p < numPositions; ++p) {
    MachineInstr *mi = lv->instrAt[p];

    for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
      uint32_t *regs[MAX_OPERAND_REGS];
      uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

      for (uint16_t r = 0; r < numRegs; ++r) {
        if (isVirtualRegister(*regs[r])) {
          extendInterval(&byVreg[*regs[r] - FIRST_VREG], *regs[r], p);
        }
      }
    }
  }

  for (uint32_t c = 0; c < lv->numChunks; ++c) {
    uint64_t mask = 0;
    const uint32_t to = (c + 1) * LIVENESS_CHUNK < numPositions ? (c + 1) * LIVENESS_CHUNK
                                                                : numPositions;
    for (uint32_t p = c * LIVENESS_CHUNK; p < to; ++p) {
      mask |= lv->physBusy[p];
    }
    lv->chunkBusy[c] = mask;
  }

  // Counting sort by start position rather than qsort: the key is already a
  // small dense integer, ties come out in virtual register order without
  // anything having to say so, and the result does not depend on a library
  // sort's behaviour on equal keys - which a golden dump would notice.
  lv->vregToInterval = heapAllocate(sizeof(int32_t) * (lv->numVregs ? lv->numVregs : 1));

  uint32_t counted = 0;
  for (uint32_t v = 0; v < lv->numVregs; ++v) {
    lv->vregToInterval[v] = -1;
    if (byVreg[v].vreg != NO_REG) {
      counted += 1;
    }
  }

  lv->numIntervals = counted;
  lv->intervals = heapAllocate(sizeof(LiveInterval) * (counted ? counted : 1));

  uint32_t *starts = heapAllocate(sizeof(uint32_t) * (numPositions + 1));
  for (uint32_t v = 0; v < lv->numVregs; ++v) {
    if (byVreg[v].vreg != NO_REG) {
      starts[byVreg[v].start] += 1;
    }
  }

  uint32_t running = 0;
  for (uint32_t p = 0; p <= numPositions; ++p) {
    const uint32_t here = starts[p];
    starts[p] = running;
    running += here;
  }

  for (uint32_t v = 0; v < lv->numVregs; ++v) {
    if (byVreg[v].vreg == NO_REG) {
      continue;
    }

    const uint32_t at = starts[byVreg[v].start]++;
    lv->intervals[at] = byVreg[v];
    lv->vregToInterval[v] = (int32_t)at;
  }

  releaseHeap(starts);
  releaseHeap(byVreg);

  for (size_t idx = 0; idx < numBlocks; ++idx) {
    releaseBitSet(&bls[idx].in);
    releaseBitSet(&bls[idx].out);
  }

  releaseHeap(bls);
  releaseHeap(blockIndex);
}

void releaseMachineLiveness(MachineLiveness *lv) {
  if (lv->instrAt != NULL) {
    releaseHeap(lv->instrAt);
    releaseHeap(lv->physBusy);
    releaseHeap(lv->chunkBusy);
  }

  if (lv->intervals != NULL) {
    releaseHeap(lv->intervals);
    releaseHeap(lv->vregToInterval);
  }

  memset(lv, 0, sizeof *lv);
}

uint64_t machineLivenessBusyRange(const MachineLiveness *lv, uint32_t from, uint32_t to) {
  assert(from <= to && to < lv->numPositions);

  uint64_t busy = 0;
  uint32_t p = from;

  while (p <= to) {
    if (p % LIVENESS_CHUNK == 0 && p + LIVENESS_CHUNK - 1 <= to) {
      busy |= lv->chunkBusy[p / LIVENESS_CHUNK];
      p += LIVENESS_CHUNK;
    } else {
      busy |= lv->physBusy[p];
      p += 1;
    }
  }

  return busy;
}

LiveInterval *machineLivenessIntervalFor(const MachineLiveness *lv, uint32_t vreg) {
  assert(isVirtualRegister(vreg));

  const uint32_t idx = vreg - FIRST_VREG;
  if (lv->vregToInterval == NULL || idx >= lv->numVregs || lv->vregToInterval[idx] < 0) {
    return NULL;
  }

  return &lv->intervals[lv->vregToInterval[idx]];
}
