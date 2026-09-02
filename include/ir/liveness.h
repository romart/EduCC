#ifndef __IR_LIVENESS_H__
#define __IR_LIVENESS_H__ 1

#include "ir/machine.h"

// How many positions one entry of MachineLiveness.chunkBusy summarizes.
#define LIVENESS_CHUNK 64

// -============================ Liveness over machine code ================-
//
// What every allocator past stage 2A needs and none of them should compute
// twice: where each value is live, and which physical registers are already
// spoken for where. Stage 2B reads it as intervals to sweep; stage 2C will
// read the same numbers as an interference graph. Sharing it is not only
// economy - two allocators disagreeing about liveness would disagree about
// correctness, and the disagreement would show up as one of them producing
// wrong code on inputs the other handles.
//
// Physical and virtual registers go through the same dataflow, which is the
// part that is easy to get wrong by leaving out. Selection puts values in
// fixed registers where the ABI or the ISA demands it - an argument into rdi
// ahead of a call, a dividend into rdx:rax - and those registers are live from
// where selection wrote them to where it reads them. An allocator that only
// tracked virtual registers would see rdi as free between the two and hand it
// to something else.

// One virtual register's live range, as a single span over the linearized
// instruction order.
//
// One span and not a list of them: this is the hull of every position the
// register is live at, so a value live at the top and bottom of a loop is
// treated as live through the middle even where it is not. That over-
// approximates - it can cost a register that was really free - and it can
// never be unsound, which is the trade the original linear-scan paper makes
// and the reason the whole structure is an array of four integers.
typedef struct _LiveInterval {
  uint32_t vreg;

  // Inclusive, in positions; start <= end always, since a register with no
  // occurrence at all gets no interval.
  uint32_t start;
  uint32_t end;

  // Filled in by whichever allocator is sweeping; NO_REG until then, and
  // NO_REG afterwards for an interval that had to be spilled.
  uint32_t phys;
} LiveInterval;

typedef struct _MachineLiveness {
  MachineFunction *mf;

  // Instructions numbered in layout order, which is the order stage 3 will
  // emit them in. Nothing between selection and emission reorders, so a
  // position is a program point and not merely an index.
  uint32_t numPositions;
  MachineInstr **instrAt; // [numPositions]

  // Which physical registers are unavailable at each position: those live
  // across it, those the instruction names itself, and - at a call - every
  // caller-saved register, because that is what a call destroys whether or not
  // it names one. A virtual register's interval may not be given any register
  // this says is busy anywhere inside it.
  uint64_t *physBusy; // [numPositions], a bit per register id

  // The same thing summarized over blocks of LIVENESS_CHUNK positions, so that
  // asking "what is busy anywhere in this interval" costs the length of the
  // interval divided by 64 rather than the length of it. A long-lived value in
  // a large function is otherwise quadratic, and the self-hosted compile is
  // full of both.
  uint64_t *chunkBusy;
  uint32_t numChunks;

  // Sorted by start position, which is the order a linear scan wants and the
  // order a colouring allocator does not mind.
  LiveInterval *intervals;
  uint32_t numIntervals;

  // (vreg - FIRST_VREG) -> index into 'intervals', or -1 for a register that
  // occurs nowhere. Rewriting operands needs this and a sorted array cannot
  // give it.
  int32_t *vregToInterval;
  uint32_t numVregs;

  // The block-level half of the dataflow, kept rather than thrown away once
  // the intervals are built. An allocator that wants interference instead of
  // intervals replays the transfer backwards through a block from its live-out
  // set, and replaying it is how stage 2C reads this answer rather than
  // computing a second one of its own.
  //
  // Blocks are in layout order, the same order 'instrAt' numbers positions in.
  // A block with no instructions has first == last and is not worth walking:
  // nothing is live across it that is not live across its neighbours.
  size_t numBlocks;
  MachineBasicBlock **blockAt;
  BitSet *blockLiveOut; // [numBlocks], indexed by the bit layout below
  uint32_t *blockFirst; // [numBlocks], position of the block's first instruction
  uint32_t *blockLast;

  // Bits in each of those sets: the physical registers first, the virtual ones
  // after them. See machineLivenessRegBit().
  size_t setSize;
} MachineLiveness;

// Computes all of the above over the function as it stands. Cheap enough to
// redo after spill code is inserted, which is exactly what the allocators do
// rather than trying to patch the old answer.
void computeMachineLiveness(MachineFunction *mf, MachineLiveness *lv);
void releaseMachineLiveness(MachineLiveness *lv);

// Every physical register busy anywhere in [from, to] inclusive.
uint64_t machineLivenessBusyRange(const MachineLiveness *lv, uint32_t from, uint32_t to);

// The interval of a virtual register, or NULL if it occurs nowhere.
LiveInterval *machineLivenessIntervalFor(const MachineLiveness *lv, uint32_t vreg);

// Where a register sits in a live set: physical registers at their own ids,
// virtual ones above them. One namespace, so an ABI copy needs no translation
// between the two halves.
size_t machineLivenessRegBit(const MachineFunction *mf, uint32_t reg);

// One instruction's effect on the set live *after* it, turning it into the set
// live before it. Exposed so that an allocator can replay a block backwards
// from its live-out set: one description of what an instruction does to a
// register, and every pass that needs to know asks this one.
void machineLivenessTransfer(const MachineFunction *mf, MachineInstr *mi, BitSet *live);

#endif // __IR_LIVENESS_H__
