#ifndef __IR_LOOPS_H__
#define __IR_LOOPS_H__ 1

#include "ir/machine.h"

// -============================ Loops over machine code ===================-
//
// The loop forest of a MachineFunction: which blocks close a loop, which
// blocks are inside it, and how the loops nest. The IR side has dominators of
// its own (src/ir/dominators.c) but the machine CFG is a separate graph -
// stage 0 splits critical edges into it and phi destruction fills the new
// blocks - so the answer has to be recomputed here rather than carried across.
//
// Two consumers today and the same question asked twice before this existed:
// block layout wants a loop's body laid out ahead of its exit (stage 1,
// layoutBlocks), and the colouring allocator wants occurrences inside a loop
// to count for more than occurrences outside one (stage 2C,
// computeBlockWeights). Neither needs the other's precision, which is how the
// second one came to approximate the first from layout order instead.
//
// Back edges are found by depth-first search - an edge to a block still on the
// walk's own stack goes backwards - rather than from a dominance frontier,
// because layout runs before anything computes dominators over the machine
// CFG and because both consumers are heuristics. The difference between the
// two definitions is an irreducible CFG, which in C means a 'goto' into the
// middle of a loop: there a DFS back edge need not be a dominating one, and
// which edges get called back edges depends on the order the walk took. That
// costs layout quality and spill ranking. It cannot cost correctness - no
// caller of this is allowed to be a correctness question, and the two that
// exist state so where they read it.

// One natural loop: a header, the latches that jump back to it, and every
// block that reaches a latch without passing through the header again.
//
// Two back edges onto one header are one loop with two latches, not two loops
// - which is what a loop with a 'continue' in it looks like once the edges are
// split, and counting it twice would say its body is nested inside itself.
typedef struct _MachineLoop {
  MachineBasicBlock *header;

  // Nesting. parent is the innermost loop that properly contains this one, or
  // NULL for an outermost one; depth counts from 1 at an outermost loop.
  struct _MachineLoop *parent;
  uint32_t depth;

  Vector latches; // of MachineBasicBlock *, the tails of this loop's back edges
  Vector blocks;  // of MachineBasicBlock *, the header included
} MachineLoop;

typedef struct _MachineLoopInfo {
  MachineFunction *mf;

  Vector loops; // of MachineLoop *, outermost before innermost

  // Indexed by block id, which createMachineBasicBlock hands out densely from
  // zero. numIds is one past the largest, so a block added after the analysis
  // ran indexes out of range rather than silently reading a neighbour's answer
  // - see machineLoopDepthOf, which is why these are not read directly.
  size_t numIds;
  uint32_t *depthOf;         // 0 for a block in no loop
  MachineLoop **innermostOf; // NULL for a block in no loop
} MachineLoopInfo;

// Computes the forest over the function as it stands. The CFG is read and not
// written, so a caller that changes it - layout does not, it only relinks the
// layout order - has to recompute rather than patch.
void computeMachineLoops(MachineFunction *mf, MachineLoopInfo *li);
void releaseMachineLoops(MachineLoopInfo *li);

// How many loops a block is inside; 0 outside every loop. Safe for a block the
// analysis never saw, which answers 0.
uint32_t machineLoopDepthOf(const MachineLoopInfo *li, const MachineBasicBlock *mbb);

// The innermost loop containing a block, or NULL if it is in none.
MachineLoop *machineLoopOf(const MachineLoopInfo *li, const MachineBasicBlock *mbb);

// Whether a block heads some loop, and whether an edge closes one.
Boolean machineLoopIsHeader(const MachineLoopInfo *li, const MachineBasicBlock *mbb);
Boolean machineLoopIsBackEdge(const MachineLoopInfo *li, const MachineBasicBlock *from,
                              const MachineBasicBlock *to);

#endif // __IR_LOOPS_H__
