#include <assert.h>

#include "ir/loops.h"
#include "mem.h"

// -============================ Back edges ============================-
//
// A depth-first walk with the usual three colours, except that "grey" is all
// that has to be recorded: an edge onto a block still on the walk's own stack
// is a back edge, an edge onto a finished block is a forward or cross edge and
// says nothing here.
//
// Recursive, like every other walk over this CFG. The depth is the length of
// the longest acyclic path through one function's blocks, which is what
// layoutVisit and the dominator builder already stand on.

typedef struct _BackEdgeSearch {
  Boolean *seen;
  Boolean *onStack;
  Vector *backEdges; // latch, header, latch, header, ...
} BackEdgeSearch;

static void findBackEdges(MachineBasicBlock *mbb, BackEdgeSearch *s) {
  s->seen[mbb->id] = TRUE;
  s->onStack[mbb->id] = TRUE;

  for (size_t idx = 0; idx < mbb->succs.size; ++idx) {
    MachineBasicBlock *succ = (MachineBasicBlock *)getFromVector(&mbb->succs, idx);

    if (s->onStack[succ->id]) {
      addToVector(s->backEdges, (intptr_t)mbb);
      addToVector(s->backEdges, (intptr_t)succ);
    } else if (!s->seen[succ->id]) {
      findBackEdges(succ, s);
    }
  }

  s->onStack[mbb->id] = FALSE;
}

// -============================ Natural loops ============================-

static MachineLoop *loopForHeader(MachineLoopInfo *li, MachineBasicBlock *header) {
  for (size_t idx = 0; idx < li->loops.size; ++idx) {
    MachineLoop *loop = (MachineLoop *)getFromVector(&li->loops, idx);
    if (loop->header == header) {
      return loop;
    }
  }

  MachineLoop *loop = heapAllocate(sizeof(MachineLoop));
  memset(loop, 0, sizeof(MachineLoop));
  loop->header = header;
  initVector(&loop->latches, 2);
  initVector(&loop->blocks, 8);
  addToVector(&loop->blocks, (intptr_t)header);

  addToVector(&li->loops, (intptr_t)loop);
  return loop;
}

// Everything that reaches the latch without leaving through the header, walked
// backwards from the latch. Marking the header before the walk starts is what
// stops it: a predecessor chain that would have to pass through the header to
// get here is not in the loop.
static void addNaturalLoopBody(MachineLoop *loop, MachineBasicBlock *latch, Boolean *inLoop) {
  if (latch == loop->header) {
    return;
  }

  Vector stack = {0};
  initVector(&stack, 8);

  if (!inLoop[latch->id]) {
    inLoop[latch->id] = TRUE;
    addToVector(&loop->blocks, (intptr_t)latch);
    pushToStack(&stack, (intptr_t)latch);
  }

  while (stack.size != 0) {
    MachineBasicBlock *mbb = (MachineBasicBlock *)popFromStack(&stack);

    for (size_t idx = 0; idx < mbb->preds.size; ++idx) {
      MachineBasicBlock *pred = (MachineBasicBlock *)getFromVector(&mbb->preds, idx);

      if (!inLoop[pred->id]) {
        inLoop[pred->id] = TRUE;
        addToVector(&loop->blocks, (intptr_t)pred);
        pushToStack(&stack, (intptr_t)pred);
      }
    }
  }

  releaseVector(&stack);
}

// -============================ Nesting ============================-
//
// Loop B is inside loop A when A's blocks are a strict superset of B's, and a
// loop's parent is the smallest of the loops that contain it that way.
//
// Strict superset and not "A contains B's header", which is the cheaper test
// and the wrong one. In an irreducible CFG - which in C means a 'goto' into
// the middle of a loop, and which computed_goto.c builds several of - two
// loops can each hold the other's header while neither is inside the other.
// Containment by header would make each the other's parent, and the walk up
// the parent chain below would not terminate; that is not a hypothetical, it
// is what the first version of this file did to test/testData/codegen/
// experimental/computed_goto.c. Strict superset is antisymmetric, so the
// parent chain strictly shrinks and always ends.
//
// Two loops with the same block set and different headers are therefore
// siblings rather than one inside the other. That is a multi-entry loop, which
// has no single header to name and no nesting to state; the depth it gets is
// the depth of whatever properly contains them both.
//
// Quadratic in the number of loops in a function, over sets linear in its
// blocks. Functions here have single-digit loop counts; if one day they do
// not, this is the place that has to become a real forest build.

static void buildNesting(MachineLoopInfo *li) {
  const size_t numLoops = li->loops.size;

  if (numLoops == 0) {
    return;
  }

  // Membership as a row per loop, so containment is a walk over the smaller
  // loop rather than over the product of the two.
  const size_t stride = li->numIds ? li->numIds : 1;
  Boolean *member = heapAllocate(numLoops * stride * sizeof(Boolean));
  memset(member, 0, numLoops * stride * sizeof(Boolean));

  for (size_t idx = 0; idx < numLoops; ++idx) {
    const MachineLoop *loop = (const MachineLoop *)getFromVector(&li->loops, idx);

    for (size_t b = 0; b < loop->blocks.size; ++b) {
      const MachineBasicBlock *mbb = (const MachineBasicBlock *)getFromVector(&loop->blocks, b);
      member[idx * stride + mbb->id] = TRUE;
    }
  }

  for (size_t idx = 0; idx < numLoops; ++idx) {
    MachineLoop *inner = (MachineLoop *)getFromVector(&li->loops, idx);

    for (size_t other = 0; other < numLoops; ++other) {
      MachineLoop *outer = (MachineLoop *)getFromVector(&li->loops, other);

      // Strict, so equal block sets are neither one's parent.
      if (other == idx || outer->blocks.size <= inner->blocks.size) {
        continue;
      }

      // Already known to be no tighter than the parent in hand.
      if (inner->parent != NULL && outer->blocks.size >= inner->parent->blocks.size) {
        continue;
      }

      Boolean contained = TRUE;
      for (size_t b = 0; b < inner->blocks.size && contained; ++b) {
        const MachineBasicBlock *mbb = (const MachineBasicBlock *)getFromVector(&inner->blocks, b);
        contained = member[other * stride + mbb->id];
      }

      if (contained) {
        inner->parent = outer;
      }
    }
  }

  releaseHeap(member);

  // Depth is the parent chain's length, which terminates because every step up
  // it lands on a strictly larger block set. Walked per loop rather than in one
  // pass, since the parent of a loop can appear after it in li->loops.
  for (size_t idx = 0; idx < numLoops; ++idx) {
    MachineLoop *loop = (MachineLoop *)getFromVector(&li->loops, idx);
    uint32_t depth = 1;

    for (const MachineLoop *p = loop->parent; p != NULL; p = p->parent) {
      depth += 1;
      assert(depth <= numLoops && "loop nesting is not a forest");
    }

    loop->depth = depth;
  }
}

static void buildBlockIndex(MachineLoopInfo *li) {
  for (size_t idx = 0; idx < li->loops.size; ++idx) {
    MachineLoop *loop = (MachineLoop *)getFromVector(&li->loops, idx);

    for (size_t b = 0; b < loop->blocks.size; ++b) {
      const MachineBasicBlock *mbb = (const MachineBasicBlock *)getFromVector(&loop->blocks, b);
      assert(mbb->id < li->numIds);

      if (loop->depth > li->depthOf[mbb->id]) {
        li->depthOf[mbb->id] = loop->depth;
        li->innermostOf[mbb->id] = loop;
      }
    }
  }
}

void computeMachineLoops(MachineFunction *mf, MachineLoopInfo *li) {
  memset(li, 0, sizeof(MachineLoopInfo));
  li->mf = mf;
  initVector(&li->loops, 4);

  size_t numIds = 0;
  for (const MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    if (mbb->id + 1 > numIds) {
      numIds = mbb->id + 1;
    }
  }

  const size_t slots = numIds ? numIds : 1;

  li->numIds = numIds;
  li->depthOf = heapAllocate(slots * sizeof(uint32_t));
  li->innermostOf = heapAllocate(slots * sizeof(MachineLoop *));
  memset(li->depthOf, 0, slots * sizeof(uint32_t));
  memset(li->innermostOf, 0, slots * sizeof(MachineLoop *));

  if (numIds == 0) {
    return;
  }

  Vector backEdges = {0};
  initVector(&backEdges, 4);

  BackEdgeSearch search = {0};
  search.seen = heapAllocate(numIds * sizeof(Boolean));
  search.onStack = heapAllocate(numIds * sizeof(Boolean));
  search.backEdges = &backEdges;
  memset(search.seen, 0, numIds * sizeof(Boolean));
  memset(search.onStack, 0, numIds * sizeof(Boolean));

  findBackEdges(mf->blocks.head, &search);

  releaseHeap(search.onStack);
  releaseHeap(search.seen);

  if (backEdges.size != 0) {
    Boolean *inLoop = heapAllocate(numIds * sizeof(Boolean));

    for (size_t idx = 0; idx < backEdges.size; idx += 2) {
      MachineBasicBlock *latch = (MachineBasicBlock *)getFromVector(&backEdges, idx);
      MachineBasicBlock *header = (MachineBasicBlock *)getFromVector(&backEdges, idx + 1);

      MachineLoop *loop = loopForHeader(li, header);
      addToVector(&loop->latches, (intptr_t)latch);

      // Seeded from what the loop already holds, so a second back edge onto
      // the same header extends the one loop instead of restarting it.
      memset(inLoop, 0, numIds * sizeof(Boolean));
      for (size_t b = 0; b < loop->blocks.size; ++b) {
        const MachineBasicBlock *mbb = (const MachineBasicBlock *)getFromVector(&loop->blocks, b);
        inLoop[mbb->id] = TRUE;
      }

      addNaturalLoopBody(loop, latch, inLoop);
    }

    releaseHeap(inLoop);
  }

  releaseVector(&backEdges);

  buildNesting(li);
  buildBlockIndex(li);
}

void releaseMachineLoops(MachineLoopInfo *li) {
  for (size_t idx = 0; idx < li->loops.size; ++idx) {
    MachineLoop *loop = (MachineLoop *)getFromVector(&li->loops, idx);
    releaseVector(&loop->latches);
    releaseVector(&loop->blocks);
    releaseHeap(loop);
  }

  releaseVector(&li->loops);
  releaseHeap(li->innermostOf);
  releaseHeap(li->depthOf);

  li->depthOf = NULL;
  li->innermostOf = NULL;
  li->numIds = 0;
}

uint32_t machineLoopDepthOf(const MachineLoopInfo *li, const MachineBasicBlock *mbb) {
  return mbb->id < li->numIds ? li->depthOf[mbb->id] : 0;
}

MachineLoop *machineLoopOf(const MachineLoopInfo *li, const MachineBasicBlock *mbb) {
  return mbb->id < li->numIds ? li->innermostOf[mbb->id] : NULL;
}

Boolean machineLoopIsHeader(const MachineLoopInfo *li, const MachineBasicBlock *mbb) {
  for (size_t idx = 0; idx < li->loops.size; ++idx) {
    if (((const MachineLoop *)getFromVector(&li->loops, idx))->header == mbb) {
      return TRUE;
    }
  }

  return FALSE;
}

Boolean machineLoopIsBackEdge(const MachineLoopInfo *li, const MachineBasicBlock *from,
                              const MachineBasicBlock *to) {
  for (size_t idx = 0; idx < li->loops.size; ++idx) {
    const MachineLoop *loop = (const MachineLoop *)getFromVector(&li->loops, idx);

    if (loop->header != to) {
      continue;
    }

    for (size_t l = 0; l < loop->latches.size; ++l) {
      if ((const MachineBasicBlock *)getFromVector(&loop->latches, l) == from) {
        return TRUE;
      }
    }
  }

  return FALSE;
}
