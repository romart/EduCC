
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>

extern IrContext *ctx;

static IrBasicBlock *removeUnreachableBlock(IrBasicBlock *block, IrFunction *func) {

  Vector *preds = &block->preds;
  for (size_t idx = 0; idx < preds->size; ++idx) {
    IrBasicBlock *pred = getBlockFromVector(preds, idx);
    removeFromVector(&pred->succs, (intptr_t)block);
  }

  // Leaving a successor has to go through removeSuccessor(): a phi there
  // still lists this block as one of its incoming edges, and dropping the
  // edge without dropping that entry would leave a live phi reading a value
  // defined in a block that no longer exists. That use also keeps the value
  // alive, so the erase loop below would wait forever for this block to go
  // empty. removeSuccessor() shrinks block->succs, hence the index-free walk.
  while (block->succs.size != 0) {
    removeSuccessor(block, getBlockFromVector(&block->succs, 0));
  }

  // When this runs from buildDominatorInfo() the tree is about to be rebuilt
  // anyway, but from dce() nothing rebuilds it - so a block that leaves the
  // function must leave its dominator's child list with it, or the dump (and
  // any later walk) still reports a block that is gone. Anything this block
  // dominated is unreachable too and detaches itself the same way.
  IrBasicBlock *idom = block->dominators.sdom;
  if (idom != NULL) {
    removeFromVector(&idom->dominators.dominatees, (intptr_t)block);
  }

  /* func->numOfBlocks -= 1; */
  return eraseBlock(block);
}

static void dfs(IrBasicBlock *block, BitSet *visited) {
  if (getBit(visited, block->id))
    return;

  setBit(visited, block->id);

  Vector *succs = &block->succs;
  for (size_t idx = 0; idx < succs->size; ++idx) {
    IrBasicBlock *succ = getBlockFromVector(succs, idx);
    dfs(succ, visited);
  }
}

static Boolean removeUnusedInstructions(IrBasicBlock *block, size_t *erased) {
  IrInstruction *instr = block->instrunctions.tail;
  while (instr != NULL) {
    IrInstruction *p = instr->prev;

    if (instr->uses.size == 0) {
      eraseInstruction(instr);
      releaseInstruction(instr);
      *erased += 1;
    }
    instr = p;
  }

  return block->instrunctions.head == NULL;
}

// Erases every instruction of every unreachable block. One sweep is not
// enough: an instruction only goes when nothing uses it, and the users of one
// unreachable instruction are usually other unreachable instructions further
// down, so a use is dropped only once its user has been erased. Sweeping until
// nothing is left cascades through those chains whatever order they are in.
//
// Everything still standing after a sweep that erased nothing is used from
// outside the unreachable set, which means a live instruction reads a value
// defined in dead code. That is IR this pass cannot fix - erasing the
// definition would leave the use dangling, and keeping it would leave the
// function holding a block it no longer lists. It is also not supposed to
// happen: the only definitions whose uses are not dominated by them are the
// allocas of jumped-over declarations, and those are emitted in the entry
// block for exactly this reason (createLocalSlot in src/ir/ast2ir.c). So the
// no-progress case is a bug elsewhere, and it stops here rather than looping
// forever waiting for a use count that will never fall.
static void unlinkAndEraseInstructions(IrFunction *func, Vector *ublocks) {

  while (ublocks->size != 0) {
    size_t erased = 0;

    for (size_t i = 0; i < ublocks->size;) {
      IrBasicBlock *block = getBlockFromVector(ublocks, i);

      // TODO: deal with phi-nodes
      Boolean empty = removeUnusedInstructions(block, &erased);
      if (empty) {
        removeFromVector(ublocks, (intptr_t) block);
        continue;
      }

      ++i;
    }

    assert(erased != 0 &&
           "unreachable block defines a value used from a reachable one");
  }
}

void cleanupUnreachableBlock(IrFunction *func) {
    uint32_t blockCount = func->numOfBlocks;
    BitSet visited;
    initBitSet(&visited, blockCount);

    dfs(func->entry, &visited);
    Vector unreachableBlocks = { 0 };
    initVector(&unreachableBlocks, INITIAL_VECTOR_CAPACITY);

    IrBasicBlock *b = func->blocks.head;
    while (b != NULL) {
      if (getBit(&visited, b->id)) {
        b = b->next;
      } else {
        addBlockToVector(&unreachableBlocks, b);
        b = removeUnreachableBlock(b, func);
      }
    }

    unlinkAndEraseInstructions(func, &unreachableBlocks);

    releaseVector(&unreachableBlocks);
    releaseBitSet(&visited);
}


static Boolean hasSideEffects(enum IrIntructionKind k) {
  switch (k) {
  case IR_DEF_CONST:
  case IR_M_STORE:
  case IR_M_COPY:
  // Nothing reads what it produces - it produces nothing - and everything it
  // matters to is addressed through a register it moves.
  case IR_STACK_RESTORE:
  case IR_CALL:
  case IR_IBRANCH:
  case IR_TBRANCH:
  case IR_CBRANCH:
  case IR_BRANCH:
  case IR_RET:
    return TRUE;
  default:
    return FALSE;
  }
}

void cleanupDeadInstructions(IrFunction *func) {
  Boolean changed = TRUE;
  uint32_t iter = 0;
  while (changed) {
    changed = FALSE;

    printf("DCE iteration %u...\n", iter++);
    for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
      IrInstruction *instr = block->instrunctions.tail;

      while (instr != NULL) {
        IrInstruction *p = instr->prev;
        if (instr->uses.size == 0) {
          if (!hasSideEffects(instr->kind)) {
            eraseInstruction(instr);
            releaseInstruction(instr);
            changed = TRUE;
          }
        }

        instr = p;
      }
    }
  }
}

void dce(IrFunction *func) {
  assert(func->numOfBlocks == ctx->bbCnt);
  cleanupDeadInstructions(func);
  cleanupUnreachableBlock(func);
}

