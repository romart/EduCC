
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

  Vector *succs = &block->succs;
  for (size_t idx = 0; idx < succs->size; ++idx) {
    IrBasicBlock *succ = getBlockFromVector(succs, idx);
    removeFromVector(&succ->preds, (intptr_t)block);
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

static Boolean removeUnusedInstructions(IrBasicBlock *block) {
  IrInstruction *instr = block->instrunctions.tail;
  while (instr != NULL) {
    IrInstruction *p = instr->prev;

    if (instr->uses.size == 0) {
      eraseInstruction(instr);
      releaseInstruction(instr);
    }
    instr = p;
  }

  return block->instrunctions.head == NULL;
}

static void unlinkAndEraseInstructions(IrFunction *func, Vector *ublocks) {

  size_t i = 0;

  while (ublocks->size != 0) {
    for (size_t i = 0; i < ublocks->size;) {
      IrBasicBlock *block = getBlockFromVector(ublocks, i);

      // TODO: deal with phi-nodes
      Boolean empty = removeUnusedInstructions(block);
      if (empty) {
        removeFromVector(ublocks, (intptr_t) block);
        continue;
      }

      ++i;
    }
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

