
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>

extern IrContext *ctx;

static IrBasicBlockListNode *removeUnreachableBlock(IrBasicBlockListNode *bn, IrFunction *func) {
  IrBasicBlock *block = bn->block;

  for (IrBasicBlockListNode *pn = block->preds.head; pn != NULL; pn = pn->next) {
    IrBasicBlock *pred = pn->block;
    removeFromBlockList(&pred->succs, block);
  }
  for (IrBasicBlockListNode *sn = block->succs.head; sn != NULL; sn = sn->next) {
    IrBasicBlock *succ = sn->block;
    removeFromBlockList(&succ->preds, block);
  }

  func->numOfBlocks -= 1;
  return eraseFromBlockList(&func->blocks, bn);
}

static void dfs(IrBasicBlock *block, BitSet *visited) {
  if (getBit(visited, block->id))
    return;

  setBit(visited, block->id);
  for (IrBasicBlockListNode *sn = block->succs.head; sn != NULL; sn = sn->next) {
    dfs(sn->block, visited);
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

    IrBasicBlockListNode *bn = func->blocks.head;
    while (bn != NULL) {
      IrBasicBlock *b = bn->block;
      if (getBit(&visited, b->id)) {
        bn = bn->next;
      } else {
        addBlockToVector(&unreachableBlocks, b);
        bn = removeUnreachableBlock(bn, func);
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
    for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
      IrBasicBlock *block = bn->block;

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


