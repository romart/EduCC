
#include "ir/ir.h"
#include <assert.h>



static void computeDominationSets(BitSet *dom, size_t blockCount, IrFunction *func) {
    // Algorithm D

    for (size_t idx = 0; idx < blockCount; ++idx) {
      setAll(&dom[idx]);
    }

    BitSet temp = {0};
    initBitSet(&temp, blockCount);
    // Dom[entry] = Set<Block>{entry};
    clearAll(&dom[func->entry->id]);
    setBit(&dom[func->entry->id], func->entry->id);

    Boolean changed = TRUE;
    IrBasicBlock *entryBB = func->entry;

    while (changed) {
      changed = FALSE;

      for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
        IrBasicBlock *bb = bn->block;
        if (bb == entryBB)
          continue;
        setAll(&temp);
        //printf("Check block #%u...\n", bb->id);
        for (IrBasicBlockListNode *pn = bb->preds.head; pn != NULL; pn = pn->next) {
          IrBasicBlock *pred = pn->block;
          intersectBitSets(&temp, &dom[pred->id], &temp);
        }
        setBit(&temp, bb->id);
        if (compareBitSets(&dom[bb->id], &temp) != 0) {
          //printf("Block #%u changed state\n", bb->id);
          changed = TRUE;
          copyBitSet(&temp, &dom[bb->id]);
        }
      }
    }

    releaseBitSet(&temp);
}


static IrBasicBlock *closestDominator(const IrBasicBlock *block, const BitSet *dominationSet) {
    if (block->preds.head == NULL)
      return NULL;

    IrBasicBlock *pred = block->preds.head->block;
    assert(pred != NULL);

    if (getBit(dominationSet, pred->id))
      return pred;

    return closestDominator(pred, dominationSet);
}

static void buildDominatorTree(IrFunction *func, BitSet *domSets, const size_t blockCount) {
    // Algorithm DT

    IrBasicBlock *entryBB = func->entry;

    for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
        IrBasicBlock *bb = bn->block;
        BitSet *blockDomSet = &domSets[bb->id];

        clearBit(blockDomSet, bb->id);
        if (isEmptyBitSet(blockDomSet)) {
          printf("Block #%u has no dominator...\n", bb->id);
          continue;
        }

        if (countBits(blockDomSet) == 1) {
          assert(getBit(blockDomSet, entryBB->id));
          bb->dominators.sdom = entryBB;
          continue;
        }

        IrBasicBlock *dom = closestDominator(bb, blockDomSet);
        printf("Set dominator for block #%u to block #%d (%p)\n", bb->id, dom ? dom->id : -1, dom);
        bb->dominators.sdom = dom;
    }

    for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
      IrBasicBlock *bb = bn->block;
      IrBasicBlock *dominator = bb->dominators.sdom;
      if (dominator != NULL) {
        addBBTail(&dominator->dominators.dominatees, bb);
      }
    }
}


static void buildDominationFrontier(IrFunction *func, BitSet *bitsets, const size_t blockCount) {
    // Algorithm DF

    for (size_t idx = 0; idx < blockCount; ++idx) {
      clearAll(&bitsets[idx]);
    }

    for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
      IrBasicBlock *bb = bn->block;
      IrBasicBlock *dom = bb->dominators.sdom;
      for (IrBasicBlockListNode *pn = bb->preds.head; pn != NULL; pn = pn->next) {
        IrBasicBlock *r = pn->block;
        while (r != dom) {
          BitSet *df = &bitsets[r->id];
          setBit(df, bb->id);
          r = r->dominators.sdom;
        }
      }
    }

    for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
      IrBasicBlock *bb = bn->block;
      BitSet *df = &bitsets[bb->id];
      for (IrBasicBlockListNode *bf = func->blocks.head; bf != NULL; bf = bf->next) {
        IrBasicBlock *f = bf->block;
        if (getBit(df, f->id)) {
          assert(f->dominators.sdom != bb);
          addBBTail(&bb->dominators.dominationFrontier, f);
        }
      }
    }
}

void buildDominatorInfo(IrContext *ctx, IrFunction *func) {

    const size_t blockCount = ctx->bbCnt;
    BitSet *dom = heapAllocate(blockCount * sizeof(BitSet));
    for (size_t idx = 0; idx < blockCount; ++idx) {
      initBitSet(&dom[idx], blockCount);
    }

    cleanupUnreachableBlock(func);
    computeDominationSets(dom, blockCount, func);
    buildDominatorTree(func, dom, blockCount);
    buildDominationFrontier(func, dom, blockCount);

    for (size_t idx = 0; idx < blockCount; ++idx) {
      releaseBitSet(&dom[idx]);
    }
    releaseHeap(dom);
}

