
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>

extern IrContext *ctx;

static void collectLocalDefs(BitSet *defBitSets, IrFunction *func) {

  for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
    IrBasicBlock *bb = bn->block;
    for (IrInstructionListNode *in = bb->instrs.head; in != NULL; in = in->next) {
      IrInstruction *instr = in->instr;
      for (IrOperandListNode *dn = instr->defs.head; dn != NULL; dn = dn->next) {
        IrOperand *def = dn->op;
        if (def->kind != IR_LOCAL)
          continue;

        setBit(&defBitSets[def->data.lid], bb->id);
      }
    }
  }
}

static void insertPhi(IrBasicBlock *phiBlock, IrBasicBlock *defBlock, LocalValueInfo *lvi) {
    IrOperand *localOp = lvi->initialOp;

    IrInstruction *phiInstr = newInstruction(IR_PHI);
    phiInstr->info.lvi = lvi;

    addInstructionDef(phiInstr, localOp);
    addInstuctionHead(&phiBlock->instrs, phiInstr);

    for (IrBasicBlockListNode *pn = phiBlock->preds.head; pn != NULL; pn = pn->next) {
      addPhiInput(phiInstr, localOp, pn->block);
    }
}

static void insertPhiNodes(IrFunction *func) {
    // Algorithm SI
    Vector stack;
    initVector(&stack, INITIAL_VECTOR_CAPACITY);
    uint32_t iter = 0;

    size_t numOfLocalOps = func->numOfLocals;
    if (func->retOperand)
      numOfLocalOps += 1;

    BitSet *bitsets = heapAllocate(numOfLocalOps * 2 * sizeof(BitSet));
    BitSet *defBitSets = bitsets;
    BitSet *insertBitSets = bitsets + numOfLocalOps;
    const size_t blockCount = ctx->bbCnt;

    for (size_t i = 0; i < numOfLocalOps; ++i) {
      initBitSet(&defBitSets[i], blockCount);
      initBitSet(&insertBitSets[i], blockCount);
    }

    collectLocalDefs(defBitSets, func);

    for (size_t i = 0; i < numOfLocalOps; ++i) {
        BitSet *defSet = &defBitSets[i];
        for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
          IrBasicBlock *bb = bn->block;
          if (getBit(defSet, bb->id)) {
            pushToStack(&stack, (intptr_t)bb);
          }
        }

        BitSet *insertSet = &insertBitSets[i];
        LocalValueInfo *lvi = &func->localOperandMap[i];
        while (stack.size > 0) {
          IrBasicBlock *defBlock = (IrBasicBlock *)popFromStack(&stack);
          for (IrBasicBlockListNode *fn = defBlock->dominators.dominationFrontier.head; fn != NULL; fn = fn->next) {
            IrBasicBlock *fb = fn->block;
            if (getBit(insertSet, fb->id)) {
              continue;
            }
            iter++;

            insertPhi(fb, defBlock, lvi);
            setBit(insertSet, fb->id);

            pushToStack(&stack, (intptr_t) fb);
          }
        }
    }

    for (size_t i = 0; i < numOfLocalOps; ++i) {
      releaseBitSet(&defBitSets[i]);
      releaseBitSet(&insertBitSets[i]);
    }

    releaseVector(&stack);
    releaseHeap(bitsets);
}

static IrOperand *vregForLocal(IrOperand *local) {
    assert(local != NULL);
    IrOperand *v = newVreg(local->type);
    v->ast = local->ast;
    v->astType = local->astType;
    return v;
}

static void replacePhiInputs(IrBasicBlock *defBlock, IrBasicBlock *phiBlock, Vector *stacks) {
  printf("Replace phi inputs in #%u coming from #%u...\n", phiBlock->id, defBlock->id);
  for (IrInstructionListNode *in = phiBlock->instrs.head; in != NULL; in = in->next) {
    IrInstruction *instr = in->instr;
    if (instr->kind != IR_PHI) { // assume all phi-nodes are at the beginning of block's instruciton list
      return;
    }

    for (IrOperandListNode *un = instr->uses.head; un != NULL; un = un->next->next) {
      IrOperand *valueOp = un->op;
      if (valueOp->kind != IR_LOCAL)
        continue; // already replaced

      assert(un->next != NULL);
      IrOperand *blockOp = un->next->op;
      assert(blockOp->kind == IR_BLOCK);
      assert(blockOp->type == IR_LABEL);
      if (blockOp->data.bb == defBlock) {
        uint32_t lviIdx = valueOp->data.lid;
        IrOperand *inputOp = (IrOperand *)topOfStack(&stacks[lviIdx]);
        assert(inputOp != NULL);
        assert(inputOp->kind == IR_VREG);
        assert(inputOp->type == valueOp->type);
        assert(inputOp->astType == valueOp->astType);
        replaceInputIn(instr, un, inputOp);
        //un->op = inputOp;
        printf("  replace phi (id = %u) input for @%u -> %c%u\n", instr->id, lviIdx, '%', inputOp->data.vid);
        break;
      }
    }
  }
}

static void renameLocalsImpl(IrBasicBlock *block, LocalValueInfo *lvis, Vector *stacks, const size_t numOfLocalOps) {
  // Algorithm SR

  size_t *resetPoints = (size_t *)heapAllocate(numOfLocalOps * sizeof(size_t));
  memset(resetPoints, 0, numOfLocalOps * sizeof(size_t));

  for (IrInstructionListNode *in = block->instrs.head; in != NULL; in = in->next) {
    IrInstruction *instr = in->instr;
    if (instr->kind != IR_PHI) {
      for (IrOperandListNode *un = instr->uses.head; un != NULL; un = un->next) {
        IrOperand *useOp = un->op;
        if (useOp->kind == IR_LOCAL) {
          IrOperand *current = (IrOperand *)topOfStack(&stacks[useOp->data.lid]);
          assert(current != NULL);
          assert(current->kind == IR_VREG);
          assert(current->type == useOp->type);
          assert(current->astType == useOp->astType);
          replaceInputIn(instr, un, current);
        }
      }
    }

    for (IrOperandListNode *dn = instr->defs.head; dn != NULL; dn = dn->next) {
      IrOperand *defOp = dn->op;
      if (defOp->kind == IR_LOCAL) {
        uint32_t lviIdx = defOp->data.lid;
        IrOperand *newDef = vregForLocal(defOp);
        pushToStack(&stacks[lviIdx], (intptr_t) newDef);
        resetPoints[lviIdx] += 1;
        dn->op = newDef;
        newDef->def = instr;
        addToVector(&newDef->uses, (intptr_t)instr);
      }
    }
  }

  for (IrBasicBlockListNode *sn = block->succs.head; sn != NULL; sn = sn->next) {
    IrBasicBlock *succ = sn->block;
    replacePhiInputs(block, succ, stacks);
  }

  for (IrBasicBlockListNode *dn = block->dominators.dominatees.head; dn != NULL; dn = dn->next) {
    IrBasicBlock *dominatee = dn->block;
    renameLocalsImpl(dominatee, lvis, stacks, numOfLocalOps);
  }

  for (size_t i = 0; i < numOfLocalOps; ++i) {
    popOffStack(&stacks[i], resetPoints[i]);
  }

  releaseHeap(resetPoints);
}

static void renameLocals(IrFunction *func) {
    // Algorithm SR (init part)

    size_t numOfLocalOps = func->numOfLocals;
    if (func->retOperand)
      numOfLocalOps += 1;

    Vector *stacks = heapAllocate(numOfLocalOps * sizeof (Vector));
    for (size_t i = 0; i < numOfLocalOps; ++i) {
      initVector(&stacks[i], INITIAL_VECTOR_CAPACITY);

      LocalValueInfo *lvi = &func->localOperandMap[i];
      assert(lvi != NULL);
      assert(i == lvi->initialOp->data.lid);
      IrOperand *v0 = vregForLocal(lvi->initialOp);

      pushToStack(&stacks[i], (intptr_t) v0);
    }

    renameLocalsImpl(func->entry, func->localOperandMap, stacks, numOfLocalOps);

    for (size_t i = 0; i < numOfLocalOps; ++i)
      releaseVector(&stacks[i]);

    releaseHeap(stacks);
}


static void cleanupMoves(IrFunction *func) {
  for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn  = bn->next) {
    IrBasicBlock *bb = bn->block;
    IrInstructionListNode *in = bb->instrs.head;
    while (in != NULL) {
      IrInstruction *instr = in->instr;
      IrInstructionListNode *cur = in;
      in = in->next;
      if (instr->kind == IR_MOVE) {
        IrOperand *use = instr->uses.head->op;
        IrOperand *def = instr->defs.head->op;

        replaceInputWith(def, use);
        printf("Remove instruction id = %u, kind = %u\n", cur->instr->id, cur->instr->kind);
        removeInstruction(cur);

        releaseOperand(def);
        releaseInstruction(instr);
      }
    }
  }
}

 void buildSSA(IrFunction *func) {
  buildDominatorInfo(ctx, func);
  insertPhiNodes(func);
  renameLocals(func);
  cleanupMoves(func);
}

