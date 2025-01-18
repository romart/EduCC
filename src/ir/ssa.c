
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>

extern IrContext *ctx;

typedef struct _AllocaOptInfo {
  IrInstruction *allocaInstr;
  BitSet useBlocks;
  BitSet defBlocks;
  BitSet inserted;
  size_t index;
  IrInstruction **phiInBlocks;
} AllocaOptInfo;

static void updatePhiInput(IrInstruction *phi, IrInstruction *value, size_t idx) {
  assert(idx < phi->inputs.size);
  phi->inputs.storage[idx] = (intptr_t)value;
  addInstructionToVector(&value->uses, phi);
}

static void replacePhiInputs(IrBasicBlock *defBlock, IrBasicBlock *phiBlock, Vector *stacks) {
  printf("Replace phi inputs in #%u coming from #%u...\n", phiBlock->id, defBlock->id);
  for (IrInstruction *instr = phiBlock->instrunctions.head; instr != NULL; instr = instr->next) {
    if (instr->kind != IR_PHI) { // assume all phi-nodes are at the beginning of block's instruciton list
      return;
    }

    Vector *inputs = &instr->inputs;
    Vector *blocks = &instr->info.phi.phiBlocks;
    assert(inputs->size == blocks->size);
    AllocaOptInfo *info = instr->info.phi.info;
    if (info == NULL)
      continue;

    for (size_t i = 0; i < instr->inputs.size; ++i) {
       IrBasicBlock *block = getBlockFromVector(blocks, i);
       if (block == defBlock) {
         IrInstruction *oldValue = getInstructionFromVector(inputs, i);
         assert(oldValue->kind == IR_BAD && "Placeholder is expected");
         IrInstruction *inputOp = (IrInstruction *)topOfStack(&stacks[info->index]);
         updatePhiInput(instr, inputOp, i);
         break;
       }
    }
  }
}

static Boolean analyzeAllocaInstruction(IrInstruction *allocaInstr, AllocaOptInfo *info) {

  printf("Analyze Alloca %c%u...\n", '%',  allocaInstr->id);
  assert(allocaInstr->inputs.size == 1);
  IrInstruction *sizeOp = getInstructionFromVector(&allocaInstr->inputs, 0);
  if (sizeOp->kind != IR_DEF_CONST) {
    printf(".. alloca %c%u is VLA-lile, size = %c%u\n", '%',  allocaInstr->id, '%', sizeOp->id);
    return FALSE; // looks like VLA or explicit alloca call with unkown size
  }

  assert(sizeOp->info.constant.kind == IR_CK_INTEGER);
  int64_const_t allocaSizeValue = sizeOp->info.constant.data.i;

  Vector *uses = &allocaInstr->uses;

  for (size_t i = 0; i < uses->size; ++i) {
    IrInstruction *useInstr = getInstructionFromVector(uses, i);
    switch (useInstr->kind) {
      case IR_M_LOAD:
        setBit(&info->useBlocks, useInstr->block->id);
        break;
      case IR_M_STORE: {
          assert(useInstr->inputs.size == 2);
          IrInstruction *ptr = getInstructionFromVector(&useInstr->inputs, 0);
          IrInstruction *value = getInstructionFromVector(&useInstr->inputs, 1);

          if (ptr == allocaInstr) {
            setBit(&info->defBlocks, useInstr->block->id);
            break;
          } else {
            assert(value == allocaInstr);
            printf("  alloca %c%u is stored at %c%u\n", '%',  allocaInstr->id, '%', useInstr->id);
            // alloca ptr is stored into somewhere so it escapes
            return FALSE;
          }
        }
      default:
        printf("  alloca %c%u is used in unsafe instruction %c%u\n", '%',  allocaInstr->id, '%', useInstr->id);
        return FALSE;
    }
  }

  return TRUE;
}

static void collectAllocaCandidates(IrFunction *func, Vector *results) {
  Vector *allocas = &ctx->allocas;
  for (size_t i = 0; i < allocas->size; ++i) {
    IrInstruction *allocaInstr = getInstructionFromVector(allocas, i);
    assert(allocaInstr->kind == IR_ALLOCA);

    AllocaOptInfo *info = heapAllocate(sizeof (AllocaOptInfo));
    initBitSet(&info->defBlocks, ctx->bbCnt);
    initBitSet(&info->useBlocks, ctx->bbCnt);
    initBitSet(&info->inserted, ctx->bbCnt);
    info->allocaInstr = allocaInstr;

    Boolean optimizable = analyzeAllocaInstruction(allocaInstr, info);
    if (optimizable) {
      info->index = results->size;
      addToVector(results, (intptr_t)info);
    } else {
      releaseBitSet(&info->inserted);
      releaseBitSet(&info->defBlocks);
      releaseBitSet(&info->useBlocks);
      releaseHeap(info);
    }
  }
}

static void insertPhiNode(IrBasicBlock *phiBlock, AllocaOptInfo *info) {
    IrInstruction *allocaInstr = info->allocaInstr;
    enum IrTypeKind irType = allocaInstr->info.alloca.valueType;
    assert(irType != IR_VOID);
    IrInstruction *phiInstr = newPhiInstruction(irType);
    IrInstruction *placeHolder = newInstruction(IR_BAD, irType);

    assert(info->phiInBlocks[phiBlock->id] == NULL);
    phiInstr->info.phi.info = info;
    phiInstr->info.phi.declaration = info->allocaInstr->info.alloca.v;

    info->phiInBlocks[phiBlock->id] = phiInstr;
    addInstructionHead(phiBlock, phiInstr);
    printf("Insert phi %c%u for alloca %c%u into block #%u\n", '%', phiInstr->id, '%', allocaInstr->id, phiBlock->id);

    Vector *preds = &phiBlock->preds;
    for (size_t idx = 0; idx < preds->size; ++idx) {
      IrBasicBlock *pred = getBlockFromVector(preds, idx);
      addPhiInput(phiInstr, placeHolder, pred);
    }
}


static Boolean analyzeLiveness(IrBasicBlock *block, IrInstruction *allocaInstr) {
  //return TRUE;
  for (IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
    if (i->kind == IR_M_LOAD) {
      IrInstruction *ptr = getInstructionFromVector(&i->inputs, 0);
      if (ptr == allocaInstr) {
        // the first usage is read so the value is alive
        printf("LIVE: In block #%u found first USE of %c%u in %c%u\n", block->id, '%', allocaInstr->id, '%', i->id);
        return TRUE;
      }
    } else if (i->kind == IR_M_STORE) {
      IrInstruction *ptr = getInstructionFromVector(&i->inputs, 0);
      if (ptr == allocaInstr) {
        printf("KILL: In block #%u found first DEF of %c%u in %c%u\n", block->id, '%', allocaInstr->id, '%', i->id);
        // first is store so the incoming value is not used and var at the beginning is dead
        return FALSE;
      }
    }
  }

  return TRUE;
}

static void transformAllocasIntoPhis(IrFunction *func, Vector *candidates) {
  // Algorithm SI
  Vector stackImpl = { 0 };
  Vector *stack = &stackImpl;
  initVector(stack, INITIAL_VECTOR_CAPACITY);

  for (size_t i = 0; i < candidates->size; ++i) {
    AllocaOptInfo *info = (AllocaOptInfo *)getFromVector(candidates, i);
    BitSet *defBitSet = &info->defBlocks;

    info->phiInBlocks = heapAllocate(ctx->bbCnt * sizeof(IrInstruction *));

    for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
       if (getBit(defBitSet, bb->id)) {
         pushToStack(stack, (intptr_t)bb);
       }
    }

    BitSet *insertBitSet = &info->inserted;
    while (stack->size > 0) {
      IrBasicBlock *defBlock = (IrBasicBlock *)popFromStack(stack);
      Vector *df = &defBlock->dominators.dominationFrontier;
      for (size_t idx = 0; idx < df->size; ++idx) {
        IrBasicBlock *bb = getBlockFromVector(df, idx);
        if (getBit(insertBitSet, bb->id)) {
          continue;
        }

//        if (analyzeLiveness(bb, info->allocaInstr)) {
//          insertPhiNode(bb, info);
//        }
        insertPhiNode(bb, info);

        setBit(insertBitSet, bb->id);
        pushToStack(stack, (intptr_t)bb);
      }
    }
  }

  releaseVector(stack);
}

static AllocaOptInfo *findAllocaInfo(IrInstruction *allocaInstr, Vector *infos) {
  for (size_t i = 0; i < infos->size; ++i) {
    AllocaOptInfo *info = (AllocaOptInfo *)getFromVector(infos, i);
    if (info->allocaInstr == allocaInstr)
      return info;
  }

  return NULL;
}

static void renameLocalsImpl(IrBasicBlock *block, Vector *infos, Vector *stacks) {
  // Algorithm SR

  static int cnt = 0;
  const size_t numOfAllocas = infos->size;
  size_t *resetPoints = (size_t *)heapAllocate(numOfAllocas * sizeof(size_t));
  memset(resetPoints, 0, numOfAllocas * sizeof(size_t));

  for (IrInstruction *i = block->instrunctions.head; i != NULL;) {
    IrInstruction *n = i->next;
    if (i->kind == IR_M_STORE) {
      IrInstruction *ptr = getInstructionFromVector(&i->inputs, 0);
      printf("Check STORE instruction %c%u...\n", '%', i->id);
      if (ptr->kind != IR_ALLOCA) {
        printf("  not alloca ptr. We done here\n");
        i = n;
        continue;
      }

      AllocaOptInfo *info = findAllocaInfo(ptr, infos);

      if (info == NULL) {
        printf("  cannot find alloca info. We done here\n");
        i = n;
        continue;
      }

      uint32_t idx = info->index;
      IrInstruction *newValue = getInstructionFromVector(&i->inputs, 1);
      Vector *stack = &stacks[idx];

      pushToStack(stack, (intptr_t)newValue);
      assert(idx < numOfAllocas);
      resetPoints[idx] += 1;

      printf("For alloca[%u] %c%u in block #%u found new value %c%u from %c%u\n", idx, '%', info->allocaInstr->id, block->id, '%', newValue->id, '%', i->id);

      eraseInstruction(i);
      releaseInstruction(i);
    } else if (i->kind == IR_M_LOAD) {
      printf("Check LOAD instruction %c%u...\n", '%', i->id);
      IrInstruction *ptr = getInstructionFromVector(&i->inputs, 0);
      if (ptr->kind != IR_ALLOCA) {
        i = n;
        continue;
      }

      AllocaOptInfo *info = findAllocaInfo(ptr, infos);

      if (info == NULL) {
        i = n;
        continue;
      }

      uint32_t idx = info->index;
      Vector *stack = &stacks[idx];

      IrInstruction *actualValue = (IrInstruction *)topOfStack(stack);

      printf("For alloca[%u] %c%u in block #%u replace usage of %c%u with %c%u\n", idx, '%', info->allocaInstr->id, block->id, '%', i->id, '%', actualValue->id);

      replaceUsageWith(i, actualValue);
      eraseInstruction(i);
      releaseInstruction(i);
    } else if (i->kind == IR_PHI) {

      printf("Check PHI instruction %c%u...\n", '%', i->id);
      AllocaOptInfo *info = i->info.phi.info;

      if (info == NULL) {
        printf("This is not interesting phi...\n");
        i = n;
        continue;
      }

      uint32_t idx = info->index;
      assert(idx < numOfAllocas);
      printf("For alloca[%u] %c%u in block #%u found new PHI value %c%u\n", idx, '%', info->allocaInstr->id, block->id, '%', i->id);
      Vector *stack = &stacks[idx];
      pushToStack(stack, (intptr_t)i);
      resetPoints[idx] += 1;
    }
    i = n;
  }

  Vector *succs = &block->succs;
  for (size_t idx = 0; idx < succs->size; ++idx) {
    IrBasicBlock *succ = getBlockFromVector(succs, idx);
    replacePhiInputs(block, succ, stacks);
  }

  Vector *dominatees = &block->dominators.dominatees;
  for (size_t idx = 0; idx < dominatees->size; ++idx) {
    IrBasicBlock *dominatee = getBlockFromVector(dominatees, idx);
    renameLocalsImpl(dominatee, infos, stacks);
  }

  for (size_t i = 0; i < numOfAllocas; ++i) {
    popOffStack(&stacks[i], resetPoints[i]);
  }

  releaseHeap(resetPoints);
}
static void renameLocals(IrFunction *func, Vector *allocas) {
    // Algorithm SR (init part)

    size_t numOfAllocas = allocas->size;
    Vector *stacks = heapAllocate(numOfAllocas * sizeof (Vector));

    for (size_t i = 0; i < numOfAllocas; ++i) {
      Vector *stack = &stacks[i];
      initVector(stack, INITIAL_VECTOR_CAPACITY);

      AllocaOptInfo *info = (AllocaOptInfo *)getFromVector(allocas, i);
      IrInstruction *v0 = newInstruction(IR_BAD, info->allocaInstr->info.alloca.valueType);

      pushToStack(&stacks[i], (intptr_t) v0);
    }

    renameLocalsImpl(func->entry, allocas, stacks);

    for (size_t i = 0; i < numOfAllocas; ++i)
      releaseVector(&stacks[i]);

    releaseHeap(stacks);
}

static void releaseOptimizableVector(Vector *v) {
  for (size_t i = 0; i < v->size; ++i) {
    AllocaOptInfo *info = (AllocaOptInfo *)v->storage[i];
    releaseBitSet(&info->defBlocks);
    releaseBitSet(&info->useBlocks);
    releaseBitSet(&info->inserted);

    releaseHeap(info->phiInBlocks);
    releaseHeap(info);
  }

  releaseVector(v);
}

void buildSSA(IrFunction *func) {
  buildDominatorInfo(ctx, func);

  Vector optimizableAllocas = { 0 };
  initVector(&optimizableAllocas, ctx->allocas.size);
  collectAllocaCandidates(func, &optimizableAllocas);
  printf("Found %lu candidates for alloca opt..\n", optimizableAllocas.size);
  if (optimizableAllocas.size == 0)
    return;

  transformAllocasIntoPhis(func, &optimizableAllocas);
  renameLocals(func, &optimizableAllocas);
  cleanupDeadInstructions(func);

  releaseOptimizableVector(&optimizableAllocas);
}

