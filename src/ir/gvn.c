

#include <assert.h>
#include "ir/ir.h"
#include <stdlib.h>
#include "mem.h"

extern IrContext *ctx;

static void computePostOrderImpl(IrBasicBlock *block, Vector *rpo_order, BitSet *visited) {
  if (block->po != -1)
    return;

  if (getBit(visited, block->id)) {
    return;
  }

  setBit(visited, block->id);
  Vector *succs = &block->succs;

  for (size_t i = 0; i < succs->size; ++i) {
    IrBasicBlock *succ = getBlockFromVector(succs, i);
    computePostOrderImpl(succ, rpo_order, visited);
  }

  block->po = rpo_order->size;
  addBlockToVector(rpo_order, block);
}

static void computePostOrder(IrFunction *func, Vector *rpo_order) {
  for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
    bb->po = -1;
  }

  BitSet visited;
  initBitSet(&visited, func->numOfBlocks);

  computePostOrderImpl(func->entry, rpo_order, &visited);

  assert(rpo_order->size - 1 == func->entry->po);
  /* assert(func->exit->po == -1 || func->exit->po == 0); */
  releaseBitSet(&visited);
}

static uint32_t rpo(IrBasicBlock *block, Vector *rpo_order) {
  return rpo_order->size - block->po - 1;
}

static size_t computeInputsCount(const IrInstruction *instr) {
  return instr->inputs.size;
}

static size_t computeExtrasCount(const IrInstruction *instr) {

  switch (instr->kind) {
  case IR_M_COPY:
  case IR_M_LOAD:
    return 1;
  case IR_DEF_CONST:
  case IR_P_REG:
    return 1;
  default:
    return 0;
  }
}

static Boolean isCommutativeInstr(enum IrIntructionKind kind) {
  switch (kind) {
  case IR_E_ADD:
  case IR_E_MUL:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_FADD:
  case IR_E_FMUL:
  case IR_E_FEQ:
  case IR_E_FNE:
    return TRUE;
  default: return FALSE;
  }
}

typedef struct {
  enum IrIntructionKind kind;
  size_t numOfInputs;
  size_t numOfExtras;
  uint32_t *inputs;
  uint32_t *extras;
} GVNExpression;


static int gvn_hash(intptr_t ptr) {
  const GVNExpression *gvne = (const GVNExpression *)ptr;

  int h = (intptr_t) gvne->kind;

  h *= 31;
  h += gvne->numOfInputs;

  for (size_t i = 0; i < gvne->numOfInputs; ++i) {
    h *= 31;
    h += gvne->inputs[i];
  }

  h *= 31;
  h += gvne->numOfExtras;

  for (size_t i = 0; i < gvne->numOfExtras; ++i) {
    h *= 31;
    h += gvne->extras[i];
  }

  return h;
}

static int gvn_cmp(intptr_t a, intptr_t b) {
  const GVNExpression *lhs = (const GVNExpression *)a;
  const GVNExpression *rhs = (const GVNExpression *)b;

  if (rhs->kind != lhs->kind) {
    return rhs->kind - lhs->kind;
  }

  // Call could have different number of inputs
  if (rhs->numOfInputs != lhs->numOfInputs) {
    return rhs->numOfInputs - lhs->numOfInputs;
  }

  int icmp = memcmp(rhs->inputs, lhs->inputs, rhs->numOfInputs * sizeof (uint32_t));

  if (icmp != 0) {
    return icmp;
  }

  if (rhs->numOfExtras != lhs->numOfExtras) {
    return rhs->numOfExtras - lhs->numOfExtras;
  }

  return memcmp(rhs->extras, lhs->extras, rhs->numOfExtras * sizeof (uint32_t));
}


typedef struct {
  HashMap *table;
  Vector exprMap;
  uint32_t nextVN;
  Arena *arena;
} VNTable;

static uint32_t getOrAddVN(VNTable *vnt, IrInstruction *i);

static int vn_cmp(const void *ap, const void *bp) {
  uint32_t a = *(uint32_t *)ap;
  uint32_t b = *(uint32_t *)bp;
  return b - a;
}

static void fillExtras(GVNExpression *gvne, const IrInstruction *i) {

  static uint32_t memCnt = 0;
  switch (i->kind) {
  case IR_M_COPY:
  case IR_M_LOAD:
    gvne->extras[0] = memCnt++;
    break;
  case IR_DEF_CONST:
    gvne->extras[0] = i->info.constant.cacheIdx;
    break;
  case IR_P_REG:
    gvne->extras[0] = i->info.physReg;
    break;
  default:
    return;
  }
}

static GVNExpression *createGVNExpression(VNTable *vnt, IrInstruction *i) {
  size_t inputs = computeInputsCount(i);
  size_t extras = computeExtrasCount(i);
  GVNExpression *gvne = areanAllocate(vnt->arena, sizeof (GVNExpression) + (inputs * sizeof (uint32_t)));
  gvne->kind = i->kind;

  uint32_t *ptr = (uint32_t *)&gvne[1];
  gvne->numOfInputs = inputs;
  gvne->inputs = inputs != 0 ? ptr : NULL;
  ptr += inputs;

  gvne->numOfExtras = extras;
  gvne->extras = extras != 0 ? ptr : NULL;

  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    gvne->inputs[idx] = getOrAddVN(vnt, input);
  }

  if (isCommutativeInstr(i->kind)) {
    qsort(gvne->inputs, gvne->numOfInputs, sizeof (uint32_t), &vn_cmp);
  }

  fillExtras(gvne, i);

  return gvne;
}

static uint32_t getFromGVNTable(HashMap *table, const GVNExpression *gvne) {
  uint32_t vn = getFromHashMap(table, (intptr_t)gvne);
  return vn;
}

static Boolean isInGVNTable(HashMap *table, const GVNExpression *gvne) {
  return isInHashMap(table, (intptr_t)gvne);
}

static uint32_t putIntoGVNTable(HashMap *table, const GVNExpression *gvne, uint32_t newValue) {
  return putToHashMap(table, (intptr_t)gvne, newValue);
}

static uint32_t getOrAddVN(VNTable *vnt, IrInstruction *i) {
  if (i->kind == IR_PHI) {
    if (i->algoIdx == -1) {
        i->algoIdx = vnt->exprMap.size;
        addInstructionToVector(&vnt->exprMap, (IrInstruction *)i);
    }
    return i->algoIdx;
  }
  GVNExpression *gvne = createGVNExpression(vnt, i);

  uint32_t newVN = vnt->exprMap.size;
  uint32_t aVN = putIntoGVNTable(vnt->table, gvne, newVN);
  if (aVN < newVN)
    return aVN;

  addInstructionToVector(&vnt->exprMap, (IrInstruction *)i);
  assert(getInstructionFromVector(&vnt->exprMap, newVN) == i);

  return newVN;
}

static Boolean dominates(const IrBasicBlock *dom, const IrBasicBlock *block) {
  const IrBasicBlock *cur = block;
  while (cur != NULL) {
    if (cur == dom)
      return TRUE;

    cur = cur->dominators.sdom;
  }

  return FALSE;
}


static Boolean isCriticalEdge(const IrBasicBlock *src, const IrBasicBlock *dst) {
  if (src->succs.size == 1)
    return FALSE;

  return dst->preds.size != 1;
}

static void updateTerminatorTarget(IrInstruction *term, IrBasicBlock *oldTarget, IrBasicBlock *newTarget) {
  if (term->kind == IR_CBRANCH) {
    if (term->info.branch.notTaken == oldTarget) {
      term->info.branch.notTaken = newTarget;
    } else {
      term->info.branch.taken = newTarget;
    }
  } else if (term->kind == IR_TBRANCH) {
    SwitchTable *st = term->info.switchTable;
    if (st->defaultBB == oldTarget) {
      st->defaultBB = newTarget;
    } else {
      for (uint32_t i = 0; i < st->caseCount; ++i) {
        IrBasicBlock *cur = st->caseBlocks[i].block;
        if (cur == oldTarget) {
          st->caseBlocks[i].block = newTarget;
          return;
        }
      }
    }
  } else {
    unreachable("Unexpected terminator kind");
  }
}

static void updateDomTreeInfo(IrBasicBlock *from, IrBasicBlock *split, IrBasicBlock *to) {

  IrBasicBlock *dom = to->dominators.sdom;
  split->dominators.sdom = dom;
  addBlockToVector(&dom->dominators.dominatees, split);

  while (dom != NULL) {
    Vector *df = &dom->dominators.dominationFrontier;
    for (size_t i = 0; i < df->size; ++i) {
      IrBasicBlock *bb = getBlockFromVector(df, i);
      if (bb == to) {
        addBlockToVector(df, split);
        break;
      }
    }
    dom = dom->dominators.sdom;
  }
}

static void splitCriticalEdge(IrInstruction *term, size_t succIdx) {
  IrBasicBlock *block = term->block;
  IrBasicBlock *succ = getBlockFromVector(&block->succs, succIdx);

  printf("Split critical edge [#%u -> #%u]...", block->id, succ->id);

  Vector *preds = &succ->preds;
  size_t pIdx = 0;
  for (; pIdx < preds->size; ++pIdx) {
    IrBasicBlock *pred = getBlockFromVector(preds, pIdx);
    if (pred == block)
      break;
  }

  assert(pIdx < preds->size);

  IrBasicBlock *newBB = newBasicBlock("<crit_splitter>");

  block->succs.storage[succIdx] = (intptr_t)newBB;
  preds->storage[pIdx] = (intptr_t)newBB;

  IrInstruction *gotoI = newGotoInstruction(succ);
  addInstructionHead(newBB, gotoI);
  newBB->term = gotoI;
  updateTerminatorTarget(term, succ, newBB);

  for (IrInstruction *i = succ->instrunctions.head; i != NULL; i = i->next) {
    if (i->kind != IR_PHI) {
      break;
    }

    IrBasicBlock *curEdge = (IrBasicBlock *)i->info.phi.phiBlocks.storage[pIdx];
    assert(curEdge == block);
    i->info.phi.phiBlocks.storage[pIdx] = (intptr_t)newBB;
  }

  printf(" with #%u\n", newBB->id);

  updateDomTreeInfo(block, newBB, succ);
}

static void splitCriticalEdges(IrFunction *func) {
  for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
    Vector *succs = &block->succs;
    IrInstruction *terminator = block->term;
    assert(terminator != NULL);

    if (terminator->kind == IR_IBRANCH)
      continue;

    for (size_t idx = 0; idx < succs->size; ++idx) {
      IrBasicBlock *succ = getBlockFromVector(succs, idx);
      if (isCriticalEdge(block, succ)) {
        splitCriticalEdge(terminator, idx);
      }
    }
  }
}

static void buildValueNumberingInfo(Vector *poOrder, VNTable *gvnTable) {
  for (size_t idx = poOrder->size - 1; idx != -1; --idx) {
    IrBasicBlock *block = getBlockFromVector(poOrder, idx);
    for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
      instr->algoIdx = -1;
      uint32_t vn = getOrAddVN(gvnTable, instr);
      instr->algoIdx = vn;
    }
  }
}

static void computeAvailability(Vector *poOrder, VNTable *gvnTable, BitSet *availIns, BitSet *availOuts) {
  Boolean changed = TRUE;

  BitSet tmp;
  initBitSet(&tmp, availIns[0].size);

  while (changed) {
    changed = FALSE;
    for (size_t idx = poOrder->size - 1; idx != -1; --idx) {
      IrBasicBlock *block = getBlockFromVector(poOrder, idx);
      BitSet *oldIn = &availIns[block->id];
      BitSet *oldOut = &availOuts[block->id];

      if (block->preds.size == 0) {
        clearAll(&tmp);
      } else {
        size_t idx = 0;
        IrBasicBlock *pred = getBlockFromVector(&block->preds, idx);
        copyBitSet(&availOuts[pred->id], &tmp);
        for (; idx < block->preds.size; ++idx) {
          pred = getBlockFromVector(&block->preds, idx);
          intersectBitSets(&tmp, &availOuts[pred->id], &tmp);
        }
      }

      if (compareBitSets(&tmp, oldIn) != 0) {
        changed = TRUE;
      }
     
      copyBitSet(&tmp, oldIn);
      copyBitSet(oldIn, oldOut);

      for (IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
        uint32_t vn = i->algoIdx;
        if (!getBit(oldOut, vn)) {
          setBit(oldOut, vn);
          changed = TRUE;
        }
      }
    }
  }

  releaseBitSet(&tmp);
}

static IrInstruction *cloneInstruction(IrInstruction *i) {
  IrInstruction *copy = NULL;
  if (i->kind == IR_PHI) {
    copy = newPhiInstruction(i->type);
    for (size_t idx = 0; idx < i->inputs.size; ++idx) {
      IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
      IrBasicBlock *b = getBlockFromVector(&i->info.phi.phiBlocks, idx);
      addPhiInput(i, input, b);
    }
  } else {
    copy = newInstruction(i->kind, i->type);
    for (size_t idx = 0; idx < i->inputs.size; ++idx) {
      IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
      addInstructionInput(copy, input);
    }
    memcpy(&copy->info, &i->info, sizeof (i->info));
  }

  copy->astType = i->astType;
  copy->meta = i->meta;

  return copy;
}

static void insertBeforeTerm(IrBasicBlock *block, IrInstruction *i) {
  if (block->term == NULL) {
    addInstructionTail(block, i);
    return;
  }

  IrInstruction *term = block->instrunctions.tail;
  assert(term == block->term);

  i->prev = term->prev;

  if (term->prev == NULL) {
    block->instrunctions.head = i;
  } else {
    term->prev->next = i;
  }

  term->prev = i;
  i->next = term;
  i->block = block;
}

static void insertComputationOnIncommingEdge(IrBasicBlock *block, uint32_t vn, IrInstruction *phiInstr, IrInstruction *other, IrBasicBlock *pred) {
  IrInstruction *copy = cloneInstruction(other);
  insertBeforeTerm(block, copy);
  copy->algoIdx = vn;
  addPhiInput(phiInstr, copy, pred);
}

static void removePartialRedundancy(IrFunction *func, BitSet *availIn, BitSet *availOut, VNTable *gvnTable) {
  BitSet removeMark = { 0 };
  initBitSet(&removeMark, ctx->instrCnt);

  for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
    BitSet *aIn = &availIn[block->id];
    BitSet *aOut = &availOut[block->id];

    for (IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {

      uint32_t vn = i->algoIdx;
      IrInstruction *other = getInstructionFromVector(&gvnTable->exprMap, vn);
      if (i == other) {
        continue;
      }

      if (getBit(aIn, vn)) {
        replaceUsageWith(i, other);
        setBit(&removeMark, i->id);
        continue;
      }

      Vector *preds = &block->preds;
      IrInstruction *phiInstr = newPhiInstruction(i->type);
      for (size_t idx = 0; idx < preds->size; ++idx) {
        IrBasicBlock *pred = getBlockFromVector(preds, idx);
        BitSet *pOut = &availOut[pred->id];
        if (!getBit(pOut, vn)) {
          insertComputationOnIncommingEdge(block, vn, phiInstr, other, pred); 
        } else {
          addPhiInput(phiInstr, other, pred);
        }
      }
      addInstructionHead(block, phiInstr);
      replaceUsageWith(i, phiInstr);
      setBit(&removeMark, i->id);
    }
  }

  releaseBitSet(&removeMark);
}

void gvn(IrFunction *func) {

  Vector poOrderImpl = { 0 }, *poOrder = &poOrderImpl;
  initVector(poOrder, func->numOfBlocks);

  computePostOrder(func, poOrder);
  VNTable gvnTable = { 0 };
  gvnTable.arena = createArena("GVN Arena", DEFAULT_CHUNCK_SIZE);
  gvnTable.table = createHashMap(DEFAULT_MAP_CAPACITY, &gvn_hash, &gvn_cmp);
  initVector(&gvnTable.exprMap, ctx->instrCnt);

  splitCriticalEdges(func);

  BitSet *availIn = heapAllocate(sizeof (BitSet) * func->numOfBlocks);
  BitSet *availOut = heapAllocate(sizeof (BitSet) * func->numOfBlocks);

  buildValueNumberingInfo(poOrder, &gvnTable);
  for (size_t i = 0; i < func->numOfBlocks; ++i) {
    initBitSet(&availIn[i], gvnTable.exprMap.size);
    initBitSet(&availOut[i], gvnTable.exprMap.size);
  }
  computeAvailability(poOrder, &gvnTable, availIn, availOut);
  removePartialRedundancy(func, availIn, availOut, &gvnTable);

  /* for (size_t idx = poOrder->size - 1; idx != -1; --idx) { */
  /*   IrBasicBlock *block = getBlockFromVector(poOrder, idx); */
  /*   for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) { */
  /*     uint32_t vn = getOrAddVN(&gvnTable, instr); */
  /*     IrInstruction *vnInstuction = getInstructionFromVector(&gvnTable.exprMap, vn); */
  /*     if (vnInstuction == instr) { */
  /*       continue; */
  /*     } */

  /*     if (dominates(vnInstuction->block, block)) { */
  /*       replaceUsageWith(instr, vnInstuction); */
  /*     } */
  /*   } */
  /* } */

  for (size_t i = 0; i < func->numOfBlocks; ++i) {
    releaseBitSet(&availIn[i]);
    releaseBitSet(&availOut[i]);
  }

  releaseHeap(availIn);
  releaseHeap(availOut);

  releaseHashMap(gvnTable.table);
  releaseVector(&gvnTable.exprMap);
  releaseVector(poOrder);
  releaseArena(gvnTable.arena);
}

