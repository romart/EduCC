

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "mem.h"
#include "ir/ir.h"

extern IrContext *ctx;

// Dominator-based global value numbering (Simpson/Briggs-style DVNT).
//
// The dominator tree is walked in preorder carrying a scoped hash table of
// expression -> value number. A redundant pure instruction is therefore
// always replaced by a leader that dominates it by construction; entries
// added while processing a block are popped once its dominator subtree is
// done, so a computation in one branch can never shadow (or be "reused" by)
// a sibling branch.
//
// Only pure instructions - those with no side effects whose value is fully
// determined by (kind, result type, input VNs, extras) - participate in
// value numbering. Everything else (loads, stores, calls, allocas, phis,
// control flow) is assigned a fresh unique VN: loads are never merged since
// there is no alias analysis yet, and side-effecting instructions must
// never be merged at all.

static Boolean isPureInstr(const IrInstruction *i) {
  switch (i->kind) {
  case IR_E_ADD:
  case IR_E_SUB:
  case IR_E_MUL:
  case IR_E_DIV:
  case IR_E_MOD:
  case IR_E_SHL:
  case IR_E_SHR:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_CMP:
  case IR_E_FADD:
  case IR_E_FSUB:
  case IR_E_FMUL:
  case IR_E_FDIV:
  case IR_E_FMOD:
  case IR_E_FCMP:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
  case IR_U_NOT:
  case IR_U_BNOT:
  case IR_E_BITCAST:
  case IR_GET_ELEMENT_PTR:
  case IR_DEF_CONST:
  case IR_P_REG:
    return TRUE;
  default:
    return FALSE;
  }
}

static size_t computeExtrasCount(const IrInstruction *instr) {
  switch (instr->kind) {
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
  enum IrTypeKind type;
  size_t numOfInputs;
  size_t numOfExtras;
  uint32_t *inputs;
  uint32_t *extras;
} GVNExpression;


static int gvn_hash(intptr_t ptr) {
  const GVNExpression *gvne = (const GVNExpression *)ptr;

  int h = (intptr_t) gvne->kind;

  h *= 31;
  h += (intptr_t) gvne->type;

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

  if (rhs->type != lhs->type) {
    return rhs->type - lhs->type;
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
  HashMap *table;  // GVNExpression* -> VN
  Vector exprMap;  // VN -> leader IrInstruction*
  Arena *arena;
} VNTable;

static int vn_cmp(const void *ap, const void *bp) {
  uint32_t a = *(uint32_t *)ap;
  uint32_t b = *(uint32_t *)bp;
  return a < b ? 1 : a > b ? -1 : 0;
}

static void fillExtras(GVNExpression *gvne, const IrInstruction *i) {
  switch (i->kind) {
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

static GVNExpression *createGVNExpression(VNTable *vnt, const IrInstruction *i) {
  size_t inputs = i->inputs.size;
  size_t extras = computeExtrasCount(i);
  GVNExpression *gvne = areanAllocate(vnt->arena, sizeof (GVNExpression) + ((inputs + extras) * sizeof (uint32_t)));
  gvne->kind = i->kind;
  gvne->type = i->type;

  uint32_t *ptr = (uint32_t *)&gvne[1];
  gvne->numOfInputs = inputs;
  gvne->inputs = inputs != 0 ? ptr : NULL;

  ptr += inputs;
  gvne->numOfExtras = extras;
  gvne->extras = extras != 0 ? ptr : NULL;

  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    // Defs dominate uses in SSA, so every input has been numbered already.
    assert(input->algoIdx != (uint32_t)-1);
    gvne->inputs[idx] = input->algoIdx;
  }

  if (isCommutativeInstr(i->kind)) {
    qsort(gvne->inputs, gvne->numOfInputs, sizeof (uint32_t), &vn_cmp);
  }

  fillExtras(gvne, i);

  return gvne;
}

static uint32_t assignUniqueVN(VNTable *vnt, IrInstruction *i) {
  uint32_t vn = vnt->exprMap.size;
  addInstructionToVector(&vnt->exprMap, i);
  return vn;
}

static void gvnBlock(VNTable *vnt, IrBasicBlock *block) {
  // Expressions first defined in this block; popped from the table when its
  // dominator subtree is done so they are not visible to non-dominated blocks.
  Vector scopeKeys = { 0 };
  initVector(&scopeKeys, INITIAL_VECTOR_CAPACITY);

  for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
    if (!isPureInstr(instr)) {
      instr->algoIdx = assignUniqueVN(vnt, instr);
      continue;
    }

    GVNExpression *gvne = createGVNExpression(vnt, instr);
    uint32_t newVN = vnt->exprMap.size;
    uint32_t vn = putIfNotExistsToHashMap(vnt->table, (intptr_t)gvne, newVN);

    if (vn == newVN) {
      // First occurrence along this dominator path - becomes the leader.
      addInstructionToVector(&vnt->exprMap, instr);
      addToVector(&scopeKeys, (intptr_t)gvne);
    } else {
      IrInstruction *leader = getInstructionFromVector(&vnt->exprMap, vn);
      assert(leader != instr);
      replaceUsageWith(instr, leader); // the dead instruction is left for dce
    }

    instr->algoIdx = vn;
  }

  Vector *dominatees = &block->dominators.dominatees;
  for (size_t idx = 0; idx < dominatees->size; ++idx) {
    gvnBlock(vnt, getBlockFromVector(dominatees, idx));
  }

  for (size_t idx = 0; idx < scopeKeys.size; ++idx) {
    removeFromHashMap(vnt->table, scopeKeys.storage[idx]);
  }

  releaseVector(&scopeKeys);
}

void gvn(IrFunction *func) {
  VNTable vnt = { 0 };
  vnt.arena = createArena("GVN Arena", DEFAULT_CHUNCK_SIZE);
  vnt.table = createHashMap(DEFAULT_MAP_CAPACITY, &gvn_hash, &gvn_cmp);
  initVector(&vnt.exprMap, ctx->instrCnt);

  // Unreachable blocks are not visited by the walk; poison every algoIdx so
  // a stale value can never be mistaken for a valid VN.
  for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
    for (IrInstruction *i = bb->instrunctions.head; i != NULL; i = i->next) {
      i->algoIdx = (uint32_t)-1;
    }
  }

  gvnBlock(&vnt, func->entry);

  releaseHashMap(vnt.table);
  releaseVector(&vnt.exprMap);
  releaseArena(vnt.arena);

  func->phases.gvn = 1;
}

#if 0 // PRE draft, to be finished in a follow-up ("Phase B").
      // Partial redundancy elimination on top of the value numbering above:
      // split critical edges, compute expression availability, then insert
      // computations on incoming edges + a phi where an expression is only
      // partially available. Known issues to fix before enabling:
      //  - computeAvailability(): 'changed' re-fires forever because OUT is
      //    overwritten with IN and the gen bits are re-added every sweep;
      //    compare the freshly computed OUT against the previous one instead.
      //  - insertComputationOnIncommingEdge() inserts the clone into 'block'
      //    (the merge point) instead of 'pred'.
      //  - cloneInstruction() adds phi inputs to the original, not the copy.
      //  - No check that the leader's inputs dominate the insertion point.
      //  - updateDomTreeInfo(): idom of the split block is 'from', not
      //    'to'-s old idom.
      //  - The postorder must be (re)computed after splitCriticalEdges().

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
  releaseBitSet(&visited);
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

#endif // PRE draft
