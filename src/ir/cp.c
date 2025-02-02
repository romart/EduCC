
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>
#include "mem.h"

extern IrContext *ctx;
extern IrInstruction *topI;
extern IrInstruction *bottomI;

typedef struct {

  uint32_t fromId;
  uint32_t toId;
} Edge;

struct SetNode {
  struct SetNode *next;
  Edge edge;
};

static int32_t edge_hashcode(Edge *e) {
  return e->fromId ^ e->toId;
}

static int32_t edge_compare(Edge *l, Edge *r) {
  if (l == r)
    return 0;

  if (l->fromId != r->fromId) {
    return r->fromId - l->fromId;
  }

  return r->toId - l->toId;
}

struct SetNode *newSetNode(Arena *arena, Edge *e) {
  struct SetNode *n = (struct SetNode *)areanAllocate(arena, sizeof(struct SetNode));
  n->edge = *e;
  return n;
}

typedef struct {

  size_t size;
  struct SetNode **buckets;
  Arena *arena;
} EdgeSet;

static void initEdgeSet(EdgeSet *set, Arena *arena, size_t capacity) {
  set->arena = arena;
  set->size = capacity;
  set->buckets = (struct SetNode **)areanAllocate(arena, sizeof (struct SetNode *) * capacity);
}

static Boolean insertEdge(EdgeSet *set, Edge *e) {
  int32_t hash = edge_hashcode(e);
  int32_t bucketId = hash % set->size;
  struct SetNode *node = set->buckets[bucketId];

  while (node != NULL) {
    if (edge_compare(e, &node->edge) == 0) {
      return FALSE;
    }
    node = node->next;
  }

  node = newSetNode(set->arena, e);
  node->next = set->buckets[bucketId];
  set->buckets[bucketId] = node;

  return TRUE;
}

static Edge *findEdge(EdgeSet *set, Edge *e) {
  int32_t hash = edge_hashcode(e);
  int32_t bucketId = hash % set->size;
  struct SetNode *node = set->buckets[bucketId];

  while (node != NULL) {
    if (edge_compare(e, &node->edge) == 0) {
      return &node->edge;
    }
    node = node->next;
  }

  return NULL;
}

typedef struct {
  HashMap *LVs;
  Arena *LVArena;
  BitSet executableBlocks;
  EdgeSet executableEdges;
  EdgeSet ssaEdges;
  Vector mapping;
  Vector ssaWL;
  Vector cfgWL;
} SCCPContext;

static Boolean computeBranchCondition(IrInstruction *condition) {
  assert(isConstantInstr(condition));

  switch (condition->info.constant.kind) {
  case IR_CK_FLOAT: return condition->info.constant.data.f != 0.0f;
  case IR_CK_INTEGER: return condition->info.constant.data.i != 0;
  case IR_CK_LITERAL:
  case IR_CK_SYMBOL: return TRUE;
  }
}

enum LatticeState {
  LS_UNDEF,
  LS_OVERDEF,
  LS_CONSTANT
};

typedef struct {
  enum LatticeState state;
  IrInstruction *constant;
} LatticeValue;


static Boolean meetLattice(LatticeValue *cur, const LatticeValue *other) {
    if (cur->state == LS_OVERDEF)
      return FALSE;

    if (other->state == LS_OVERDEF) {
      cur->state = LS_OVERDEF;
      cur->constant = NULL;
      return TRUE;
    }

    if (cur->state == LS_UNDEF) {
      if (other->state == LS_UNDEF)
        return FALSE;

      assert(other->state == LS_CONSTANT);

      cur->state = LS_CONSTANT;
      cur->constant = other->constant;
      return TRUE;
    }

    assert(cur->state == LS_CONSTANT);
    if (other->state == LS_UNDEF)
      return FALSE;

    if (other->state == LS_CONSTANT) {
      if (cur->constant == other->constant)
        return FALSE;
    }

    cur->state = LS_OVERDEF;
    cur->constant = NULL;
    return TRUE;
}


static int instr_hashcode(intptr_t key) {
  const IrInstruction *instr = (const IrInstruction *)key;
  return instr->id;
}

static int instr_compare(intptr_t lhs, intptr_t rhs) {
  const IrInstruction *l = (const IrInstruction *)lhs;
  const IrInstruction *r = (const IrInstruction *)rhs;
  return r->id - l->id;
}

static LatticeValue *getLV(SCCPContext *cpCtx, const IrInstruction *i) {
  return (LatticeValue *)getFromHashMap(cpCtx->LVs, (intptr_t)i);
}

static void putLV(SCCPContext *cpCtx, const IrInstruction *i, LatticeValue *lv) {
  putToHashMap(cpCtx->LVs, (intptr_t)i, (intptr_t)lv);
}

static void pushBlockToStack(Vector *stack, IrBasicBlock *block) {
    pushToStack(stack, (intptr_t)block);
}

static void pushInstrToStack(Vector *stack, IrInstruction *i) {
    pushToStack(stack, (intptr_t)i);
}

static IrBasicBlock *popBlockFromStack(Vector *stack) {
  return (IrBasicBlock *)popFromStack(stack);
}

static IrInstruction *popInstrFromStack(Vector *stack) {
  return (IrInstruction *)popFromStack(stack);
}

static LatticeValue *makeContantLV(SCCPContext *cpCtx, IrInstruction *constant) {
  assert(constant->kind == IR_DEF_CONST);

  LatticeValue *res = (LatticeValue *)areanAllocate(cpCtx->LVArena, sizeof (LatticeValue));
  res->state = LS_CONSTANT;
  res->constant = constant;
  return res;
}

static LatticeValue *makeUndefLV(SCCPContext *cpCtx) {

  LatticeValue *res = (LatticeValue *)areanAllocate(cpCtx->LVArena, sizeof (LatticeValue));
  res->state = LS_UNDEF;
  return res;
}

static LatticeValue *makeOverdefinedLV(SCCPContext *cpCtx) {

  LatticeValue *res = (LatticeValue *)areanAllocate(cpCtx->LVArena, sizeof (LatticeValue));
  res->state = LS_OVERDEF;
  return res;
}

static void initLattices(IrFunction *func, SCCPContext *cpCtx) {
  const size_t instrCount = ctx->instrCnt;
  // overdef == top
  // undef == bottom
  Vector *instrMapping = &cpCtx->mapping;

  for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
    for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
      uint32_t iid = instr->id;
      assert(iid < instrCount);
      putAtInstrVector(instrMapping, instr, iid);
      LatticeValue *lv = NULL;
      if (isConstantInstr(instr)) {
        printf("Put into LVs[%c%u] <- %c%u\n", '%', iid, '%', iid);
        lv = makeContantLV(cpCtx, instr);
      } else if (!isLeafInstr(instr) || instr->kind == IR_BAD) {
        printf("Put into LVs[%c%u] <- BOTTOM\n", '%', iid);
        lv = makeUndefLV(cpCtx);
      } else {
        printf("Put into LVs[%c%u] <- TOP\n", '%', iid);
        lv = makeOverdefinedLV(cpCtx);
      }
      putLV(cpCtx, instr, lv);
    }
  }
}

static void markOverdefined(SCCPContext *cpCtx, LatticeValue *lv, IrInstruction *i) {
  if (lv->state == LS_OVERDEF)
    return;
  lv->state = LS_OVERDEF;
  pushInstrToStack(&cpCtx->ssaWL, i);
}

static void markConstant(SCCPContext *cpCtx, LatticeValue *lv, IrInstruction *i, IrInstruction *c) {
  assert(c->kind == IR_DEF_CONST);

  if (lv->state == LS_CONSTANT) {
    IrInstruction *lvC = lv->constant;
    assert(lvC != NULL && lvC->kind == IR_DEF_CONST);
    assert(lv->constant == c);
    return;
  }

  assert(lv->state == LS_UNDEF);

  lv->state = LS_CONSTANT;
  lv->constant = c;
  pushInstrToStack(&cpCtx->ssaWL, i);
}


static Boolean markBlockExecutable(SCCPContext *cpCtx, IrBasicBlock *block) {
  if (getBit(&cpCtx->executableBlocks, block->id))
    return FALSE;

  setBit(&cpCtx->executableBlocks, block->id);
  pushBlockToStack(&cpCtx->cfgWL, block);
  return TRUE;
}

static void propagatePhi(SCCPContext *cpCtx, IrInstruction *i) {
  LatticeValue *lvi = getLV(cpCtx, i);
  if (i->type == IR_P_AGG) {
    markOverdefined(cpCtx, lvi, i);
    return;
  }

  if (lvi->state == LS_OVERDEF) {
    return;
  }

  Vector *inputs = &i->inputs;
  Vector *blocks = &i->info.phi.phiBlocks;

  assert(inputs->size == blocks->size);
  IrBasicBlock *phiBlock = i->block;

  LatticeValue tmp = *lvi;

  for (size_t idx = 0; idx < inputs->size; ++idx) {
    IrInstruction *input = getInstructionFromVector(inputs, idx);
    IrBasicBlock *incom = getBlockFromVector(blocks, idx);
    Edge e = { incom->id, phiBlock->id };
    if (findEdge(&cpCtx->executableEdges, &e) == NULL) {
      continue;
    }
    LatticeValue *lvInput = getLV(cpCtx, input);

    meetLattice(&tmp, lvInput);
    if (tmp.state == LS_OVERDEF)
      break;
  }

  if (tmp.state != lvi->state) {
    *lvi = tmp;
    pushInstrToStack(&cpCtx->ssaWL, i);
  }
}


static Boolean markCfgEdgeExecutable(SCCPContext *cpCtx, IrBasicBlock *fromBB, IrBasicBlock *toBB) {
  Edge e = { fromBB->id, toBB->id };
  if (findEdge(&cpCtx->executableEdges, &e)) {
    return FALSE;
  }

  insertEdge(&cpCtx->executableEdges, &e);
  if (!markBlockExecutable(cpCtx, toBB)) {
    for (IrInstruction *phi = toBB->instrunctions.head; phi != NULL && phi->kind == IR_PHI; phi = phi->next) {
      propagatePhi(cpCtx, phi);
    }
  }

  return TRUE;
}

static void propagateCondBranch(SCCPContext *cpCtx, IrInstruction *i) {
  IrInstruction *condition = getInstructionFromVector(&i->inputs, 0);
  LatticeValue *lvc = getLV(cpCtx, condition);

  IrBasicBlock *block = i->block;
  if (lvc->state != LS_CONSTANT) {
    if (lvc->state == LS_OVERDEF) {
      markCfgEdgeExecutable(cpCtx, block, i->info.branch.taken);
      markCfgEdgeExecutable(cpCtx, block, i->info.branch.notTaken);
    }

    return;
  }

  condition = lvc->constant;
  Boolean condValue = computeBranchCondition(condition);

  IrBasicBlock *target = condValue ? i->info.branch.taken : i->info.branch.notTaken;

  markCfgEdgeExecutable(cpCtx, block, target);
}

static void propagateSwitch(SCCPContext *cpCtx, IrInstruction *i) {
  assert(i->kind == IR_TBRANCH);

  IrInstruction *condition = getInstructionFromVector(&i->inputs, 0);
  LatticeValue *lvc = getLV(cpCtx, condition);

  IrBasicBlock *block = i->block;
  SwitchTable *table = i->info.switchTable;
  assert(table != NULL);
  if (lvc->state != LS_CONSTANT) {
    if (lvc->state == LS_OVERDEF) {
      markCfgEdgeExecutable(cpCtx, block, table->defaultBB);
      for (size_t idx = 0; idx < table->caseCount; ++idx) {
        IrBasicBlock *cb = table->caseBlocks[idx].block;
        markCfgEdgeExecutable(cpCtx, block, cb);
      }
    }

    return;
  }

  IrInstruction *cond = lvc->constant;
  assert(cond->info.constant.kind == IR_CK_INTEGER);
  int64_const_t switchValue = cond->info.constant.data.i;

  IrBasicBlock *targetBlock = table->defaultBB;
  for (uint32_t i = 0; i < table->caseCount; ++i) {
    int64_const_t c = table->caseBlocks[i].caseConst;
    IrBasicBlock *cb = table->caseBlocks[i].block;
    if (c == switchValue) {
      targetBlock = cb;
      break;
    }
  }

  markCfgEdgeExecutable(cpCtx, block, targetBlock);
}

static void propagateBranch(SCCPContext *cpCtx, IrInstruction *i) {
  assert(i->kind == IR_BRANCH);
  IrBasicBlock *block = i->block;
  IrBasicBlock *target = i->info.branch.taken;
  assert(target != NULL);

  markCfgEdgeExecutable(cpCtx, block, target);
}

static void propagateIndirectBranch(SCCPContext *cpCtx, IrInstruction *i) {
  assert(i->kind == IR_IBRANCH);
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);
  LatticeValue *lva = getLV(cpCtx, arg);
  // TODO: fold to known block

  IrBasicBlock *block = i->block;

  Vector *succs = &block->succs;
  for (size_t idx = 0; idx < succs->size; ++idx) {
    IrBasicBlock *succ = getBlockFromVector(succs, idx);
    markCfgEdgeExecutable(cpCtx, block, succ);
  }
}


static void propagateUnary(SCCPContext *cpCtx, IrInstruction *i) {
  /* assert(isUnary()); */

  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);
  LatticeValue *lva = getLV(cpCtx, arg);
  LatticeValue *lvi = getLV(cpCtx, i);

  if (lvi->state == LS_OVERDEF) {
    return markOverdefined(cpCtx, lvi, i);
  }

  if (lva->state == LS_UNDEF)
    return;

  if (lva->state == LS_CONSTANT) {
    IrInstruction *constant = lva->constant;
    IrInstruction *newValue = i->kind == IR_E_BITCAST ? evaluateBitCast(i, constant) : evaluateUnary(i, constant);
    if (newValue->kind == IR_DEF_CONST) {
      markConstant(cpCtx, lvi, i, newValue);
      return;
    }
  }

  markOverdefined(cpCtx, lvi, i);
}

static void propagateBinary(SCCPContext *cpCtx, IrInstruction *i) {
  IrInstruction *lhs = getInstructionFromVector(&i->inputs, 0);
  IrInstruction *rhs = getInstructionFromVector(&i->inputs, 1);

  LatticeValue *lvl = getLV(cpCtx, lhs);
  LatticeValue *lvr = getLV(cpCtx, rhs);
  LatticeValue *lvi = getLV(cpCtx, i);

  if (lvi->state == LS_OVERDEF)
    return;

  if (lvl->state == LS_UNDEF || lvr->state == LS_UNDEF)
    return;

  if (lvl->state == LS_CONSTANT && lvr->state == LS_CONSTANT) {
    IrInstruction *newValue = evaluateBinary(i, lvl->constant, lvr->constant);
    if (newValue->kind == IR_DEF_CONST) {
      markConstant(cpCtx, lvi, i, newValue);
      return;
    }
  }

  markOverdefined(cpCtx, lvi, i);
}

static void propagateInstruction(SCCPContext *cpCtx, IrInstruction *i) {
  switch (i->kind) {
  case IR_DEF_CONST:
    return;
  case IR_PHI:
    return propagatePhi(cpCtx, i);
  case IR_E_ADD:
  case IR_E_SUB:
  case IR_E_MUL:
  case IR_E_DIV:
  case IR_E_MOD:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_SHL:
  case IR_E_SHR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
  case IR_E_FADD:
  case IR_E_FSUB:
  case IR_E_FMUL:
  case IR_E_FDIV:
  case IR_E_FMOD:
  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    return propagateBinary(cpCtx, i);
  case IR_U_NOT:
  case IR_U_BNOT:
  case IR_E_BITCAST:
    return propagateUnary(cpCtx, i);
  case IR_CBRANCH:
    return propagateCondBranch(cpCtx, i);
  case IR_TBRANCH:
    return propagateSwitch(cpCtx, i);
  case IR_BRANCH:
    return propagateBranch(cpCtx, i);
  case IR_IBRANCH:
    return propagateIndirectBranch(cpCtx, i);
  default:
    markOverdefined(cpCtx, getLV(cpCtx, i), i);
    return;
  }
}

static void propagate(IrInstruction *i, IrInstruction *newValue) {
  if (newValue == bottomI || newValue == topI)
    return;
  replaceUsageWith(i, newValue);
}

void dumpInstr(IrInstruction *i) {
  if (i == topI)
    printf("TOP");
  else if (i == bottomI) {
    printf("BOTTOM");
  } else {
    printf("%c%u", '%', i->id);
  }
}

static void visitInstuction(SCCPContext *cpCtx, IrInstruction *instruction) {
  Vector *uses = &instruction->uses;
  for (size_t idx = 0; idx < uses->size; ++idx) {

    IrInstruction *use = getInstructionFromVector(uses, idx);
    if (getBit(&cpCtx->executableBlocks, use->block->id)) {
      propagateInstruction(cpCtx, use);
    }
  }
}

static void visitBlock(SCCPContext *cpCtx, IrBasicBlock *block) {
  for (IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
    propagateInstruction(cpCtx, i);
  }
}

static void processConditionBranch(SCCPContext *cpCtx, IrInstruction *i) {
  assert(i->kind == IR_CBRANCH);

  IrBasicBlock *taken = i->info.branch.taken;
  IrBasicBlock *notTaken = i->info.branch.notTaken;
  BitSet *executables = &cpCtx->executableBlocks;

  if (getBit(executables, taken->id) && getBit(executables, notTaken->id)) {
    return;
  }

  IrBasicBlock *target, *nonTarget;
  if (getBit(executables, taken->id)) {
    target = taken;
    nonTarget = notTaken;
  } else {
    target = notTaken;
    nonTarget = taken;
  }

  assert(getBit(executables, target->id));
  assert(!getBit(executables, nonTarget->id));

  IrInstruction *newBranch = newGotoInstruction(target);
  IrBasicBlock *curBlock = i->block;
  IrInstruction *condition = getInstructionFromVector(&i->inputs, 0);

  removeSuccessor(curBlock, nonTarget);
  updateBlockTerminator(curBlock, newBranch);

  releaseInstruction(i);
}

static void processSwitchBranch(SCCPContext *cpCtx, IrInstruction *i) {
  assert(i->kind == IR_TBRANCH);
  IrInstruction *cond = getInstructionFromVector(&i->inputs, 0);
  LatticeValue *lvc = getLV(cpCtx, cond);
  if (lvc->state != LS_CONSTANT)
    return;

  IrInstruction *value = lvc->constant;
  SwitchTable *table = i->info.switchTable;
  assert(table != NULL);
  IrBasicBlock *target = table->defaultBB;
  IrBasicBlock *block = i->block;

  BitSet *executables = &cpCtx->executableBlocks;
  if (!getBit(executables, target->id)) {
    removeSuccessor(block, target);
    target = NULL;
  }

  for (uint32_t i = 0; i < table->caseCount; ++i) {
    int64_const_t c = table->caseBlocks[i].caseConst;
    IrBasicBlock *cb = table->caseBlocks[i].block;
    if (getBit(executables, cb->id)) {
      assert(target == NULL);
      target = cb;
    } else {
      removeSuccessor(block, cb);
    }
  }


  IrInstruction *gotoInstr = newGotoInstruction(target);
  IrInstruction *oldTerminator = updateBlockTerminator(block, gotoInstr);

  releaseInstruction(oldTerminator);
}

static void replaceConstantInstructions(SCCPContext *cpCtx, IrFunction *func) {
  for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
    /* printf("Process block #%u..", block->id); */
    if (!getBit(&cpCtx->executableBlocks, block->id)) {
        /* printf(".. block dead, skip\n"); */
        continue;
    }

    /* printf(".. block alive, handle\n"); */

    for (IrInstruction *i = block->instrunctions.head; i != NULL;) {
      /* printf("Process instruction %c%u...", '%', i->id); */
      IrInstruction *next = i->next;
      if (i->kind == IR_TBRANCH) {
        /* printf(" SWITCH\n"); */
        processSwitchBranch(cpCtx, i);
      } else if (i->kind == IR_CBRANCH) {
        /* printf(" CBRANCH\n"); */
        processConditionBranch(cpCtx, i);
      } else {
        /* printf(" REGULAR\n"); */
        LatticeValue *lvi = getLV(cpCtx, i);
        if (lvi != NULL && lvi->state == LS_CONSTANT) {
          if (i != lvi->constant) {
            replaceUsageWith(i, lvi->constant);
            cleanAndErase(i);
          }
        }
      }
      i = next;
    }
  }
}

static void initSCCPContext(SCCPContext *cpCtx) {
  initVector(&cpCtx->cfgWL, INITIAL_VECTOR_CAPACITY);
  initVector(&cpCtx->ssaWL, INITIAL_VECTOR_CAPACITY);
  initVector(&cpCtx->mapping, 2 * ctx->instrCnt);
  initBitSet(&cpCtx->executableBlocks, ctx->bbCnt);
  cpCtx->LVs = createHashMap(DEFAULT_MAP_CAPACITY, &instr_hashcode, &instr_compare);
  cpCtx->LVArena = createArena("Lattice Value arena", DEFAULT_CHUNCK_SIZE);
  initEdgeSet(&cpCtx->executableEdges, cpCtx->LVArena, 1024);
  initEdgeSet(&cpCtx->ssaEdges, cpCtx->LVArena, 1024);
}

static void releaseSCCPContext(SCCPContext *cpCtx) {
  releaseVector(&cpCtx->cfgWL);
  releaseVector(&cpCtx->ssaWL);
  releaseVector(&cpCtx->mapping);
  releaseBitSet(&cpCtx->executableBlocks);
  releaseHashMap(cpCtx->LVs);
  releaseArena(cpCtx->LVArena);
}

void scp(IrFunction *func) {
  SCCPContext cpCtx = { 0 };

  initSCCPContext(&cpCtx);

  initLattices(func, &cpCtx);
  markBlockExecutable(&cpCtx, func->entry);

  while (cpCtx.cfgWL.size != 0 || cpCtx.ssaWL.size != 0) {
    while (cpCtx.ssaWL.size != 0) {
      IrInstruction *i = popInstrFromStack(&cpCtx.ssaWL);
      printf("SCCP: visit Instruction %c%u\n", '%', i->id);
      visitInstuction(&cpCtx, i);
    }
    while (cpCtx.cfgWL.size != 0) {
      IrBasicBlock *block = popBlockFromStack(&cpCtx.cfgWL);
      printf("SCCP: visit Block #%u\n", block->id);
      visitBlock(&cpCtx, block);
    }
  }

  replaceConstantInstructions(&cpCtx, func);
  releaseSCCPContext(&cpCtx);

  func->phases.cp_1 = 1;
}


