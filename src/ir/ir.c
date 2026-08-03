
#include <assert.h>
#include "ir/ir.h"
#include "parser.h"
#include "tree.h"
#include "sema.h"
#include <signal.h>

IrContext *ctx = NULL;
IrInstruction *topI = (IrInstruction *)0;
IrInstruction *bottomI = (IrInstruction *)-1;


enum IrTypeKind sizeToMemoryType(int32_t size) {
  switch (size) {
  case 1: return IR_U8;
  case 2: return IR_U16;
  case 4: return IR_U32;
  case 8: return IR_U64;
  default: unreachable("Unexpected type size");
  }

  return -1;
}

Boolean isConstantInstr(const IrInstruction *i) {
  return i->kind == IR_DEF_CONST;
}

Boolean isLeafInstr(const IrInstruction *instr) {
  return instr->inputs.size == 0;
}

Boolean isFloatIrType(enum IrTypeKind k) {
    return IR_F32 <= k && k <= IR_F80;
}

Boolean isIntegerIrType(enum IrTypeKind k) {
    return IR_I8 <= k && k <= IR_U64;
}

Boolean isSignedIrType(enum IrTypeKind k) {
    return IR_I8 <= k && k <= IR_I64;
}

Boolean isUnsignedIrType(enum IrTypeKind k) {
    return IR_U8 <= k && k <= IR_U64;
}

Boolean isUnsignedIrOperand(enum IrTypeKind k) {
    // Pointers compare unsigned - an address above the middle of the space is
    // not a negative address - and so does anything already narrowed to a
    // predicate, whose only two values are 0 and 1.
    return isUnsignedIrType(k) || k == IR_PTR || k == IR_REF || k == IR_LITERAL || k == IR_BOOL;
}

void cleanAndErase(IrInstruction *i) {
    assert(i->uses.size == 0);
    for (size_t ii = 0; ii < i->inputs.size; ++ii) {
      IrInstruction *input = getInstructionFromVector(&i->inputs, ii);
      removeFromVector(&input->uses, (intptr_t)i);
    }
    clearVector(&i->inputs);
    if (i->kind == IR_PHI) {
      clearVector(&i->info.phi.phiBlocks);
    }
    eraseInstructionFromBlock(i);
    releaseInstruction(i);
}

IrInstruction *putAtInstrVector(Vector *v, IrInstruction *instr, size_t idx) {
  return (IrInstruction *)putAtVector(v, idx, (intptr_t)instr);
}

enum IrTypeKind typeRefToIrType(const TypeRef *t) {
  	switch (t->kind) {
	case TR_VALUE:
	switch (t->descriptorDesc->typeId) {
	  case T_ENUM: return IR_I32;
	  case T_UNION:
	  case T_STRUCT: return IR_P_AGG;
	  case T_ERROR: unreachable("Unexpected error type in backend");
	  case T_VOID: return IR_VOID;

	  case T_BOOL: return IR_BOOL;

	  case T_S1: return IR_I8;
	  case T_S2: return IR_I16;
	  case T_S4: return IR_I32;
	  case T_S8: return IR_I64;

	  case T_U1: return IR_U8;
	  case T_U2: return IR_U16;
	  case T_U4: return IR_U32;
	  case T_U8: return IR_U64;

	  case T_F4: return IR_F32;
	  case T_F8: return IR_F64;
	  case T_F10: return IR_F80;
	  default: unreachable("Unexpected type");
	}
	case TR_VLA:
	case TR_ARRAY:
	case TR_FUNCTION:
	case TR_POINTED: return IR_PTR;
    case TR_BITFIELD: return typeRefToIrType(t->bitFieldDesc.storageType);
	default: unreachable("unexpected type ref");
	}
    return IR_U64;
}

void initializeIrContext(IrContext *_ctx, ParserContext* pctx) {

    memset(_ctx, 0, sizeof *_ctx);

    // TODO: check for NULL
    _ctx->irArena = createArena("IR Arena", 8 * DEFAULT_CHUNCK_SIZE);
    _ctx->pctx = pctx;
    _ctx->target = getTargetDescriptor(pctx->config->arch);
    _ctx->labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
    initVector(&_ctx->constantCache, INITIAL_VECTOR_CAPACITY);
    initVector(&_ctx->allocas, INITIAL_VECTOR_CAPACITY);
    initVector(&_ctx->referencedBlocks, INITIAL_VECTOR_CAPACITY);
   ctx = _ctx;
}

void releaseIrContext(IrContext *_ctx) {
    ctx = NULL;
    releaseArena(_ctx->irArena);
    releaseHashMap(_ctx->labelMap);
    releaseVector(&_ctx->constantCache);
    releaseVector(&_ctx->allocas);
    releaseVector(&_ctx->referencedBlocks);
}

void resetIrContext(IrContext *_ctx) {
  clearVector(&_ctx->constantCache);
  clearVector(&_ctx->allocas);
  clearVector(&_ctx->referencedBlocks);

  // A label is scoped to the function it appears in (C99 6.2.1p3), so the
  // block standing for it must not outlive that function either. Two
  // functions in one translation unit naming the same label is ordinary C,
  // and carrying the map over means the second one finds the first one's
  // block: the goto is wired into a foreign, already terminated block, and
  // translation walks straight into it. There is no clear operation on
  // HashMap, so the map is replaced outright.
  releaseHashMap(_ctx->labelMap);
  _ctx->labelMap =
      createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);

  _ctx->bbCnt = _ctx->opCnt = _ctx->instrCnt = _ctx->vregCnt = 0;
}

void addInstructionToVector(Vector *v, IrInstruction *instr) {
  addToVector(v, (intptr_t)instr);
}

IrInstruction *getInstructionFromVector(const Vector *v, uint32_t i) {
  return (IrInstruction *)getFromVector(v, i);
}

void addInstructionInput(IrInstruction *instruction, IrInstruction *input) {
   addInstructionToVector(&instruction->inputs, input);
   addInstructionToVector(&input->uses, instruction);
}

void releaseInstruction(IrInstruction *instr) {
  assert(instr->next == NULL);
  assert(instr->prev == NULL);
  assert(instr->block == NULL);
  assert(instr->inputs.size == 0);
  assert(instr->uses.size == 0);

  releaseVector(&instr->inputs);
  releaseVector(&instr->uses);

  if (instr->kind == IR_PHI) {
    assert(instr->info.phi.phiBlocks.size == 0);
    releaseVector(&instr->info.phi.phiBlocks);
  }
}

void removeInstruction(IrInstructionListNode *inode) {
/*
  IrInstruction *instr = inode->instr;
  IrBasicBlock *bb = instr->block;
  assert(bb != NULL);

  if (inode->prev) {
    inode->prev->next = inode->next;
  } else {
    assert(bb->instrs.head == inode);
    bb->instrs.head = inode->next;
  }

  if (inode->next) {
    inode->next->prev = inode->prev;
  } else {
    assert(bb->instrs.tail == inode);
    bb->instrs.tail = inode->prev;
  }

  if (bb->instrs.head)
    bb->instrs.head->prev = NULL;

  if (bb->instrs.tail)
    bb->instrs.tail->next = NULL;

  inode->next = inode->prev = NULL;
 */
}

void addBasicBlockTail(IrFunction *function, IrBasicBlock *bb) {
    assert(bb->function == NULL);

	if (function->blocks.head == NULL) {
	  function->blocks.head = bb;
    }

	if (function->blocks.tail) {
    	function->blocks.tail->next = bb;
	}

    bb->prev = function->blocks.tail;
    function->blocks.tail = bb;

    function->numOfBlocks += 1;
    bb->function = function;
}

IrFunctionListNode *newFunctionListNode(IrFunction *f) {
    IrFunctionListNode *node = areanAllocate(ctx->irArena, sizeof (IrFunctionListNode));
    node->function = f;
    return node;
}

void addFunctionTail(IrFunctionList *list, IrFunction *function) {
    IrFunctionListNode *node = newFunctionListNode(function);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

void addInstructionHead(IrBasicBlock *block, IrInstruction *instr) {
    assert(instr->block == NULL && "Instruction already in block");
    instr->block = block;

    if (block->instrunctions.head == NULL) {
      block->instrunctions.head = block->instrunctions.tail = instr;
    } else {
      instr->next = block->instrunctions.head;
      block->instrunctions.head->prev = instr;
      block->instrunctions.head = instr;
    }
}

void addInstructionTail(IrBasicBlock *block, IrInstruction *instr) {
    assert(instr->block == NULL && "Instruction already in block");
    assert(block->term == NULL && "Add instruction into terminated block");
    instr->block = block;

    if (block->instrunctions.head == NULL) {
      block->instrunctions.head = block->instrunctions.tail = instr;
    } else {
      instr->prev = block->instrunctions.tail;
      block->instrunctions.tail->next = instr;
      block->instrunctions.tail = instr;
    }
}

IrBasicBlock *newBasicBlock(const char *name) {
    IrBasicBlock *bb = areanAllocate(ctx->irArena, sizeof (IrBasicBlock));
    bb->name = name;
    bb->id = ctx->bbCnt++;

    initVector(&bb->succs, 3);
    initVector(&bb->preds, 3);

    initVector(&bb->dominators.dominatees, INITIAL_VECTOR_CAPACITY);
    initVector(&bb->dominators.dominationFrontier, INITIAL_VECTOR_CAPACITY);

    addBasicBlockTail(ctx->currentFunc, bb);

    return bb;
}

void addSuccessor(IrBasicBlock *block, IrBasicBlock *succ) {
    addBlockToVector(&block->succs, succ);
    addBlockToVector(&succ->preds, block);
}

void addPredecessor(IrBasicBlock *block, IrBasicBlock *pred) {
    addBlockToVector(&block->preds, pred);
    addBlockToVector(&pred->succs, block);
}

void addBlockToVector(Vector *v, IrBasicBlock *block) {
  addToVector(v, (intptr_t)block);
}

IrBasicBlock *getBlockFromVector(const Vector *v, uint32_t i) {
  return (IrBasicBlock *)getFromVector(v, i);
}

void addPhiInput(IrInstruction *phi, IrInstruction *value, IrBasicBlock *block) {
  assert(phi->kind == IR_PHI);
  assert(block != NULL);
  Vector *inputs = &phi->inputs;
  Vector *blocks = &phi->info.phi.phiBlocks;
  assert(inputs->size == blocks->size);

  addInstructionInput(phi, value);
  addBlockToVector(blocks, block);
  assert(inputs->size == blocks->size);
}

IrInstruction *newPhiInstruction(enum IrTypeKind irType) {
  IrInstruction *phi = newInstruction(IR_PHI, irType);
  initVector(&phi->info.phi.phiBlocks, 4);
  return phi;
}

IrInstruction *newInstruction(enum IrIntructionKind kind, enum IrTypeKind type) {
    IrInstruction *instr = areanAllocate(ctx->irArena, sizeof (IrInstruction));
    instr->id = ctx->instrCnt++;
    instr->vreg = ctx->vregCnt++;
    instr->kind = kind;
    instr->type = type;

    initVector(&instr->inputs, 4);
    initVector(&instr->uses, INITIAL_VECTOR_CAPACITY);

    return instr;
}

IrInstruction *newGotoInstruction(IrBasicBlock *bb) {
    IrInstruction *instr = newInstruction(IR_BRANCH, IR_VOID);
    instr->info.branch.taken = bb;
    instr->info.branch.notTaken = NULL;

    return instr;
}

IrInstruction *newLabelInstruction(IrBasicBlock *block) {
  IrInstruction *instr = newInstruction(IR_CFG_LABEL, IR_LABEL);
  instr->info.block = block;
  return instr;
}

IrInstruction *newPhysRegister(enum IrTypeKind type, uint32_t regId) {
  IrInstruction *instr = newInstruction(IR_P_REG, type);
  instr->info.physReg = regId;
  return instr;
}

IrInstruction *newCondBranch(IrInstruction *cond, IrBasicBlock *takenBB, IrBasicBlock *notTakenBB) {
    IrInstruction *instr = newInstruction(IR_CBRANCH, IR_VOID);

    addInstructionInput(instr, cond);
    instr->info.branch.taken = takenBB;
    instr->info.branch.notTaken = notTakenBB;

    return instr;
}

IrInstruction *newTableBranch(IrInstruction *cond, SwitchTable *table) {
    IrInstruction *instr = newInstruction(IR_TBRANCH, IR_VOID);

    addInstructionInput(instr, cond);
    instr->info.switchTable = table;

    return instr;
}

static IrInstruction *newConstantInstruction(enum IrTypeKind irType, enum IrConstKind ckind) {
  IrInstruction *instr = newInstruction(IR_DEF_CONST, irType);
  instr->info.constant.kind = ckind;
  return instr;
}

IrBasicBlock *updateBlock() {
    IrBasicBlock *newBlock = newBasicBlock(NULL);
    ctx->currentBB = newBlock;
    return newBlock;
}

static void addInstructionToBlock(IrInstruction *instr, IrBasicBlock *block) {
  addInstructionTail(block, instr);
}

void addInstruction(IrInstruction *instr) {
    IrBasicBlock *bb = ctx->currentBB;
    if (bb != NULL) {
        assert(bb->term == NULL && "Adding instruction into terminated block");
    } else {
        bb = updateBlock();
    }

    addInstructionToBlock(instr, bb);
}

void termintateBlock(IrInstruction *instr) {
    // assert(instr->isTerminator())
    addInstruction(instr);
    ctx->currentBB->term = instr;
    ctx->currentBB = NULL;
}

IrInstruction *updateBlockTerminator(IrBasicBlock *block, IrInstruction *newTerminator) {

  IrInstruction *currentTerm = block->term;

  if (currentTerm != NULL) {
    assert(block->instrunctions.tail == currentTerm);
    clearVector(&currentTerm->inputs);
    eraseInstructionFromBlock(currentTerm);
    block->term = NULL;
  }

  addInstructionToBlock(newTerminator, block);
  block->term = newTerminator;

  currentTerm->prev = currentTerm->next = NULL;

  return currentTerm;
}

void gotoToBlock(IrBasicBlock *gotoBB) {
    IrInstruction *gotoInstr = newGotoInstruction(gotoBB);
    addSuccessor(ctx->currentBB, gotoBB);
    termintateBlock(gotoInstr);
}

void replaceInputAt(IrInstruction *instr, IrInstruction *v, size_t i) {
  assert(i < instr->inputs.size);

  IrInstruction *oldValue = getInstructionFromVector(&instr->inputs, i);
  removeFromVector(&oldValue->uses, (intptr_t) instr);

  instr->inputs.storage[i] = (intptr_t)v;
  addInstructionToVector(&v->uses, instr);
}

void replaceUsageWith(IrInstruction *instr, IrInstruction *newInstr) {
  if (instr == newInstr)
    return; // TODO: assert??

  Vector *uses = &instr->uses;
  size_t idx = 0;
  while (uses->size != 0) {
    size_t index = uses->size - idx - 1;
    assert(index < uses->size);
    IrInstruction *user = getInstructionFromVector(uses, index);

    removeFromVector(uses, (intptr_t)user);
    Vector *inputs = &user->inputs;
    Boolean added = FALSE;
    for (size_t j = 0; j < inputs->size; ++j) {
      IrInstruction *input = getInstructionFromVector(inputs, j);
      if (input == instr) {
        added = TRUE;
        inputs->storage[j] = (intptr_t)newInstr;
      }
    }

    if (added) {
      addInstructionToVector(&newInstr->uses, user);
    }
  }

  assert(uses->size == 0);
}

void eraseInstructionFromBlock(IrInstruction *instr) {
  IrBasicBlock *block = instr->block;

  assert(block != NULL);
  assert(instr->uses.size == 0);
  assert(instr->inputs.size == 0);

  IrInstruction *prev = instr->prev;
  IrInstruction *next = instr->next;

  if (block->instrunctions.head == instr) {
    block->instrunctions.head = next;
  }

  if (block->instrunctions.tail == instr) {
    block->instrunctions.tail = prev;
  }

  if (prev)
    prev->next = next;

  if (next)
    next->prev = prev;

  instr->prev = instr->next = NULL;
  instr->block = NULL;
}

void eraseInstruction(IrInstruction *instr) {
  assert(instr->uses.size == 0);

  Vector *inputs = &instr->inputs;

  for (size_t i = 0; i < inputs->size; ++i) {
    IrInstruction *input = getInstructionFromVector(inputs, i);
    removeFromVector(&input->uses, (intptr_t)instr);
  }
  clearVector(inputs);

  if (instr->kind == IR_PHI) {
    clearVector(&instr->info.phi.phiBlocks);
  }

  eraseInstructionFromBlock(instr);
}

IrBasicBlock *eraseBlock(IrBasicBlock *block) {
  IrBasicBlock *prev = block->prev;
  IrBasicBlock *next = block->next;
  IrFunction *func = block->function;

  assert(func != NULL);

  if (func->blocks.head == block) {
    func->blocks.head = next;
  }

  if (func->blocks.tail == block) {
    func->blocks.tail = prev;
  }

  if (prev) {
    prev->next = next;
  }

  if (next) {
    next->prev = prev;
  }

  block->prev = block->next = NULL;
  block->function = NULL;

  return next;
}

static IrInstruction *processSinglePhiNode(IrInstruction *phiInstr, IrBasicBlock *removingEdge) {
  assert(phiInstr->kind == IR_PHI);

  Vector *inputs = &phiInstr->inputs;
  Vector *blocks = &phiInstr->info.phi.phiBlocks;

  IrInstruction *next = phiInstr->next;

  assert(inputs->size == blocks->size);

  for (size_t idx = 0; idx < inputs->size; ++idx) {
    IrInstruction *input = getInstructionFromVector(inputs, idx);
    IrBasicBlock *block = getBlockFromVector(blocks, idx);

    if (block != removingEdge) {
      continue;
    }

    // By index, not by value: the same definition can reach the phi along
    // several edges, and removeFromVector() would drop that value's *first*
    // occurrence, sliding the inputs out of step with the blocks they are
    // paired with.
    removeFromVectorAt(inputs, idx);
    removeFromVectorAt(blocks, idx);

    assert(blocks->size == phiInstr->block->preds.size);

    removeFromVector(&input->uses, (intptr_t)phiInstr);

    if (inputs->size == 1) {
      assert(phiInstr->block->preds.size == 1);
      IrInstruction *lastUsage = getInstructionFromVector(inputs, 0);
      removeFromVector(&lastUsage->uses, (intptr_t)phiInstr);
      replaceUsageWith(phiInstr, lastUsage);
      clearVector(&phiInstr->info.phi.phiBlocks);
      cleanAndErase(phiInstr);
    }

    return next;
  }

  unreachable("Should be done in loop body");
  return NULL;
}

static void processPhiNodes(IrBasicBlock *phiBlock, IrBasicBlock *removingEdge) {
  IrInstruction *i = phiBlock->instrunctions.head;

  while (i != NULL) {
    if (i->kind != IR_PHI) {
      // PHI-nodes are placed always in the beginning of the block
      break;
    }

    i = processSinglePhiNode(i, removingEdge);
  }
}

void removeSuccessor(IrBasicBlock *block, IrBasicBlock *succ) {
  removeFromVector(&block->succs, (intptr_t)succ);
  removeFromVector(&succ->preds, (intptr_t)block);

  processPhiNodes(succ, block);
}

// -============================ Critical edges ============================-
//
// An edge is critical when its source has several successors and its target
// several predecessors: there is nowhere on it to put anything. Splitting one
// means interposing a block that only jumps on, which gives every edge a place
// of its own to hold code that must run exactly when that edge is taken.
//
// Two passes need this and need it at different times, which is why it lives
// here rather than in either of them: gvn's pre() splits so a computation
// cloned into a predecessor is never executed speculatively, and codegen's
// stage 0 (src/ir/codegen/prepare.c) splits so phi destruction has an edge to
// put its copies on. The two are independent - dce runs in between and both
// removes blocks and rewires branches, so the property gvn established cannot
// be assumed still to hold by the time codegen looks.

Boolean isCriticalEdge(const IrBasicBlock *src, const IrBasicBlock *dst) {
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
  // The split block's only predecessor is 'from', so 'from' is its idom.
  split->dominators.sdom = from;
  addBlockToVector(&from->dominators.dominatees, split);

  // 'to' keeps its other predecessors, so 'split' does not strictly
  // dominate it: DF(split) = { to }. No other frontier changes: every
  // block dominating 'from' strictly dominates 'split' (its only pred),
  // so 'split' enters no frontier, and 'to'-s frontier memberships are
  // unaffected because 'split' is dominated by exactly the dominators
  // of 'from'.
  addBlockToVector(&split->dominators.dominationFrontier, to);
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

  addToVector(&newBB->preds, (intptr_t)block);
  addToVector(&newBB->succs, (intptr_t)succ);

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

void splitCriticalEdges(IrFunction *func) {
  for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
    Vector *succs = &block->succs;
    IrInstruction *terminator = block->term;
    assert(terminator != NULL);

    // A computed goto's targets cannot be rewritten; leave its edges alone
    // (pre() refuses to insert on them instead).
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

void removeFromBlockList(IrBasicBlockList *list, IrBasicBlock *block) {
  /* IrBasicBlockListNode *bn = list->head; */
  /* while (bn != NULL) { */
  /*   if (bn->block == block) { */
  /*     bn = eraseFromBlockList(list, bn); */
  /*   } else { */
  /*     bn = bn->next; */
  /*   } */
  /* } */
}

typedef struct _ConstantCacheData {
    // The whole key, together with the payload below. A constant instruction
    // carries the width and signedness everything downstream reads off it, so 0
    // asked for as IR_I32 and 0 asked for as IR_U64 are two different
    // instructions even though they hold the same bits. Keying on the value
    // alone handed back whichever one happened to be built first, so a caller
    // could ask for an I32 and be given a U64 - which is what made both arms of
    // a ternary in tinyc/46_grep.c come out with different types and trip
    // translateTernary's same-type assertion.
    //
    // The kind is deliberately *not* stored here: constKindOfType() derives it
    // from the type, so keeping a copy would be a second field saying what the
    // first already says, and comparing both would only ever confirm the
    // derivation. The instruction still carries one, since its readers want a
    // four-way discriminator on hand; the caller's is checked against the type
    // in getOrAddConstant() and then dropped.
    enum IrTypeKind type;
    IrConstantData data;
    IrInstruction *value;
} ConstantCacheData;

ConstantCacheData *getCCDFromVector(Vector *v, uint32_t i) {
  return (ConstantCacheData *)getFromVector(v, i);
}

void addToCCDVector(Vector *v, ConstantCacheData *data) {
  addToVector(v, (intptr_t)data);
}

// Which union member of IrConstantData a constant of this type holds. The kind
// is not independent information: a float type carries a float and nothing
// else, and IR_LITERAL/IR_REF exist only to say "string" and "symbol" - they
// are not machine types anything can compute with. Deriving it here is what
// lets the cache key be the type alone; the instruction keeps a stored copy
// because three of its four readers (isel, cp, irdump) want a four-way
// discriminator on hand rather than a call at every use.
static enum IrConstKind constKindOfType(enum IrTypeKind type) {
    switch (type) {
    case IR_F32:
    case IR_F64:
    case IR_F80:
      return IR_CK_FLOAT;
    case IR_LITERAL:
      return IR_CK_LITERAL;
    case IR_REF:
      return IR_CK_SYMBOL;
    default:
      return IR_CK_INTEGER;
    }
}

static IrInstruction *getFromCache(const ConstantCacheData *data) {
    const ConstantCacheData **cacheData = (const ConstantCacheData **)ctx->constantCache.storage;
    for (size_t i = 0; i < ctx->constantCache.size; ++i) {
        ConstantCacheData *cacheData = getCCDFromVector(&ctx->constantCache, i);
        if (cacheData->type == data->type) {
            // Same type means same union member on both sides, so one lookup
            // covers the entry as well as the query.
            switch (constKindOfType(data->type)) {
            case  IR_CK_INTEGER:
                if (data->data.i == cacheData->data.i) {
                    assert(cacheData->value != NULL);
                    return cacheData->value;
                }
                break;
            case IR_CK_FLOAT:
                if (memcmp(&data->data.f, &cacheData->data.f, sizeof data->data.f) == 0)
                    return cacheData->value;
                break;
            case IR_CK_LITERAL:
                if (data->data.l.length == cacheData->data.l.length) {
                    if (strncmp(data->data.l.s, cacheData->data.l.s, data->data.l.length) == 0)
                        return cacheData->value;
                }
                break;
            case IR_CK_SYMBOL:
                if (data->data.s == cacheData->data.s)
                  return cacheData->value;
                break;
            }
        }
    }

    return NULL;
}

static IrInstruction *getOrAddConstant(ConstantCacheData *data, enum IrConstKind kind, enum IrTypeKind type) {

    // The caller says which union member it filled in; the type says which one
    // a constant of that type holds. They have to agree, and this is the one
    // place both are in hand. Asking for an *integer* constant of type
    // IR_LITERAL - which is what 'flag && "text"' used to do - fails here
    // rather than building a constant whose payload and whose type disagree
    // about which member is live. Past this point only the type is kept.
    assert(kind == constKindOfType(type));

    data->type = type;

    IrInstruction *cached = getFromCache(data);

    if (cached != NULL)
      return cached;

    // not found
    IrInstruction *instr = newConstantInstruction(type, kind);
    instr->info.constant.data = data->data;

    ConstantCacheData *newValue = areanAllocate(ctx->irArena, sizeof(ConstantCacheData));
    memcpy(newValue, data, sizeof(ConstantCacheData));
    newValue->value = instr;
    instr->info.constant.cacheIdx = ctx->constantCache.size;
    addToCCDVector(&ctx->constantCache, newValue);
    addInstructionHead(ctx->currentFunc->entry, instr);

    return instr;
}

static const ConstantCacheData *getCachedConstant(uint32_t idx) {
    assert(idx < ctx->constantCache.size);
    return (const ConstantCacheData *)ctx->constantCache.storage[idx];
}

IrInstruction *createIntegerConstant(enum IrTypeKind type, int64_const_t v) {
    ConstantCacheData d;
    d.data.i = v;
    return getOrAddConstant(&d, IR_CK_INTEGER, type);
}

IrInstruction *createFloatConstant(enum IrTypeKind type, float80_const_t v) {
    ConstantCacheData d;
    memset(&d, 0, sizeof d);
    d.data.f = v;
    return getOrAddConstant(&d, IR_CK_FLOAT, type);
}

IrInstruction *createSymbolConstant(Symbol *s) {
    ConstantCacheData d;
    d.data.s = s;
    return getOrAddConstant(&d, IR_CK_SYMBOL, IR_REF);
}

IrInstruction *createLiteralConstant(const char *v, size_t l) {
    ConstantCacheData d;
    d.data.l.length = l;
    d.data.l.s = v;
    return getOrAddConstant(&d, IR_CK_LITERAL, IR_LITERAL);
}

IrInstruction *newGEPInstruction(IrInstruction *base, IrInstruction *offset, const TypeRef *underType) {
    TypeRef *pointee = makePointedType(ctx->pctx, 0, underType);
    enum IrTypeKind irType = typeRefToIrType(pointee);
    IrInstruction *gepInstr = newInstruction(IR_GET_ELEMENT_PTR, irType);
    addInstructionInput(gepInstr, base);
    addInstructionInput(gepInstr, offset);

    gepInstr->info.gep.underlyingType = underType;
    gepInstr->astType = pointee;

    return gepInstr;
}

IrInstruction *newMemoryCopyInstruction(IrInstruction *dst, IrInstruction *src, IrInstruction *count, const TypeRef *copyType) {

  IrInstruction *copyInstr = newInstruction(IR_M_COPY, IR_VOID);
  addInstructionInput(copyInstr, dst);
  addInstructionInput(copyInstr, src);
  addInstructionInput(copyInstr, count);

  copyInstr->info.copy.elementType = copyType;

  return copyInstr;
}

IrInstruction *addLoadInstr(enum IrTypeKind valueType, IrInstruction *ptr, const AstExpression *ast) {
    assert(valueType != IR_VOID);
    IrInstruction *loadInstr = newInstruction(IR_M_LOAD, valueType);
    addInstructionInput(loadInstr, ptr);
    addInstruction(loadInstr);
    loadInstr->meta.astExpr = ast;
    loadInstr->astType = ast ? ast->type : NULL;
    loadInstr->info.memory.opType = valueType;

	return loadInstr;
}

IrInstruction *addStoreInstr(IrInstruction *ptr, IrInstruction *value, const AstExpression *ast) {
    IrInstruction *storeInstr = newInstruction(IR_M_STORE, IR_VOID);
    addInstructionInput(storeInstr, ptr);
    addInstructionInput(storeInstr, value);
    storeInstr->meta.astExpr = ast;
    storeInstr->info.memory.opType = value->type;
    addInstruction(storeInstr);
    return storeInstr;
}

IrInstruction *addBinaryOpeartion(enum IrIntructionKind op, IrInstruction *lhs, IrInstruction *rhs, enum IrTypeKind irType, TypeRef *astType, AstExpression *astExpr) {
  // TODO: assert op is binary

  IrInstruction *instr = newInstruction(op, irType);
  addInstructionInput(instr, lhs);
  addInstructionInput(instr, rhs);
  addInstruction(instr);

  instr->astType = astType;
  instr->meta.astExpr = astExpr;

  return instr;
}


