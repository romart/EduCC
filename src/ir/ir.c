
#include <assert.h>
#include "ir/ir.h"
#include "parser.h"
#include "tree.h"
#include "sema.h"

typedef struct _CaseBlock {
    int64_t caseConst;
    IrBasicBlock *block;
} CaseBlock;

IrContext *ctx = NULL;

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
    _ctx->labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
    initVector(&_ctx->constantCache, INITIAL_VECTOR_CAPACITY);
    initVector(&_ctx->allocas, INITIAL_VECTOR_CAPACITY);
   ctx = _ctx;
}

void releaseIrContext(IrContext *_ctx) {
    ctx = NULL;
    releaseArena(_ctx->irArena);
    releaseHashMap(_ctx->labelMap);
    releaseVector(&_ctx->constantCache);
    releaseVector(&_ctx->allocas);
}

void resetIrContext(IrContext *_ctx) {
  clearVector(&_ctx->constantCache);
  clearVector(&_ctx->allocas);
  // clear ctx->labelMap
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

IrBasicBlockListNode *newBBListNode(IrBasicBlock *bb) {
    IrBasicBlockListNode *node = areanAllocate(ctx->irArena, sizeof (IrBasicBlockListNode));
    node->block = bb;
    return node;
}

void addBBTail(IrBasicBlockList *list, IrBasicBlock *bb) {
    IrBasicBlockListNode *node = newBBListNode(bb);

	if (list->head == NULL)
	  list->head = node;
	if (list->tail) {
    	list->tail->next = node;
	}
    node->prev = list->tail;
    list->tail = node;
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

    addBBTail(&ctx->currentFunc->blocks, bb);

    return bb;
}

void addSuccessor(IrBasicBlock *block, IrBasicBlock *succ) {
    addBBTail(&block->succs, succ);
    addBBTail(&succ->preds, block);
}

void addPredecessor(IrBasicBlock *block, IrBasicBlock *pred) {
    addBBTail(&block->preds, pred);
    addBBTail(&pred->succs, block);
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

IrInstruction *newMoveInstruction(IrOperand *src, IrOperand *dst) {
//    IrInstruction *instr = newInstruction(IR_MOVE, dst->type);
//    addInstructionUse(instr, src);
//    addInstructionDef(instr, dst);
//    return instr;

  return NULL;
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

void gotoToBlock(IrBasicBlock *gotoBB) {
    IrInstruction *gotoInstr = newGotoInstruction(gotoBB);
    addSuccessor(ctx->currentBB, gotoBB);
    termintateBlock(gotoInstr);
}

void replaceInputWith(IrOperand *oldValue, IrOperand *newValue) {
//  for (size_t i = 0; i < oldValue->uses.size; ++i) {
//    IrInstruction *useInstr = (IrInstruction *)getFromVector(&oldValue->uses, i);
//    for (IrOperandListNode *on = useInstr->uses.head; on != NULL; on = on->next) {
//      IrOperand *op = on->op;
//      if (op == oldValue) {
//        on->op = newValue;
//        addToVector(&newValue->uses, (intptr_t)useInstr);
//      }
//    }
//  }
//  clearVector(&oldValue->uses);

  unimplemented("replace inputs");
}


void replaceInputAt(IrInstruction *instr, IrInstruction *v, size_t i) {
  assert(i < instr->inputs.size);

  IrInstruction *oldValue = getInstructionFromVector(&instr->inputs, i);
  removeFromVector(&oldValue->uses, (intptr_t) instr);

  instr->inputs.storage[i] = (intptr_t)v;
  addInstructionToVector(&v->uses, instr);
}

void replaceInputIn(IrInstruction *instr, IrOperandListNode *opNode, IrOperand *newOp) {
   // TODO: remove from old op uses
   opNode->op = newOp;
   addToVector(&newOp->uses, (intptr_t)instr);
}

void replaceUsageWith(IrInstruction *instr, IrInstruction *newInstr) {
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

  eraseInstructionFromBlock(instr);
}

IrBasicBlockListNode *eraseFromBlockList(IrBasicBlockList *list, IrBasicBlockListNode *bn) {
  IrBasicBlockListNode *prev = bn->prev;
  IrBasicBlockListNode *next = bn->next;

  if (list->head == bn)
    list->head = next;
  if (list->tail == bn)
    list->tail = prev;

  if (prev)
    prev->next = next;

  if (next)
    next->prev = prev;

  bn->prev = bn->next = NULL;

  return next;
}

void removeFromBlockList(IrBasicBlockList *list, IrBasicBlock *block) {
  IrBasicBlockListNode *bn = list->head;
  while (bn != NULL) {
    if (bn->block == block) {
      bn = eraseFromBlockList(list, bn);
    } else {
      bn = bn->next;
    }
  }
}

typedef struct _ConstantCacheData {
    enum IrConstKind kind;
    IrConstantData data;
    IrInstruction *value;
} ConstantCacheData;

ConstantCacheData *getCCDFromVector(Vector *v, uint32_t i) {
  return (ConstantCacheData *)getFromVector(v, i);
}

void addToCCDVector(Vector *v, ConstantCacheData *data) {
  addToVector(v, (intptr_t)data);
}

static IrInstruction *getFromCache(const ConstantCacheData *data) {
    const ConstantCacheData **cacheData = (const ConstantCacheData **)ctx->constantCache.storage;
    for (size_t i = 0; i < ctx->constantCache.size; ++i) {
        ConstantCacheData *cacheData = getCCDFromVector(&ctx->constantCache, i);
        if (cacheData->kind == data->kind) {
            switch (data->kind) {
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

static IrInstruction *getOrAddConstant(ConstantCacheData *data, enum IrTypeKind type) {

    IrInstruction *cached = getFromCache(data);

    if (cached != NULL)
      return cached;

    // not found
    IrInstruction *instr = newConstantInstruction(type, data->kind);
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
    d.kind = IR_CK_INTEGER;
    d.data.i = v;
    return getOrAddConstant(&d, type);
}

IrInstruction *createFloatConstant(enum IrTypeKind type, float80_const_t v) {
    ConstantCacheData d;
    d.kind = IR_CK_FLOAT;
    d.data.f = v;
    return getOrAddConstant(&d, type);
}

IrInstruction *createSymbolConstant(Symbol *s) {
    ConstantCacheData d;
    d.kind = IR_CK_SYMBOL;
    d.data.s = s;
    return getOrAddConstant(&d, IR_REF);
}

IrInstruction *createLiteralConstant(const char *v, size_t l) {
    ConstantCacheData d;
    d.kind = IR_CK_LITERAL;
    d.data.l.length = l;
    d.data.l.s = v;
    return getOrAddConstant(&d, IR_LITERAL);
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

