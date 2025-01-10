
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

void initializeIrContext(IrContext *_ctx, ParserContext* pctx) {

    memset(_ctx, 0, sizeof *_ctx);

    // TODO: check for NULL
    _ctx->irArena = createArena("IR Arena", 8 * DEFAULT_CHUNCK_SIZE);
    _ctx->pctx = pctx;
    _ctx->labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
    _ctx->constantCache = createVector(INITIAL_VECTOR_CAPACITY);
   ctx = _ctx;
}

void releaseIrContext(IrContext *_ctx) {
    ctx = NULL;
    releaseArena(_ctx->irArena);
    releaseHashMap(_ctx->labelMap);
    releaseVector(_ctx->constantCache);
}

IrOperandListNode *newOpListNode(IrOperand *op) {
    IrOperandListNode *node = areanAllocate(ctx->irArena, sizeof (IrOperandListNode));
    node->op = op;
    return node;
}

void addOperandTail(IrOperandList *list, IrOperand *op) {
    IrOperandListNode *node = newOpListNode(op);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

IrInstructionListNode *newInstListNode(IrInstruction *instr) {
    IrInstructionListNode *node = areanAllocate(ctx->irArena, sizeof (IrInstructionListNode));
    node->instr = instr;
    return node;
}

void addInstuctionTail(IrInstructionList *list, IrInstruction *instr) {
    IrInstructionListNode *node = newInstListNode(instr);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

void addInstuctionHead(IrInstructionList *list, IrInstruction *instr) {
    IrInstructionListNode *node = newInstListNode(instr);
	if (list->tail == NULL)
	  list->tail = node;
	if (list->head)
    	list->head->prev = node;
    node->next = list->head;
    list->head = node;
}

void releaseOperand(IrOperand *op) {
  releaseVector(&op->uses);
}

void releaseInstruction(IrInstruction *instr) {
}

void removeInstruction(IrInstructionListNode *inode) {
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

IrOperand *newIrOperand(enum IrTypeKind type, enum IrOperandKind kind) {
    IrOperand *op = areanAllocate(ctx->irArena, sizeof (IrOperand));
    op->id = ctx->opCnt++;
    op->type = type;
    op->kind = kind;
    initVector(&op->uses, 10);
    return op;
}


IrOperand *newVreg(enum IrTypeKind type) {
  IrOperand *v = newIrOperand(type, IR_VREG);
  v->data.vid = ctx->vregCnt++;
  return v;
}

IrOperand *newPreg(enum IrTypeKind type, uint32_t pid) {
  IrOperand *v = newIrOperand(type, IR_PREG);
  v->data.pid = pid;
  return v;
}

IrOperand *newLabelOperand(IrBasicBlock *block) {
    IrOperand *op = newIrOperand(IR_LABEL, IR_BLOCK);
    op->data.bb = block;
    return op;
}

void addInstructionDef(IrInstruction *instr, IrOperand *def) {
    addOperandTail(&instr->defs, def);
    def->def = instr;
}

void addInstructionUse(IrInstruction *instr, IrOperand *use) {
    addOperandTail(&instr->uses, use);
    addToVector(&use->uses, (intptr_t)instr);
}

void addPhiInput(IrInstruction *instr, IrOperand *value, IrBasicBlock *block) {

  assert(instr->kind == IR_PHI);
  assert(block != NULL);

  IrOperand *labelOp = newLabelOperand(block);
  addInstructionUse(instr, value);
  addInstructionUse(instr, labelOp);
}

IrInstruction *newInstruction(enum IrIntructionKind kind) {
    IrInstruction *instr = areanAllocate(ctx->irArena, sizeof (IrInstruction));
    instr->id = ctx->instrCnt++;
    instr->kind = kind;
    return instr;
}

IrInstruction *newMoveInstruction(IrOperand *src, IrOperand *dst) {
    IrInstruction *instr = newInstruction(IR_MOVE);
    addInstructionUse(instr, src);
    addInstructionDef(instr, dst);
    return instr;
}

IrInstruction *newGotoInstruction(IrBasicBlock *bb) {
    IrInstruction *instr = newInstruction(IR_BRANCH);
    IrOperand *op = newLabelOperand(bb);

    addInstructionUse(instr, op);

    return instr;
}

IrInstruction *newCondBranch(IrOperand *cond, IrBasicBlock *thenBB, IrBasicBlock *elseBB) {
    IrInstruction *instr = newInstruction(IR_CBRANCH);

    IrOperand *thenOp = newLabelOperand(thenBB);
    IrOperand *elseOp = newLabelOperand(elseBB);

    addInstructionUse(instr, cond);
    addInstructionUse(instr, thenOp);
    addInstructionUse(instr, elseOp);

    return instr;
}

IrInstruction *newTableBranch(IrOperand *cond, SwitchTable *table) {
    IrInstruction *instr = newInstruction(IR_TBRANCH);

    addInstructionUse(instr, cond);
    instr->meta.switchTable = table;

    return instr;
}

IrBasicBlock *updateBlock() {
    IrBasicBlock *newBlock = newBasicBlock(NULL);
    ctx->currentBB = newBlock;
    return newBlock;
}


void addInstruction(IrInstruction *instr) {
    IrBasicBlock *bb = ctx->currentBB;
    if (bb != NULL) {
        assert(bb->term == NULL && "Adding instruction into terminated block");
    } else {
        bb = updateBlock();
    }

    instr->block = bb;

    addInstuctionTail(&bb->instrs, instr);
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
  for (size_t i = 0; i < oldValue->uses.size; ++i) {
    IrInstruction *useInstr = (IrInstruction *)getFromVector(&oldValue->uses, i);
    for (IrOperandListNode *on = useInstr->uses.head; on != NULL; on = on->next) {
      IrOperand *op = on->op;
      if (op == oldValue) {
        on->op = newValue;
        addToVector(&newValue->uses, (intptr_t)useInstr);
      }
    }
  }
  clearVector(&oldValue->uses);
}

void replaceInputIn(IrInstruction *instr, IrOperandListNode *opNode, IrOperand *newOp) {
   // TODO: remove from old op uses
   opNode->op = newOp;
   addToVector(&newOp->uses, (intptr_t)instr);
}

IrOperand *getOrAddConstant(ConstantCacheData *data, enum IrTypeKind type) {
    const ConstantCacheData **cacheData = (const ConstantCacheData **)ctx->constantCache->storage;
    for (size_t i = 0; i < ctx->constantCache->size; ++i) {
        if (cacheData[i]->kind == data->kind) {
            switch (data->kind) {
            case CK_INT_CONST:
                if (data->data.i == cacheData[i]->data.i) {
                    assert(cacheData[i]->op != NULL);
                    return cacheData[i]->op;
                }
                break;
            case CK_FLOAT_CONST:
                if (memcmp(&data->data.f, &cacheData[i]->data.f, sizeof data->data.f) == 0)
                    return cacheData[i]->op;
                break;
            case CK_STRING_LITERAL:
                if (data->data.l.length == cacheData[i]->data.l.length) {
                    if (strncmp(data->data.l.s, cacheData[i]->data.l.s, data->data.l.length) == 0)
                        return cacheData[i]->op;
                }
                break;
            }
        }
    }

    // not found
    IrOperand *constOp = newIrOperand(type, IR_CONST);
    ConstantCacheData *newValue = areanAllocate(ctx->irArena, sizeof(ConstantCacheData));
    memcpy(newValue, data, sizeof(ConstantCacheData));
    newValue->op = constOp;
    constOp->data.literalIndex = ctx->constantCache->size;
    // raise(SIGTRAP);
    addToVector(ctx->constantCache, (intptr_t)newValue);
    ConstantCacheData *fromCache = ((ConstantCacheData **)ctx->constantCache->storage)[constOp->data.literalIndex];
    printf("fromCache %p, newValue %p, cache size = %u, capacity = %u\n", fromCache, newValue, ctx->constantCache->size, ctx->constantCache->capacity);
    assert(fromCache == newValue);
    assert(fromCache->op == constOp);
    return constOp;
}

static const ConstantCacheData *getCachedConstant(uint32_t idx) {
    assert(idx < ctx->constantCache->size);
    return (const ConstantCacheData *)ctx->constantCache->storage[idx];
}

IrOperand *createIntegerConstant(enum IrTypeKind type, int64_const_t v) {
    ConstantCacheData d;
    d.kind = CK_INT_CONST;
    d.data.i = v;
    return getOrAddConstant(&d, type);
}

IrOperand *createFloatConstant(enum IrTypeKind type, float80_const_t v) {
    ConstantCacheData d;
    d.kind = CK_FLOAT_CONST;
    d.data.f = v;
    return getOrAddConstant(&d, type);
}

IrOperand *addLoadInstr(enum IrTypeKind valueType, IrOperand *base, IrOperand *offset, const AstExpression *ast) {
    IrOperand *loadee = newVreg(valueType);
    IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
    addInstructionUse(loadInstr, base);
    addInstructionUse(loadInstr, offset);
    addInstructionDef(loadInstr, loadee);
    addInstruction(loadInstr);
    loadInstr->meta.astExpr = loadee->ast.e = ast;
    loadee->astType = ast->type;

	return loadee;
}

void addStoreInstr(IrOperand *base, IrOperand *offset,IrOperand *value, const AstExpression *ast) {
    IrInstruction *storeInstr = newInstruction(IR_M_STORE);
    addInstructionUse(storeInstr, base);
    addInstructionUse(storeInstr, offset);
    addInstructionUse(storeInstr, value);
    storeInstr->meta.astExpr = ast;
    addInstruction(storeInstr);
}

