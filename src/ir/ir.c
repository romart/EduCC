
#include <assert.h>
#include "ir/ir.h"
#include "parser.h"
#include "tree.h"
#include "types.h"
#include "sema.h"
#include <signal.h>

typedef struct _CaseBlock {
    int64_t caseConst;
    IrBasicBlock *block;
} CaseBlock;



static IrBasicBlock *getOrCreateLabelBlock(IrContext *ctx, const char *labelName);

void initializeIrContext(IrContext *ctx, ParserContext* pctx) {

    memset(ctx, 0, sizeof *ctx);

    // TODO: check for NULL
    ctx->irArena = createArena("IR Arena", 8 * DEFAULT_CHUNCK_SIZE);
    ctx->pctx = pctx;
    ctx->labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
    ctx->constantCache = createVector(64);
}

void releaseIrContext(IrContext *ctx) {
    releaseArena(ctx->irArena);
    releaseHashMap(ctx->labelMap);
    releaseVector(ctx->constantCache);
}

static IrOperandListNode *newOpListNode(IrContext *ctx, IrOperand *op) {
    IrOperandListNode *node = areanAllocate(ctx->irArena, sizeof (IrOperandListNode));
    node->op = op;
    return node;
}

static void addOperandTail(IrContext *ctx, IrOperandList *list, IrOperand *op) {
    IrOperandListNode *node = newOpListNode(ctx, op);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrInstructionListNode *newInstListNode(IrContext *ctx, IrInstruction *instr) {
    IrInstructionListNode *node = areanAllocate(ctx->irArena, sizeof (IrInstructionListNode));
    node->instr = instr;
    return node;
}

static void addInstuctionTail(IrContext *ctx, IrInstructionList *list, IrInstruction *instr) {
    IrInstructionListNode *node = newInstListNode(ctx, instr);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrBasicBlockListNode *newBBListNode(IrContext *ctx, IrBasicBlock *bb) {
    IrBasicBlockListNode *node = areanAllocate(ctx->irArena, sizeof (IrBasicBlockListNode));
    node->block = bb;
    return node;
}

static void addBBTail(IrContext *ctx, IrBasicBlockList *list, IrBasicBlock *bb) {
    IrBasicBlockListNode *node = newBBListNode(ctx, bb);

	if (list->head == NULL)
	  list->head = node;
	if (list->tail) {
    	list->tail->next = node;
	}
    node->prev = list->tail;
    list->tail = node;
}

static IrFunctionListNode *newFunctionListNode(IrContext *ctx, IrFunction *f) {
    IrFunctionListNode *node = areanAllocate(ctx->irArena, sizeof (IrFunctionListNode));
    node->function = f;
    return node;
}

static void addFunctionTail(IrContext *ctx, IrFunctionList *list, IrFunction *function) {
    IrFunctionListNode *node = newFunctionListNode(ctx, function);
	if (list->head == NULL)
	  list->head = node;
	if (list->tail)
    	list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrFunction *translateFunction(IrContext *ctx, AstFunctionDefinition *function);
static Boolean translateStatement(IrContext *ctx, AstStatement *stmt);
static Boolean translateBlock(IrContext *ctx, AstStatement *block);
static Boolean translateStatement(IrContext *ctx, AstStatement *stmt);
static Boolean translateDeclaration(IrContext *ctx, AstDeclaration *decl);
static IrOperand *translateExpression(IrContext* ctx, AstExpression *expr);
static Boolean translateLabel(IrContext* ctx, AstStatement *stmt);
static Boolean translateGotoLabel(IrContext* ctx, AstStatement *stmt);
static Boolean translateGotoPtr(IrContext* ctx, AstStatement *stmt);
static Boolean translateReturn(IrContext* ctx, AstStatement *stmt);
static Boolean translateBreak(IrContext* ctx, AstStatement *stmt);
static Boolean translateContinue(IrContext* ctx, AstStatement *stmt);
static Boolean translateIf(IrContext* ctx, AstStatement *stmt);
static Boolean translateSwitch(IrContext* ctx, AstStatement *stmt);
static Boolean translateWhile(IrContext* ctx, AstStatement *stmt);
static Boolean translateDoWhile(IrContext* ctx, AstStatement *stmt);
static Boolean translateFor(IrContext* ctx, AstStatement *stmt);

IrFunctionList translateAstToIr(IrContext *ctx, AstFile *file) {
    IrFunctionList list = {0};


    AstTranslationUnit *unit = file->units;

    while (unit != NULL) {
        if (unit->kind == TU_FUNCTION_DEFINITION) {
		  	fprintf(stdout, "Translate function '%s' into IR\n", unit->definition->declaration->name);
            IrFunction *function = translateFunction(ctx, unit->definition);
            addFunctionTail(ctx, &list, function);
        } else {
            assert(unit->kind == TU_DECLARATION);
            translateDeclaration(ctx, unit->declaration);
        }
        unit = unit->next;
    }

    return list;
}

static IrBasicBlock *newBasicBlock(IrContext *ctx, const char *name) {
    IrBasicBlock *bb = areanAllocate(ctx->irArena, sizeof (IrBasicBlock));
    bb->name = name;
    bb->id = ctx->bbCnt++;

    addBBTail(ctx, &ctx->currentFunc->blocks, bb);

    return bb;
}

static IrFunction *newIrFunction(IrContext *ctx, AstFunctionDefinition *function) {
    IrFunction *func = areanAllocate(ctx->irArena, sizeof (IrFunction));
	ctx->currentFunc = func;
    func->ast = function;
    func->id = ctx->functionCnt++;
    func->entry = newBasicBlock(ctx, "<entry>");
    func->exit = newBasicBlock(ctx, "<exit>");
    return func;
}

static IrOperand *newIrOperand(IrContext *ctx, enum IrTypeKind type, enum IrOperandKind kind) {
    IrOperand *op = areanAllocate(ctx->irArena, sizeof (IrOperand));
    op->id = ctx->opCnt++;
    op->type = type;
    op->kind = kind;
    return op;
}


static IrOperand *newVreg(IrContext *ctx, enum IrTypeKind type) {
  IrOperand *v = newIrOperand(ctx, type, IR_VREG);
  v->data.vid = ctx->vregCnt++;
  return v;
}

static void addInstructionDef(IrContext *ctx, IrInstruction *instr, IrOperand *def) {
    addOperandTail(ctx, &instr->defs, def);
}

static void addInstructionUse(IrContext *ctx, IrInstruction *instr, IrOperand *use) {
    addOperandTail(ctx, &instr->uses, use);
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
	default: unreachable("unexpected type ref");
	}
    return IR_U64;
}

static IrInstruction *newInstruction(IrContext *ctx, enum IrIntructionKind kind) {
    IrInstruction *instr = areanAllocate(ctx->irArena, sizeof (IrInstruction));
    instr->id = ctx->instrCnt++;
    instr->kind = kind;
    return instr;
}

static IrInstruction *newMoveInstruction(IrContext *ctx, IrOperand *src, IrOperand *dst) {
    IrInstruction *instr = newInstruction(ctx, IR_MOVE);
    addInstructionUse(ctx, instr, src);
    addInstructionDef(ctx, instr, dst);
    return instr;
}

static IrInstruction *newGotoInstruction(IrContext *ctx, IrBasicBlock *bb) {
    IrInstruction *instr = newInstruction(ctx, IR_BRANCH);
    IrOperand *op = newIrOperand(ctx, IR_LABEL, IR_BLOCK);

    op->data.bb = bb;
    addInstructionUse(ctx, instr, op);

    return instr;
}

static IrInstruction *newCondBranch(IrContext *ctx, IrOperand *cond, IrBasicBlock *thenBB, IrBasicBlock *elseBB) {
    IrInstruction *instr = newInstruction(ctx, IR_CBRANCH);

    IrOperand *thenOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
    IrOperand *elseOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);

    thenOp->data.bb = thenBB;
    elseOp->data.bb = elseBB;

    addOperandTail(ctx, &instr->uses, cond);
    addOperandTail(ctx, &instr->uses, thenOp);
    addOperandTail(ctx, &instr->uses, elseOp);

    return instr;
}

static IrInstruction *newTableBranch(IrContext *ctx, IrOperand *cond, SwitchTable *table) {
    IrInstruction *instr = newInstruction(ctx, IR_TBRANCH);

    addOperandTail(ctx, &instr->uses, cond);
    instr->meta.switchTable = table;

    return instr;
}

static IrBasicBlock *updateBlock(IrContext *ctx) {
    IrBasicBlock *newBlock = newBasicBlock(ctx, NULL);
    ctx->currentBB = newBlock;
    return newBlock;
}


static void addSuccessor(IrContext *ctx, IrBasicBlock *block, IrBasicBlock *succ) {
    addBBTail(ctx, &block->succs, succ);
    addBBTail(ctx, &succ->preds, block);
}

static void addPredecessor(IrContext *ctx, IrBasicBlock *block, IrBasicBlock *pred) {
    addBBTail(ctx, &block->preds, pred);
    addBBTail(ctx, &pred->succs, block);
}

static void addInstruction(IrContext *ctx, IrInstruction *instr) {
    IrBasicBlock *bb = ctx->currentBB;
    if (bb != NULL) {
        assert(bb->term == NULL && "Adding instruction into terminated block");
    } else {
        bb = updateBlock(ctx);
    }

    addInstuctionTail(ctx, &bb->instrs, instr);
}

static void termintateBlock(IrContext* ctx, IrInstruction *instr) {
    // assert(instr->isTerminator())
    addInstruction(ctx, instr);
    ctx->currentBB->term = instr;
    ctx->currentBB = NULL;
}

static void gotoToBlock(IrContext* ctx, IrBasicBlock *gotoBB) {
    IrInstruction *gotoInstr = newGotoInstruction(ctx, gotoBB);
    addSuccessor(ctx, ctx->currentBB, gotoBB);
    termintateBlock(ctx, gotoInstr);
}

static IrOperand *lastDefinedOperand(IrContext *ctx) {
//    IrInstructionListNode *lastInstrNode = ctx->currentBB->instrs.tail;
//    assert(lastInstrNode != NULL);
//    IrInstruction *instr = lastInstrNode->instr;
//    IrOperandListNode *lastOperNode = instr->defs.tail;
//    assert(lastOperNode != NULL);
//    return lastOperNode->op;
    return ctx->lastOp;
}

typedef struct _ConstantCacheData {
    ConstKind kind;
    union {
        int64_const_t i;
        float80_const_t f;
        struct {
          literal_const_t s;
          size_t length;
        } l;
    } data;
    IrOperand *op;
} ConstantCacheData;

static IrOperand *getOrAddConstant(IrContext *ctx, ConstantCacheData *data, enum IrTypeKind type) {
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
    IrOperand *constOp = newIrOperand(ctx, type, IR_CONST);
    ConstantCacheData *newValue = areanAllocate(ctx->irArena, sizeof(ConstantCacheData));
    memcpy(newValue, data, sizeof(ConstantCacheData));
    newValue->op = constOp;
    constOp->data.literalIndex = ctx->constantCache->size;
    // raise(SIGTRAP);
    addToVector(ctx->constantCache, (intptr_t)newValue);
    ConstantCacheData *fromCache = ((ConstantCacheData **)ctx->constantCache->storage)[constOp->data.literalIndex];
    printf("fromCache %p, newValue %p, cache size = %lu, capacity = %lu\n", fromCache, newValue, ctx->constantCache->size, ctx->constantCache->capacity);
    assert(fromCache == newValue);
    assert(fromCache->op == constOp);
    return constOp;
}

static const ConstantCacheData *getCachedConstant(IrContext *ctx, uint32_t idx) {
    assert(idx < ctx->constantCache->size);
    return (const ConstantCacheData *)ctx->constantCache->storage[idx];
}

static IrOperand *createIntegerConstant(IrContext *ctx, enum IrTypeKind type, int64_const_t v) {
    ConstantCacheData d;
    d.kind = CK_INT_CONST;
    d.data.i = v;
    return getOrAddConstant(ctx, &d, type);
}

static IrOperand *createFloatConstant(IrContext *ctx, enum IrTypeKind type, float80_const_t v) {
    ConstantCacheData d;
    d.kind = CK_FLOAT_CONST;
    d.data.f = v;
    return getOrAddConstant(ctx, &d, type);
}

static IrOperand *computeEffectiveAddress(IrContext *ctx, IrOperand *op, const AstExpression *ast) {
    if (op->kind == IR_MEMORY) {

        IrInstruction *instr = newInstruction(ctx, IR_E_ADD);

        IrOperand *result = newVreg(ctx, op->data.address.base->type);

        addInstructionUse(ctx, instr, op->data.address.base);
        addInstructionUse(ctx, instr, op->data.address.offset);
        addInstructionDef(ctx, instr, result);

        result->astType = ast->type;
        result->ast.e = ast;

        addInstruction(ctx, instr);
	    return result;
	}

	return op;
}

static IrOperand *encodeBitField(IrContext *ctx, TypeRef *type, IrOperand *storageOp, IrOperand *valueOp) {
    assert(type->kind == TR_BITFIELD);
    uint64_t w = type->bitFieldDesc.width;
    uint64_t s = type->bitFieldDesc.offset;

	uint64_t m1 = (1ULL << (w + s)) - 1ULL;
	uint64_t m2 = ~((1ULL << s) - 1ULL);
	uint64_t mask = m1 & m2;

	TypeRef *memoryType = type->bitFieldDesc.storageType;

    size_t storageSize = computeTypeSize(memoryType);
    size_t W = storageSize * 8;
    int32_t l = W - (w + s);

	enum IrTypeKind irMemoryType = typeRefToIrType(memoryType);

	IrInstruction *storageMask = newInstruction(ctx, IR_E_AND);
	IrOperand *mask1Op = createIntegerConstant(ctx, irMemoryType, ~mask);
	IrOperand *maskedStorageOp = newVreg(ctx, irMemoryType);
	addInstructionUse(ctx, storageMask, storageOp);
	addInstructionUse(ctx, storageMask, mask1Op);
	addInstructionDef(ctx, storageMask, maskedStorageOp);
	addInstruction(ctx, storageMask);

	IrInstruction *shiftValueInstr = newInstruction(ctx, IR_E_LHS);
	IrOperand *shiftOp = createIntegerConstant(ctx, irMemoryType, s);
	IrOperand *shiftedValueOp = newVreg(ctx, IR_I32);
	addInstructionUse(ctx, shiftValueInstr, valueOp);
	addInstructionUse(ctx, shiftValueInstr, shiftOp);
	addInstructionDef(ctx, shiftValueInstr, shiftedValueOp);
	addInstruction(ctx, shiftValueInstr);

	IrInstruction *maskValueInstr = newInstruction(ctx, IR_E_AND);
	IrOperand *mask2Op = createIntegerConstant(ctx, irMemoryType, mask);
	IrOperand *maskedValueOp = newVreg(ctx, irMemoryType);
	addInstructionUse(ctx, maskValueInstr, shiftedValueOp);
	addInstructionUse(ctx, maskValueInstr, mask2Op);
	addInstructionDef(ctx, maskValueInstr, maskedValueOp);
	addInstruction(ctx, maskValueInstr);

	IrInstruction *mergeInstr = newInstruction(ctx, IR_E_OR);
	IrOperand *resultOp = newVreg(ctx, irMemoryType);
	addInstructionUse(ctx, mergeInstr, maskedStorageOp);
	addInstructionUse(ctx, mergeInstr, maskedValueOp);
	addInstructionDef(ctx, mergeInstr, resultOp);
	addInstruction(ctx, mergeInstr);
    // TODO: sign extend

    return resultOp;
}

static IrOperand *decodeBitField(IrContext *ctx, TypeRef *type, IrOperand *storageOp) {
    assert(type->kind == TR_BITFIELD);
    uint64_t w = type->bitFieldDesc.width;
    uint64_t mask = ~(~0LLu << w);
    uint64_t s = type->bitFieldDesc.offset;

	TypeRef *memoryType = type->bitFieldDesc.storageType;

    size_t storageSize = computeTypeSize(memoryType);
    size_t W = storageSize * 8;
    int32_t l = W - (w + s);

	enum IrTypeKind irMemoryType = typeRefToIrType(memoryType);

    IrOperand *loadedValue = storageOp;
    IrOperand *shlSizeOp = createIntegerConstant(ctx, IR_I32, l);
    IrOperand *shlOp = newVreg(ctx, irMemoryType);
    shlOp->astType = memoryType;

    IrInstruction *shlInstr = newInstruction(ctx, IR_E_LHS);
    addInstructionUse(ctx, shlInstr, loadedValue);
    addInstructionUse(ctx, shlInstr, shlSizeOp);
    addInstructionDef(ctx, shlInstr, shlOp);
    addInstruction(ctx, shlInstr);

    int32_t r = W - w;
    IrOperand *shrSizeOp = createIntegerConstant(ctx, IR_I32, r);
    IrOperand *shrOp = newVreg(ctx, irMemoryType);
    shrOp->astType = memoryType;

    IrInstruction *shrInstr = newInstruction(ctx, IR_E_LHS);
    addInstructionUse(ctx, shrInstr, shlOp);
    addInstructionUse(ctx, shrInstr, shrSizeOp);
    addInstructionDef(ctx, shrInstr, shrOp);
    addInstruction(ctx, shrInstr);

    // TODO: sign extend

    return shrOp;
}

IrOperand *createAllocaSlot(IrContext* ctx, size_t slotSize) {
  	slotSize = alignSize(slotSize, sizeof(intptr_t));
    IrInstruction *allocaInstr = newInstruction(ctx, IR_ALLOCA);
	IrOperand *sizeOp = createIntegerConstant(ctx, IR_U64, slotSize);
	IrOperand *resultOp = newVreg(ctx, IR_PTR);

    addInstructionUse(ctx, allocaInstr, sizeOp);
    addInstructionDef(ctx, allocaInstr, resultOp);
    addInstruction(ctx, allocaInstr);

	allocaInstr->info.stackSize = slotSize;

	return resultOp;
}

static IrOperand *addLoadInstr(IrContext *ctx, enum IrTypeKind valueType, IrOperand *base, IrOperand *offset, const AstExpression *ast) {
    IrOperand *loadee = newVreg(ctx, valueType);
    IrInstruction *loadInstr = newInstruction(ctx, IR_M_LOAD);
    addInstructionUse(ctx, loadInstr, base);
    addInstructionUse(ctx, loadInstr, offset);
    addInstructionDef(ctx, loadInstr, loadee);
    addInstruction(ctx, loadInstr);
    loadInstr->meta.astExpr = loadee->ast.e = ast;
    loadee->astType = ast->type;

	return loadee;
}

static void addStoreInstr(IrContext *ctx, IrOperand *base, IrOperand *offset,IrOperand *value, const AstExpression *ast) {
    IrInstruction *storeInstr = newInstruction(ctx, IR_M_STORE);
    addInstructionUse(ctx, storeInstr, base);
    addInstructionUse(ctx, storeInstr, offset);
    addInstructionUse(ctx, storeInstr, value);
    storeInstr->meta.astExpr = ast;
    addInstruction(ctx, storeInstr);
}

static enum IrTypeKind sizeToMemoryType(int32_t size) {
  switch (size) {
  case 1: return IR_U8;
  case 2: return IR_U16;
  case 4: return IR_U32;
  case 8: return IR_U64;
  default: unreachable("Unexpected type size");
  }

  return -1;
}

static void generateCompositeCopy(IrContext *ctx, const TypeRef *type, IrOperand *src, IrOperand *dst, const AstExpression *ast) {
	assert(isCompositeType(type));

  int32_t align = type->descriptorDesc->typeDefinition->align;
  int32_t size = computeTypeSize(type);
  int32_t copied = 0;

  IrOperand *srcOp = computeEffectiveAddress(ctx, src, ast);
  IrOperand *dstOp = computeEffectiveAddress(ctx, dst, ast);

  while (copied < size) {
      int32_t chunkSize;
      int32_t left = size - copied;

      if (left >= 8) chunkSize = sizeof(int64_t);
      else if (left >= 4) chunkSize = sizeof(int32_t);
      else if (left >= 2) chunkSize = sizeof(int16_t);
      else chunkSize = sizeof(int8_t);

      chunkSize = min(align, chunkSize);
	  enum IrTypeKind memType = sizeToMemoryType(chunkSize);

	  IrOperand *offsetOp = createIntegerConstant(ctx, IR_I64, copied);

	  IrOperand *word = addLoadInstr(ctx, memType, srcOp, offsetOp, ast);
	  addStoreInstr(ctx, dstOp, offsetOp, word, ast);

      copied += chunkSize;
  }
}

// -============================ translators ============================-

static Boolean isNullConst(AstExpression *expr) {
  if (expr->op != E_CONST) return FALSE;
  return expr->constExpr.i == 0;
}

static size_t translateInitializerIntoMemory(IrContext *ctx, IrOperand *base, int32_t offset, size_t typeSize, const AstInitializer *initializer) {

  assert(ctx->addressTM == IR_TM_RVALUE);
 
  switch (initializer->kind) {
  case IK_EXPRESSION: {
	AstExpression *expr = initializer->expression;
	const size_t expeSize = computeTypeSize(expr->type);
	const TypeRef *slotType = initializer->slotType;
	const size_t slotSize = computeTypeSize(slotType);
	const int32_t slotOffset = initializer->offset;
	const int32_t emitOffset = offset + slotOffset;

	if (/*skipNull && */ isNullConst(expr)) {
	  return offset + slotSize;
	}

	if (expr->op == E_COMPOUND) {
	  const int32_t emittedOffset = translateInitializerIntoMemory(ctx, base, emitOffset, typeSize, expr->compound);
	  assert(emittedOffset == emitOffset + slotSize);
	  return emittedOffset;
	}

	IrOperand *valueOp = translateExpression(ctx, expr);
	if ((emitOffset + slotSize) <= typeSize) {
	  IrOperand *offsetOp = createIntegerConstant(ctx, IR_I64, emitOffset);

	  if (isCompositeType(slotType)) {
	  	IrOperand *dstOp = newVreg(ctx, IR_PTR);
		IrInstruction *addInstr = newInstruction(ctx, IR_E_ADD);
		addInstructionUse(ctx, addInstr, base);
		addInstructionUse(ctx, addInstr, offsetOp);
		addInstructionDef(ctx, addInstr, dstOp);
		addInstruction(ctx, addInstr);
        generateCompositeCopy(ctx, slotType, valueOp, dstOp, expr);	
	  } else if (slotType->kind == TR_BITFIELD) {
		// TODO
		unimplemented("BitField initializer");
	  } else {
		IrInstruction *storeInstr = newInstruction(ctx, IR_M_STORE);
		addInstructionUse(ctx, storeInstr, base);
		addInstructionUse(ctx, storeInstr, offsetOp);
		addInstructionUse(ctx, storeInstr, valueOp);
		addInstruction(ctx, storeInstr);
	  }
	}

	return emitOffset + slotSize;
  }
  case IK_LIST: {
	if (isUnionType(initializer->slotType) && initializer->state == IS_INIT) {
	  for (const AstInitializerList *inits = initializer->initializerList;
		  inits != NULL;
		  inits = inits->next) {
		AstInitializer *init = inits->initializer;
		if (init->state == IS_INIT) {
		  return translateInitializerIntoMemory(ctx, base, offset, typeSize, init);
		}
	  }
	} else {
  	  size_t emmited = 0;
	  for (const AstInitializerList *inits = initializer->initializerList;
		  inits != NULL;
		  inits = inits->next) {
		emmited = translateInitializerIntoMemory(ctx, base, offset, typeSize, inits->initializer);
	  }
	  return emmited;
	}
  }
  default: unreachable("Unknown initializer kind");
  }

  return 0;
}

// -============================ expressions ============================-

static IrOperand *translateConstant(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_CONST);
    // Think about representation
    enum IrTypeKind constType = expr->constExpr.op == CK_STRING_LITERAL
            ? IR_LITERAL
            : typeRefToIrType(expr->type);

    ConstantCacheData data;
    data.kind = expr->constExpr.op;
    switch (expr->constExpr.op) {
    case CK_INT_CONST:
        data.data.i = expr->constExpr.i;
        break;
    case CK_FLOAT_CONST:
        data.data.f = expr->constExpr.f;
        break;
    case CK_STRING_LITERAL:
        data.data.l.length = expr->constExpr.l.length;
        data.data.l.s = expr->constExpr.l.s;
        break;
    }

    return getOrAddConstant(ctx, &data, constType);
}

static IrOperand *translateVaArg(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_VA_ARG);
    unimplemented("Constant Expression");
    return NULL;
}

static IrOperand *translateNameRef(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_NAMEREF);

    // If this is called we probably want a function reference.
    // Value references go through DEREF node

    IrOperand *op = NULL;
    Symbol *s = expr->nameRefExpr.s;
    if (s->kind == FunctionSymbol || s->kind == ValueSymbol && !s->variableDesc->flags.bits.isLocal) {
        // Either Function of non-local variable reference
        op = newIrOperand(ctx, IR_PTR, IR_REFERENCE);
        op->ast.e = expr;
        op->astType = expr->type;
        op->data.symbol = expr->nameRefExpr.s;
    } else {
        assert(s->kind == ValueSymbol);
        AstValueDeclaration *v = s->variableDesc;
        assert(v->flags.bits.isLocal);
        LocalValueInfo *info = &ctx->localOperandMap[v->index2];
        if (info->flags.referenced || isCompositeType(info->declaration->type)) {
            IrOperand *base = ctx->frameOp;
            IrOperand *offset = createIntegerConstant(ctx, IR_I64, info->frameOffset);
            op = newIrOperand(ctx, IR_PTR, IR_MEMORY);
            op->data.address.base = base;
            op->data.address.offset = offset;
            op->astType = expr->type;
        } else {
            return info->initialOp;
        }
    }

    assert(op != NULL);

    op->ast.e = expr;

    return op;
}

static IrOperand *translateCompound(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_COMPOUND);

    size_t typeSize = computeTypeSize(expr->type);
	IrOperand *memoryOp = createAllocaSlot(ctx, typeSize);

	translateInitializerIntoMemory(ctx, memoryOp, 0, typeSize, expr->compound);

	return memoryOp;
}

static IrOperand *translateCall(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_CALL);

	// TODO: support alloca(..) function

    AstExpression *callee = expr->callExpr.callee;
    TypeRef *type = expr->type;
    TypeRef *pcalleeType = callee->type;
    TypeRef *calleeType = pcalleeType->kind == TR_POINTED ? pcalleeType->pointed : pcalleeType;
    assert(calleeType->kind == TR_FUNCTION);
    TypeRef *returnType = calleeType->functionTypeDesc.returnType;

	unsigned returnTypeSize = computeTypeSize(returnType);

	const enum IrTranslationMode tm = ctx->addressTM;
	ctx->addressTM = IR_TM_RVALUE;

	IrOperand *calleeOp = translateExpression(ctx, callee);
	IrInstruction *callInstr = newInstruction(ctx, IR_CALL);
	addInstructionUse(ctx, callInstr, calleeOp);

	IrOperand *returnSlotOp = NULL;

	// TODO: save and restore stack after series of alloca's

	if (isCompositeType(returnType) && returnTypeSize > sizeof (intptr_t)) {
	  returnSlotOp = createAllocaSlot(ctx, returnTypeSize);
	  addInstructionUse(ctx, callInstr, returnSlotOp);
  	}

	for (AstExpressionList *args = expr->callExpr.arguments;
		 args != NULL;
		 args = args->next) {
	  AstExpression *argExpr = args->expression;
	  TypeRef *argType = argExpr->type;
      unsigned alignent = max(8, typeAlignment(argType));
      unsigned argSize = max(8, computeTypeSize(argType));

	  IrOperand *argOp = translateExpression(ctx, argExpr);
	  IrOperand *realArgOp = NULL;

	  if (isCompositeType(argType)) {
		if (argSize > sizeof (intptr_t)) {
		  // TODO: make sure this should be in argument list
		  realArgOp = createAllocaSlot(ctx, argSize);
		  generateCompositeCopy(ctx, argType, argOp, realArgOp, expr);
		} else {
		  assert(argOp->kind == IR_REFERENCE);
		  IrOperand *offset = createIntegerConstant(ctx, IR_I64, 0);
		  realArgOp = addLoadInstr(ctx, IR_P_AGG, argOp, offset, expr);
		}
	  } else {
		realArgOp = argOp;
	  }

	  addInstructionUse(ctx, callInstr, realArgOp);
	}

	IrOperand *returnValueOp = NULL;
	if (!isVoidType(returnType)) {
		if (returnSlotOp != NULL) {
		  // TODO: what should be done here?
		  returnValueOp = returnSlotOp;
		} else {
		  enum IrTypeKind irRetType = typeRefToIrType(returnType);
		  returnValueOp = newVreg(ctx, irRetType);
		  addInstructionDef(ctx, callInstr, returnValueOp);
		}
	}

	addInstruction(ctx, callInstr);
	callInstr->meta.astExpr = expr;

	ctx->addressTM = tm;
    return returnValueOp;
}

static IrOperand *translateTernary(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_TERNARY);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;

    IrOperand *condOp = translateExpression(ctx, expr->ternaryExpr.condition);

    IrBasicBlock *ifTrue = newBasicBlock(ctx, "<ifTrue>");
    IrBasicBlock *ifFalse = newBasicBlock(ctx, "<ifFalse>");
    IrBasicBlock *exit = newBasicBlock(ctx, "<ternary_exit>");

    IrInstruction *cond = newCondBranch(ctx, condOp, ifTrue, ifFalse);
    addSuccessor(ctx, ctx->currentBB, ifTrue);
    addSuccessor(ctx, ctx->currentBB, ifFalse);
    termintateBlock(ctx, cond);

    ctx->currentBB = ifTrue;
    IrOperand *ifTrueOp = translateExpression(ctx, expr->ternaryExpr.ifTrue);
    gotoToBlock(ctx, exit);

    ctx->currentBB = ifFalse;
    IrOperand *ifFalseOp = translateExpression(ctx, expr->ternaryExpr.ifFalse);
    gotoToBlock(ctx, exit);

    ctx->currentBB = exit;
    assert(ifTrueOp->type == ifFalseOp->type);
    IrOperand *result = newVreg(ctx, ifTrueOp->type);
    // TODO: what if type is composite?
    IrInstruction *phi = newInstruction(ctx, IR_PHI);
    addOperandTail(ctx, &phi->defs, result);
    addOperandTail(ctx, &phi->uses, ifTrueOp);
    addOperandTail(ctx, &phi->uses, ifFalseOp);

    ctx->addressTM = tm;

    return result;
}

static IrOperand *translateBitExtend(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_BIT_EXTEND);
    unimplemented("Bit Extend Expression");
    return NULL;
}

static IrOperand *translateCast(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_CAST);
    TypeRef *fromType = expr->castExpr.argument->type;
    TypeRef *toType = expr->castExpr.type;

	enum IrTypeKind irFromType = typeRefToIrType(fromType);
	enum IrTypeKind irToType = typeRefToIrType(toType);

	IrOperand *src = translateExpression(ctx, expr->castExpr.argument);
	IrOperand *dst = newVreg(ctx, irToType);

	IrInstruction *castInstr = newInstruction(ctx, IR_E_BITCAST);

	addInstructionUse(ctx, castInstr, src);
	addInstructionDef(ctx, castInstr, dst);
	addInstruction(ctx, castInstr);

	castInstr->info.fromCastType = irFromType;
	src->ast.e = castInstr->meta.astExpr = expr;

    return dst;
}

static IrOperand *translateLogicalExpression(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EB_ANDAND || expr->op == EB_OROR);

    Boolean isAndAnd = expr->op == EB_ANDAND;
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;

    IrOperand *leftOp = translateExpression(ctx, expr->binaryExpr.left);
    IrOperand *result = newVreg(ctx, IR_I32);

    IrBasicBlock *scnd = newBasicBlock(ctx, isAndAnd ? "<&&>" : "<||>");
    IrBasicBlock *exit = newBasicBlock(ctx, isAndAnd ? "<&&-exit>" : "<||-exit>");

    IrInstruction *cond = newCondBranch(ctx, leftOp, isAndAnd ? scnd : exit, isAndAnd ? exit : scnd);
    addSuccessor(ctx, ctx->currentBB, scnd);
    addSuccessor(ctx, ctx->currentBB, exit);
    termintateBlock(ctx, cond);

    ctx->currentBB = scnd;
    IrOperand *rightOp = translateExpression(ctx, expr->binaryExpr.right);
    gotoToBlock(ctx, exit);

    ctx->currentBB = exit;
    IrInstruction *phi = newInstruction(ctx, IR_PHI);
    addOperandTail(ctx, &phi->defs, result);
    addOperandTail(ctx, &phi->uses, leftOp);
    addOperandTail(ctx, &phi->uses, rightOp);

    addInstruction(ctx, phi);

    phi->meta.astExpr = expr;

    ctx->addressTM = tm;

    return result;
}

static enum IrIntructionKind getBinaryArith(ExpressionType op, Boolean isFloatOperand) {
    // TODO: fix address arith

    enum IrIntructionKind k = IR_BAD;

    switch (op) {
    case EB_ADD: k = isFloatOperand ? IR_E_FADD : IR_E_ADD; break;
    case EB_SUB: k = isFloatOperand ? IR_E_FSUB : IR_E_SUB; break;
    case EB_MUL: k = isFloatOperand ? IR_E_FMUL : IR_E_MUL; break;
    case EB_DIV: k = isFloatOperand ? IR_E_FDIV : IR_E_DIV; break;
    case EB_MOD: k = isFloatOperand ? IR_E_FMOD : IR_E_MOD; break;
    case EB_LHS: assert(!isFloatOperand); k = IR_E_LHS; break;
    case EB_RHS: assert(!isFloatOperand); k = IR_E_RHS; break;
    case EB_AND: assert(!isFloatOperand); k = IR_E_AND; break;
    case EB_OR:  assert(!isFloatOperand); k = IR_E_OR; break;
    case EB_XOR: assert(!isFloatOperand); k = IR_E_XOR; break;
    case EB_EQ:  k = isFloatOperand ? IR_E_FEQ : IR_E_EQ; break;
    case EB_NE:  k = isFloatOperand ? IR_E_FNE : IR_E_NE; break;
    case EB_LT:  k = isFloatOperand ? IR_E_FLT : IR_E_LT; break;
    case EB_GT:  k = isFloatOperand ? IR_E_FGT : IR_E_GT; break;
    case EB_LE:  k = isFloatOperand ? IR_E_FLE : IR_E_LE; break;
    case EB_GE:  k = isFloatOperand ? IR_E_FGE : IR_E_GE; break;
    default: unreachable("wtf");
    }

    return k;
}

static ExpressionType assignArithToArith(ExpressionType op) {
    switch (op) {
    case EB_ASG_ADD: return EB_ADD;
    case EB_ASG_SUB: return EB_SUB;
    case EB_ASG_MUL: return EB_MUL;
    case EB_ASG_DIV: return EB_DIV;
    case EB_ASG_MOD: return EB_MOD;
    case EB_ASG_SHL: return EB_LHS;
    case EB_ASG_SHR: return EB_RHS;
    case EB_ASG_AND: return EB_AND;
    case EB_ASG_XOR: return EB_XOR;
    case EB_ASG_OR:  return EB_OR;
    default: unreachable("WTF?");
    }

    return E_NUM_OF_OPS;
}

static IrOperand *translateBinary(IrContext *ctx, AstExpression *expr) {
    assert(isBinary(expr->op));

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *leftOp = translateExpression(ctx, expr->binaryExpr.left);
    IrOperand *rightOp = translateExpression(ctx, expr->binaryExpr.right);
    ctx->addressTM = tm;

    enum IrTypeKind type = typeRefToIrType(expr->type);
    Boolean isFloatOperand = isRealType(expr->binaryExpr.left->type);
    enum IrIntructionKind k = getBinaryArith(expr->op, isFloatOperand);

    assert(k != IR_BAD);

    assert(leftOp != NULL);
    assert(rightOp != NULL);
    // NOTE: pointer arithmethic is desugared during parser phase
    IrOperand *result = newVreg(ctx, type);
    IrInstruction *instr = newInstruction(ctx, k);
    addOperandTail(ctx, &instr->uses, leftOp);
    addOperandTail(ctx, &instr->uses, rightOp);
    addOperandTail(ctx, &instr->defs, result);

    addInstruction(ctx, instr);

    instr->meta.astExpr = expr;

    return result;
}

static IrOperand *translateAssignment(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EB_ASSIGN);

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *ref = translateExpression(ctx, assignee);
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *valueOp = translateExpression(ctx, value);
    ctx->addressTM = tm;

    // TODO: heavy_copy_1 = heavy_copy_2 = heavy_struct

    if (isCompositeType(value->type)) {
	  generateCompositeCopy(ctx, value->type, valueOp, ref, expr);
    } else if (ref->kind == IR_MEMORY) {
		if (value->type->kind == TR_BITFIELD) {
		  enum IrTypeKind irMemType = typeRefToIrType(value->type->bitFieldDesc.storageType);
		  IrOperand *storageOp = addLoadInstr(ctx, irMemType, ref->data.address.base, ref->data.address.offset, expr);
		  valueOp = encodeBitField(ctx, value->type, storageOp, valueOp);
		}
	  	addStoreInstr(ctx, ref->data.address.base, ref->data.address.offset, valueOp, expr);
    } else if (ref->type == IR_PTR)  {
      // probably it is something like *assigne = where assignee is some scalar
        addStoreInstr(ctx, ref, createIntegerConstant(ctx, IR_I64, 0), valueOp, expr);
    } else {
        assert(ref->kind == IR_LOCAL);

        IrInstruction *moveInstr = newMoveInstruction(ctx, valueOp, ref);
        moveInstr->meta.astExpr = expr;
        addInstruction(ctx, moveInstr);
    }

    return valueOp;
}

static IrOperand *translateAssignArith(IrContext *ctx, AstExpression *expr) {
    assert(isAssignmentArith(expr->op));

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;
    assert(!isCompositeType(value->type) && "Forbiden operation in C");

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *ref = translateExpression(ctx, assignee);
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *valueOp = translateExpression(ctx, value);
    ctx->addressTM = tm;

    Boolean isFloat = isRealType(value->type);
    ExpressionType binaryArith = assignArithToArith(expr->op);
    enum IrIntructionKind ik = getBinaryArith(binaryArith, isFloat);

    IrOperand *lhs = NULL;

    enum IrTypeKind valueType = typeRefToIrType(expr->type);

    IrOperand *base = NULL, *offset = NULL;
    IrOperand *resultOp = NULL;
	IrOperand *storageOp = NULL;

    if (ref->kind == IR_MEMORY || ref->kind == IR_REFERENCE) {
        if (ref->kind == IR_MEMORY) {
            base = ref->data.address.base;
            offset = ref->data.address.offset;
        } else {
            base = ref;
            offset = createIntegerConstant(ctx, IR_I64, 0);
        }

        resultOp = newVreg(ctx, valueType);
        resultOp->astType = expr->type;
        resultOp->ast.e = expr;
        lhs = addLoadInstr(ctx, valueType, base, offset, expr);
		if (assignee->type->kind == TR_BITFIELD) {
		  storageOp = lhs;
		  lhs = decodeBitField(ctx, assignee->type, storageOp);
		}
    } else {
        assert(ref->kind == IR_LOCAL);
        resultOp = lhs = ref;
    }

    assert(resultOp != NULL);
    assert(lhs != NULL);

    // NOTE: Pointer arithmethic is desugared during parser phase
    IrInstruction *operation = newInstruction(ctx, ik);
    addInstructionUse(ctx, operation, lhs);
    addInstructionUse(ctx, operation, valueOp);
    addInstructionDef(ctx, operation, resultOp);
    addInstruction(ctx, operation);
    operation->meta.astExpr = expr;

    if (base != NULL) {
        assert(offset != NULL);

		if (assignee->type->kind == TR_BITFIELD) {
		  assert(storageOp != NULL);
		  resultOp = encodeBitField(ctx, assignee->type, storageOp, resultOp);
		}
        addStoreInstr(ctx, base, offset, resultOp, expr);
    }

    return resultOp;
}

static IrOperand *translateReference(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EU_REF);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *arg = translateExpression(ctx, expr->unaryExpr.argument);
    ctx->addressTM = tm;

    if (arg->kind == IR_MEMORY) {
        return computeEffectiveAddress(ctx, arg, expr);
    }

    return arg;
}

static IrOperand *translateDeReference(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EU_DEREF);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *arg = translateExpression(ctx, expr->unaryExpr.argument);
    ctx->addressTM = tm;

    // Check if need to do actual memory load or caller expects address operand
    if (tm == IR_TM_RVALUE) {
        if (arg->kind == IR_MEMORY) {
            // do we need load?
            IrInstruction *loadInstr = newInstruction(ctx, IR_M_LOAD);
            enum IrTypeKind type = typeRefToIrType(expr->type);
            IrOperand *result = newVreg(ctx, type);
            result->ast.e = loadInstr->meta.astExpr = expr;
            result->astType = expr->type;

            addInstructionUse(ctx, loadInstr, arg->data.address.base);
            addInstructionUse(ctx, loadInstr, arg->data.address.offset);
            addInstructionDef(ctx, loadInstr, result);
            addInstruction(ctx, loadInstr);

            return result;
        } else if (arg->kind == IR_REFERENCE || arg->kind == IR_VREG) {
            // Check if we do not dereference function reference due to is it non-sense
            if (arg->kind == IR_VREG || arg->data.symbol->kind != FunctionSymbol) {
                IrInstruction *loadInstr = newInstruction(ctx, IR_M_LOAD);
                enum IrTypeKind type = typeRefToIrType(expr->type);
                IrOperand *result = newVreg(ctx, type);
                result->ast.e = loadInstr->meta.astExpr = expr;
                result->astType = expr->type;

                IrOperand *zeroOffset = createIntegerConstant(ctx, IR_I64, 0);

                addInstructionUse(ctx, loadInstr, arg);
                addInstructionUse(ctx, loadInstr, zeroOffset);
                addInstructionDef(ctx, loadInstr, result);
                addInstruction(ctx, loadInstr);

                loadInstr->meta.astExpr = expr;

                return result;
            }
        } else {
            assert(arg->kind == IR_LOCAL);
        }
    }

    return arg;
}

static IrOperand *translateUnary(IrContext *ctx, AstExpression *expr) {
    assert(isUnary(expr->op));

    enum IrTypeKind type = typeRefToIrType(expr->type);
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *arg = translateExpression(ctx, expr->unaryExpr.argument);
    ctx->addressTM = tm;

    Boolean isFloat = isRealType(expr->type);

    Boolean exl = FALSE;

    IrOperand *result = NULL;

    switch (expr->op) {
    case EU_PLUS: result = arg; break;
    case EU_MINUS: {
        IrOperand *zeroConst;
        ConstantCacheData data;
        enum IrIntructionKind op;
        if (isFloat) {
            data.kind = CK_FLOAT_CONST;
            data.data.f = 0.0;
            op = IR_E_FSUB;
        } else {
            data.kind = CK_INT_CONST;
            data.data.i = 0;
            op = IR_E_SUB;
        }
        zeroConst = getOrAddConstant(ctx, &data, type);
        IrInstruction *instr = newInstruction(ctx, op);
        result = newVreg(ctx, type);
        addInstructionUse(ctx, instr, zeroConst);
        addInstructionUse(ctx, instr, arg);
        addInstructionDef(ctx, instr, result);
        addInstruction(ctx, instr);
        break;
    }
    case EU_EXL: exl = TRUE;
    case EU_TILDA: {
        assert(!isFloat);
        result = newVreg(ctx, type);
        enum IrIntructionKind op = exl ? IR_U_NOT : IR_U_BNOT;
        IrInstruction *instr = newInstruction(ctx, op);
        addInstructionUse(ctx, instr, arg);
        addInstructionDef(ctx, instr, result);
        addInstruction(ctx, instr);
        break;
    }
    default: unreachable("wtf?");
    }

    assert(result != NULL);

    return result;
}

static IrOperand *translateAddressLikeExpression(IrContext *ctx, IrOperand *base, IrOperand *offset, AstExpression *ast, TypeRef *valueType) {
    IrOperand *result = NULL;

    TypeRef *memoryType = valueType->kind == TR_BITFIELD ? valueType->bitFieldDesc.storageType : valueType;

    if (ctx->addressTM == IR_TM_RVALUE) {
        if (isCompositeType(memoryType)) {
            assert(valueType == memoryType);
            result = newVreg(ctx, IR_PTR);
            result->astType = makePointedType(ctx->pctx, 0U, valueType);

            IrInstruction *addInstr = newInstruction(ctx, IR_E_ADD);

            addInstructionUse(ctx, addInstr, base);
            addInstructionUse(ctx, addInstr, offset);
            addInstructionDef(ctx, addInstr, result);
            addInstruction(ctx, addInstr);

            addInstr->meta.astExpr = ast;
        } else {
            enum IrTypeKind irMemoryType = typeRefToIrType(memoryType);

            result = newVreg(ctx, irMemoryType);
            result->astType = memoryType;

            IrInstruction *loadInstr = newInstruction(ctx, IR_M_LOAD);
            addInstructionUse(ctx, loadInstr, base);
            addInstructionUse(ctx, loadInstr, offset);
            addInstructionDef(ctx, loadInstr, result);
            addInstruction(ctx, loadInstr);

            loadInstr->meta.astExpr = ast;

            if (memoryType != valueType) {
			  result = decodeBitField(ctx, valueType, result);
            }
        }
    } else {
        assert(ctx->addressTM == IR_TM_LVALUE);

        result = newIrOperand(ctx, IR_PTR, IR_MEMORY);
        result->astType = makePointedType(ctx->pctx, 0U, valueType);
        result->data.address.base = base;
        result->data.address.offset = offset;
    }

    assert(result != NULL);

    result->ast.e = ast;

    return result;
}

static IrOperand *translateArrayAccess(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EB_A_ACC);

    AstExpression *left = expr->binaryExpr.left;
    AstExpression *right = expr->binaryExpr.right;
    AstExpression *base = isPointerLikeType(left->type) ? left : right;
    AstExpression *index = base == left ? right : left;

    TypeRef *arrayType = base->type;
    assert(isPointerLikeType(arrayType));
    TypeRef *pointerType = NULL;

    if (arrayType->kind == TR_ARRAY) {
        pointerType = makePointedType(ctx->pctx, arrayType->flags.storage, arrayType->arrayTypeDesc.elementType);
    } else if (arrayType->kind == TR_VLA) {
        pointerType = makePointedType(ctx->pctx, arrayType->flags.storage, arrayType->vlaDescriptor.elementType);
    } else {
        pointerType = arrayType;
    }

    TypeRef *elementType = pointerType->pointed;
    int32_t indexOrigSize = computeTypeSize(index->type);
    TypeRef *indexType = makePrimitiveType(ctx->pctx, isUnsignedType(index->type) ? T_U8 : T_S8, 0);

    Boolean isFlat = (base->type->kind == TR_ARRAY /*|| base->type->kind == TR_VLA*/) && base->op != E_CONST;

    const enum IrTypeKind indexIrType = typeRefToIrType(indexType);
    const enum IrTranslationMode oldTM = ctx->addressTM;

    ctx->addressTM = IR_TM_RVALUE;

    IrOperand *baseOp = translateExpression(ctx, base);
    IrOperand *indexOp = translateExpression(ctx, index);

    ctx->addressTM = oldTM;

    IrOperand *scaledIndexOp = NULL;

    if (elementType->kind == TR_VLA) {
        unimplemented("VLA array access");
    } else {
        int32_t elementSize = computeTypeSize(elementType);
        if (elementSize > 1) {
            scaledIndexOp = newVreg(ctx, indexIrType);
            if (isPowerOf2(elementSize)) {
                IrOperand *elementSizeOpScale = createIntegerConstant(ctx, IR_I32, log2Integer(elementSize));
                IrInstruction *shlInstr = newInstruction(ctx, IR_E_LHS);
                addInstructionUse(ctx, shlInstr, indexOp);
                addInstructionUse(ctx, shlInstr, elementSizeOpScale);
                addInstructionDef(ctx, shlInstr, scaledIndexOp);
                addInstruction(ctx, shlInstr);
                shlInstr->meta.astExpr = expr;
            } else {
                IrOperand *elementSizeOp = createIntegerConstant(ctx, indexIrType, elementSize);
                IrInstruction *mulInstr = newInstruction(ctx, IR_E_MUL);
                addInstructionUse(ctx, mulInstr, indexOp);
                addInstructionUse(ctx, mulInstr, elementSizeOp);
                addInstructionDef(ctx, mulInstr, scaledIndexOp);
                addInstruction(ctx, mulInstr);
                mulInstr->meta.astExpr = expr;
            }
        } else {
            assert(elementSize == 1);
            scaledIndexOp = indexOp;
        }

        if (scaledIndexOp->type != IR_I64) {
            IrOperand *castedScaledOp = newVreg(ctx, IR_I64);
            IrInstruction *castInstruction = newInstruction(ctx, IR_E_BITCAST);
            // TODO
            castInstruction->type = IR_I64;
            castInstruction->info.fromCastType = scaledIndexOp->type;
            castInstruction->meta.astExpr = expr;

            addInstructionUse(ctx, castInstruction, scaledIndexOp);
            addInstructionDef(ctx, castInstruction, castedScaledOp);
            addInstruction(ctx, castInstruction);
            scaledIndexOp = castedScaledOp;
        }
    }

    return translateAddressLikeExpression(ctx, baseOp, scaledIndexOp, expr, elementType);
}

static IrOperand *translateFieldAccess(IrContext *ctx, AstExpression *expr, Boolean isDot) {

//    expr->block->block.type->arrayTypeDesc

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *recevierOp = translateExpression(ctx, expr->fieldExpr.recevier);
    ctx->addressTM = tm;

    assert(recevierOp->type == IR_P_AGG || recevierOp->type == IR_PTR);

    int64_t memberOffset = effectiveMemberOffset(expr->fieldExpr.member);
    IrOperand *memberOffsetOp = createIntegerConstant(ctx, IR_I64, memberOffset);
    TypeRef *memberType = expr->fieldExpr.member->type;

    return translateAddressLikeExpression(ctx, recevierOp, memberOffsetOp, expr, memberType);
}

static IrOperand *translateDotAccess(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EF_DOT);
    assert(isCompositeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(ctx, expr, /*isDOT = */ TRUE);
}

static IrOperand *translateArrowAccess(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EF_ARROW);
    assert(isPointerLikeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(ctx, expr, /* isDOR = */ FALSE);
}

static IrOperand *translatePreOp(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EU_PRE_INC || expr->op == EU_PRE_DEC);
    unreachable("Pre ++/-- Expressions should be desugared"
                " into corresponding +=/*- operations in parser");
    return NULL;
}

static IrOperand *translatePostOp(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == EU_POST_INC || expr->op == EU_POST_DEC);
    // NOTE: Pointer arith is not desugared in parser.
    // TODO: Generalize it with generic binary opeartions

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *referenceOp = translateExpression(ctx, expr->unaryExpr.argument);
    ctx->addressTM = tm;

	TypeRef* type = expr->type;
  	int64_t delta = isPointerLikeType(type) ? computeTypeSize(type->pointed) : 1;
  	TypeId tid = typeToId(type);
	IrOperand *delta_op;

	enum IrTypeKind irType = typeRefToIrType(type);

	enum IrIntructionKind irInstr = IR_BAD;

	if (tid < T_F4) {
		delta_op = createIntegerConstant(ctx, irType, delta);
		irInstr = expr->op == EU_POST_DEC ? IR_E_SUB : IR_E_ADD;
	} else {
	  	float80_const_t fc = (float80_const_t)delta;
		delta_op = createFloatConstant(ctx, irType, fc);
		irInstr = expr->op == EU_POST_DEC ? IR_E_FSUB : IR_E_FADD;
	}

	IrOperand *oldValue = NULL, *newValue = NULL, *returnValue = NULL;
    IrOperand *base = NULL, *offset = NULL;
	IrOperand *storageOp = NULL;
	if (referenceOp->kind == IR_MEMORY || referenceOp->kind == IR_REFERENCE) {
        if (referenceOp->kind == IR_MEMORY) {
            base = referenceOp->data.address.base;
            offset = referenceOp->data.address.offset;
        } else {
            base = referenceOp;
            offset = createIntegerConstant(ctx, IR_I64, 0);
        }

		returnValue = oldValue = addLoadInstr(ctx, irType, base, offset, expr);
		newValue = newVreg(ctx, irType);
		if (type->kind == TR_BITFIELD) {
		  storageOp = oldValue;
		  returnValue = oldValue = decodeBitField(ctx, type, oldValue);
		}
	} else {
        assert(referenceOp->kind == IR_LOCAL);

		returnValue = newVreg(ctx, irType);
        newValue = oldValue = referenceOp;

        IrInstruction *moveInstr = newMoveInstruction(ctx, referenceOp, returnValue);
        addInstruction(ctx, moveInstr);
        moveInstr->meta.astExpr = returnValue->ast.e = expr;
        returnValue->astType = expr->type;
	}

	newValue->astType = expr->type;
	newValue->ast.e = expr;

    IrInstruction *operation = newInstruction(ctx, irInstr);
    addInstructionUse(ctx, operation, oldValue);
    addInstructionUse(ctx, operation, delta_op);
    addInstructionDef(ctx, operation, newValue);
    addInstruction(ctx, operation);
    operation->meta.astExpr = expr;

	if (base != NULL) {
	  	if (type->kind == TR_BITFIELD) {
		  assert(storageOp != NULL);
		  newValue = encodeBitField(ctx, type, storageOp, newValue);
		}

		addStoreInstr(ctx, base, offset, newValue, expr);
	}

    return returnValue;
}

static IrOperand *translateLabelRef(IrContext *ctx, AstExpression *expr) {
    assert(expr->op == E_LABEL_REF);

    IrBasicBlock *target = getOrCreateLabelBlock(ctx, expr->label);
    IrOperand *resultOp = newVreg(ctx, IR_PTR);
    IrOperand *targetOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);

    targetOp->data.bb = target;

    IrInstruction *instr = newInstruction(ctx, IR_BLOCK_PTR);
    addInstructionUse(ctx, instr, targetOp);
    addInstructionDef(ctx, instr, resultOp);
    addInstruction(ctx, instr);

    return resultOp;
}

static IrOperand *translateExpression(IrContext *ctx, AstExpression *expr) {
    switch (expr->op) {
      case E_PAREN:
        return ctx->lastOp = translateExpression(ctx, expr->parened);
      case E_BLOCK:
        translateStatement(ctx, expr->block);
        return lastDefinedOperand(ctx);
      case E_CONST:
        return ctx->lastOp = translateConstant(ctx, expr);
      case E_VA_ARG:
        return ctx->lastOp = translateVaArg(ctx, expr);
      case E_NAMEREF:
        return ctx->lastOp = translateNameRef(ctx, expr);
      case E_COMPOUND:
        return ctx->lastOp = translateCompound(ctx, expr);
      case E_CALL:
        return ctx->lastOp = translateCall(ctx, expr);
      case E_TERNARY:
        return ctx->lastOp = translateTernary(ctx, expr);
      case E_BIT_EXTEND:
        return ctx->lastOp = translateBitExtend(ctx, expr);
      case E_CAST:
        return ctx->lastOp = translateCast(ctx, expr);
      case EB_ANDAND:
      case EB_OROR:
        return ctx->lastOp = translateLogicalExpression(ctx, expr);
      case EB_ADD:
      case EB_SUB:
      case EB_LHS: /** << */
      case EB_RHS: /** >> */
      case EB_AND:
      case EB_OR:
      case EB_XOR:
      case EB_MUL:
      case EB_DIV:
      case EB_MOD:
      case EB_EQ:
      case EB_NE:
      case EB_LT:
      case EB_LE:
      case EB_GT:
      case EB_GE:
        return ctx->lastOp = translateBinary(ctx, expr);
      case EB_ASSIGN:
        return ctx->lastOp = translateAssignment(ctx, expr);
      case EB_ASG_MUL:
      case EB_ASG_ADD:
      case EB_ASG_SUB:
      case EB_ASG_SHL:
      case EB_ASG_SHR:
      case EB_ASG_AND:
      case EB_ASG_XOR:
      case EB_ASG_OR:
      case EB_ASG_DIV:
      case EB_ASG_MOD:
        return ctx->lastOp = translateAssignArith(ctx, expr);
      case EB_COMMA:
        translateExpression(ctx, expr->binaryExpr.left);
        return ctx->lastOp = translateExpression(ctx, expr->binaryExpr.right);
      case EU_REF:
        return ctx->lastOp = translateReference(ctx, expr);
      case EU_DEREF:
        return ctx->lastOp = translateDeReference(ctx, expr);
      case EU_PLUS:
      case EU_MINUS:
      case EU_TILDA:
      case EU_EXL:
        return ctx->lastOp = translateUnary(ctx, expr);
      case EB_A_ACC:
        return ctx->lastOp = translateArrayAccess(ctx, expr);
      case EF_DOT:
        return ctx->lastOp = translateDotAccess(ctx, expr);
      case EF_ARROW:
        return ctx->lastOp = translateArrowAccess(ctx, expr);
      case EU_PRE_DEC:
      case EU_PRE_INC:
        return ctx->lastOp = translatePreOp(ctx, expr);
      case EU_POST_DEC:
      case EU_POST_INC:
        return ctx->lastOp = translatePostOp(ctx, expr);
      case E_LABEL_REF:
        return ctx->lastOp = translateLabelRef(ctx, expr);

    default: unreachable("unexpcted expression op");
    }
    return NULL;
}

// -============================ statements =============================-

static Boolean translateStatement(IrContext *ctx, AstStatement *stmt) {
    switch (stmt->statementKind) {
    case SK_BLOCK: return translateBlock(ctx, stmt);
    case SK_DECLARATION: return translateDeclaration(ctx, stmt->declStmt.declaration);
    case SK_EMPTY: return FALSE;
    case SK_EXPR_STMT:
        translateExpression(ctx, stmt->exprStmt.expression);
        return FALSE;
    case SK_LABEL: return translateLabel(ctx, stmt);
    case SK_GOTO_L: return translateGotoLabel(ctx, stmt);
    case SK_GOTO_P: return translateGotoPtr(ctx, stmt);
    case SK_RETURN: return translateReturn(ctx, stmt);
    case SK_BREAK: return translateBreak(ctx, stmt);
    case SK_CONTINUE: return translateContinue(ctx, stmt);
    case SK_IF: return translateIf(ctx, stmt);
    case SK_SWITCH: return translateSwitch(ctx, stmt);
    case SK_WHILE: return translateWhile(ctx, stmt);
    case SK_DO_WHILE: return translateDoWhile(ctx, stmt);
    case SK_FOR: return translateFor(ctx, stmt);
    default:
        unreachable("Unknown statement kind");
        return TRUE;
    }
}

static Boolean translateBlock(IrContext *ctx, AstStatement *block) {

    IrBasicBlock *bb = ctx->currentBB;

    if (bb == NULL || bb->term != NULL) { // emit into existed block if it not terminated
        bb = updateBlock(ctx);
        bb->ast = block;
    }

    AstStatementList *stmt = block->block.stmts;
    Boolean terminated = FALSE;

    while (stmt != NULL) {
        terminated |= translateStatement(ctx, stmt->stmt);
        stmt = stmt->next;
    }

    return terminated;
}

static void translateGlobalVariable(IrContext *ctx, AstValueDeclaration *v) {
    assert(!v->flags.bits.isLocal && "Should be non-local storaged variable");

    unimplemented("Global variable");
}

static void translateLocalInitializer(IrContext *ctx, AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);

	LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
	assert(lvi != NULL);

    const AstInitializer *init = v->initializer;
    if (init == NULL) 
      return; // we are done here

    const size_t typeSize = computeTypeSize(v->type);

    Boolean hasMemorySlot = lvi->flags.referenced || isCompositeType(v->type);

    if (hasMemorySlot) {
      IrOperand *localAddrOp = newVreg(ctx, IR_PTR);
      localAddrOp->ast.v = v;
      localAddrOp->astType = v->type;
      IrOperand *frameOffsetOp = createIntegerConstant(ctx, IR_I64, lvi->frameOffset);

      IrInstruction *addInstr = newInstruction(ctx, IR_E_ADD);
      addInstructionUse(ctx, addInstr, ctx->frameOp);
      addInstructionUse(ctx, addInstr, frameOffsetOp);
      addInstructionDef(ctx, addInstr, localAddrOp);
      addInstruction(ctx, addInstr);

      translateInitializerIntoMemory(ctx, localAddrOp, 0, typeSize, init);
    } else {
      // Register-based local variable which means it's
      //  1. Has no explicit stack slot
      IrOperand *localOp = lvi->initialOp;
      assert(localOp != NULL);
      //  2. Has scalar type
      assert(!isCompositeType(v->type));
     
      if (init->kind == IK_EXPRESSION) {
        IrOperand *valueOp = translateExpression(ctx, init->expression);

        IrInstruction *moveInstr = newMoveInstruction(ctx, valueOp, localOp);
        addInstruction(ctx, moveInstr);
      } else {
        IrOperand *tmp = createAllocaSlot(ctx, typeSize);
        IrOperand *offsetOp = createIntegerConstant(ctx, IR_I64, 0); 
        translateInitializerIntoMemory(ctx, tmp, 0, typeSize, init);

        IrInstruction *loadInstr = newInstruction(ctx, IR_M_LOAD);
        addInstructionUse(ctx, loadInstr, tmp);
        addInstructionUse(ctx, loadInstr, offsetOp);
        addInstructionDef(ctx, loadInstr, localOp);
        addInstruction(ctx, loadInstr);
      }
    }
}

static void translateVLA(IrContext *ctx, AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);
    assert(v->type->kind == TR_VLA);

    unimplemented("VLA arrays");
}

static Boolean translateDeclaration(IrContext *ctx, AstDeclaration *decl) {

    if (decl->kind == DK_VAR) {
        AstValueDeclaration *varDecl = decl->variableDeclaration;
        if (varDecl->flags.bits.isLocal) {
            assert(varDecl->index2 >= 0);
            if (varDecl->type->kind == TR_VLA) {
                translateVLA(ctx, varDecl);
            } else {
                translateLocalInitializer(ctx, varDecl);
            }
        } else {
            //assert(varDecl->flags.bits.isStatic);
            translateGlobalVariable(ctx, varDecl);
        }
    }

    return FALSE;
}

static Boolean translateIf(IrContext *ctx, AstStatement *ifStmt) {
    assert(ifStmt->statementKind == SK_IF);

    AstExpression *condition = ifStmt->ifStmt.condition;
    AstStatement *thenStmt = ifStmt->ifStmt.thenBranch;
    AstStatement *elseStmt = ifStmt->ifStmt.elseBranch;

    IrBasicBlock *ifBB = ctx->currentBB;
    IrBasicBlock *continueBB = newBasicBlock(ctx, "<if_exit>");
    IrBasicBlock *thenBB = newBasicBlock(ctx, "<if_then>");
    IrBasicBlock *elseBB = elseStmt != NULL ? newBasicBlock(ctx, "<if_else>") : continueBB;

    IrOperand *irCond = translateExpression(ctx, condition);

    IrInstruction *condBranch = newCondBranch(ctx, irCond, thenBB, elseBB);
    addSuccessor(ctx, ifBB, thenBB);
    addSuccessor(ctx, ifBB, elseBB);
    termintateBlock(ctx, condBranch);

    ctx->currentBB = thenBB;
    translateStatement(ctx, thenStmt);
    if (ctx->currentBB != NULL) {
      IrInstruction *thenGoto = newGotoInstruction(ctx, continueBB);
      addSuccessor(ctx, ctx->currentBB, continueBB);
      termintateBlock(ctx, thenGoto);
    }
    thenBB->ast = thenStmt;

    if (elseStmt != NULL) {
        assert(elseBB != continueBB);
        ctx->currentBB = elseBB;
        translateStatement(ctx, elseStmt);
        if (ctx->currentBB != NULL) {
          IrInstruction *thenGoto = newGotoInstruction(ctx, continueBB);
          addSuccessor(ctx, ctx->currentBB, continueBB);
          termintateBlock(ctx, thenGoto);
        }
        elseBB->ast = elseStmt;
    }

    ctx->currentBB = continueBB;

    return FALSE;
}

static Boolean translateWhile(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstExpression *condition = stmt->loopStmt.condition;
    AstStatement *body = stmt->loopStmt.body;

    IrBasicBlock *loopHead = ctx->continueBB = newBasicBlock(ctx, "<while_head>");
    IrBasicBlock *loopBody = newBasicBlock(ctx, "<while_body>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock(ctx, "<while_exit>");

    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(ctx, loopHead);
    addSuccessor(ctx, ctx->currentBB, loopHead);
    termintateBlock(ctx, gotoHead);

    ctx->currentBB = loopHead;
    IrOperand *irCond = translateExpression(ctx, condition);

    IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
    addSuccessor(ctx, ctx->currentBB, loopBody);
    addSuccessor(ctx, ctx->currentBB, loopExit);
    termintateBlock(ctx, irCondBranch);

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoLoop = newGotoInstruction(ctx, loopHead);
        addSuccessor(ctx, ctx->currentBB, loopHead);
        termintateBlock(ctx, gotoLoop);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static Boolean translateDoWhile(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_DO_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstStatement *body = stmt->loopStmt.body;
    AstExpression *condition = stmt->loopStmt.condition;

    IrBasicBlock *loopBody = newBasicBlock(ctx, "<do_body>");
    IrBasicBlock *loopTail = ctx->continueBB = newBasicBlock(ctx, "<do_tail>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock(ctx, "<do_exit>");

    loopBody->ast = loopTail->ast = stmt;

    IrInstruction *gotoBody = newGotoInstruction(ctx, loopBody);
    addSuccessor(ctx, ctx->currentBB, loopBody);
    termintateBlock(ctx, gotoBody);

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoTail = newGotoInstruction(ctx, loopTail);
        addSuccessor(ctx, ctx->currentBB, loopTail);
        termintateBlock(ctx, gotoTail);
    }

    ctx->currentBB = loopTail;
    IrOperand *irCond = translateExpression(ctx, condition);

    IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
    addSuccessor(ctx, ctx->currentBB, loopBody);
    addSuccessor(ctx, ctx->currentBB, loopExit);
    termintateBlock(ctx, irCondBranch);

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static Boolean translateFor(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_FOR);

    AstStatementList *decl = stmt->forStmt.initial;
    AstExpression *condition = stmt->forStmt.condition;
    AstExpression *modifier = stmt->forStmt.modifier;
    AstStatement *body = stmt->forStmt.body;

    while (decl != NULL) {
        translateStatement(ctx, decl->stmt);
        decl = decl->next;
    }

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    IrBasicBlock *loopHead = newBasicBlock(ctx, "<for_head>");
    IrBasicBlock *loopBody = newBasicBlock(ctx, "<for_body>");
    IrBasicBlock *loopExit = newBasicBlock(ctx, "<for_exit>");
    IrBasicBlock *modifierBB = modifier != NULL ? newBasicBlock(ctx, "<for_mod>") : NULL;

    ctx->breakBB = loopExit;
    ctx->continueBB = modifierBB != NULL ? modifierBB : loopHead;
    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(ctx, loopHead);
    addSuccessor(ctx, ctx->currentBB, loopHead);
    termintateBlock(ctx, gotoHead);

    ctx->currentBB = loopHead;
    if (condition != NULL) {
        translateExpression(ctx, condition);
        IrOperand *irCond = lastDefinedOperand(ctx);
        IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
        addSuccessor(ctx, ctx->currentBB, loopBody);
        addSuccessor(ctx, ctx->currentBB, loopExit);
        termintateBlock(ctx, irCondBranch);
    } else {
        // TODO: merge with body block
        IrInstruction *gotoBody = newGotoInstruction(ctx, loopBody);
        addSuccessor(ctx, ctx->currentBB, loopBody);
        termintateBlock(ctx, gotoBody);
    }

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrBasicBlock *leaveBB = modifierBB ? modifierBB : loopExit;
        IrInstruction *gotoLeave = newGotoInstruction(ctx, leaveBB);
        addSuccessor(ctx, ctx->currentBB, leaveBB);
        termintateBlock(ctx, gotoLeave);
    }

    if (modifierBB != NULL) {
        ctx->currentBB = modifierBB;
        translateExpression(ctx, modifier);
        IrInstruction *gotoExit = newGotoInstruction(ctx, loopExit);
        addSuccessor(ctx, ctx->currentBB, loopExit);
        termintateBlock(ctx, gotoExit);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static void jumpToBlock(IrContext *ctx, IrBasicBlock *target, AstStatement *ast) {
    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoExit = newGotoInstruction(ctx, target);
        gotoExit->meta.astStmt = ast;
        addSuccessor(ctx, ctx->currentBB, target);
        termintateBlock(ctx, gotoExit);
    }
}

static Boolean translateReturn(IrContext* ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_RETURN);

    AstExpression *expr = stmt->jumpStmt.expression;
    IrOperand *returnValue = NULL;

    if (expr != NULL) {
        if (isCompositeType(expr->type)) {
            // TODO:
            unimplemented("Return composite type");
        } else {
            assert(ctx->currentFunc->retOperand != NULL);
            returnValue = translateExpression(ctx, expr);
            IrInstruction *moveInstr = newMoveInstruction(ctx, returnValue, ctx->currentFunc->retOperand);
            addInstruction(ctx, moveInstr);
        }
    }

    jumpToBlock(ctx, ctx->currentFunc->exit, stmt);

    return TRUE;
}

static Boolean translateBreak(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_BREAK);
    assert(ctx->breakBB != NULL);

    jumpToBlock(ctx, ctx->breakBB, stmt);
    return TRUE;
}

static Boolean translateContinue(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_CONTINUE);
    assert(ctx->continueBB != NULL);

    jumpToBlock(ctx, ctx->continueBB, stmt);
    return TRUE;
}

static IrBasicBlock *getOrCreateLabelBlock(IrContext *ctx, const char *labelName) {
    HashMap *labelMap = ctx->labelMap;
    IrBasicBlock *block = (IrBasicBlock *)getFromHashMap(labelMap, (intptr_t)labelName);
    if (block != NULL)
        return block;

    block = newBasicBlock(ctx, labelName);
    putToHashMap(labelMap, (intptr_t)labelName, (intptr_t)block);
    return block;
}

static Boolean translateGotoLabel(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_L);

    IrBasicBlock *labelBlock = getOrCreateLabelBlock(ctx, stmt->jumpStmt.label);

    jumpToBlock(ctx, labelBlock, stmt);
    return TRUE;
}

static void addSuccessors(intptr_t l, intptr_t b, void *x) {
    const char *label = (const char *)l;
    IrBasicBlock *bb = (IrBasicBlock *)b;
    IrContext *ctx = (IrContext *)x;

    addSuccessor(ctx, ctx->currentBB, bb);
}

static Boolean translateGotoPtr(IrContext* ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_P);

    translateExpression(ctx, stmt->jumpStmt.expression);
    IrOperand *target = lastDefinedOperand(ctx);

    IrInstruction *iBranch = newInstruction(ctx, IR_IBRANCH);
    addOperandTail(ctx, &iBranch->uses, target);
    for (IrBasicBlockListNode *n = ctx->referencedBlocks.head; n; n = n->next) {
        addSuccessor(ctx, ctx->currentBB, n->block);
    }
    termintateBlock(ctx, iBranch);

    return TRUE;
}

static Boolean translateLabel(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_LABEL);

    IrBasicBlock *labelBlock = NULL;

    switch (stmt->labelStmt.kind) {
    case LK_LABEL: {
        labelBlock = getOrCreateLabelBlock(ctx, stmt->labelStmt.label);
        break;
    }
    case LK_CASE: {
        SwitchTable *table = ctx->switchTable;
        assert(table != NULL);
        CaseBlock *caseBlocks = table->caseBlocks;
        IrBasicBlock *caseBlock = NULL;
        for (uint32_t i = 0; i < table->caseCount; ++i) {
            if (caseBlocks[i].caseConst == stmt->labelStmt.caseConst) {
                labelBlock = caseBlocks[i].block;
                break;
            }
        }
        break;
    }
    case LK_DEFAULT: {
        labelBlock = ctx->defaultCaseBB;
        break;
    }
    }

    assert(labelBlock != NULL);

    labelBlock->ast = stmt;
    jumpToBlock(ctx, labelBlock, stmt);
    ctx->currentBB = labelBlock;
    translateStatement(ctx, stmt->labelStmt.body);

    return FALSE;
}

static unsigned walkCaseLabels(AstStatement *body, CaseBlock *caseBlocks, unsigned idx) {
  unsigned visited = 0;
  switch (body->statementKind) {
    case SK_BLOCK: {
        AstStatementList *stmts = body->block.stmts;
        while (stmts) {
            unsigned tmp = walkCaseLabels(stmts->stmt, caseBlocks, idx);
            visited += tmp;
            idx += tmp;
            stmts = stmts->next;
        }
        return visited;
    }
    case SK_DECLARATION: break;
    case SK_BREAK:
    case SK_CONTINUE:
    case SK_RETURN:
    case SK_EMPTY:
    case SK_EXPR_STMT:
    case SK_GOTO_L:
    case SK_GOTO_P:
      break;

    case SK_IF:
      visited = walkCaseLabels(body->ifStmt.thenBranch, caseBlocks, idx);
      idx += visited;
      if (body->ifStmt.elseBranch)
        visited += walkCaseLabels(body->ifStmt.elseBranch, caseBlocks, idx);
      return visited;
    case SK_SWITCH:
      return 0; // stop
    case SK_WHILE:
    case SK_DO_WHILE:
      return walkCaseLabels(body->loopStmt.body, caseBlocks, idx);
    case SK_FOR:
      return walkCaseLabels(body->forStmt.body, caseBlocks, idx);
    case SK_LABEL:
      switch (body->labelStmt.kind) {
      case LK_DEFAULT:
      case LK_LABEL: return walkCaseLabels(body->labelStmt.body, caseBlocks, idx);
      case LK_CASE:
          caseBlocks[idx++].caseConst = body->labelStmt.caseConst;
          return walkCaseLabels(body->labelStmt.body, caseBlocks, idx) + 1;
      }

      break;
    default: unreachable("Unknown statement kind");
  }

  return 0;

}

static Boolean translateSwitch(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_SWITCH);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldDefaultCaseBB = ctx->defaultCaseBB;

    SwitchTable *oldSwitchTable = ctx->switchTable;

    SwitchTable *switchTable = areanAllocate(ctx->irArena, sizeof(SwitchTable) + stmt->switchStmt.caseCount * sizeof(CaseBlock));
    CaseBlock *caseBlocks = (CaseBlock *)(&switchTable[1]);

    switchTable->caseCount = stmt->switchStmt.caseCount;
    switchTable->caseBlocks = caseBlocks;

    memset(caseBlocks, 0, sizeof(CaseBlock) * switchTable->caseCount);

    IrBasicBlock *switchExitBB = newBasicBlock(ctx, "<switch_exit>");
    IrBasicBlock *defaultBB = stmt->switchStmt.hasDefault ? newBasicBlock(ctx, "<default_case>") : switchExitBB;

    ctx->breakBB = switchExitBB;
    ctx->defaultCaseBB = defaultBB;

    IrOperand *condOp = translateExpression(ctx, stmt->switchStmt.condition);

    IrInstruction *tableBranch = newTableBranch(ctx, condOp, switchTable);
    tableBranch->meta.astStmt = stmt;

    unsigned walked = walkCaseLabels(stmt->switchStmt.body, caseBlocks, 0);
    assert(walked == switchTable->caseCount);

    for (uint32_t i = 0; i < switchTable->caseCount; ++i) {
        IrBasicBlock *caseBlock = newBasicBlock(ctx, "<case_block>");
        caseBlocks[i].block = caseBlock;
        IrOperand *caseLableOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
        caseLableOp->data.bb = caseBlock;
        addOperandTail(ctx, &tableBranch->uses, caseLableOp);
        addSuccessor(ctx, ctx->currentBB, caseBlock);
    }

    IrOperand *defaultLableOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
    defaultLableOp->data.bb = defaultBB;
    addOperandTail(ctx, &tableBranch->uses, defaultLableOp);
    addSuccessor(ctx, ctx->currentBB, defaultBB);
    termintateBlock(ctx, tableBranch);

    IrBasicBlock *switchBody = newBasicBlock(ctx, "<switch_body>");
    ctx->currentBB = switchBody;
    translateStatement(ctx, stmt->switchStmt.body);

    jumpToBlock(ctx, switchExitBB, stmt);
    ctx->currentBB = switchExitBB;

    ctx->switchTable = oldSwitchTable;
    ctx->breakBB = oldBreakBB;
    ctx->defaultCaseBB = oldDefaultCaseBB;

    return FALSE;
}

static void collectTranslationInfoExpr(IrContext *ctx, const AstExpression *expr);
static void collectTranslationInfoStmt(IrContext *ctx, const AstStatement *stmt);

static void collectRerenecedLabelsInit(IrContext *ctx, const AstInitializer *init) {
    switch (init->kind) {
    case IK_EXPRESSION:
      return collectTranslationInfoExpr(ctx, init->expression);
    case IK_LIST:
      for (const AstInitializerList *inits = init->initializerList; inits != NULL; inits = inits->next) {
        collectRerenecedLabelsInit(ctx, inits->initializer);
      }
      break;
    default: unreachable("Unexpected initializer type");
    }
}

static void collectTranslationInfoExpr(IrContext *ctx, const AstExpression *expr) {
    switch (expr->op) {
      case E_PAREN:
        return collectTranslationInfoExpr(ctx, expr->parened);
      case E_BLOCK:
        return collectTranslationInfoStmt(ctx, expr->block);
      case E_CONST:
      case E_NAMEREF:
        return ;
      case E_VA_ARG:
        return collectTranslationInfoExpr(ctx, expr->vaArg.va_list);
      case E_COMPOUND:
        return collectRerenecedLabelsInit(ctx, expr->compound);
      case E_CALL:
        collectTranslationInfoExpr(ctx, expr->callExpr.callee);
        for (const AstExpressionList *arg = expr->callExpr.arguments; arg; arg = arg->next) {
            collectTranslationInfoExpr(ctx, arg->expression);
        }
        return ;
      case E_TERNARY:
        collectTranslationInfoExpr(ctx, expr->ternaryExpr.condition);
        collectTranslationInfoExpr(ctx, expr->ternaryExpr.ifTrue);
        collectTranslationInfoExpr(ctx, expr->ternaryExpr.ifFalse);
        return;
      case E_BIT_EXTEND:
        return collectTranslationInfoExpr(ctx, expr->extendExpr.argument);
      case E_CAST:
        return collectTranslationInfoExpr(ctx, expr->castExpr.argument);
      case EB_ADD:
      case EB_SUB:
      case EB_LHS: /** << */
      case EB_RHS: /** >> */
      case EB_AND:
      case EB_OR:
      case EB_XOR:
      case EB_MUL:
      case EB_DIV:
      case EB_MOD:
      case EB_ANDAND:
      case EB_OROR:
      case EB_EQ:
      case EB_NE:
      case EB_LT:
      case EB_LE:
      case EB_GT:
      case EB_GE:
      case EB_ASG_MUL:
      case EB_ASG_ADD:
      case EB_ASG_SUB:
      case EB_ASG_SHL:
      case EB_ASG_SHR:
      case EB_ASG_AND:
      case EB_ASG_XOR:
      case EB_ASG_OR:
      case EB_ASG_DIV:
      case EB_ASG_MOD:
      case EB_ASSIGN:
      case EB_A_ACC:
      case EB_COMMA:
        collectTranslationInfoExpr(ctx, expr->binaryExpr.left);
        collectTranslationInfoExpr(ctx, expr->binaryExpr.right);
        return;
      case EU_REF:
        if (expr->unaryExpr.argument->op == E_NAMEREF) {
            const AstExpression *ref = expr->unaryExpr.argument;
            const Symbol *s = ref->nameRefExpr.s;
            if (s->kind == ValueSymbol) {
                const AstValueDeclaration *vd = s->variableDesc;
                assert(vd != NULL);
                if (vd->flags.bits.isLocal) {
                    assert(!vd->flags.bits.isRegister && "This should be verified during Sema analysis");
                    assert(vd->index2 >= 0);
                    ctx->localOperandMap[vd->index2].flags.referenced = 1;
                }
            }
        }
      case EU_DEREF:
      case EU_PLUS:
      case EU_MINUS:
      case EU_TILDA:
      case EU_EXL:
      case EU_PRE_DEC:
      case EU_PRE_INC:
      case EU_POST_DEC:
      case EU_POST_INC:
        return collectTranslationInfoExpr(ctx, expr->unaryExpr.argument);
      case EF_DOT:
      case EF_ARROW:
        return collectTranslationInfoExpr(ctx, expr->fieldExpr.recevier);
      case E_LABEL_REF: {
            const char *label = expr->label;
            IrBasicBlock *labelBlock = getOrCreateLabelBlock(ctx, label);
            for (IrBasicBlockListNode *n = ctx->referencedBlocks.head; n; n = n->next) {
                if (n->block == labelBlock)
                    return;
            }
            addBBTail(ctx, &ctx->referencedBlocks, labelBlock);
            return;
        }

    default: unreachable("unexpected expression op");
    }
}

static void collectTranslationInfoStmt(IrContext *ctx, const AstStatement *stmt) {

    switch (stmt->statementKind) {
      case SK_BLOCK: {
          AstStatementList *stmts = stmt->block.stmts;
          while (stmts) {
              collectTranslationInfoStmt(ctx, stmts->stmt);
              stmts = stmts->next;
          }
          break;
      }
      case SK_DECLARATION:
        if (stmt->declStmt.declaration->kind == DK_VAR) {
            const AstValueDeclaration *v = stmt->declStmt.declaration->variableDeclaration;
            if (v->initializer && v->flags.bits.isLocal) {
                return collectRerenecedLabelsInit(ctx, v->initializer);
            }
        }
        return;
      case SK_BREAK:
      case SK_CONTINUE:
        return;
      case SK_RETURN:
      case SK_GOTO_P:
        if (stmt->jumpStmt.expression) {
            collectTranslationInfoExpr(ctx, stmt->jumpStmt.expression);
        }
        return;
      case SK_EXPR_STMT:
        collectTranslationInfoExpr(ctx, stmt->exprStmt.expression);
        return;

      case SK_IF:
        collectTranslationInfoExpr(ctx, stmt->ifStmt.condition);
        collectTranslationInfoStmt(ctx, stmt->ifStmt.thenBranch);
        if (stmt->ifStmt.elseBranch) {
            collectTranslationInfoStmt(ctx, stmt->ifStmt.elseBranch);
        }
        return;
      case SK_SWITCH:
        collectTranslationInfoExpr(ctx, stmt->switchStmt.condition);
        collectTranslationInfoStmt(ctx, stmt->switchStmt.body);
        return;
      case SK_WHILE:
      case SK_DO_WHILE:
        collectTranslationInfoExpr(ctx, stmt->loopStmt.condition);
        collectTranslationInfoStmt(ctx, stmt->loopStmt.body);
        return;
      case SK_FOR: {
        const AstStatementList *init = stmt->forStmt.initial;
        while (init) {
            collectTranslationInfoStmt(ctx, init->stmt);
            init = init->next;
        }
        if (stmt->forStmt.condition) {
            collectTranslationInfoExpr(ctx, stmt->forStmt.condition);
        }
        if (stmt->forStmt.modifier) {
            collectTranslationInfoExpr(ctx, stmt->forStmt.modifier);
        }
        collectTranslationInfoStmt(ctx, stmt->forStmt.body);
        return;
        }
      case SK_LABEL:
        return collectTranslationInfoStmt(ctx, stmt->labelStmt.body);
      default: unreachable("Unknown statement kind");
    }
}

static void collectTranslationInfo(IrContext *ctx, const AstStatement *body) {
    assert(ctx->labelMap != NULL && "Label map need to be allocated at this point");
    assert(ctx->localOperandMap != NULL && "Local Operand map need to be allocated at this point");

    collectTranslationInfoStmt(ctx, body);
}

static void generateExitBlock(IrContext *ctx, IrFunction *func) {
  ctx->currentBB = func->exit;

  IrInstruction *ret = newInstruction(ctx, IR_RET);
  addInstruction(ctx, ret);
  if (func->retOperand) {
	addInstructionUse(ctx, ret, func->retOperand);
  }
}

static uint32_t buildInitialIr(IrContext *ctx, IrFunction *func, AstFunctionDefinition *function) {
    AstFunctionDeclaration *declaration = function->declaration;
    AstValueDeclaration *local = function->locals;
    size_t numOfParams = 0;
    for (AstValueDeclaration *param = declaration->parameters;
         param != NULL;
         param = param->next, ++numOfParams);
    declaration->parameterCount = numOfParams;

    size_t numOfLocals = 0;
    for (AstValueDeclaration *local = function->locals;
         local != NULL;
         local = local->next, ++numOfLocals);

    LocalValueInfo *localOperandsMap = areanAllocate(ctx->irArena, (numOfParams + numOfLocals) * sizeof (LocalValueInfo));
    ctx->localOperandMap = localOperandsMap;
    func->localOperandMap = localOperandsMap;
    func->numOfLocals = numOfParams + numOfLocals;
    

    int32_t frameOffset = 0;

    size_t idx = 0;
    for (AstValueDeclaration *param = declaration->parameters;
         param != NULL;
         param = param->next, ++idx) {
        enum IrTypeKind type = typeRefToIrType(param->type);
        IrOperand *op = newIrOperand(ctx, type, IR_LOCAL);
        op->ast.v = param;
        param->index2 = idx;
        localOperandsMap[idx].initialOp = op;
        localOperandsMap[idx].declaration = param;
        frameOffset += alignSize(computeTypeSize(param->type), sizeof (intptr_t));
    }

	printf("idx = %lu, param count = %lu\n", idx, numOfParams);
    assert(idx == numOfParams);

    for (local = function->locals;
         local != NULL;
         local = local->next, ++idx) {
        enum IrTypeKind type = typeRefToIrType(local->type);
        IrOperand *op = newIrOperand(ctx, type, IR_LOCAL);
        op->ast.v = local;
        localOperandsMap[idx].initialOp = op;
        localOperandsMap[idx].declaration = local;
        localOperandsMap[idx].frameOffset = frameOffset;
        local->index2 = idx;
        frameOffset += alignSize(computeTypeSize(local->type), sizeof (intptr_t));
    }

	printf("idx = %lu (%lu), numOfLocals = %lu\n", idx, idx - numOfParams, numOfLocals);
    assert((idx - numOfParams)  == numOfLocals);

    if (!isVoidType(declaration->returnType)) {
        enum IrTypeKind type = typeRefToIrType(declaration->returnType);
        func->retOperand = newIrOperand(ctx, type, IR_LOCAL);
    }

    IrOperand *frameOp = ctx->frameOp = newIrOperand(ctx, IR_PTR, IR_FRAME_PTR);

    AstStatement *body = function->body;
    assert(body->statementKind == SK_BLOCK);

    collectTranslationInfo(ctx, body);

    IrBasicBlock *firstBB = ctx->currentBB = newBasicBlock(ctx, "<FIRST>");
    translateBlock(ctx, body);
	generateExitBlock(ctx, func);

    ctx->currentBB = func->entry;
    gotoToBlock(ctx, firstBB);
    ctx->currentBB = NULL;

    ctx->localOperandMap = NULL;

    return 0;
}

static IrFunction *translateFunction(IrContext *ctx, AstFunctionDefinition *function) {
    ctx->bbCnt = ctx->vregCnt = ctx->opCnt = 0;
    IrFunction *func = newIrFunction(ctx, function);

    buildInitialIr(ctx, func, function);


    return func;
}
