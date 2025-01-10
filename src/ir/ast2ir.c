
#include <assert.h>
#include "ir/ir.h"
#include "tree.h"
#include "types.h"
#include "sema.h"
#include <signal.h>


extern IrContext *ctx;

typedef struct _CaseBlock {
    int64_t caseConst;
    IrBasicBlock *block;
} CaseBlock;

static IrFunction *translateFunction(AstFunctionDefinition *function);
static Boolean translateStatement(AstStatement *stmt);
static Boolean translateBlock(AstStatement *block);
static Boolean translateStatement(AstStatement *stmt);
static Boolean translateDeclaration(AstDeclaration *decl);
static IrOperand *translateExpression(AstExpression *expr);
static Boolean translateLabel(AstStatement *stmt);
static Boolean translateGotoLabel(AstStatement *stmt);
static Boolean translateGotoPtr(AstStatement *stmt);
static Boolean translateReturn(AstStatement *stmt);
static Boolean translateBreak(AstStatement *stmt);
static Boolean translateContinue(AstStatement *stmt);
static Boolean translateIf(AstStatement *stmt);
static Boolean translateSwitch(AstStatement *stmt);
static Boolean translateWhile(AstStatement *stmt);
static Boolean translateDoWhile(AstStatement *stmt);
static Boolean translateFor(AstStatement *stmt);

static IrBasicBlock *getOrCreateLabelBlock(const char *labelName);

static IrFunction *newIrFunction(AstFunctionDefinition *function) {
    IrFunction *func = areanAllocate(ctx->irArena, sizeof (IrFunction));
	ctx->currentFunc = func;
    func->ast = function;
    func->id = ctx->functionCnt++;
    func->entry = newBasicBlock("<entry>");
    func->exit = newBasicBlock("<exit>");
    return func;
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

void addTestIrFunction(IrFunctionList *list) {
  ctx->bbCnt = ctx->opCnt = ctx->instrCnt = ctx->vregCnt = 0;

  IrFunction *f = newIrFunction(NULL);

  f->entry->name = "0";
  f->exit->name = "3";

  IrBasicBlock *bb0 = f->entry;
  IrBasicBlock *bb1 = newBasicBlock("1");
  IrBasicBlock *bb2 = newBasicBlock("2");
  IrBasicBlock *bb3 = f->exit;
  IrBasicBlock *bb4 = newBasicBlock("4");
  IrBasicBlock *bb5 = newBasicBlock("5");
  IrBasicBlock *bb6 = newBasicBlock("6");
  IrBasicBlock *bb7 = newBasicBlock("7");

  addSuccessor(bb0, bb1);
  addSuccessor(bb1, bb2);
  addSuccessor(bb1, bb4);
  addSuccessor(bb2, bb3);
  addSuccessor(bb4, bb5);
  addSuccessor(bb4, bb6);
  addSuccessor(bb5, bb7);
  addSuccessor(bb6, bb1);
  addSuccessor(bb6, bb7);
  addSuccessor(bb7, bb3);

  buildSSA(f);

  addFunctionTail(list, f);
}

IrFunctionList translateAstToIr(AstFile *file) {
    IrFunctionList list = {0};


    AstTranslationUnit *unit = file->units;

    while (unit != NULL) {
        if (unit->kind == TU_FUNCTION_DEFINITION) {
		  	fprintf(stdout, "Translate function '%s' into IR\n", unit->definition->declaration->name);
            IrFunction *function = translateFunction(unit->definition);
            addFunctionTail(&list, function);
        } else {
            assert(unit->kind == TU_DECLARATION);
            translateDeclaration(unit->declaration);
        }
        unit = unit->next;
    }

//    addTestIrFunction(&list);

    return list;
}

static IrOperand *computeEffectiveAddress(IrOperand *op, const AstExpression *ast) {
    if (op->kind == IR_MEMORY) {

        IrInstruction *instr = newInstruction(IR_E_ADD);

        IrOperand *result = newVreg(op->data.address.base->type);

        addInstructionUse(instr, op->data.address.base);
        addInstructionUse(instr, op->data.address.offset);
        addInstructionDef(instr, result);

        result->astType = ast->type;
        result->ast.e = ast;

        addInstruction(instr);
	    return result;
	}

	return op;
}

static IrOperand *encodeBitField(TypeRef *type, IrOperand *storageOp, IrOperand *valueOp) {
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

	IrInstruction *storageMask = newInstruction(IR_E_AND);
	IrOperand *mask1Op = createIntegerConstant(irMemoryType, ~mask);
	IrOperand *maskedStorageOp = newVreg(irMemoryType);
	addInstructionUse(storageMask, storageOp);
	addInstructionUse(storageMask, mask1Op);
	addInstructionDef(storageMask, maskedStorageOp);
	addInstruction(storageMask);

	IrInstruction *shiftValueInstr = newInstruction(IR_E_SHL);
	IrOperand *shiftOp = createIntegerConstant(irMemoryType, s);
	IrOperand *shiftedValueOp = newVreg(IR_I32);
	addInstructionUse(shiftValueInstr, valueOp);
	addInstructionUse(shiftValueInstr, shiftOp);
	addInstructionDef(shiftValueInstr, shiftedValueOp);
	addInstruction(shiftValueInstr);

	IrInstruction *maskValueInstr = newInstruction(IR_E_AND);
	IrOperand *mask2Op = createIntegerConstant(irMemoryType, mask);
	IrOperand *maskedValueOp = newVreg(irMemoryType);
	addInstructionUse(maskValueInstr, shiftedValueOp);
	addInstructionUse(maskValueInstr, mask2Op);
	addInstructionDef(maskValueInstr, maskedValueOp);
	addInstruction(maskValueInstr);

	IrInstruction *mergeInstr = newInstruction(IR_E_OR);
	IrOperand *resultOp = newVreg(irMemoryType);
	addInstructionUse(mergeInstr, maskedStorageOp);
	addInstructionUse(mergeInstr, maskedValueOp);
	addInstructionDef(mergeInstr, resultOp);
	addInstruction(mergeInstr);
    // TODO: sign extend

    return resultOp;
}

static IrOperand *decodeBitField(TypeRef *type, IrOperand *storageOp) {
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
    IrOperand *shlSizeOp = createIntegerConstant(IR_I32, l);
    IrOperand *shlOp = newVreg(irMemoryType);
    shlOp->astType = memoryType;

    IrInstruction *shlInstr = newInstruction(IR_E_SHL);
    addInstructionUse(shlInstr, loadedValue);
    addInstructionUse(shlInstr, shlSizeOp);
    addInstructionDef(shlInstr, shlOp);
    addInstruction(shlInstr);

    int32_t r = W - w;
    IrOperand *shrSizeOp = createIntegerConstant(IR_I32, r);
    IrOperand *shrOp = newVreg(irMemoryType);
    shrOp->astType = memoryType;

    IrInstruction *shrInstr = newInstruction(IR_E_SHR);
    addInstructionUse(shrInstr, shlOp);
    addInstructionUse(shrInstr, shrSizeOp);
    addInstructionDef(shrInstr, shrOp);
    addInstruction(shrInstr);

    // TODO: sign extend

    return shrOp;
}

IrInstruction *createAllocaInstr(IrOperand *sizeOp, IrOperand *defOp) {
    IrInstruction *allocaInstr = newInstruction(IR_ALLOCA);

    addInstructionUse(allocaInstr, sizeOp);
    addInstructionDef(allocaInstr, defOp);
    addInstruction(allocaInstr);

	return allocaInstr;
}

IrInstruction *createAllocaSlotInPlace(size_t slotSize, IrOperand *defOp) {
  slotSize = alignSize(slotSize, sizeof(intptr_t));
  IrOperand *sizeOp = createIntegerConstant(IR_U64, slotSize);
  IrInstruction *allocaInstr = createAllocaInstr(sizeOp, defOp);

  allocaInstr->info.stackSize = slotSize;

  return allocaInstr;
}

IrOperand *createAllocaSlot(size_t slotSize) {
  IrOperand *defOp = newVreg(IR_PTR);
  createAllocaSlotInPlace(slotSize, defOp);
  return defOp;
}

static void generateCompositeCopy(const TypeRef *type, IrOperand *src, IrOperand *dst, const AstExpression *ast) {
	assert(isCompositeType(type));

  int32_t align = type->descriptorDesc->typeDefinition->align;
  int32_t size = computeTypeSize(type);
  int32_t copied = 0;

  IrOperand *srcOp = computeEffectiveAddress(src, ast);
  IrOperand *dstOp = computeEffectiveAddress(dst, ast);

  while (copied < size) {
      int32_t chunkSize;
      int32_t left = size - copied;

      if (left >= 8) chunkSize = sizeof(int64_t);
      else if (left >= 4) chunkSize = sizeof(int32_t);
      else if (left >= 2) chunkSize = sizeof(int16_t);
      else chunkSize = sizeof(int8_t);

      chunkSize = min(align, chunkSize);
	  enum IrTypeKind memType = sizeToMemoryType(chunkSize);

	  IrOperand *offsetOp = createIntegerConstant(IR_I64, copied);

	  IrOperand *word = addLoadInstr(memType, srcOp, offsetOp, ast);
	  addStoreInstr(dstOp, offsetOp, word, ast);

      copied += chunkSize;
  }
}

// -============================ translators ============================-

static Boolean isNullConst(AstExpression *expr) {
  if (expr->op != E_CONST) return FALSE;
  return expr->constExpr.i == 0;
}

static size_t translateInitializerIntoMemory(IrOperand *base, int32_t offset, size_t typeSize, const AstInitializer *initializer) {

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
	  const int32_t emittedOffset = translateInitializerIntoMemory(base, emitOffset, typeSize, expr->compound);
	  assert(emittedOffset == emitOffset + slotSize);
	  return emittedOffset;
	}

	IrOperand *valueOp = translateExpression(expr);
	if ((emitOffset + slotSize) <= typeSize) {
	  IrOperand *offsetOp = createIntegerConstant(IR_I64, emitOffset);

	  if (isCompositeType(slotType)) {
	  	IrOperand *dstOp = newVreg(IR_PTR);
		IrInstruction *addInstr = newInstruction(IR_E_ADD);
		addInstructionUse(addInstr, base);
		addInstructionUse(addInstr, offsetOp);
		addInstructionDef(addInstr, dstOp);
		addInstruction(addInstr);
        generateCompositeCopy(slotType, valueOp, dstOp, expr);
	  } else if (slotType->kind == TR_BITFIELD) {
		// TODO
		unimplemented("BitField initializer");
	  } else {
		IrInstruction *storeInstr = newInstruction(IR_M_STORE);
		addInstructionUse(storeInstr, base);
		addInstructionUse(storeInstr, offsetOp);
		addInstructionUse(storeInstr, valueOp);
		addInstruction(storeInstr);
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
		  return translateInitializerIntoMemory(base, offset, typeSize, init);
		}
	  }
	} else {
  	  size_t emmited = 0;
	  for (const AstInitializerList *inits = initializer->initializerList;
		  inits != NULL;
		  inits = inits->next) {
		emmited = translateInitializerIntoMemory(base, offset, typeSize, inits->initializer);
	  }
	  return emmited;
	}
  }
  default: unreachable("Unknown initializer kind");
  }

  return 0;
}

// -============================ expressions ============================-

static IrOperand *translateConstant(AstExpression *expr) {
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

    return getOrAddConstant(&data, constType);
}

static IrOperand *translateVaArg(AstExpression *expr) {
    assert(expr->op == E_VA_ARG);
    unimplemented("Constant Expression");
    return NULL;
}

static IrOperand *translateNameRef(AstExpression *expr) {
    assert(expr->op == E_NAMEREF);

    // If this is called we probably want a function reference.
    // Value references go through DEREF node

    IrOperand *op = NULL;
    Symbol *s = expr->nameRefExpr.s;
    if (s->kind == FunctionSymbol || s->kind == ValueSymbol && !s->variableDesc->flags.bits.isLocal) {
        // Either Function of non-local variable reference
        op = newIrOperand(IR_PTR, IR_REFERENCE);
        op->ast.e = expr;
        op->astType = expr->type;
        op->data.symbol = expr->nameRefExpr.s;
    } else {
        assert(s->kind == ValueSymbol);
        AstValueDeclaration *v = s->variableDesc;
        assert(v->flags.bits.isLocal);
        LocalValueInfo *info = &ctx->localOperandMap[v->index2];
        assert(info != NULL);
        if (info->flags.referenced || isCompositeType(info->declaration->type)) {
            IrOperand *base = ctx->frameOp;
            IrOperand *offset = createIntegerConstant(IR_I64, info->frameOffset);
            op = newIrOperand(IR_PTR, IR_MEMORY);
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

static IrOperand *translateCompound(AstExpression *expr) {
    assert(expr->op == E_COMPOUND);

    size_t typeSize = computeTypeSize(expr->type);
	IrOperand *memoryOp = createAllocaSlot(typeSize);

	translateInitializerIntoMemory(memoryOp, 0, typeSize, expr->compound);

	return memoryOp;
}

static IrOperand *translateCall(AstExpression *expr) {
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

	IrOperand *calleeOp = translateExpression(callee);
	IrInstruction *callInstr = newInstruction(IR_CALL);
	addInstructionUse(callInstr, calleeOp);

	IrOperand *returnSlotOp = NULL;

	// TODO: save and restore stack after series of alloca's

	if (isCompositeType(returnType) && returnTypeSize > sizeof (intptr_t)) {
	  returnSlotOp = createAllocaSlot(returnTypeSize);
	  addInstructionUse(callInstr, returnSlotOp);
  	}

	for (AstExpressionList *args = expr->callExpr.arguments;
		 args != NULL;
		 args = args->next) {
	  AstExpression *argExpr = args->expression;
	  TypeRef *argType = argExpr->type;
      unsigned alignent = max(8, typeAlignment(argType));
      unsigned argSize = max(8, computeTypeSize(argType));

	  IrOperand *argOp = translateExpression(argExpr);
	  IrOperand *realArgOp = NULL;

	  if (isCompositeType(argType)) {
		if (argSize > sizeof (intptr_t)) {
		  // TODO: make sure this should be in argument list
		  realArgOp = createAllocaSlot(argSize);
		  generateCompositeCopy(argType, argOp, realArgOp, expr);
		} else {
		  assert(argOp->kind == IR_REFERENCE);
		  IrOperand *offset = createIntegerConstant(IR_I64, 0);
		  realArgOp = addLoadInstr(IR_P_AGG, argOp, offset, expr);
		}
	  } else {
		realArgOp = argOp;
	  }

	  addInstructionUse(callInstr, realArgOp);
	}

	IrOperand *returnValueOp = NULL;
	if (!isVoidType(returnType)) {
		if (returnSlotOp != NULL) {
		  // TODO: what should be done here?
		  returnValueOp = returnSlotOp;
		} else {
		  enum IrTypeKind irRetType = typeRefToIrType(returnType);
		  returnValueOp = newVreg(irRetType);
		  addInstructionDef(callInstr, returnValueOp);
		}
	}

	addInstruction(callInstr);
	callInstr->meta.astExpr = expr;

	ctx->addressTM = tm;
    return returnValueOp;
}

static IrOperand *translateTernary(AstExpression *expr) {
    assert(expr->op == E_TERNARY);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;

    IrOperand *condOp = translateExpression(expr->ternaryExpr.condition);

    IrBasicBlock *ifTrue = newBasicBlock("<ifTrue>");
    IrBasicBlock *ifFalse = newBasicBlock("<ifFalse>");
    IrBasicBlock *exit = newBasicBlock("<ternary_exit>");

    IrInstruction *cond = newCondBranch(condOp, ifTrue, ifFalse);
    addSuccessor(ctx->currentBB, ifTrue);
    addSuccessor(ctx->currentBB, ifFalse);
    termintateBlock(cond);

    ctx->currentBB = ifTrue;
    IrOperand *ifTrueOp = translateExpression(expr->ternaryExpr.ifTrue);
    gotoToBlock(exit);

    ctx->currentBB = ifFalse;
    IrOperand *ifFalseOp = translateExpression(expr->ternaryExpr.ifFalse);
    gotoToBlock(exit);

    ctx->currentBB = exit;
    assert(ifTrueOp->type == ifFalseOp->type);
    IrOperand *result = newVreg(ifTrueOp->type);
    // TODO: what if type is composite?
    IrInstruction *phi = newInstruction(IR_PHI);
    addOperandTail(&phi->defs, result);
    addOperandTail(&phi->uses, ifTrueOp);
    addOperandTail(&phi->uses, ifFalseOp);

    ctx->addressTM = tm;

    return result;
}

static IrOperand *translateBitExtend(AstExpression *expr) {
    assert(expr->op == E_BIT_EXTEND);
    unimplemented("Bit Extend Expression");
    return NULL;
}

static IrOperand *translateCast(AstExpression *expr) {
    assert(expr->op == E_CAST);
    TypeRef *fromType = expr->castExpr.argument->type;
    TypeRef *toType = expr->castExpr.type;

	enum IrTypeKind irFromType = typeRefToIrType(fromType);
	enum IrTypeKind irToType = typeRefToIrType(toType);

	IrOperand *src = translateExpression(expr->castExpr.argument);
	IrOperand *dst = newVreg(irToType);

	IrInstruction *castInstr = newInstruction(IR_E_BITCAST);

	addInstructionUse(castInstr, src);
	addInstructionDef(castInstr, dst);
	addInstruction(castInstr);

	castInstr->info.fromCastType = irFromType;
	src->ast.e = castInstr->meta.astExpr = expr;

    return dst;
}

static IrOperand *translateLogicalExpression(AstExpression *expr) {
    assert(expr->op == EB_ANDAND || expr->op == EB_OROR);

    Boolean isAndAnd = expr->op == EB_ANDAND;
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;

    IrOperand *leftOp = translateExpression(expr->binaryExpr.left);
    IrOperand *result = newVreg(IR_I32);

    IrBasicBlock *fst = ctx->currentBB;
    IrBasicBlock *scnd = newBasicBlock(isAndAnd ? "<&&>" : "<||>");
    IrBasicBlock *exit = newBasicBlock(isAndAnd ? "<&&-exit>" : "<||-exit>");

    IrInstruction *cond = newCondBranch(leftOp, isAndAnd ? scnd : exit, isAndAnd ? exit : scnd);
    addSuccessor(ctx->currentBB, scnd);
    addSuccessor(ctx->currentBB, exit);
    termintateBlock(cond);

    ctx->currentBB = scnd;
    IrOperand *rightOp = translateExpression(expr->binaryExpr.right);
    gotoToBlock(exit);

    ctx->currentBB = exit;
    IrInstruction *phi = newInstruction(IR_PHI);
    addInstructionDef(phi, result);
    addPhiInput(phi, leftOp, fst);
    addPhiInput(phi, rightOp, scnd);

    addInstruction(phi);

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
    case EB_LHS: assert(!isFloatOperand); k = IR_E_SHL; break;
    case EB_RHS: assert(!isFloatOperand); k = IR_E_SHR; break;
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

static IrOperand *translateBinary(AstExpression *expr) {
    assert(isBinary(expr->op));

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *leftOp = translateExpression(expr->binaryExpr.left);
    IrOperand *rightOp = translateExpression(expr->binaryExpr.right);
    ctx->addressTM = tm;

    enum IrTypeKind type = typeRefToIrType(expr->type);
    Boolean isFloatOperand = isRealType(expr->binaryExpr.left->type);
    enum IrIntructionKind k = getBinaryArith(expr->op, isFloatOperand);

    assert(k != IR_BAD);

    assert(leftOp != NULL);
    assert(rightOp != NULL);
    // NOTE: pointer arithmethic is desugared during parser phase
    IrOperand *result = newVreg(type);
    IrInstruction *instr = newInstruction(k);
    addOperandTail(&instr->uses, leftOp);
    addOperandTail(&instr->uses, rightOp);
    addOperandTail(&instr->defs, result);

    addInstruction(instr);

    instr->meta.astExpr = expr;

    return result;
}

static IrOperand *translateAssignment(AstExpression *expr) {
    assert(expr->op == EB_ASSIGN);

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *ref = translateExpression(assignee);
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *valueOp = translateExpression(value);
    ctx->addressTM = tm;

    // TODO: heavy_copy_1 = heavy_copy_2 = heavy_struct

    if (isCompositeType(value->type)) {
	  generateCompositeCopy(value->type, valueOp, ref, expr);
    } else if (ref->kind == IR_MEMORY) {
		if (value->type->kind == TR_BITFIELD) {
		  enum IrTypeKind irMemType = typeRefToIrType(value->type->bitFieldDesc.storageType);
		  IrOperand *storageOp = addLoadInstr(irMemType, ref->data.address.base, ref->data.address.offset, expr);
		  valueOp = encodeBitField(value->type, storageOp, valueOp);
		}
	  	addStoreInstr(ref->data.address.base, ref->data.address.offset, valueOp, expr);
    } else if (ref->type == IR_PTR)  {
      // probably it is something like *assigne = where assignee is some scalar
        addStoreInstr(ref, createIntegerConstant(IR_I64, 0), valueOp, expr);
    } else {
        assert(ref->kind == IR_LOCAL);

        IrInstruction *moveInstr = newMoveInstruction(valueOp, ref);
        moveInstr->meta.astExpr = expr;
        addInstruction(moveInstr);
    }

    return valueOp;
}

static IrOperand *translateAssignArith(AstExpression *expr) {
    assert(isAssignmentArith(expr->op));

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;
    assert(!isCompositeType(value->type) && "Forbiden operation in C");

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *ref = translateExpression(assignee);
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *valueOp = translateExpression(value);
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
            offset = createIntegerConstant(IR_I64, 0);
        }

        resultOp = newVreg(valueType);
        resultOp->astType = expr->type;
        resultOp->ast.e = expr;
        lhs = addLoadInstr(valueType, base, offset, expr);
		if (assignee->type->kind == TR_BITFIELD) {
		  storageOp = lhs;
		  lhs = decodeBitField(assignee->type, storageOp);
		}
    } else {
        assert(ref->kind == IR_LOCAL);
        resultOp = lhs = ref;
    }

    assert(resultOp != NULL);
    assert(lhs != NULL);

    // NOTE: Pointer arithmethic is desugared during parser phase
    IrInstruction *operation = newInstruction(ik);
    addInstructionUse(operation, lhs);
    addInstructionUse(operation, valueOp);
    addInstructionDef(operation, resultOp);
    addInstruction(operation);
    operation->meta.astExpr = expr;

    if (base != NULL) {
        assert(offset != NULL);

		if (assignee->type->kind == TR_BITFIELD) {
		  assert(storageOp != NULL);
		  resultOp = encodeBitField(assignee->type, storageOp, resultOp);
		}
        addStoreInstr(base, offset, resultOp, expr);
    }

    return resultOp;
}

static IrOperand *translateReference(AstExpression *expr) {
    assert(expr->op == EU_REF);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *arg = translateExpression(expr->unaryExpr.argument);
    ctx->addressTM = tm;

    if (arg->kind == IR_MEMORY) {
        return computeEffectiveAddress(arg, expr);
    }

    return arg;
}

static IrOperand *translateDeReference(AstExpression *expr) {
    assert(expr->op == EU_DEREF);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *arg = translateExpression(expr->unaryExpr.argument);
    ctx->addressTM = tm;

    // Check if need to do actual memory load or caller expects address operand
    if (tm == IR_TM_RVALUE) {
        if (arg->kind == IR_MEMORY) {
            // do we need load?
            IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
            enum IrTypeKind type = typeRefToIrType(expr->type);
            IrOperand *result = newVreg(type);
            result->ast.e = loadInstr->meta.astExpr = expr;
            result->astType = expr->type;

            addInstructionUse(loadInstr, arg->data.address.base);
            addInstructionUse(loadInstr, arg->data.address.offset);
            addInstructionDef(loadInstr, result);
            addInstruction(loadInstr);

            return result;
        } else if (arg->kind == IR_REFERENCE || arg->kind == IR_VREG) {
            // Check if we do not dereference function reference due to is it non-sense
            if (arg->kind == IR_VREG || arg->data.symbol->kind != FunctionSymbol) {
                IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
                enum IrTypeKind type = typeRefToIrType(expr->type);
                IrOperand *result = newVreg(type);
                result->ast.e = loadInstr->meta.astExpr = expr;
                result->astType = expr->type;

                IrOperand *zeroOffset = createIntegerConstant(IR_I64, 0);

                addInstructionUse(loadInstr, arg);
                addInstructionUse(loadInstr, zeroOffset);
                addInstructionDef(loadInstr, result);
                addInstruction(loadInstr);

                loadInstr->meta.astExpr = expr;

                return result;
            }
        } else {
            assert(arg->kind == IR_LOCAL);
        }
    }

    return arg;
}

static IrOperand *translateUnary(AstExpression *expr) {
    assert(isUnary(expr->op));

    enum IrTypeKind type = typeRefToIrType(expr->type);
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrOperand *arg = translateExpression(expr->unaryExpr.argument);
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
        zeroConst = getOrAddConstant(&data, type);
        IrInstruction *instr = newInstruction(op);
        result = newVreg(type);
        addInstructionUse(instr, zeroConst);
        addInstructionUse(instr, arg);
        addInstructionDef(instr, result);
        addInstruction(instr);
        break;
    }
    case EU_EXL: exl = TRUE;
    case EU_TILDA: {
        assert(!isFloat);
        result = newVreg(type);
        enum IrIntructionKind op = exl ? IR_U_NOT : IR_U_BNOT;
        IrInstruction *instr = newInstruction(op);
        addInstructionUse(instr, arg);
        addInstructionDef(instr, result);
        addInstruction(instr);
        break;
    }
    default: unreachable("wtf?");
    }

    assert(result != NULL);

    return result;
}

static IrOperand *translateAddressLikeExpression(IrOperand *base, IrOperand *offset, AstExpression *ast, TypeRef *valueType) {
    IrOperand *result = NULL;

    TypeRef *memoryType = valueType->kind == TR_BITFIELD ? valueType->bitFieldDesc.storageType : valueType;

    if (ctx->addressTM == IR_TM_RVALUE) {
        if (isCompositeType(memoryType)) {
            assert(valueType == memoryType);
            result = newVreg(IR_PTR);
            result->astType = makePointedType(ctx->pctx, 0U, valueType);

            IrInstruction *addInstr = newInstruction(IR_E_ADD);

            addInstructionUse(addInstr, base);
            addInstructionUse(addInstr, offset);
            addInstructionDef(addInstr, result);
            addInstruction(addInstr);

            addInstr->meta.astExpr = ast;
        } else {
            enum IrTypeKind irMemoryType = typeRefToIrType(memoryType);

            result = newVreg(irMemoryType);
            result->astType = memoryType;

            IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
            addInstructionUse(loadInstr, base);
            addInstructionUse(loadInstr, offset);
            addInstructionDef(loadInstr, result);
            addInstruction(loadInstr);

            loadInstr->meta.astExpr = ast;

            if (memoryType != valueType) {
			  result = decodeBitField(valueType, result);
            }
        }
    } else {
        assert(ctx->addressTM == IR_TM_LVALUE);

        result = newIrOperand(IR_PTR, IR_MEMORY);
        result->astType = makePointedType(ctx->pctx, 0U, valueType);
        result->data.address.base = base;
        result->data.address.offset = offset;
    }

    assert(result != NULL);

    result->ast.e = ast;

    return result;
}

static IrOperand *translateArrayAccess(AstExpression *expr) {
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

    IrOperand *baseOp = translateExpression(base);
    IrOperand *indexOp = translateExpression(index);

    ctx->addressTM = oldTM;

    IrOperand *scaledIndexOp = NULL;

    if (elementType->kind == TR_VLA) {
        unimplemented("VLA array access");
    } else {
        int32_t elementSize = computeTypeSize(elementType);
        if (elementSize > 1) {
            scaledIndexOp = newVreg(indexIrType);
            if (isPowerOf2(elementSize)) {
                IrOperand *elementSizeOpScale = createIntegerConstant(IR_I32, log2Integer(elementSize));
                IrInstruction *shlInstr = newInstruction(IR_E_SHL);
                addInstructionUse(shlInstr, indexOp);
                addInstructionUse(shlInstr, elementSizeOpScale);
                addInstructionDef(shlInstr, scaledIndexOp);
                addInstruction(shlInstr);
                shlInstr->meta.astExpr = expr;
            } else {
                IrOperand *elementSizeOp = createIntegerConstant(indexIrType, elementSize);
                IrInstruction *mulInstr = newInstruction(IR_E_MUL);
                addInstructionUse(mulInstr, indexOp);
                addInstructionUse(mulInstr, elementSizeOp);
                addInstructionDef(mulInstr, scaledIndexOp);
                addInstruction(mulInstr);
                mulInstr->meta.astExpr = expr;
            }
        } else {
            assert(elementSize == 1);
            scaledIndexOp = indexOp;
        }

        if (scaledIndexOp->type != IR_I64) {
            IrOperand *castedScaledOp = newVreg(IR_I64);
            IrInstruction *castInstruction = newInstruction(IR_E_BITCAST);
            // TODO
            castInstruction->type = IR_I64;
            castInstruction->info.fromCastType = scaledIndexOp->type;
            castInstruction->meta.astExpr = expr;

            addInstructionUse(castInstruction, scaledIndexOp);
            addInstructionDef(castInstruction, castedScaledOp);
            addInstruction(castInstruction);
            scaledIndexOp = castedScaledOp;
        }
    }

    return translateAddressLikeExpression(baseOp, scaledIndexOp, expr, elementType);
}

static IrOperand *translateFieldAccess(AstExpression *expr, Boolean isDot) {

//    expr->block->block.type->arrayTypeDesc

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *recevierOp = translateExpression(expr->fieldExpr.recevier);
    ctx->addressTM = tm;

    assert(recevierOp->type == IR_P_AGG || recevierOp->type == IR_PTR);

    int64_t memberOffset = effectiveMemberOffset(expr->fieldExpr.member);
    IrOperand *memberOffsetOp = createIntegerConstant(IR_I64, memberOffset);
    TypeRef *memberType = expr->fieldExpr.member->type;

    return translateAddressLikeExpression(recevierOp, memberOffsetOp, expr, memberType);
}

static IrOperand *translateDotAccess(AstExpression *expr) {
    assert(expr->op == EF_DOT);
    assert(isCompositeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(expr, /*isDOT = */ TRUE);
}

static IrOperand *translateArrowAccess(AstExpression *expr) {
    assert(expr->op == EF_ARROW);
    assert(isPointerLikeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(expr, /* isDOR = */ FALSE);
}

static IrOperand *translatePreOp(AstExpression *expr) {
    assert(expr->op == EU_PRE_INC || expr->op == EU_PRE_DEC);
    unreachable("Pre ++/-- Expressions should be desugared"
                " into corresponding +=/*- operations in parser");
    return NULL;
}

static IrOperand *translatePostOp(AstExpression *expr) {
    assert(expr->op == EU_POST_INC || expr->op == EU_POST_DEC);
    // NOTE: Pointer arith is not desugared in parser.
    // TODO: Generalize it with generic binary opeartions

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrOperand *referenceOp = translateExpression(expr->unaryExpr.argument);
    ctx->addressTM = tm;

	TypeRef* type = expr->type;
  	int64_t delta = isPointerLikeType(type) ? computeTypeSize(type->pointed) : 1;
  	TypeId tid = typeToId(type);
	IrOperand *delta_op;

	enum IrTypeKind irType = typeRefToIrType(type);

	enum IrIntructionKind irInstr = IR_BAD;

	if (tid < T_F4) {
		delta_op = createIntegerConstant(irType, delta);
		irInstr = expr->op == EU_POST_DEC ? IR_E_SUB : IR_E_ADD;
	} else {
	  	float80_const_t fc = (float80_const_t)delta;
		delta_op = createFloatConstant(irType, fc);
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
            offset = createIntegerConstant(IR_I64, 0);
        }

		returnValue = oldValue = addLoadInstr(irType, base, offset, expr);
		newValue = newVreg(irType);
		if (type->kind == TR_BITFIELD) {
		  storageOp = oldValue;
		  returnValue = oldValue = decodeBitField(type, oldValue);
		}
	} else {
        assert(referenceOp->kind == IR_LOCAL);

		returnValue = newVreg(irType);
        newValue = oldValue = referenceOp;

        IrInstruction *moveInstr = newMoveInstruction(referenceOp, returnValue);
        addInstruction(moveInstr);
        moveInstr->meta.astExpr = returnValue->ast.e = expr;
        returnValue->astType = expr->type;
	}

	newValue->astType = expr->type;
	newValue->ast.e = expr;

    IrInstruction *operation = newInstruction(irInstr);
    addInstructionUse(operation, oldValue);
    addInstructionUse(operation, delta_op);
    addInstructionDef(operation, newValue);
    addInstruction(operation);
    operation->meta.astExpr = expr;

	if (base != NULL) {
	  	if (type->kind == TR_BITFIELD) {
		  assert(storageOp != NULL);
		  newValue = encodeBitField(type, storageOp, newValue);
		}

		addStoreInstr(base, offset, newValue, expr);
	}

    return returnValue;
}

static IrOperand *translateLabelRef(AstExpression *expr) {
    assert(expr->op == E_LABEL_REF);

    IrBasicBlock *target = getOrCreateLabelBlock(expr->label);
    IrOperand *resultOp = newVreg(IR_PTR);
    IrOperand *targetOp = newIrOperand(IR_LABEL, IR_BLOCK);

    targetOp->data.bb = target;

    IrInstruction *instr = newInstruction(IR_BLOCK_PTR);
    addInstructionUse(instr, targetOp);
    addInstructionDef(instr, resultOp);
    addInstruction(instr);

    return resultOp;
}

static IrOperand *translateExpression(AstExpression *expr) {
    switch (expr->op) {
      case E_PAREN:
        return ctx->lastOp = translateExpression(expr->parened);
      case E_BLOCK:
        translateStatement(expr->block);
        return ctx->lastOp;
      case E_CONST:
        return ctx->lastOp = translateConstant(expr);
      case E_VA_ARG:
        return ctx->lastOp = translateVaArg(expr);
      case E_NAMEREF:
        return ctx->lastOp = translateNameRef(expr);
      case E_COMPOUND:
        return ctx->lastOp = translateCompound(expr);
      case E_CALL:
        return ctx->lastOp = translateCall(expr);
      case E_TERNARY:
        return ctx->lastOp = translateTernary(expr);
      case E_BIT_EXTEND:
        return ctx->lastOp = translateBitExtend(expr);
      case E_CAST:
        return ctx->lastOp = translateCast(expr);
      case EB_ANDAND:
      case EB_OROR:
        return ctx->lastOp = translateLogicalExpression(expr);
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
        return ctx->lastOp = translateBinary(expr);
      case EB_ASSIGN:
        return ctx->lastOp = translateAssignment(expr);
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
        return ctx->lastOp = translateAssignArith(expr);
      case EB_COMMA:
        translateExpression(expr->binaryExpr.left);
        return ctx->lastOp = translateExpression(expr->binaryExpr.right);
      case EU_REF:
        return ctx->lastOp = translateReference(expr);
      case EU_DEREF:
        return ctx->lastOp = translateDeReference(expr);
      case EU_PLUS:
      case EU_MINUS:
      case EU_TILDA:
      case EU_EXL:
        return ctx->lastOp = translateUnary(expr);
      case EB_A_ACC:
        return ctx->lastOp = translateArrayAccess(expr);
      case EF_DOT:
        return ctx->lastOp = translateDotAccess(expr);
      case EF_ARROW:
        return ctx->lastOp = translateArrowAccess(expr);
      case EU_PRE_DEC:
      case EU_PRE_INC:
        return ctx->lastOp = translatePreOp(expr);
      case EU_POST_DEC:
      case EU_POST_INC:
        return ctx->lastOp = translatePostOp(expr);
      case E_LABEL_REF:
        return ctx->lastOp = translateLabelRef(expr);

    default: unreachable("unexpcted expression op");
    }
    return NULL;
}

// -============================ statements =============================-

static Boolean translateStatement(AstStatement *stmt) {
    switch (stmt->statementKind) {
    case SK_BLOCK: return translateBlock(stmt);
    case SK_DECLARATION: return translateDeclaration(stmt->declStmt.declaration);
    case SK_EMPTY: return FALSE;
    case SK_EXPR_STMT:
        translateExpression(stmt->exprStmt.expression);
        return FALSE;
    case SK_LABEL: return translateLabel(stmt);
    case SK_GOTO_L: return translateGotoLabel(stmt);
    case SK_GOTO_P: return translateGotoPtr(stmt);
    case SK_RETURN: return translateReturn(stmt);
    case SK_BREAK: return translateBreak(stmt);
    case SK_CONTINUE: return translateContinue(stmt);
    case SK_IF: return translateIf(stmt);
    case SK_SWITCH: return translateSwitch(stmt);
    case SK_WHILE: return translateWhile(stmt);
    case SK_DO_WHILE: return translateDoWhile(stmt);
    case SK_FOR: return translateFor(stmt);
    default:
        unreachable("Unknown statement kind");
        return TRUE;
    }
}

static Boolean translateBlock(AstStatement *block) {

    IrBasicBlock *bb = ctx->currentBB;

    if (bb == NULL || bb->term != NULL) { // emit into existed block if it not terminated
        bb = updateBlock();
        bb->ast = block;
    }

    AstStatementList *stmt = block->block.stmts;
    Boolean terminated = FALSE;

    while (stmt != NULL) {
        terminated |= translateStatement(stmt->stmt);
        stmt = stmt->next;
    }

    return terminated;
}

static void translateGlobalVariable(AstValueDeclaration *v) {
    assert(!v->flags.bits.isLocal && "Should be non-local storaged variable");

    //unimplemented("Global variable");
    if (v->flags.bits.isExternal)
      return;

    // TODO: generate initializer
}

static void translateLocalInitializer(AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);

	LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
	assert(lvi != NULL);

    const AstInitializer *init = v->initializer;
    if (init == NULL)
      return; // we are done here

    const size_t typeSize = computeTypeSize(v->type);

    Boolean hasMemorySlot = lvi->flags.referenced || isCompositeType(v->type);

    if (hasMemorySlot) {
      IrOperand *localAddrOp = newVreg(IR_PTR);
      localAddrOp->ast.v = v;
      localAddrOp->astType = v->type;
      IrOperand *frameOffsetOp = createIntegerConstant(IR_I64, lvi->frameOffset);

      IrInstruction *addInstr = newInstruction(IR_E_ADD);
      addInstructionUse(addInstr, ctx->frameOp);
      addInstructionUse(addInstr, frameOffsetOp);
      addInstructionDef(addInstr, localAddrOp);
      addInstruction(addInstr);

      translateInitializerIntoMemory(localAddrOp, 0, typeSize, init);
    } else {
      // Register-based local variable which means it's
      //  1. Has no explicit stack slot
      IrOperand *localOp = lvi->initialOp;
      assert(localOp != NULL);
      //  2. Has scalar type
      assert(!isCompositeType(v->type));

      if (init->kind == IK_EXPRESSION) {
        IrOperand *valueOp = translateExpression(init->expression);

        IrInstruction *moveInstr = newMoveInstruction(valueOp, localOp);
        addInstruction(moveInstr);
      } else {
        IrOperand *tmp = createAllocaSlot(typeSize);
        IrOperand *offsetOp = createIntegerConstant(IR_I64, 0);
        translateInitializerIntoMemory(tmp, 0, typeSize, init);

        IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
        addInstructionUse(loadInstr, tmp);
        addInstructionUse(loadInstr, offsetOp);
        addInstructionDef(loadInstr, localOp);
        addInstruction(loadInstr);
      }
    }
}

static void translateLocalDeclaration(AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);
    assert(v->type->kind != TR_VLA);
    assert(v->index2 >= 0);

    printf("Translate local variable '%s'..., next = %p..\n", v->name, v->next);

    LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
    assert(lvi->initialOp == NULL);

    TypeRef *astType = v->type;
    Boolean isAggregate = isCompositeType(astType);
    enum IrTypeKind irType = typeRefToIrType(astType);
    lvi->declaration = v;

    if (lvi->flags.referenced || isAggregate) {
      size_t size = computeTypeSize(astType);
      size = ALIGN_SIZE(size, sizeof (intptr_t));

      // IrOperand *allocaSlotOp = newVreg(irType);
      // createAllocaSlotInPlace(size, allocaSlotOp);
      IrOperand *allocaSlotOp = createAllocaSlot(size);
      lvi->initialOp = allocaSlotOp;
      allocaSlotOp->ast.v = v;
      allocaSlotOp->astType = astType;
      allocaSlotOp->data.lid = v->index2;
    } else {
        IrOperand *localOp = newIrOperand(irType, IR_LOCAL);

        lvi->initialOp = localOp;
        localOp->data.lid = v->index2;
        localOp->ast.v = v;
        localOp->astType = astType;
        localOp->data.lid = v->index2;
    }

    translateLocalInitializer(v);
}

static void translateVLA(AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);
    assert(v->type->kind == TR_VLA);

    assert(v->initializer != NULL);
    TypeRef *astType = v->type;
    //AstExpression *sizeExpr = astType->vlaDescriptor.sizeExpression;
    AstExpression *sizeExpr = v->initializer->expression;
    IrOperand *sizeOperand = translateExpression(sizeExpr);
    IrOperand *vlaOp = newVreg(IR_PTR);
    IrInstruction *vlaAlloca = createAllocaInstr(sizeOperand, vlaOp);
    vlaOp->astType = astType;
    vlaOp->ast.v = v;

    ctx->localOperandMap[v->index2].initialOp = vlaOp;
    ctx->localOperandMap[v->index2].declaration = v;
}

static Boolean translateDeclaration(AstDeclaration *decl) {

    if (decl->kind == DK_VAR) {
        AstValueDeclaration *varDecl = decl->variableDeclaration;
        if (varDecl->flags.bits.isLocal) {
            assert(varDecl->index2 >= 0);
            if (varDecl->type->kind == TR_VLA) {
                translateVLA(varDecl);
            } else {
        //        translateLocalInitializer(varDecl);
              translateLocalDeclaration(varDecl);
            }
        } else {
            //assert(varDecl->flags.bits.isStatic);
            translateGlobalVariable(varDecl);
        }
    }

    return FALSE;
}

static Boolean translateIf(AstStatement *ifStmt) {
    assert(ifStmt->statementKind == SK_IF);

    AstExpression *condition = ifStmt->ifStmt.condition;
    AstStatement *thenStmt = ifStmt->ifStmt.thenBranch;
    AstStatement *elseStmt = ifStmt->ifStmt.elseBranch;

    IrBasicBlock *ifBB = ctx->currentBB;
    IrBasicBlock *continueBB = newBasicBlock("<if_exit>");
    IrBasicBlock *thenBB = newBasicBlock("<if_then>");
    IrBasicBlock *elseBB = elseStmt != NULL ? newBasicBlock("<if_else>") : continueBB;

    IrOperand *irCond = translateExpression(condition);

    IrInstruction *condBranch = newCondBranch(irCond, thenBB, elseBB);
    addSuccessor(ifBB, thenBB);
    addSuccessor(ifBB, elseBB);
    termintateBlock(condBranch);

    ctx->currentBB = thenBB;
    translateStatement(thenStmt);
    if (ctx->currentBB != NULL) {
      IrInstruction *thenGoto = newGotoInstruction(continueBB);
      addSuccessor(ctx->currentBB, continueBB);
      termintateBlock(thenGoto);
    }
    thenBB->ast = thenStmt;

    if (elseStmt != NULL) {
        assert(elseBB != continueBB);
        ctx->currentBB = elseBB;
        translateStatement(elseStmt);
        if (ctx->currentBB != NULL) {
          IrInstruction *thenGoto = newGotoInstruction(continueBB);
          addSuccessor(ctx->currentBB, continueBB);
          termintateBlock(thenGoto);
        }
        elseBB->ast = elseStmt;
    }

    ctx->currentBB = continueBB;

    return FALSE;
}

static Boolean translateWhile(AstStatement *stmt) {
    assert(stmt->statementKind == SK_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstExpression *condition = stmt->loopStmt.condition;
    AstStatement *body = stmt->loopStmt.body;

    IrBasicBlock *loopHead = ctx->continueBB = newBasicBlock("<while_head>");
    IrBasicBlock *loopBody = newBasicBlock("<while_body>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock("<while_exit>");

    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(loopHead);
    addSuccessor(ctx->currentBB, loopHead);
    termintateBlock(gotoHead);

    ctx->currentBB = loopHead;
    IrOperand *irCond = translateExpression(condition);

    IrInstruction *irCondBranch = newCondBranch(irCond, loopBody, loopExit);
    addSuccessor(ctx->currentBB, loopBody);
    addSuccessor(ctx->currentBB, loopExit);
    termintateBlock(irCondBranch);

    ctx->currentBB = loopBody;
    translateStatement(body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoLoop = newGotoInstruction(loopHead);
        addSuccessor(ctx->currentBB, loopHead);
        termintateBlock(gotoLoop);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static Boolean translateDoWhile(AstStatement *stmt) {
    assert(stmt->statementKind == SK_DO_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstStatement *body = stmt->loopStmt.body;
    AstExpression *condition = stmt->loopStmt.condition;

    IrBasicBlock *loopBody = newBasicBlock("<do_body>");
    IrBasicBlock *loopTail = ctx->continueBB = newBasicBlock("<do_tail>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock("<do_exit>");

    loopBody->ast = loopTail->ast = stmt;

    IrInstruction *gotoBody = newGotoInstruction(loopBody);
    addSuccessor(ctx->currentBB, loopBody);
    termintateBlock(gotoBody);

    ctx->currentBB = loopBody;
    translateStatement(body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoTail = newGotoInstruction(loopTail);
        addSuccessor(ctx->currentBB, loopTail);
        termintateBlock(gotoTail);
    }

    ctx->currentBB = loopTail;
    IrOperand *irCond = translateExpression(condition);

    IrInstruction *irCondBranch = newCondBranch(irCond, loopBody, loopExit);
    addSuccessor(ctx->currentBB, loopBody);
    addSuccessor(ctx->currentBB, loopExit);
    termintateBlock(irCondBranch);

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static Boolean translateFor(AstStatement *stmt) {
    assert(stmt->statementKind == SK_FOR);

    AstStatementList *decl = stmt->forStmt.initial;
    AstExpression *condition = stmt->forStmt.condition;
    AstExpression *modifier = stmt->forStmt.modifier;
    AstStatement *body = stmt->forStmt.body;

    while (decl != NULL) {
        translateStatement(decl->stmt);
        decl = decl->next;
    }

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    IrBasicBlock *loopHead = newBasicBlock("<for_head>");
    IrBasicBlock *loopBody = newBasicBlock("<for_body>");
    IrBasicBlock *loopExit = newBasicBlock("<for_exit>");
    IrBasicBlock *modifierBB = modifier != NULL ? newBasicBlock("<for_mod>") : NULL;

    ctx->breakBB = loopExit;
    ctx->continueBB = modifierBB != NULL ? modifierBB : loopHead;
    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(loopHead);
    addSuccessor(ctx->currentBB, loopHead);
    termintateBlock(gotoHead);

    ctx->currentBB = loopHead;
    if (condition != NULL) {
        translateExpression(condition);
        IrOperand *irCond = ctx->lastOp;
        IrInstruction *irCondBranch = newCondBranch(irCond, loopBody, loopExit);
        addSuccessor(ctx->currentBB, loopBody);
        addSuccessor(ctx->currentBB, loopExit);
        termintateBlock(irCondBranch);
    } else {
        // TODO: merge with body block
        IrInstruction *gotoBody = newGotoInstruction(loopBody);
        addSuccessor(ctx->currentBB, loopBody);
        termintateBlock(gotoBody);
    }

    ctx->currentBB = loopBody;
    translateStatement(body);

    if (ctx->currentBB->term == NULL) {
        IrBasicBlock *leaveBB = modifierBB ? modifierBB : loopExit;
        IrInstruction *gotoLeave = newGotoInstruction(leaveBB);
        addSuccessor(ctx->currentBB, leaveBB);
        termintateBlock(gotoLeave);
    }

    if (modifierBB != NULL) {
        ctx->currentBB = modifierBB;
        translateExpression(modifier);
        IrInstruction *gotoExit = newGotoInstruction(loopExit);
        addSuccessor(ctx->currentBB, loopExit);
        termintateBlock(gotoExit);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static void jumpToBlock(IrBasicBlock *target, AstStatement *ast) {
    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoExit = newGotoInstruction(target);
        gotoExit->meta.astStmt = ast;
        addSuccessor(ctx->currentBB, target);
        termintateBlock(gotoExit);
    }
}

static Boolean translateReturn(AstStatement *stmt) {
    assert(stmt->statementKind == SK_RETURN);

    AstExpression *expr = stmt->jumpStmt.expression;
    IrOperand *returnValue = NULL;

    if (expr != NULL) {
        if (isCompositeType(expr->type)) {
            // TODO:
            unimplemented("Return composite type");
        } else {
            assert(ctx->currentFunc->retOperand != NULL);
            returnValue = translateExpression(expr);
            IrInstruction *moveInstr = newMoveInstruction(returnValue, ctx->currentFunc->retOperand);
            addInstruction(moveInstr);
        }
    }

    jumpToBlock(ctx->currentFunc->exit, stmt);

    return TRUE;
}

static Boolean translateBreak(AstStatement *stmt) {
    assert(stmt->statementKind == SK_BREAK);
    assert(ctx->breakBB != NULL);

    jumpToBlock(ctx->breakBB, stmt);
    return TRUE;
}

static Boolean translateContinue(AstStatement *stmt) {
    assert(stmt->statementKind == SK_CONTINUE);
    assert(ctx->continueBB != NULL);

    jumpToBlock(ctx->continueBB, stmt);
    return TRUE;
}

static IrBasicBlock *getOrCreateLabelBlock(const char *labelName) {
    HashMap *labelMap = ctx->labelMap;
    IrBasicBlock *block = (IrBasicBlock *)getFromHashMap(labelMap, (intptr_t)labelName);
    if (block != NULL)
        return block;

    block = newBasicBlock(labelName);
    putToHashMap(labelMap, (intptr_t)labelName, (intptr_t)block);
    return block;
}

static Boolean translateGotoLabel(AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_L);

    IrBasicBlock *labelBlock = getOrCreateLabelBlock(stmt->jumpStmt.label);

    jumpToBlock(labelBlock, stmt);
    return TRUE;
}

static void addSuccessors(intptr_t l, intptr_t b, void *x) {
    const char *label = (const char *)l;
    IrBasicBlock *bb = (IrBasicBlock *)b;
    IrContext *ctx = (IrContext *)x;

    addSuccessor(ctx->currentBB, bb);
}

static Boolean translateGotoPtr(AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_P);

    translateExpression(stmt->jumpStmt.expression);
    IrOperand *target = ctx->lastOp;

    IrInstruction *iBranch = newInstruction(IR_IBRANCH);
    addOperandTail(&iBranch->uses, target);
    for (IrBasicBlockListNode *n = ctx->referencedBlocks.head; n; n = n->next) {
        addSuccessor(ctx->currentBB, n->block);
    }
    termintateBlock(iBranch);

    return TRUE;
}

static Boolean translateLabel(AstStatement *stmt) {
    assert(stmt->statementKind == SK_LABEL);

    IrBasicBlock *labelBlock = NULL;

    switch (stmt->labelStmt.kind) {
    case LK_LABEL: {
        labelBlock = getOrCreateLabelBlock(stmt->labelStmt.label);
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
    jumpToBlock(labelBlock, stmt);
    ctx->currentBB = labelBlock;
    translateStatement(stmt->labelStmt.body);

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

static Boolean translateSwitch(AstStatement *stmt) {
    assert(stmt->statementKind == SK_SWITCH);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldDefaultCaseBB = ctx->defaultCaseBB;

    SwitchTable *oldSwitchTable = ctx->switchTable;

    SwitchTable *switchTable = areanAllocate(ctx->irArena, sizeof(SwitchTable) + stmt->switchStmt.caseCount * sizeof(CaseBlock));
    CaseBlock *caseBlocks = (CaseBlock *)(&switchTable[1]);

    switchTable->caseCount = stmt->switchStmt.caseCount;
    switchTable->caseBlocks = caseBlocks;

    memset(caseBlocks, 0, sizeof(CaseBlock) * switchTable->caseCount);

    IrBasicBlock *switchExitBB = newBasicBlock("<switch_exit>");
    IrBasicBlock *defaultBB = stmt->switchStmt.hasDefault ? newBasicBlock("<default_case>") : switchExitBB;

    ctx->breakBB = switchExitBB;
    ctx->defaultCaseBB = defaultBB;

    IrOperand *condOp = translateExpression(stmt->switchStmt.condition);

    IrInstruction *tableBranch = newTableBranch(condOp, switchTable);
    tableBranch->meta.astStmt = stmt;

    unsigned walked = walkCaseLabels(stmt->switchStmt.body, caseBlocks, 0);
    assert(walked == switchTable->caseCount);

    for (uint32_t i = 0; i < switchTable->caseCount; ++i) {
        IrBasicBlock *caseBlock = newBasicBlock("<case_block>");
        caseBlocks[i].block = caseBlock;
        IrOperand *caseLableOp = newIrOperand(IR_LABEL, IR_BLOCK);
        caseLableOp->data.bb = caseBlock;
        addOperandTail(&tableBranch->uses, caseLableOp);
        addSuccessor(ctx->currentBB, caseBlock);
    }

    IrOperand *defaultLableOp = newIrOperand(IR_LABEL, IR_BLOCK);
    defaultLableOp->data.bb = defaultBB;
    addOperandTail(&tableBranch->uses, defaultLableOp);
    addSuccessor(ctx->currentBB, defaultBB);
    termintateBlock(tableBranch);

    IrBasicBlock *switchBody = newBasicBlock("<switch_body>");
    ctx->currentBB = switchBody;
    translateStatement(stmt->switchStmt.body);

    jumpToBlock(switchExitBB, stmt);
    ctx->currentBB = switchExitBB;

    ctx->switchTable = oldSwitchTable;
    ctx->breakBB = oldBreakBB;
    ctx->defaultCaseBB = oldDefaultCaseBB;

    return FALSE;
}

static void collectTranslationInfoExpr(const AstExpression *expr);
static void collectTranslationInfoStmt(const AstStatement *stmt);

static void collectRerenecedLabelsInit(const AstInitializer *init) {
    switch (init->kind) {
    case IK_EXPRESSION:
      return collectTranslationInfoExpr(init->expression);
    case IK_LIST:
      for (const AstInitializerList *inits = init->initializerList; inits != NULL; inits = inits->next) {
        collectRerenecedLabelsInit(inits->initializer);
      }
      break;
    default: unreachable("Unexpected initializer type");
    }
}

static void collectTranslationInfoExpr(const AstExpression *expr) {
    switch (expr->op) {
      case E_PAREN:
        return collectTranslationInfoExpr(expr->parened);
      case E_BLOCK:
        return collectTranslationInfoStmt(expr->block);
      case E_CONST:
      case E_NAMEREF:
        return ;
      case E_VA_ARG:
        return collectTranslationInfoExpr(expr->vaArg.va_list);
      case E_COMPOUND:
        return collectRerenecedLabelsInit(expr->compound);
      case E_CALL:
        collectTranslationInfoExpr(expr->callExpr.callee);
        for (const AstExpressionList *arg = expr->callExpr.arguments; arg; arg = arg->next) {
            collectTranslationInfoExpr(arg->expression);
        }
        return ;
      case E_TERNARY:
        collectTranslationInfoExpr(expr->ternaryExpr.condition);
        collectTranslationInfoExpr(expr->ternaryExpr.ifTrue);
        collectTranslationInfoExpr(expr->ternaryExpr.ifFalse);
        return;
      case E_BIT_EXTEND:
        return collectTranslationInfoExpr(expr->extendExpr.argument);
      case E_CAST:
        return collectTranslationInfoExpr(expr->castExpr.argument);
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
        collectTranslationInfoExpr(expr->binaryExpr.left);
        collectTranslationInfoExpr(expr->binaryExpr.right);
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
        return collectTranslationInfoExpr(expr->unaryExpr.argument);
      case EF_DOT:
      case EF_ARROW:
        return collectTranslationInfoExpr(expr->fieldExpr.recevier);
      case E_LABEL_REF: {
            const char *label = expr->label;
            IrBasicBlock *labelBlock = getOrCreateLabelBlock(label);
            for (IrBasicBlockListNode *n = ctx->referencedBlocks.head; n; n = n->next) {
                if (n->block == labelBlock)
                    return;
            }
            addBBTail(&ctx->referencedBlocks, labelBlock);
            return;
        }

    default: unreachable("unexpected expression op");
    }
}

static void collectTranslationInfoStmt(const AstStatement *stmt) {

    switch (stmt->statementKind) {
      case SK_BLOCK: {
          AstStatementList *stmts = stmt->block.stmts;
          while (stmts) {
              collectTranslationInfoStmt(stmts->stmt);
              stmts = stmts->next;
          }
          break;
      }
      case SK_DECLARATION:
        if (stmt->declStmt.declaration->kind == DK_VAR) {
            const AstValueDeclaration *v = stmt->declStmt.declaration->variableDeclaration;
            if (v->initializer && v->flags.bits.isLocal) {
                return collectRerenecedLabelsInit(v->initializer);
            }
        }
        return;
      case SK_BREAK:
      case SK_CONTINUE:
        return;
      case SK_RETURN:
      case SK_GOTO_P:
        if (stmt->jumpStmt.expression) {
            collectTranslationInfoExpr(stmt->jumpStmt.expression);
        }
        return;
      case SK_EXPR_STMT:
        collectTranslationInfoExpr(stmt->exprStmt.expression);
        return;

      case SK_IF:
        collectTranslationInfoExpr(stmt->ifStmt.condition);
        collectTranslationInfoStmt(stmt->ifStmt.thenBranch);
        if (stmt->ifStmt.elseBranch) {
            collectTranslationInfoStmt(stmt->ifStmt.elseBranch);
        }
        return;
      case SK_SWITCH:
        collectTranslationInfoExpr(stmt->switchStmt.condition);
        collectTranslationInfoStmt(stmt->switchStmt.body);
        return;
      case SK_WHILE:
      case SK_DO_WHILE:
        collectTranslationInfoExpr(stmt->loopStmt.condition);
        collectTranslationInfoStmt(stmt->loopStmt.body);
        return;
      case SK_FOR: {
        const AstStatementList *init = stmt->forStmt.initial;
        while (init) {
            collectTranslationInfoStmt(init->stmt);
            init = init->next;
        }
        if (stmt->forStmt.condition) {
            collectTranslationInfoExpr(stmt->forStmt.condition);
        }
        if (stmt->forStmt.modifier) {
            collectTranslationInfoExpr(stmt->forStmt.modifier);
        }
        collectTranslationInfoStmt(stmt->forStmt.body);
        return;
        }
      case SK_LABEL:
        return collectTranslationInfoStmt(stmt->labelStmt.body);
      default: unreachable("Unknown statement kind");
    }
}

static void collectTranslationInfo(const AstStatement *body) {
    assert(ctx->labelMap != NULL && "Label map need to be allocated at this point");
    assert(ctx->localOperandMap != NULL && "Local Operand map need to be allocated at this point");

    collectTranslationInfoStmt(body);
}

static void generateExitBlock(IrFunction *func) {
  ctx->currentBB = func->exit;

  IrInstruction *ret = newInstruction(IR_RET);
  addInstruction(ret);
  if (func->retOperand) {
	addInstructionUse(ret, func->retOperand);
  }
}

typedef struct {
    AstValueDeclaration *declaration;
    LocalValueInfo *lvi;
    union {
        int32_t stackOffset;
        int32_t pregId;
    } loc;
    uint32_t idx;
    Boolean isRegister;
} ParamtersABIInfo;

static void initializeParamterLocal(IrBasicBlock *entryBB, IrOperand *stackPtrOp, ParamtersABIInfo *paramInfo) {

    AstValueDeclaration *param = paramInfo->declaration;
    TypeRef *astType = param->type;
    uint32_t paramIndex = paramInfo->idx;
    enum IrTypeKind type = typeRefToIrType(param->type);
    LocalValueInfo *lvi = paramInfo->lvi;

    ctx->currentBB = entryBB;

    if (isCompositeType(astType)) {
      // aggregate type
      IrOperand *op = newVreg(type);
      op->ast.v = param;
      param->index2 = paramIndex;
      op->data.lid = paramIndex;
      lvi->initialOp = op;
      lvi->declaration = param;

      IrOperand *offset = createIntegerConstant(IR_I64, paramInfo->loc.stackOffset);
      IrInstruction *addInstr = newInstruction(IR_E_ADD);
      addInstructionUse(addInstr, stackPtrOp);
      addInstructionUse(addInstr, offset);
      addInstructionDef(addInstr, op);
      addInstruction(addInstr);
    } else {
      IrOperand *op = newIrOperand(type, IR_LOCAL);
      op->ast.v = param;
      param->index2 = paramIndex;
      op->data.lid = paramIndex;
      lvi->initialOp = op;
      lvi->declaration = param;

      if (paramInfo->isRegister) {
        // scalar type in register
        IrOperand *inputRegister = newPreg(type, paramInfo->loc.pregId);
        inputRegister->astType = astType;
        inputRegister->ast.v = param;

        IrInstruction *moveInstr = newInstruction(IR_MOVE);
        addInstructionUse(moveInstr, inputRegister);
        addInstructionDef(moveInstr, op);
        addInstruction(moveInstr);
      } else {
        IrOperand *offset = createIntegerConstant(IR_I64, paramInfo->loc.stackOffset);
        IrInstruction *loadInstr = newInstruction(IR_M_LOAD);
        addInstructionUse(loadInstr, stackPtrOp);
        addInstructionUse(loadInstr, offset);
        addInstructionDef(loadInstr, op);
        addInstruction(loadInstr);
        // scalar type in stack
      }
    }
    // frameOffset += alignSize(computeTypeSize(param->type), sizeof (intptr_t));
}

static const uint32_t R_FP_PARAM_COUNT = 10;
static const uint32_t R_PARAM_COUNT = 10;

static uint32_t computeParametersABIInfo(AstFunctionDeclaration *declaration, ParamtersABIInfo *infos, size_t numberOfParams, LocalValueInfo *lvis) {

  unsigned intRegParams = 0;
  unsigned fpRegParams = 0;

  int32_t baseOffset = 0; // from rbp;
  int32_t stackParamOffset = sizeof(intptr_t) + sizeof(intptr_t); // rbp itself + return pc

  uint32_t idx = 0;
  for (AstValueDeclaration *param = declaration->parameters; param; param = param->next, idx++) {
    TypeRef *paramType = param->type;
    assert(idx < numberOfParams);
    ParamtersABIInfo *pi = &infos[idx];
    LocalValueInfo *lvi = &lvis[idx];
    pi->lvi = lvi;
    pi->idx = idx;
    pi->declaration = param;

    size_t size = max(computeTypeSize(paramType), sizeof(intptr_t));
    size_t align = max(typeAlignment(paramType), sizeof(intptr_t));

    if (isCompositeType(paramType) && size > sizeof(intptr_t)) {
      int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
      pi->isRegister = FALSE;
      pi->loc.stackOffset = alignedOffset;
    } else if (isRealType(paramType)) {
        if (fpRegParams < R_FP_PARAM_COUNT && size <= 8) {
          pi->isRegister = TRUE;
          pi->loc.pregId = fpRegParams++;
        } else {
          int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
          pi->isRegister = FALSE;
          pi->loc.stackOffset = alignedOffset;
          stackParamOffset = alignedOffset + size;
        }
    } else {
      if (intRegParams < R_PARAM_COUNT) {
        baseOffset += size;
        baseOffset = ALIGN_SIZE(baseOffset, align);
        pi->isRegister = TRUE;
        pi->loc.pregId = intRegParams++;
      } else {
        int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
        pi->isRegister = FALSE;
        pi->loc.stackOffset = alignedOffset;
        stackParamOffset = alignedOffset + size;
      }
    }
  }

  return idx;
}

static uint32_t buildInitialIr(IrFunction *func, AstFunctionDefinition *function) {
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

    size_t numOfReturnSlots = 0;
    if (!isVoidType(declaration->returnType)) {
        enum IrTypeKind type = typeRefToIrType(declaration->returnType);
        func->retOperand = newIrOperand(type, IR_LOCAL);
        numOfReturnSlots = 1;
    }

    LocalValueInfo *localOperandsMap = areanAllocate(ctx->irArena, (numOfParams + numOfLocals + numOfReturnSlots) * sizeof (LocalValueInfo));
    ctx->localOperandMap = localOperandsMap;
    func->localOperandMap = localOperandsMap;
    func->numOfLocals = numOfParams + numOfLocals;

    int32_t frameOffset = 0;


    ParamtersABIInfo *paramABIInfo = heapAllocate(numOfParams * sizeof (ParamtersABIInfo));

    static const uint32_t R_SP = 3;
    IrOperand *stackPtrOp = ctx->stackOp = newPreg(IR_PTR, R_SP);

    computeParametersABIInfo(declaration, paramABIInfo, numOfParams, localOperandsMap);
    size_t idx =  0;
    for (AstValueDeclaration *param = declaration->parameters;
         param != NULL;
         param = param->next, ++idx) {
        ParamtersABIInfo *pi = &paramABIInfo[idx];
        initializeParamterLocal(func->entry, stackPtrOp, pi);
        frameOffset += alignSize(computeTypeSize(param->type), sizeof (intptr_t));
    }

	printf("idx = %lu, param count = %lu\n", idx, numOfParams);
    assert(idx == numOfParams);

    for (local = function->locals;
         local != NULL;
         local = local->next, ++idx) {
        local->index2 = idx;
    }

	printf("idx = %lu (%lu), numOfLocals = %lu\n", idx, idx - numOfParams, numOfLocals);
    assert((idx - numOfParams)  == numOfLocals);

    if (numOfReturnSlots) {
      localOperandsMap[idx].initialOp = func->retOperand;
      func->retOperand->data.lid = idx;
    }

    IrOperand *frameOp = ctx->frameOp = newIrOperand(IR_PTR, IR_FRAME_PTR);

    AstStatement *body = function->body;
    assert(body->statementKind == SK_BLOCK);

    collectTranslationInfo(body);

    IrBasicBlock *firstBB = ctx->currentBB = newBasicBlock("<FIRST>");
    translateBlock(body);
    if (ctx->currentBB) {
      gotoToBlock(func->exit);
    }
	generateExitBlock(func);

    ctx->currentBB = func->entry;
    gotoToBlock(firstBB);
    ctx->currentBB = NULL;

    ctx->localOperandMap = NULL;

    return 0;
}

static IrFunction *translateFunction(AstFunctionDefinition *function) {
    ctx->bbCnt = ctx->vregCnt = ctx->opCnt = 0;
    IrFunction *func = newIrFunction(function);

    buildInitialIr(func, function);
    buildSSA(func);

    return func;
}
