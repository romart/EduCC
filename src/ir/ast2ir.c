
#include <assert.h>
#include "ir/ir.h"
#include "tree.h"
#include "types.h"
#include "sema.h"
#include <signal.h>

static const uint32_t R_FP_PARAM_COUNT = 10;
static const uint32_t R_PARAM_COUNT = 10;


extern IrContext *ctx;

static IrFunction *translateFunction(AstFunctionDefinition *function);
static Boolean translateStatement(AstStatement *stmt);
static Boolean translateBlock(AstStatement *block);
static Boolean translateStatement(AstStatement *stmt);
static Boolean translateDeclaration(AstDeclaration *decl);
static IrInstruction *translateExpression(AstExpression *expr);
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

static IrInstruction *encodeBitField(const TypeRef *type, IrInstruction *storageOp, IrInstruction *valueOp) {
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

	IrInstruction *storageMask = newInstruction(IR_E_AND, irMemoryType);
	IrInstruction *mask1Op = createIntegerConstant(irMemoryType, ~mask);
    addInstructionInput(storageMask, storageOp);
    addInstructionInput(storageMask, mask1Op);
    addInstruction(storageMask);

	IrInstruction *shiftValueInstr = newInstruction(IR_E_SHL, irMemoryType);
	IrInstruction *shiftOp = createIntegerConstant(irMemoryType, s);
    addInstructionInput(shiftValueInstr, storageMask);
    addInstructionInput(shiftValueInstr, shiftOp);
    addInstruction(shiftValueInstr);

	IrInstruction *maskValueInstr = newInstruction(IR_E_AND, irMemoryType);
	IrInstruction *mask2Op = createIntegerConstant(irMemoryType, mask);
    addInstructionInput(maskValueInstr, shiftValueInstr);
    addInstructionInput(maskValueInstr, mask2Op);
    addInstruction(maskValueInstr);

	IrInstruction *mergeInstr = newInstruction(IR_E_OR, irMemoryType);
    addInstructionInput(mergeInstr, storageMask);
    addInstructionInput(mergeInstr, maskValueInstr);
    addInstruction(mergeInstr);

    return mergeInstr;
}

static IrInstruction *decodeBitField(const TypeRef *type, IrInstruction *storageOp) {
    assert(type->kind == TR_BITFIELD);
    uint64_t w = type->bitFieldDesc.width;
    uint64_t mask = ~(~0LLu << w);
    uint64_t s = type->bitFieldDesc.offset;

	TypeRef *memoryType = type->bitFieldDesc.storageType;

    size_t storageSize = computeTypeSize(memoryType);
    size_t W = storageSize * 8;
    int32_t l = W - (w + s);

	enum IrTypeKind irMemoryType = typeRefToIrType(memoryType);

    IrInstruction *shlSizeOp = createIntegerConstant(IR_I32, l);
    IrInstruction *shlInstr = newInstruction(IR_E_SHL, irMemoryType);
    addInstructionInput(shlInstr, storageOp);
    addInstructionInput(shlInstr, shlSizeOp);
    addInstruction(shlInstr);

    int32_t r = W - w;
    IrInstruction *shrSizeOp = createIntegerConstant(IR_I32, r);

    IrInstruction *shrInstr = newInstruction(IR_E_SHR, irMemoryType);
    addInstructionInput(shrInstr, shlInstr);
    addInstructionInput(shrInstr, shrSizeOp);
    addInstruction(shrInstr);

    // TODO: sign extend

    return shrInstr;
}

IrInstruction *createAllocaInstr(IrInstruction *sizeOp) {
    IrInstruction *allocaInstr = newInstruction(IR_ALLOCA, IR_PTR);

    addInstructionInput(allocaInstr, sizeOp);
    addInstruction(allocaInstr);

    addInstructionToVector(&ctx->allocas, allocaInstr);

	return allocaInstr;
}

IrInstruction *createAllocaSlot(size_t slotSize) {
  slotSize = alignSize(slotSize, sizeof(intptr_t));
  IrInstruction *sizeOp = createIntegerConstant(IR_U64, slotSize);
  IrInstruction *allocaInstr = createAllocaInstr(sizeOp);

  allocaInstr->info.alloca.stackSize = slotSize;

  return allocaInstr;
}

static IrInstruction *generateCompositeCopy(const TypeRef *type, IrInstruction *src, IrInstruction *dst, const AstExpression *ast) {
	assert(isCompositeType(type));

  int32_t align = type->descriptorDesc->typeDefinition->align;
  int32_t size = computeTypeSize(type);
  int32_t copied = 0;

  IrInstruction *sizeOp = createIntegerConstant(IR_U64, size);
  IrInstruction *copyInstr = newMemoryCopyInstruction(dst, src, sizeOp, type);
  addInstruction(copyInstr);
  return copyInstr;
}

static IrInstruction *translateLValue(AstExpression *expr) {
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_LVALUE;
    IrInstruction *lvalue = translateExpression(expr);
    ctx->addressTM = tm;
    return lvalue;
}

static IrInstruction *translateRValue(AstExpression *expr) {
    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrInstruction *rvalue = translateExpression(expr);
    ctx->addressTM = tm;
    return rvalue;
}

// -============================ translators ============================-

static size_t translateInitializerIntoMemory(IrInstruction *base, int32_t offset, size_t typeSize, const AstInitializer *initializer) {

  assert(ctx->addressTM == IR_TM_RVALUE);

  switch (initializer->kind) {
  case IK_EXPRESSION: {
	AstExpression *expr = initializer->expression;
	const size_t expeSize = computeTypeSize(expr->type);
	const TypeRef *slotType = initializer->slotType;
	const size_t slotSize = computeTypeSize(slotType);
	const int32_t slotOffset = initializer->offset;
	const int32_t emitOffset = offset + slotOffset;

    /*
	if (skipNull &&  isNullConst(expr)) {
	  return offset + slotSize;
	}
    */

	if (expr->op == E_COMPOUND) {
	  const int32_t emittedOffset = translateInitializerIntoMemory(base, emitOffset, typeSize, expr->compound);
	  assert(emittedOffset == emitOffset + slotSize);
	  return emittedOffset;
	}

	IrInstruction *valueOp = translateRValue(expr);
	if ((emitOffset + slotSize) <= typeSize) {
      IrInstruction *ptr = base;
      if (emitOffset != 0) {
        IrInstruction *offsetOp = createIntegerConstant(IR_I64, emitOffset);
        IrInstruction *gepInstr = newGEPInstruction(base, offsetOp, slotType);
        gepInstr->meta.astExpr = expr;
        addInstruction(gepInstr);
        ptr = gepInstr;
      }

	  if (isCompositeType(slotType)) {
        generateCompositeCopy(slotType, valueOp, ptr, expr);
	  } else if (slotType->kind == TR_BITFIELD) {
        TypeRef *storageType = slotType->bitFieldDesc.storageType;
        enum IrTypeKind irST = typeRefToIrType(storageType);
        IrInstruction *storage = addLoadInstr(irST, ptr, NULL);
        storage->astType = storageType;
        IrInstruction *encodedValue = encodeBitField(slotType, storage, valueOp);
        addStoreInstr(ptr, encodedValue, NULL);
	  } else {
        addStoreInstr(ptr, valueOp, NULL);
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

static IrInstruction *translateConstant(AstExpression *expr) {
    assert(expr->op == E_CONST);
    // Think about representation
    enum IrTypeKind constType = expr->constExpr.op == CK_STRING_LITERAL
            ? IR_LITERAL
            : typeRefToIrType(expr->type);

    switch (expr->constExpr.op) {
    case CK_INT_CONST:
        return createIntegerConstant(constType, expr->constExpr.i);
    case CK_FLOAT_CONST:
        return createFloatConstant(constType, expr->constExpr.f);
    case CK_STRING_LITERAL:
        return createLiteralConstant(expr->constExpr.l.s, expr->constExpr.l.length);
    }

    unreachable("Unknown constant kind");
}


static IrInstruction *computeVAListValuePtr(IrInstruction *valistInstr, IrBasicBlock *memoryBlock, IrBasicBlock *updateBlock, IrBasicBlock *doneBlock, TypeDefiniton *vastruct, const char *offsetMemberName, size_t areaBound) {
  const static int32_t dataSize = sizeof(intptr_t);
  IrInstruction *dataSizeInstr = createIntegerConstant(IR_I64, dataSize);
  StructualMember *rsam = findStructualMember(vastruct, "reg_save_area");
  enum IrTypeKind irRSAType = typeRefToIrType(rsam->type);

  StructualMember *m = findStructualMember(vastruct, offsetMemberName);
  enum IrTypeKind irOffsetType = typeRefToIrType(m->type);
  assert(m != NULL);
  IrInstruction *offsetOff = createIntegerConstant(IR_I64, m->offset);
  IrInstruction *offsetGep = newGEPInstruction(valistInstr, offsetOff, m->type);
  addInstruction(offsetGep);
  IrInstruction *offValue = addLoadInstr(IR_I64, offsetGep, NULL);
  offValue->astType = m->type;
  IrInstruction *areaSize = createIntegerConstant(irOffsetType, areaBound * dataSize);

  IrInstruction *cmpgeInstr = addBinaryOpeartion(IR_E_GE, offValue, areaSize, IR_BOOL, NULL, NULL);

  IrInstruction *condInstr = newCondBranch(cmpgeInstr, memoryBlock, updateBlock);
  addSuccessor(ctx->currentBB, memoryBlock);
  addSuccessor(ctx->currentBB, updateBlock);
  termintateBlock(condInstr);

  ctx->currentBB = updateBlock;

  IrInstruction *regSaveOff = createIntegerConstant(IR_I64, rsam->offset);
  IrInstruction *regSaveGEP = newGEPInstruction(valistInstr, regSaveOff, rsam->type);
  addInstruction(regSaveGEP);

  IrInstruction *regSaveAreaValue = addLoadInstr(irRSAType, regSaveGEP, NULL);
  regSaveAreaValue->astType = rsam->type;

  IrInstruction *areaPtr = addBinaryOpeartion(IR_E_ADD, regSaveAreaValue, offValue, irRSAType, rsam->type, NULL);

  IrInstruction *newAreaOffset = addBinaryOpeartion(IR_E_ADD, offValue, dataSizeInstr, irOffsetType, m->type, NULL);
  addStoreInstr(offsetGep, newAreaOffset, NULL);
  gotoToBlock(doneBlock);

  return areaPtr;
}

static IrInstruction *translateVaArg(AstExpression *expr) {
    assert(expr->op == E_VA_ARG);

    IrInstruction *valistInstr = translateRValue(expr->vaArg.va_list);
    TypeRef *vatype = expr->vaArg.argType;
    TypeRef *valistType = expr->vaArg.va_list->type;

    assert(is_va_list_Type(valistType));
    TypeDefiniton *vastruct = valistType->pointed->descriptorDesc->typeDefinition;

    const static int32_t dataSize = sizeof(intptr_t);
    IrInstruction *dataSizeInstr = createIntegerConstant(IR_I64, dataSize);

    /**
     * typedef struct {
     *   intptr_t gp_offset;
     *   intptr_t fp_offset;
     *   void *overflow_arg_area;
     *   const void *reg_save_area;
     * } __va_elem;
     */

    IrBasicBlock *memoryBlock = newBasicBlock("<va_arg_mem>");
    IrBasicBlock *updateBlock = newBasicBlock("<va_arg_update>");
    IrBasicBlock *doneBlock = newBasicBlock("<va_arg_done>");

    IrInstruction *valuePtr = NULL;

    if (isRealType(vatype)) {
      valuePtr = computeVAListValuePtr(valistInstr, memoryBlock, updateBlock, doneBlock, vastruct, "fp_offset", R_PARAM_COUNT + R_FP_PARAM_COUNT);
    } else if (isScalarType(vatype)) {
      valuePtr = computeVAListValuePtr(valistInstr, memoryBlock, updateBlock, doneBlock, vastruct, "gp_offset", R_PARAM_COUNT);
    } else {
      unreachable("WTF in va args??");
    }

    assert(valuePtr != NULL);

    ctx->currentBB = memoryBlock;

    StructualMember *oaam = findStructualMember(vastruct, "overflow_arg_area");
    assert(oaam != NULL);

    IrInstruction *overflowAreaOffset = createIntegerConstant(IR_I64, oaam->offset);
    enum IrTypeKind irOAType = typeRefToIrType(oaam->type);

    IrInstruction *overflowAreaGep = newGEPInstruction(valistInstr, overflowAreaOffset, oaam->type);
    addInstruction(overflowAreaGep);

    IrInstruction *overflowAreaValue = addLoadInstr(irOAType, overflowAreaGep, NULL);
    overflowAreaValue->astType = oaam->type;

    int32_t align = typeAlignment(vatype);

    if (align > 8) {
      int32_t mask = ~(align - 1);
      IrInstruction *alignC = createIntegerConstant(IR_I64, align - 1);
      IrInstruction *addInstr = addBinaryOpeartion(IR_E_ADD, overflowAreaValue, alignC, valuePtr->type, valuePtr->astType, NULL);
      IrInstruction *maskC = createIntegerConstant(IR_I64, mask);
      overflowAreaValue = addBinaryOpeartion(IR_E_AND, addInstr, maskC, valuePtr->type, valuePtr->astType, NULL);
    }

    int32_t argSize = max(8, computeTypeSize(vatype));

    IrInstruction *alignesArgSize = createIntegerConstant(IR_I64, ALIGN_SIZE(argSize, dataSize));
    IrInstruction *newOverflowArea = addBinaryOpeartion(IR_E_ADD, overflowAreaValue, alignesArgSize, irOAType, oaam->type, NULL);

    addStoreInstr(overflowAreaGep, newOverflowArea, NULL);

    gotoToBlock(doneBlock);

    ctx->currentBB = doneBlock;
    enum IrTypeKind irVaType = typeRefToIrType(vatype);
    IrInstruction *phiInstr = newPhiInstruction(irVaType);

    addPhiInput(phiInstr, valuePtr, updateBlock);
    addPhiInput(phiInstr, overflowAreaValue, memoryBlock);
    phiInstr->astType = vatype;
    addInstructionHead(doneBlock, phiInstr);

    return phiInstr;
}

static IrInstruction *translateNameRef(AstExpression *expr) {
    assert(expr->op == E_NAMEREF);

    // If this is called we probably want a function reference.
    // Value references go through DEREF node

    Symbol *s = expr->nameRefExpr.s;

    if (s->kind == FunctionSymbol || s->kind == ValueSymbol && !(s->variableDesc->flags.bits.isLocal || s->variableDesc->kind != VD_PARAMETER)) {
        // Either Function of non-local variable reference
        return createSymbolConstant(s);
    } else if (s->kind == ValueSymbol) {
        AstValueDeclaration *v = s->variableDesc;
        printf("Translate referenece to variable[%u] %s...", v->index2, v->name);

        if (v->kind == VD_PARAMETER || v->flags.bits.isLocal) {
          assert(v->index2 != -1);
          assert(v->flags.bits.isLocal);
          LocalValueInfo *info = &ctx->localOperandMap[v->index2];
          assert(info != NULL);
          assert(info->stackSlot != NULL);
          printf(" found local stack slot %p at index %u\n", info->stackSlot, v->index2);
          return info->stackSlot;
        } else {
          return createSymbolConstant(s);
        }
    } else {
      unreachable("Unexpected Symbol type");
      return NULL;
    }
}

static IrInstruction *translateCompound(AstExpression *expr) {
    assert(expr->op == E_COMPOUND);

    size_t typeSize = computeTypeSize(expr->type);
	IrInstruction *memoryOp = createAllocaSlot(typeSize);
    memoryOp->info.alloca.valueType = typeRefToIrType(expr->type);

	translateInitializerIntoMemory(memoryOp, 0, typeSize, expr->compound);

	return memoryOp;
}

static IrInstruction *maybeTranslateAlloca(AstExpression *expr) {
  AstExpression *callee = expr->callExpr.callee;

  if (callee->op != E_NAMEREF) return NULL;
  if (strcmp("alloca", callee->nameRefExpr.s->name)) return NULL;

  const int32_t dataSize = sizeof(intptr_t);
  AstExpressionList *args = expr->callExpr.arguments;
  assert(args->next == NULL);

  IrInstruction *sizeOp = translateRValue(args->expression);
  IrInstruction *allocaInstr = createAllocaInstr(sizeOp);
  allocaInstr->astType = expr->type;
  allocaInstr->meta.astExpr = expr;
  allocaInstr->info.alloca.sizeInstr = sizeOp;
  allocaInstr->info.alloca.valueType = IR_U8;

  return allocaInstr;
}

static IrInstruction *translateCall(AstExpression *expr) {
    assert(expr->op == E_CALL);

    IrInstruction *allocaInstr = maybeTranslateAlloca(expr);
    if (allocaInstr != NULL)
      return allocaInstr;

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

	IrInstruction *calleeOp = translateExpression(callee);
	IrInstruction *callInstr = newInstruction(IR_CALL, typeRefToIrType(expr->type));
    addInstructionInput(callInstr, calleeOp);

    callInstr->astType = expr->type;
    callInstr->meta.astExpr = expr;

	IrInstruction *returnSlotOp = NULL;

	// TODO: save and restore stack after series of alloca's

	if (isCompositeType(returnType) && returnTypeSize > sizeof (intptr_t)) {
	  returnSlotOp = createAllocaSlot(returnTypeSize);
      returnSlotOp->info.alloca.valueType = typeRefToIrType(returnType);
      addInstructionInput(callInstr, returnSlotOp);
      callInstr->info.call.returnBuffer = returnSlotOp;
  	}

	for (AstExpressionList *args = expr->callExpr.arguments;
		 args != NULL;
		 args = args->next) {
	  AstExpression *argExpr = args->expression;
	  TypeRef *argType = argExpr->type;
      unsigned alignent = max(8, typeAlignment(argType));
      unsigned argSize = max(8, computeTypeSize(argType));

	  IrInstruction *argOp = translateExpression(argExpr);
	  IrInstruction *realArgOp = NULL;

	  if (isCompositeType(argType)) {
		if (argSize > sizeof (intptr_t)) {
		  // TODO: make sure this should be in argument list
		  realArgOp = createAllocaSlot(argSize);
          realArgOp->info.alloca.valueType = typeRefToIrType(argType);
		  generateCompositeCopy(argType, argOp, realArgOp, expr);
		} else {
		  IrInstruction *offset = createIntegerConstant(IR_I64, 0);
          IrInstruction *gep = newGEPInstruction(argOp, offset, argType);
          addInstruction(gep);
		  realArgOp = addLoadInstr(IR_P_AGG, gep, expr);
          gep->meta.astExpr = realArgOp->meta.astExpr = argExpr;
		}
	  } else {
		realArgOp = argOp;
	  }

	  addInstructionInput(callInstr, realArgOp);
	}

	addInstruction(callInstr);

	ctx->addressTM = tm;

    return callInstr;
}

static IrInstruction *translateTernary(AstExpression *expr) {
    assert(expr->op == E_TERNARY);

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;

    IrInstruction *condOp = translateExpression(expr->ternaryExpr.condition);

    IrBasicBlock *ifTrue = newBasicBlock("<ifTrue>");
    IrBasicBlock *ifFalse = newBasicBlock("<ifFalse>");
    IrBasicBlock *exit = newBasicBlock("<ternary_exit>");

    IrInstruction *cond = newCondBranch(condOp, ifTrue, ifFalse);
    addSuccessor(ctx->currentBB, ifTrue);
    addSuccessor(ctx->currentBB, ifFalse);
    termintateBlock(cond);

    ctx->currentBB = ifTrue;
    IrInstruction *ifTrueOp = translateExpression(expr->ternaryExpr.ifTrue);
    gotoToBlock(exit);

    ctx->currentBB = ifFalse;
    IrInstruction *ifFalseOp = translateExpression(expr->ternaryExpr.ifFalse);
    gotoToBlock(exit);

    ctx->currentBB = exit;
    assert(ifTrueOp->type == ifFalseOp->type);
    // TODO: what if type is composite?
    IrInstruction *phi = newPhiInstruction(typeRefToIrType(expr->type));

    addPhiInput(phi, ifTrueOp, ifTrue);
    addPhiInput(phi, ifFalseOp, ifFalse);

    ctx->addressTM = tm;

    return phi;
}

static IrInstruction *translateBitExtend(AstExpression *expr) {
    assert(expr->op == E_BIT_EXTEND);
    unimplemented("Bit Extend Expression");
    return NULL;
}

static IrInstruction *translateCast(AstExpression *expr) {
    assert(expr->op == E_CAST);
    TypeRef *fromType = expr->castExpr.argument->type;
    TypeRef *toType = expr->castExpr.type;

	enum IrTypeKind irFromType = typeRefToIrType(fromType);
	enum IrTypeKind irToType = typeRefToIrType(toType);

	IrInstruction *src = translateRValue(expr->castExpr.argument);
	IrInstruction *castInstr = newInstruction(IR_E_BITCAST, irToType);

    addInstructionInput(castInstr, src);
	addInstruction(castInstr);

	castInstr->info.fromCastType = irFromType;
    castInstr->astType = toType;
    castInstr->meta.astExpr = expr;

    return castInstr;
}

static IrInstruction *translateLogicalExpression(AstExpression *expr) {
    assert(expr->op == EB_ANDAND || expr->op == EB_OROR);

    Boolean isAndAnd = expr->op == EB_ANDAND;
    IrInstruction *leftOp = translateRValue(expr->binaryExpr.left);

    IrBasicBlock *fst = ctx->currentBB;
    IrBasicBlock *scnd = newBasicBlock(isAndAnd ? "<&&>" : "<||>");
    IrBasicBlock *exit = newBasicBlock(isAndAnd ? "<&&-exit>" : "<||-exit>");

    IrInstruction *cond = newCondBranch(leftOp, isAndAnd ? scnd : exit, isAndAnd ? exit : scnd);
    addSuccessor(ctx->currentBB, scnd);
    addSuccessor(ctx->currentBB, exit);
    termintateBlock(cond);

    ctx->currentBB = scnd;
    IrInstruction *rightOp = translateRValue(expr->binaryExpr.right);
    gotoToBlock(exit);

    ctx->currentBB = exit;
    IrInstruction *phi = newPhiInstruction(IR_BOOL);
    addPhiInput(phi, leftOp, fst);
    addPhiInput(phi, rightOp, scnd);

    addInstruction(phi);

    phi->meta.astExpr = expr;
    phi->astType = expr->type;

    return phi;
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

static IrInstruction *translateBinary(AstExpression *expr) {
    assert(isBinary(expr->op));

    const enum IrTranslationMode tm = ctx->addressTM;
    ctx->addressTM = IR_TM_RVALUE;
    IrInstruction *leftOp = translateExpression(expr->binaryExpr.left);
    IrInstruction *rightOp = translateExpression(expr->binaryExpr.right);
    ctx->addressTM = tm;

    enum IrTypeKind type = typeRefToIrType(expr->type);
    Boolean isFloatOperand = isRealType(expr->binaryExpr.left->type);
    enum IrIntructionKind k = getBinaryArith(expr->op, isFloatOperand);

    assert(k != IR_BAD);

    assert(leftOp != NULL);
    assert(rightOp != NULL);
    // NOTE: pointer arithmethic is desugared during parser phase
    IrInstruction *instr = newInstruction(k, type);
    addInstructionInput(instr, leftOp);
    addInstructionInput(instr, rightOp);

    addInstruction(instr);

    instr->meta.astExpr = expr;
    instr->astType = expr->type;

    return instr;
}

static IrInstruction *translateAssignment(AstExpression *expr) {
    assert(expr->op == EB_ASSIGN);

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;

    const enum IrTranslationMode tm = ctx->addressTM;
    IrInstruction *lvalue = translateLValue(assignee);
    IrInstruction *rvalue = translateRValue(value);

    // TODO: heavy_copy_1 = heavy_copy_2 = heavy_struct

    if (isCompositeType(value->type)) {
	  generateCompositeCopy(value->type, rvalue, lvalue, expr);
    } else {
		if (value->type->kind == TR_BITFIELD) {
		  enum IrTypeKind irMemType = typeRefToIrType(value->type->bitFieldDesc.storageType);
		  IrInstruction *storageOp = addLoadInstr(irMemType, lvalue, expr);
		  rvalue = encodeBitField(value->type, storageOp, rvalue);
		}
	  	addStoreInstr(lvalue, rvalue, expr);
    }
    return rvalue;
}

static IrInstruction *translateAssignArith(AstExpression *expr) {
    assert(isAssignmentArith(expr->op));

    AstExpression *assignee = expr->binaryExpr.left;
    AstExpression *value = expr->binaryExpr.right;
    assert(!isFlatType(value->type) && "Forbiden operation in C");

    IrInstruction *lvalue = translateLValue(assignee);
    IrInstruction *rvalue = translateRValue(value);

    Boolean isFloat = isRealType(value->type);
    ExpressionType binaryArith = assignArithToArith(expr->op);
    enum IrIntructionKind ik = getBinaryArith(binaryArith, isFloat);
    enum IrTypeKind valueType = typeRefToIrType(expr->type);

	IrInstruction *storageOp = NULL;

    IrInstruction *lhs = addLoadInstr(valueType, lvalue, expr);
    if (assignee->type->kind == TR_BITFIELD) {
      storageOp = lhs;
      lhs = decodeBitField(assignee->type, storageOp);
    }

    // NOTE: Pointer arithmethic is desugared during parser phase
    IrInstruction *operation = newInstruction(ik, valueType);
    addInstructionInput(operation, lhs);
    addInstructionInput(operation, rvalue);
    addInstruction(operation);
    operation->meta.astExpr = expr;
    operation->astType = expr->type;

    IrInstruction *storeValue = operation;
    if (assignee->type->kind == TR_BITFIELD) {
      assert(storageOp != NULL);
      storeValue = encodeBitField(assignee->type, storageOp, operation);
    }
    addStoreInstr(lvalue, storeValue, expr);

    return operation;
}

static IrInstruction *translateReference(AstExpression *expr) {
    assert(expr->op == EU_REF);

    IrInstruction *lvalue = translateLValue(expr->unaryExpr.argument);
    lvalue->astType = expr->type;
    lvalue->meta.astExpr = expr;

    return lvalue;
}


static IrInstruction *translateDeReference(AstExpression *expr) {
    assert(expr->op == EU_DEREF);

    IrInstruction *lvalue = translateLValue(expr->unaryExpr.argument);
    TypeRef *valueType = expr->type;
    TypeRef *ptrType = expr->unaryExpr.argument->type;
    assert(isPointerLikeType(ptrType) || isFunctionalType(ptrType));

    if (ctx->addressTM == IR_TM_RVALUE) {
        if (isFlatType(valueType) || isFunctionalType(valueType)) {
          // Do not load aggregate types
          return lvalue;
        } else {
          enum IrTypeKind type = typeRefToIrType(expr->type);
          return addLoadInstr(type, lvalue, expr);
        }
    }

    return lvalue;
}

static IrInstruction *translateUnary(AstExpression *expr) {
    assert(isUnary(expr->op));

    TypeRef *type = expr->type;
    enum IrTypeKind irType = typeRefToIrType(expr->type);
    IrInstruction *arg = translateRValue(expr->unaryExpr.argument);

    Boolean isFloat = isRealType(type);

    Boolean exl = FALSE;

    IrInstruction *result = NULL;

    switch (expr->op) {
    case EU_PLUS: result = arg; break;
    case EU_MINUS: {
        IrInstruction *zeroConst = isFloat
          ? createFloatConstant(irType, 0.0)
          : createIntegerConstant(irType, 0);
        enum IrIntructionKind op = isFloat ? IR_E_FSUB : IR_E_SUB;
        result = newInstruction(op, irType);
        addInstructionInput(result, zeroConst);
        addInstructionInput(result, arg);
        addInstruction(result);
        break;
    }
    case EU_EXL: exl = TRUE;
    case EU_TILDA: {
        assert(!isFloat);
        enum IrIntructionKind op = exl ? IR_U_NOT : IR_U_BNOT;
        result = newInstruction(op, irType);
        addInstructionInput(result, arg);
        addInstruction(result);
        break;
    }
    default: unreachable("wtf?");
    }

    assert(result != NULL);
    result->astType = type;
    result->meta.astExpr = expr;

    return result;
}

static IrInstruction *handleMemoryMode(IrInstruction *ptr, TypeRef *valueType, AstExpression *expr) {
    if (ctx->addressTM == IR_TM_LVALUE || isFlatType(valueType))
      return ptr;

    IrInstruction *loadInstr = addLoadInstr(typeRefToIrType(valueType), ptr, expr);
    loadInstr->astType = valueType;

    if (valueType->kind == TR_BITFIELD) {
      IrInstruction *decoded = decodeBitField(valueType, loadInstr);
      decoded->astType = valueType;
      decoded->meta.astExpr = expr;
      return decoded;
    }

    return loadInstr;
}

static IrInstruction *computeVLAElementType(const TypeRef *vlaElementType) {
  if (vlaElementType->kind == TR_VLA) {
    IrInstruction *elementSizeOp = computeVLAElementType(vlaElementType->vlaDescriptor.elementType);
    AstExpression *sizeExpr = vlaElementType->vlaDescriptor.sizeExpression;
    assert(sizeExpr != NULL);
    // Cache at the array declaration
    IrInstruction *arraySize = translateRValue(sizeExpr);
    IrInstruction *mulInstr = newInstruction(IR_E_MUL, IR_U64);
    addInstructionInput(mulInstr, arraySize);
    addInstructionInput(mulInstr, elementSizeOp);
    addInstruction(mulInstr);
    mulInstr->astType = makePrimitiveType(ctx->pctx, T_U8, 0);
    return mulInstr;
  } else {
    int32_t elementSize = computeTypeSize(vlaElementType);
    return createIntegerConstant(IR_U64, elementSize);
  }
}

static IrInstruction *translateArrayAccess(AstExpression *expr) {
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

    IrInstruction *baseInstr = translateRValue(base);
    IrInstruction *indexInstr = translateRValue(index);

    IrInstruction *scaledIndexOp = NULL;

    if (elementType->kind == TR_VLA) {
        IrInstruction *vlaSize = computeVLAElementType(elementType);
        scaledIndexOp = newInstruction(IR_E_MUL, indexIrType);
        addInstructionInput(scaledIndexOp, indexInstr);
        addInstructionInput(scaledIndexOp, vlaSize);
        addInstruction(scaledIndexOp);
        scaledIndexOp->meta.astExpr = expr;
        scaledIndexOp->astType = indexType;
    } else {
        int32_t elementSize = computeTypeSize(elementType);
        if (elementSize > 1) {
            if (isPowerOf2(elementSize)) {
                IrInstruction *elementSizeOpScale = createIntegerConstant(IR_I32, log2Integer(elementSize));
                scaledIndexOp = newInstruction(IR_E_SHL, indexIrType);
                addInstructionInput(scaledIndexOp, indexInstr);
                addInstructionInput(scaledIndexOp, elementSizeOpScale);
            } else {
                IrInstruction *elementSizeOp = createIntegerConstant(indexIrType, elementSize);
                scaledIndexOp = newInstruction(IR_E_MUL, indexIrType);
                addInstructionInput(scaledIndexOp, indexInstr);
                addInstructionInput(scaledIndexOp, elementSizeOp);
            }
            addInstruction(scaledIndexOp);
            scaledIndexOp->meta.astExpr = expr;
            scaledIndexOp->astType = indexType;
        } else {
            assert(elementSize == 1);
            scaledIndexOp = indexInstr;
        }

        if (scaledIndexOp->type != IR_I64) {
            IrInstruction *castInstruction = newInstruction(IR_E_BITCAST, IR_I64);
            // TODO
            castInstruction->info.fromCastType = scaledIndexOp->type;
            castInstruction->meta.astExpr = expr;

            addInstructionInput(castInstruction, scaledIndexOp);
            addInstruction(castInstruction);
            scaledIndexOp = castInstruction;
        }
    }

    IrInstruction *gepInstr = newGEPInstruction(baseInstr, scaledIndexOp, elementType);
    gepInstr->astType = pointerType;
    gepInstr->meta.astExpr = expr;
    gepInstr->info.gep.indexInstr = indexInstr;

    return handleMemoryMode(gepInstr, elementType, expr);
}

static IrInstruction *translateFieldAccess(AstExpression *expr, Boolean isDot) {

    IrInstruction *receiver = translateLValue(expr->fieldExpr.recevier);

    int64_t memberOffset = effectiveMemberOffset(expr->fieldExpr.member);
    IrInstruction *memberOffsetOp = createIntegerConstant(IR_I64, memberOffset);
    TypeRef *memberType = expr->fieldExpr.member->type;

    IrInstruction *gepInstr = newGEPInstruction(receiver, memberOffsetOp, memberType);
    gepInstr->meta.astExpr = expr;
    gepInstr->info.gep.member = expr->fieldExpr.member;
    addInstruction(gepInstr);

    return handleMemoryMode(gepInstr, memberType, expr);
}

static IrInstruction *translateDotAccess(AstExpression *expr) {
    assert(expr->op == EF_DOT);
    assert(isCompositeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(expr, /*isDOT = */ TRUE);
}

static IrInstruction *translateArrowAccess(AstExpression *expr) {
    assert(expr->op == EF_ARROW);
    assert(isPointerLikeType(expr->fieldExpr.recevier->type));

    return translateFieldAccess(expr, /* isDOR = */ FALSE);
}

static IrInstruction *translatePreOp(AstExpression *expr) {
    assert(expr->op == EU_PRE_INC || expr->op == EU_PRE_DEC);
    unreachable("Pre ++/-- Expressions should be desugared"
                " into corresponding +=/*- operations in parser");
    return NULL;
}

static IrInstruction *translatePostOp(AstExpression *expr) {
    assert(expr->op == EU_POST_INC || expr->op == EU_POST_DEC);
    // NOTE: Pointer arith is not desugared in parser.
    // TODO: Generalize it with generic binary opeartions

    IrInstruction *lvalue = translateLValue(expr->unaryExpr.argument);

	TypeRef* type = expr->type;
  	int64_t delta = isPointerLikeType(type) ? computeTypeSize(type->pointed) : 1;
  	TypeId tid = typeToId(type);
	IrInstruction *delta_op = NULL;

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

    IrInstruction *base = NULL, *offset = NULL;
	IrInstruction *storageValue = NULL;

    IrInstruction *oldValue = addLoadInstr(irType, lvalue, expr);
    if (type->kind == TR_BITFIELD) {
        storageValue = oldValue;
        storageValue->astType = type->bitFieldDesc.storageType;
        storageValue->meta.astExpr = expr;
		oldValue = decodeBitField(type, oldValue);
        oldValue->astType = type;
        oldValue->meta.astExpr = expr;
    }

    IrInstruction *operation = newInstruction(irInstr, irType);
    addInstructionInput(operation, oldValue);
    addInstructionInput(operation, delta_op);
    addInstruction(operation);
    operation->meta.astExpr = expr;
    operation->astType = type;

    IrInstruction *newValue = operation;
    if (type->kind == TR_BITFIELD) {
      assert(storageValue != NULL);
      newValue = encodeBitField(type, storageValue, operation);
      newValue->astType = type->bitFieldDesc.storageType;
      newValue->meta.astExpr = expr;
    }

    addStoreInstr(lvalue, newValue, expr);

    return operation;
}

static IrInstruction *translateLabelRef(AstExpression *expr) {
    assert(expr->op == E_LABEL_REF);

    IrBasicBlock *target = getOrCreateLabelBlock(expr->label);

    // TODO: should such labels be put into entry?
    // TODO: should labels be constant nodes as well?
    IrInstruction *instr = newLabelInstruction(target);
    addInstruction(instr);

    return instr;
}

static IrInstruction *translateExpression(AstExpression *expr) {
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

static void translateLocalDeclaration(AstValueDeclaration *v) {
    assert(v->flags.bits.isLocal);
    assert(v->index2 >= 0);

    printf("Translate local variable '%s'..., next = %p, initizlier = %p..\n", v->name, v->next, v->initializer);

    LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
    assert(lvi->stackSlot == NULL && "double-allocated variable");

    TypeRef *astType = v->type;
    Boolean isAggregate = isCompositeType(astType);
    enum IrTypeKind irType = typeRefToIrType(astType);
    lvi->declaration = v;

    IrInstruction *sizeInstr = NULL;
    AstInitializer *init = v->initializer;
    size_t size = -1;
    if (astType->kind != TR_VLA) {
      size = ALIGN_SIZE(computeTypeSize(astType), sizeof (intptr_t));
      sizeInstr = createIntegerConstant(IR_U64, size);
    } else {
      assert(init != NULL);
      assert(init->kind == IK_EXPRESSION);
      sizeInstr = translateExpression(init->expression);
      init = NULL;
    }
    IrInstruction *stackSlot = createAllocaInstr(sizeInstr);

    stackSlot->info.alloca.v = v;
    stackSlot->info.alloca.valueType = irType;
    lvi->stackSlot = stackSlot;
    stackSlot->astType = makePointedType(ctx->pctx, 0, astType);

    if (astType->kind == TR_VLA) {
      stackSlot->info.alloca.sizeInstr = sizeInstr;
    } else {
      stackSlot->info.alloca.stackSize = size;
    }

    if (init) {
      assert(size != -1);
      printf(" translate initializer for variable '%s' (%c%u)\n", v->name, '%', stackSlot->id);
      translateInitializerIntoMemory(stackSlot, 0, size, init);
    }
}

static Boolean translateDeclaration(AstDeclaration *decl) {

    if (decl->kind == DK_VAR) {
        AstValueDeclaration *varDecl = decl->variableDeclaration;
        if (varDecl->flags.bits.isLocal) {
            assert(varDecl->index2 >= 0);
            translateLocalDeclaration(varDecl);
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

    IrBasicBlock *continueBB = newBasicBlock("<if_exit>");
    IrBasicBlock *thenBB = newBasicBlock("<if_then>");
    IrBasicBlock *elseBB = elseStmt != NULL ? newBasicBlock("<if_else>") : continueBB;

    IrInstruction *irCond = translateRValue(condition);
    IrBasicBlock *ifBB = ctx->currentBB;

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
    IrInstruction *irCond = translateRValue(condition);

    IrInstruction *irCondBranch = newCondBranch(irCond, loopBody, loopExit);
    addSuccessor(ctx->currentBB, loopBody);
    addSuccessor(ctx->currentBB, loopExit);
    termintateBlock(irCondBranch);

    ctx->currentBB = loopBody;
    translateStatement(body);

    if (ctx->currentBB && ctx->currentBB->term == NULL) {
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

    if (ctx->currentBB != NULL && ctx->currentBB->term == NULL) {
        IrInstruction *gotoTail = newGotoInstruction(loopTail);
        addSuccessor(ctx->currentBB, loopTail);
        termintateBlock(gotoTail);
    }

    ctx->currentBB = loopTail;
    IrInstruction *irCond = translateRValue(condition);

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

    gotoToBlock(loopHead);

    ctx->currentBB = loopHead;
    if (condition != NULL) {
        IrInstruction *irCond = translateRValue(condition);
        IrInstruction *irCondBranch = newCondBranch(irCond, loopBody, loopExit);
        addSuccessor(ctx->currentBB, loopBody);
        addSuccessor(ctx->currentBB, loopExit);
        termintateBlock(irCondBranch);
    } else {
        // TODO: merge with body block
      gotoToBlock(loopBody);
    }

    ctx->currentBB = loopBody;
    translateStatement(body);

    if (ctx->currentBB && ctx->currentBB->term == NULL) {
        IrBasicBlock *nextBlock = modifierBB ? modifierBB : loopHead;
        gotoToBlock(nextBlock);
    }

    if (modifierBB != NULL) {
        ctx->currentBB = modifierBB;
        translateRValue(modifier);
        gotoToBlock(loopHead);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return FALSE;
}

static void jumpToBlock(IrBasicBlock *target, AstStatement *ast) {
    if (ctx->currentBB && ctx->currentBB->term == NULL) {
        IrInstruction *gotoExit = newGotoInstruction(target);
        gotoExit->meta.astStmt = ast;
        addSuccessor(ctx->currentBB, target);
        termintateBlock(gotoExit);
    }
}

static Boolean translateReturn(AstStatement *stmt) {
    assert(stmt->statementKind == SK_RETURN);

    AstExpression *expr = stmt->jumpStmt.expression;

    if (expr != NULL) {
        IrInstruction *returnValue = translateRValue(expr);
        IrInstruction *returnSlot = ctx->currentFunc->retOperand;
        IrInstruction *copyInstr = NULL;
        assert(returnSlot != NULL);
        if (isCompositeType(expr->type)) {
            copyInstr = generateCompositeCopy(expr->type, returnValue, ctx->currentFunc->retOperand, NULL);

        } else {
            assert(ctx->currentFunc->retOperand != NULL);
            copyInstr = addStoreInstr(returnSlot, returnValue, NULL);
        }
        copyInstr->meta.astStmt = stmt;
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
    IrInstruction *target = ctx->lastOp;

    IrInstruction *iBranch = newInstruction(IR_IBRANCH, IR_VOID);
    addInstructionInput(iBranch, target);
    for (uint32_t idx = 0; idx < ctx->referencedBlocks.size; ++idx) {
        IrBasicBlock *b = getBlockFromVector(&ctx->referencedBlocks, idx);
        addSuccessor(ctx->currentBB, b);
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
    ctx->switchTable = switchTable;

    IrInstruction *condOp = translateRValue(stmt->switchStmt.condition);

    IrInstruction *tableBranch = newTableBranch(condOp, switchTable);
    tableBranch->meta.astStmt = stmt;

    unsigned walked = walkCaseLabels(stmt->switchStmt.body, caseBlocks, 0);
    assert(walked == switchTable->caseCount);

    for (uint32_t i = 0; i < switchTable->caseCount; ++i) {
        IrBasicBlock *caseBlock = newBasicBlock("<case_block>");
        caseBlocks[i].block = caseBlock;
        addSuccessor(ctx->currentBB, caseBlock);
    }

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
            for (uint32_t idx = 0; idx < ctx->referencedBlocks.size; ++idx) {
                IrBasicBlock *cur = getBlockFromVector(&ctx->referencedBlocks, idx);
                if (cur == labelBlock)
                    return;
            }
            addBlockToVector(&ctx->referencedBlocks, labelBlock);
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
      case SK_EMPTY:
        return;
      default: unreachable("Unknown statement kind");
    }
}

static void collectTranslationInfo(const AstStatement *body) {
    assert(ctx->labelMap != NULL && "Label map need to be allocated at this point");
    assert(ctx->localOperandMap != NULL && "Local Operand map need to be allocated at this point");

    collectTranslationInfoStmt(body);
}

static void generateExitBlock(IrFunction *func, TypeRef *returnType) {
  ctx->currentBB = func->exit;

  IrInstruction *ret = newInstruction(IR_RET, IR_VOID);
  if (!isVoidType(returnType)) {
    assert(func->retOperand != NULL);
    if (isCompositeType(returnType)) {
	  addInstructionInput(ret, func->retOperand);
    } else {
      IrInstruction *retValue = addLoadInstr(typeRefToIrType(returnType), func->retOperand, NULL);
      retValue->astType = returnType;
      addInstructionInput(ret, retValue);
    }
  }

  addInstruction(ret);
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

static void initializeParamterLocal(IrBasicBlock *entryBB, IrInstruction *stackPtrOp, ParamtersABIInfo *paramInfo) {

    AstValueDeclaration *param = paramInfo->declaration;
    TypeRef *astType = param->type;
    uint32_t paramIndex = paramInfo->idx;
    enum IrTypeKind type = typeRefToIrType(astType);
    LocalValueInfo *lvi = paramInfo->lvi;
    param->index2 = paramIndex;
    lvi->declaration = param;

    ctx->currentBB = entryBB;

    if (paramInfo->isRegister) {
      // scalar type in register
      IrInstruction *stackSlot = createAllocaSlot(computeTypeSize(astType));
      stackSlot->info.alloca.valueType = type;
      stackSlot->astType = makePointedType(ctx->pctx, astType->flags.storage, astType);
      lvi->stackSlot = stackSlot;

      IrInstruction *regInstr = newPhysRegister(type, paramInfo->loc.pregId);
      addInstruction(regInstr);

      addStoreInstr(stackSlot, regInstr, NULL);
      stackSlot->info.alloca.v = param;
    } else {
      param->index2 = paramIndex;
      lvi->declaration = param;

      IrInstruction *offset = createIntegerConstant(IR_I64, paramInfo->loc.stackOffset);
      IrInstruction *addInstr = newInstruction(IR_E_ADD, IR_P_AGG);
      addInstructionInput(addInstr, stackPtrOp);
      addInstructionInput(addInstr, offset);
      addInstruction(addInstr);
      lvi->stackSlot = addInstr;
      lvi->frameOffset = paramInfo->loc.stackOffset;
    }
}

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


static size_t generateVaArea(AstValueDeclaration *va_area, int32_t stackParamOffset, LocalValueInfo *infos, size_t idx) {
    va_area->index2 = idx;
    enum IrTypeKind irType = typeRefToIrType(va_area->type);
    IrInstruction *vaAreaSlot = createAllocaSlot(computeTypeSize(va_area->type));
    vaAreaSlot->info.alloca.valueType = irType;
    vaAreaSlot->info.alloca.v = va_area;
    infos[idx].stackSlot = vaAreaSlot;
    infos[idx].declaration = va_area;

    /**
     * typedef struct {
     *   unsinged int gp_offset;
     *   unsigned int fp_offset;
     *   void *overflow_arg_area;
     *   const void *reg_save_area;
     * } __va_elem;
     */


    const static int32_t dataSize = sizeof(intptr_t);
    assert(va_area);
    int32_t gp_offset_off = 0;
    int32_t fp_offset_off = fp_offset_off + sizeof (uint32_t);
    int32_t overflow_arg_area_ptr_off = fp_offset_off + dataSize;
    int32_t reg_save_area_ptr_off = overflow_arg_area_ptr_off + dataSize;

    int32_t gp_va_area = ALIGN_SIZE(reg_save_area_ptr_off + dataSize, dataSize);
    int32_t fp_va_area = ALIGN_SIZE(gp_va_area + R_PARAM_COUNT * dataSize, dataSize);
    int32_t reg_save_area_offset = gp_va_area;

    IrInstruction *gp_offset_off_i = createIntegerConstant(IR_I64, gp_offset_off);
    IrInstruction *gp_va_area_off = createIntegerConstant(IR_I64, gp_va_area);

    TypeRef *u32Type = makePrimitiveType(ctx->pctx, 0, T_U4);
    TypeRef *voidType = makePrimitiveType(ctx->pctx, 0, T_VOID);
    TypeRef *voidPtrType = makePointedType(ctx->pctx, 0, voidType);
    TypeRef *uintptrType = makePrimitiveType(ctx->pctx, 0, T_U8);
    TypeRef *floatType = makePrimitiveType(ctx->pctx, 0, T_F8);
    enum IrTypeKind iru32Type = IR_U32;

    // va_elem->reg_save_area = reg_save_area_begin
    IrInstruction *reg_save_area_off = createIntegerConstant(IR_I64, reg_save_area_offset);
    IrInstruction *reg_save_area_begin_ptr = newGEPInstruction(vaAreaSlot, reg_save_area_off, uintptrType);
    addInstruction(reg_save_area_begin_ptr);

    IrInstruction *reg_save_area_slot_off = createIntegerConstant(IR_I64, reg_save_area_ptr_off);
    IrInstruction *reg_save_area_slot_ptr = newGEPInstruction(vaAreaSlot, reg_save_area_slot_off, voidPtrType);
    addInstruction(reg_save_area_slot_ptr);
    addStoreInstr(reg_save_area_slot_ptr, reg_save_area_begin_ptr, NULL);

    IrInstruction *gp_area_ptr = newGEPInstruction(vaAreaSlot, gp_va_area_off, voidPtrType);
    addInstruction(gp_area_ptr);
    IrInstruction *gp_offset_ptr = newGEPInstruction(vaAreaSlot, gp_offset_off_i, u32Type);

    // va_elem->overflow_arg_area = stack_param_begin
    IrInstruction *stack_param_begin_off = createIntegerConstant(IR_I64, stackParamOffset);
    IrInstruction *stack_param_begin_ptr = newGEPInstruction(vaAreaSlot, stack_param_begin_off, uintptrType);
    addInstruction(stack_param_begin_ptr);

    IrInstruction *overflow_arg_area_slot_off = createIntegerConstant(IR_I64, overflow_arg_area_ptr_off);
    IrInstruction *overflow_arg_area_slot_ptr = newGEPInstruction(vaAreaSlot, overflow_arg_area_slot_off, voidPtrType);
    addInstruction(overflow_arg_area_slot_ptr);

    addStoreInstr(overflow_arg_area_slot_ptr, stack_param_begin_ptr, NULL);


    // save gp registers;
    // save fp registers;

    return idx + 1;
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
        numOfReturnSlots = 1;
    }

    size_t numOfVariadicSlots = 0;
    if (declaration->isVariadic) {
      numOfVariadicSlots = 1;
    }

    const size_t numOfLocalSlots = numOfParams + numOfLocals + numOfReturnSlots + numOfVariadicSlots;
    func->numOfLocalSlots = numOfLocalSlots;

    LocalValueInfo *localOperandsMap = areanAllocate(ctx->irArena, numOfLocalSlots * sizeof (LocalValueInfo));
    ctx->localOperandMap = localOperandsMap;
    func->localOperandMap = localOperandsMap;

    int32_t frameOffset = 0;

    ParamtersABIInfo *paramABIInfo = heapAllocate(numOfParams * sizeof (ParamtersABIInfo));

    static const uint32_t R_SP = 3;
    IrInstruction *stackPtrOp = ctx->stackOp = newPhysRegister(IR_PTR, R_SP);

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
      // TODO: if return type is not scalar type
      ctx->currentBB = func->entry;
      IrInstruction *returnStackSlot = func->retOperand = createAllocaSlot(computeTypeSize(declaration->returnType));
      returnStackSlot->astType = makePointedType(ctx->pctx, 0, declaration->returnType);
      returnStackSlot->info.alloca.valueType = typeRefToIrType(declaration->returnType);
      localOperandsMap[idx++].stackSlot = returnStackSlot;
    }

    if (numOfVariadicSlots) {
      AstValueDeclaration *va_area = function->va_area;
      ctx->currentBB = func->entry;
      idx = generateVaArea(va_area, 0,  localOperandsMap, idx);
    }

    AstStatement *body = function->body;
    assert(body->statementKind == SK_BLOCK);

    collectTranslationInfo(body);

    IrBasicBlock *firstBB = ctx->currentBB = newBasicBlock("<FIRST>");
    translateBlock(body);
    if (ctx->currentBB) {
      gotoToBlock(func->exit);
    }
	generateExitBlock(func, declaration->returnType);

    ctx->currentBB = func->entry;
    gotoToBlock(firstBB);
    ctx->currentBB = NULL;

    ctx->localOperandMap = NULL;
    releaseHeap(paramABIInfo);

    return 0;
}

static IrFunction *translateFunction(AstFunctionDefinition *function) {
    resetIrContext(ctx);
    IrFunction *func = newIrFunction(function);

    buildInitialIr(func, function);
    assert(func->numOfBlocks == ctx->bbCnt);
    buildSSA(func);
    scp(func);

    return func;
}

