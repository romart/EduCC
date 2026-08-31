
#include "ir/ir.h"
#include "ir/isel.h"
#include "ir/regalloc.h"
#include "ir/machine.h"
#include "sema.h"
#include "tree.h"
#include "types.h"
#include <assert.h>
#include <signal.h>

// How many arguments of each class the selected target passes in registers.
// These used to be two made-up 10s here, which is why the va_list register
// save area below and the parameter classification disagreed with every real
// ABI; they now come from the TargetDescriptor (see include/ir/target.h).
#define R_PARAM_COUNT (ctx->target->intArgRegCount)
#define R_FP_PARAM_COUNT (ctx->target->fpArgRegCount)

extern IrContext *ctx;

static IrFunction *translateFunction(AstFunctionDefinition *function);
static Boolean translateStatement(AstStatement *stmt);
static Boolean translateBlock(AstStatement *block);
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

static Boolean declaresVla(const AstStatement *stmt);
static Boolean blockDeclaresVla(const AstStatement *block);
static Boolean enterStackScope(Boolean opens);
static void leaveStackScope(Boolean opened);
static void restoreStackScopes(size_t depth);

static IrFunction *newIrFunction(AstFunctionDefinition *function) {
  IrFunction *func = areanAllocate(ctx->irArena, sizeof(IrFunction));
  ctx->currentFunc = func;
  func->ast = function;
  func->id = ctx->functionCnt++;
  func->entry = newBasicBlock("<entry>");
  func->exit = newBasicBlock("<exit>");
  initVector(&func->staticLocals, INITIAL_VECTOR_CAPACITY);
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
      trace("Translate function '%s' into IR\n",
            unit->definition->declaration->name);
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

static IrInstruction *encodeBitField(const TypeRef *type,
                                     IrInstruction *storageOp,
                                     IrInstruction *valueOp) {
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

  // The value being assigned is what moves into place, not the storage it is
  // going into: this is '(old & ~mask) | ((value << offset) & mask)'.
  IrInstruction *shiftValueInstr = newInstruction(IR_E_SHL, irMemoryType);
  IrInstruction *shiftOp = createIntegerConstant(irMemoryType, s);
  addInstructionInput(shiftValueInstr, valueOp);
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

static IrInstruction *decodeBitField(const TypeRef *type,
                                     IrInstruction *storageOp) {
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

static IrInstruction *generateCompositeCopy(const TypeRef *type,
                                            IrInstruction *src,
                                            IrInstruction *dst,
                                            const AstExpression *ast) {
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

static size_t
translateInitializerIntoMemory(IrInstruction *base, int32_t offset,
                               size_t typeSize,
                               const AstInitializer *initializer) {

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
      const int32_t emittedOffset = translateInitializerIntoMemory(
          base, emitOffset, typeSize, expr->compound);
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
        IrInstruction *encodedValue =
            encodeBitField(slotType, storage, valueOp);
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
           inits != NULL; inits = inits->next) {
        AstInitializer *init = inits->initializer;
        if (init->state == IS_INIT) {
          return translateInitializerIntoMemory(base, offset, typeSize, init);
        }
      }
    } else {
      size_t emmited = 0;
      for (const AstInitializerList *inits = initializer->initializerList;
           inits != NULL; inits = inits->next) {
        emmited = translateInitializerIntoMemory(base, offset, typeSize,
                                                 inits->initializer);
      }
      return emmited;
    }
  }
  default:
    unreachable("Unknown initializer kind");
  }

  return 0;
}

// -============================ expressions ============================-

static IrInstruction *translateConstant(AstExpression *expr) {
  assert(expr->op == E_CONST);

  switch (expr->constExpr.op) {
  case CK_INT_CONST:
    return createIntegerConstant(typeRefToIrType(expr->type), expr->constExpr.i);
  case CK_FLOAT_CONST:
    return createFloatConstant(typeRefToIrType(expr->type), expr->constExpr.f);
  case CK_STRING_LITERAL:
    return createLiteralConstant(expr->constExpr.l.s, expr->constExpr.l.length);
  }

  unreachable("Unknown constant kind");
}

static IrInstruction *
computeVAListValuePtr(IrInstruction *valistInstr, IrBasicBlock *memoryBlock,
                      IrBasicBlock *updateBlock, IrBasicBlock *doneBlock,
                      TypeDefiniton *vastruct, const char *offsetMemberName,
                      size_t areaBound) {
  const static int32_t dataSize = sizeof(intptr_t);
  StructualMember *rsam = findStructualMember(vastruct, "reg_save_area");
  enum IrTypeKind irRSAType = typeRefToIrType(rsam->type);

  StructualMember *m = findStructualMember(vastruct, offsetMemberName);
  enum IrTypeKind irOffsetType = typeRefToIrType(m->type);
  assert(m != NULL);
  IrInstruction *offsetOff = createIntegerConstant(IR_I64, m->offset);
  IrInstruction *offsetGep = newGEPInstruction(valistInstr, offsetOff, m->type);
  addInstruction(offsetGep);
  // Four bytes, which is what gp_offset and fp_offset are. Reading eight took
  // the field next door with it and made the comparison below always choose the
  // overflow area; nothing noticed while variadic definitions fell back, since
  // the legacy backend does not read this IR.
  IrInstruction *offValue = addLoadInstr(irOffsetType, offsetGep, NULL);
  offValue->astType = m->type;
  IrInstruction *areaSize =
      createIntegerConstant(irOffsetType, areaBound * dataSize);

  IrInstruction *cmpgeInstr =
      addBinaryOpeartion(IR_E_GE, offValue, areaSize, IR_BOOL, NULL, NULL);

  IrInstruction *condInstr =
      newCondBranch(cmpgeInstr, memoryBlock, updateBlock);
  addSuccessor(ctx->currentBB, memoryBlock);
  addSuccessor(ctx->currentBB, updateBlock);
  termintateBlock(condInstr);

  ctx->currentBB = updateBlock;

  IrInstruction *regSaveOff = createIntegerConstant(IR_I64, rsam->offset);
  IrInstruction *regSaveGEP =
      newGEPInstruction(valistInstr, regSaveOff, rsam->type);
  addInstruction(regSaveGEP);

  IrInstruction *regSaveAreaValue = addLoadInstr(irRSAType, regSaveGEP, NULL);
  regSaveAreaValue->astType = rsam->type;

  // Widened before it meets an address: the offset is 32 bits, the pointer is
  // 64, and the bytes above the offset are not this value's to supply.
  IrInstruction *wideOffset = newInstruction(IR_E_BITCAST, IR_I64);
  addInstructionInput(wideOffset, offValue);
  wideOffset->info.fromCastType = irOffsetType;
  addInstruction(wideOffset);

  IrInstruction *areaPtr = addBinaryOpeartion(
      IR_E_ADD, regSaveAreaValue, wideOffset, irRSAType, rsam->type, NULL);

  IrInstruction *stepInstr = createIntegerConstant(irOffsetType, dataSize);
  IrInstruction *newAreaOffset = addBinaryOpeartion(
      IR_E_ADD, offValue, stepInstr, irOffsetType, m->type, NULL);
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

  // Which save area the argument is in, by the same classification the call
  // site used. A composite is not a scalar and used to abort the translator
  // here; one small enough to travel in a register comes out of the integer
  // area like any other, and one too large is only ever in the overflow area -
  // so there is no register path to choose between, and no branch to build.
  //
  // Size is the whole question, and it used to be asked last. It is a size
  // test rather than SysV's classification because the call site's is too:
  // SysV splits an aggregate of up to sixteen bytes into two eightbytes and
  // passes those in registers, and classifyParametersGeneric does not (see the
  // TODO there). The two have to agree - va_arg reads what the caller wrote -
  // so whoever finishes that TODO has to finish this in the same commit.
  //
  // Long double is the scalar that is in neither save area even so: SysV gives
  // it the X87 class, which is passed in memory, so it is never in the
  // register save area however few SSE arguments came before it. Asking
  // isRealType first sent it there to read whatever the eighth SSE slot
  // happened to hold.
  // Nothing was passed for it, so nothing is consumed reading it back: the
  // va_list is left where it is and the next va_arg gets the argument that
  // really is next. The address handed back names zero bytes, so the va_list's
  // own is as good as any.
  if (isEmptyCompositeType(vatype))
    return valistInstr;

  Boolean inRegister = computeTypeSize(vatype) <= dataSize;

  IrBasicBlock *memoryBlock = NULL, *updateBlock = NULL, *doneBlock = NULL;
  IrInstruction *valuePtr = NULL;

  if (inRegister) {
    memoryBlock = newBasicBlock("<va_arg_mem>");
    updateBlock = newBasicBlock("<va_arg_update>");
    doneBlock = newBasicBlock("<va_arg_done>");

    if (isRealType(vatype)) {
      valuePtr = computeVAListValuePtr(valistInstr, memoryBlock, updateBlock,
                                       doneBlock, vastruct, "fp_offset",
                                       R_PARAM_COUNT + R_FP_PARAM_COUNT);
    } else {
      valuePtr = computeVAListValuePtr(valistInstr, memoryBlock, updateBlock,
                                       doneBlock, vastruct, "gp_offset",
                                       R_PARAM_COUNT);
    }

    assert(valuePtr != NULL);
    ctx->currentBB = memoryBlock;
  }

  StructualMember *oaam = findStructualMember(vastruct, "overflow_arg_area");
  assert(oaam != NULL);

  IrInstruction *overflowAreaOffset =
      createIntegerConstant(IR_I64, oaam->offset);
  enum IrTypeKind irOAType = typeRefToIrType(oaam->type);

  IrInstruction *overflowAreaGep =
      newGEPInstruction(valistInstr, overflowAreaOffset, oaam->type);
  addInstruction(overflowAreaGep);

  IrInstruction *overflowAreaValue =
      addLoadInstr(irOAType, overflowAreaGep, NULL);
  overflowAreaValue->astType = oaam->type;

  int32_t align = typeAlignment(vatype);

  if (align > 8) {
    int32_t mask = ~(align - 1);
    IrInstruction *alignC = createIntegerConstant(IR_I64, align - 1);
    IrInstruction *addInstr = addBinaryOpeartion(
        IR_E_ADD, overflowAreaValue, alignC, irOAType, oaam->type, NULL);
    IrInstruction *maskC = createIntegerConstant(IR_I64, mask);
    overflowAreaValue = addBinaryOpeartion(IR_E_AND, addInstr, maskC, irOAType,
                                           oaam->type, NULL);
  }

  int32_t argSize = max(8, computeTypeSize(vatype));

  IrInstruction *alignesArgSize =
      createIntegerConstant(IR_I64, ALIGN_SIZE(argSize, dataSize));
  IrInstruction *newOverflowArea = addBinaryOpeartion(
      IR_E_ADD, overflowAreaValue, alignesArgSize, irOAType, oaam->type, NULL);

  addStoreInstr(overflowAreaGep, newOverflowArea, NULL);

  // Nothing to merge when the overflow area is the only place to look.
  if (!inRegister) {
    return overflowAreaValue;
  }

  gotoToBlock(doneBlock);

  // Both arms are *addresses* of the argument, and so is what this returns -
  // the load is the caller's. Typing the phi by the argument instead truncated
  // the pointer for anything narrower than a word, which is every 'va_arg(ap,
  // int)' there is; it went unseen because the overflow-only path returns its
  // pointer directly and never reaches this phi.
  ctx->currentBB = doneBlock;
  IrInstruction *phiInstr = newPhiInstruction(IR_PTR);

  addPhiInput(phiInstr, valuePtr, updateBlock);
  addPhiInput(phiInstr, overflowAreaValue, memoryBlock);
  phiInstr->astType = makePointedType(ctx->pctx, 0, vatype);
  addInstructionHead(doneBlock, phiInstr);

  return phiInstr;
}

static IrInstruction *translateNameRef(AstExpression *expr) {
  assert(expr->op == E_NAMEREF);

  // If this is called we probably want a function reference.
  // Value references go through DEREF node

  Symbol *s = expr->nameRefExpr.s;

  if (s->kind == FunctionSymbol ||
      s->kind == ValueSymbol && !(s->variableDesc->flags.bits.isLocal ||
                                  s->variableDesc->kind != VD_PARAMETER)) {
    // Either Function of non-local variable reference
    return createSymbolConstant(s);
  } else if (s->kind == ValueSymbol) {
    AstValueDeclaration *v = s->variableDesc;

    if (v->kind == VD_PARAMETER || v->flags.bits.isLocal) {
      assert(v->index2 != -1);
      assert(v->flags.bits.isLocal);
      LocalValueInfo *info = &ctx->localOperandMap[v->index2];
      assert(info != NULL);
      assert(info->stackSlot != NULL);
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

  if (callee->op != E_NAMEREF)
    return NULL;
  if (strcmp("alloca", callee->nameRefExpr.s->name))
    return NULL;

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
  TypeRef *calleeType =
      pcalleeType->kind == TR_POINTED ? pcalleeType->pointed : pcalleeType;
  assert(calleeType->kind == TR_FUNCTION);
  TypeRef *returnType = calleeType->functionTypeDesc.returnType;

  unsigned returnTypeSize = computeTypeSize(returnType);

  IrInstruction *calleeOp = translateRValue(callee);
  IrInstruction *callInstr =
      newInstruction(IR_CALL, typeRefToIrType(expr->type));

  // The callee, an optional return buffer, and one input per argument - the
  // width the memory-argument bitmap has to cover, counted before the first
  // input goes in because that is when it can still be sized.
  size_t numInputs = 1;
  if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
    numInputs += 1;
  }
  for (AstExpressionList *args = expr->callExpr.arguments; args != NULL;
       args = args->next) {
    numInputs += 1;
  }
  allocateCallMemoryArgs(callInstr, numInputs);

  addInstructionInput(callInstr, calleeOp);

  callInstr->astType = expr->type;
  callInstr->meta.astExpr = expr;
  callInstr->info.call.isVariadic = calleeType->functionTypeDesc.isVariadic;

  IrInstruction *returnSlotOp = NULL;

  // TODO: save and restore stack after series of alloca's

  if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
    returnSlotOp = createAllocaSlot(returnTypeSize);
    returnSlotOp->info.alloca.valueType = typeRefToIrType(returnType);
    addInstructionInput(callInstr, returnSlotOp);
    callInstr->info.call.returnBuffer = returnSlotOp;
  }

  for (AstExpressionList *args = expr->callExpr.arguments; args != NULL;
       args = args->next) {
    AstExpression *argExpr = args->expression;
    TypeRef *argType = argExpr->type;
    unsigned alignent = max(8, typeAlignment(argType));
    unsigned argSize = max(8, computeTypeSize(argType));

    IrInstruction *argOp = translateRValue(argExpr);
    IrInstruction *realArgOp = NULL;

    // A zero-sized argument is passed in nothing at all, so it is evaluated
    // and then not handed over. Loading the eightbyte below would have read
    // past it and taken up the register the next argument goes in.
    if (isEmptyCompositeType(argType))
      continue;

    if (isCompositeType(argType)) {
      if (argSize > sizeof(intptr_t)) {
        // The temporary is the copy C says a by-value argument is, and the
        // backend passes its *bytes* rather than its address - so the slot is
        // where the argument is built, not where it is passed from. Its index
        // goes in the mask because nothing about the instruction says which of
        // the two an IR_PTR input is.
        realArgOp = createAllocaSlot(argSize);
        realArgOp->info.alloca.valueType = typeRefToIrType(argType);
        realArgOp->astType = makePointedType(ctx->pctx, 0, argType);
        generateCompositeCopy(argType, argOp, realArgOp, expr);

        setCallMemoryArg(callInstr, callInstr->inputs.size);
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

  return callInstr;
}

static IrInstruction *translateTernary(AstExpression *expr) {
  assert(expr->op == E_TERNARY);

  IrInstruction *condOp = translateRValue(expr->ternaryExpr.condition);

  IrBasicBlock *ifTrue = newBasicBlock("<ifTrue>");
  IrBasicBlock *ifFalse = newBasicBlock("<ifFalse>");
  IrBasicBlock *exit = newBasicBlock("<ternary_exit>");

  IrInstruction *cond = newCondBranch(condOp, ifTrue, ifFalse);
  addSuccessor(ctx->currentBB, ifTrue);
  addSuccessor(ctx->currentBB, ifFalse);
  termintateBlock(cond);

  // Each arm is entered at the block created for it but need not end there:
  // an arm that splits the block itself leaves translation somewhere further
  // down, and that is the block gotoToBlock() takes the edge to 'exit' from.
  // See translateLogicalExpression for why the phi must name it rather than
  // the block the arm started in.
  ctx->currentBB = ifTrue;
  IrInstruction *ifTrueOp = translateRValue(expr->ternaryExpr.ifTrue);
  IrBasicBlock *ifTrueEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = ifFalse;
  IrInstruction *ifFalseOp = translateRValue(expr->ternaryExpr.ifFalse);
  IrBasicBlock *ifFalseEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = exit;
  // Both arms of a composite ternary hand back an *address* - that is what a
  // composite value is here - but not the same kind of one: a call's result is
  // IR_P_AGG and a local's is IR_PTR. The phi is over addresses either way, so
  // the two arms agreeing on an IR type is a scalar's question.
  assert(ifTrueOp->type == ifFalseOp->type || isCompositeType(expr->type));
  IrInstruction *phi = newPhiInstruction(typeRefToIrType(expr->type));

  addPhiInput(phi, ifTrueOp, ifTrueEnd);
  addPhiInput(phi, ifFalseOp, ifFalseEnd);
  addInstruction(phi);

  return phi;
}

static IrInstruction *translateBitExtend(AstExpression *expr) {
  assert(expr->op == E_BIT_EXTEND);
  unimplemented("Bit Extend Expression");
  return NULL;
}

static IrInstruction *addCastInstr(IrInstruction *src, enum IrTypeKind irFromType,
                                   enum IrTypeKind irToType, TypeRef *astType,
                                   AstExpression *expr) {
  IrInstruction *castInstr = newInstruction(IR_E_BITCAST, irToType);

  addInstructionInput(castInstr, src);
  addInstruction(castInstr);

  castInstr->info.fromCastType = irFromType;
  castInstr->astType = astType;
  castInstr->meta.astExpr = expr;

  return castInstr;
}

// The two conversions x86-64 has no instruction for. SSE converts between a
// float and a *signed* 64-bit integer and nothing else, so a 64-bit unsigned
// one at either end needs the half of its range above 2^63 handled separately -
// which needs a branch, and a branch is something only this side of the
// pipeline can build. Selection fills blocks that already exist and asserts
// that nothing invents one (selectInstructions, src/ir/codegen/isel.c), so a
// selection rule could not have expressed either of these; both used to be a
// refusal, and the function went to the legacy backend whole.
//
// Nothing narrower needs this. A 32-bit unsigned value is not negative as a
// signed 64-bit one, so selectConversion already reaches those by widening.
static Boolean isWideUnsigned(enum IrTypeKind t) {
  return isUnsignedIrOperand(t) && irTypeMachineSize(t) == sizeof(intptr_t);
}

// Whether every 64-bit unsigned value is exactly representable in this
// floating type. Only x87's is - its mantissa is 64 bits wide - and that makes
// the halving below not merely unnecessary but wrong: ((2k+1) >> 1) | 1 is
// k+1 whenever k is even, and doubling that answers 2k+2 for a value the
// format could have held exactly.
static Boolean representsEveryWideUnsigned(enum IrTypeKind t) {
  return t == IR_F80;
}

// (double)someUnsignedLong.
//
//   x < 2^63 ? (double)(int64_t)x
//            : (double)(int64_t)((x >> 1) | (x & 1)) * 2
//
// The odd bit is put back after the shift rather than dropped: halving loses
// the low bit, and a value that was exactly halfway between two representable
// doubles would then round to even in the wrong direction. Or-ing it back -
// round to odd - leaves the halved value on the correct side of the midpoint,
// so doubling afterwards gives the correctly rounded answer.
//
// A long double destination takes the other arm, which is the same shape and
// exact rather than correctly rounded:
//
//   x < 2^63 ? (long double)(int64_t)x
//            : (long double)(int64_t)x + 2^64
//
// Reading x signed above 2^63 gives x - 2^64, and both that and the constant
// put back are exact in a 64-bit mantissa, so the sum is x itself.
static IrInstruction *translateWideUnsignedToFloat(IrInstruction *src, enum IrTypeKind irFromType,
                                                   enum IrTypeKind irToType, TypeRef *toType,
                                                   AstExpression *expr) {
  Boolean exact = representsEveryWideUnsigned(irToType);
  IrInstruction *signBit = createIntegerConstant(irFromType, (int64_const_t)1 << 63);
  IrInstruction *fits = addBinaryOpeartion(IR_E_LT, src, signBit, IR_BOOL, NULL, NULL);

  IrBasicBlock *small = newBasicBlock("<u2f_signed>");
  IrBasicBlock *large = newBasicBlock(exact ? "<u2f_biased>" : "<u2f_halved>");
  IrBasicBlock *exit = newBasicBlock("<u2f_exit>");

  IrInstruction *branch = newCondBranch(fits, small, large);
  addSuccessor(ctx->currentBB, small);
  addSuccessor(ctx->currentBB, large);
  termintateBlock(branch);

  ctx->currentBB = small;
  IrInstruction *asSigned = addCastInstr(src, irFromType, IR_I64, NULL, expr);
  IrInstruction *direct = addCastInstr(asSigned, IR_I64, irToType, toType, expr);
  IrBasicBlock *smallEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = large;
  IrInstruction *wide = NULL;

  if (exact) {
    IrInstruction *asSigned2 = addCastInstr(src, irFromType, IR_I64, NULL, expr);
    IrInstruction *converted = addCastInstr(asSigned2, IR_I64, irToType, toType, expr);
    IrInstruction *twoTo64 = createFloatConstant(irToType, 18446744073709551616.0L);
    wide = addBinaryOpeartion(IR_E_FADD, converted, twoTo64, irToType, toType, NULL);
  } else {
    IrInstruction *one = createIntegerConstant(irFromType, 1);
    IrInstruction *shifted = addBinaryOpeartion(IR_E_SHR, src, one, irFromType, NULL, NULL);
    IrInstruction *odd = addBinaryOpeartion(IR_E_AND, src, one, irFromType, NULL, NULL);
    IrInstruction *rounded = addBinaryOpeartion(IR_E_OR, shifted, odd, irFromType, NULL, NULL);
    IrInstruction *halfSigned = addCastInstr(rounded, irFromType, IR_I64, NULL, expr);
    IrInstruction *half = addCastInstr(halfSigned, IR_I64, irToType, toType, expr);
    // Doubled by adding it to itself rather than by multiplying by two: both
    // are exact, and this needs no float constant to materialize.
    wide = addBinaryOpeartion(IR_E_FADD, half, half, irToType, toType, NULL);
  }

  IrBasicBlock *largeEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = exit;
  IrInstruction *phi = newPhiInstruction(irToType);
  addPhiInput(phi, direct, smallEnd);
  addPhiInput(phi, wide, largeEnd);
  addInstruction(phi);
  phi->astType = toType;

  return phi;
}

// (unsigned long)someDouble.
//
//   x < 2^63 ? (uint64_t)(int64_t)x
//            : (uint64_t)(int64_t)(x - 2^63) + 2^63
//
// 2^63 is exactly representable in both float and double, so the subtraction is
// exact and the bit put back afterwards is the one it took away. Anything at or
// above 2^64 is undefined in C either way, and this leaves it that way rather
// than choosing an answer for it.
static IrInstruction *translateFloatToWideUnsigned(IrInstruction *src, enum IrTypeKind irFromType,
                                                   enum IrTypeKind irToType, TypeRef *toType,
                                                   AstExpression *expr) {
  IrInstruction *bound = createFloatConstant(irFromType, 9223372036854775808.0L);
  IrInstruction *fits = addBinaryOpeartion(IR_E_FLT, src, bound, IR_BOOL, NULL, NULL);

  IrBasicBlock *small = newBasicBlock("<f2u_signed>");
  IrBasicBlock *large = newBasicBlock("<f2u_biased>");
  IrBasicBlock *exit = newBasicBlock("<f2u_exit>");

  IrInstruction *branch = newCondBranch(fits, small, large);
  addSuccessor(ctx->currentBB, small);
  addSuccessor(ctx->currentBB, large);
  termintateBlock(branch);

  ctx->currentBB = small;
  IrInstruction *signed1 = addCastInstr(src, irFromType, IR_I64, NULL, expr);
  IrInstruction *direct = addCastInstr(signed1, IR_I64, irToType, toType, expr);
  IrBasicBlock *smallEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = large;
  IrInstruction *lowered = addBinaryOpeartion(IR_E_FSUB, src, bound, irFromType, NULL, NULL);
  IrInstruction *signed2 = addCastInstr(lowered, irFromType, IR_I64, NULL, expr);
  IrInstruction *unbiased = addCastInstr(signed2, IR_I64, irToType, toType, expr);
  IrInstruction *signBit = createIntegerConstant(irToType, (int64_const_t)1 << 63);
  IrInstruction *biased = addBinaryOpeartion(IR_E_ADD, unbiased, signBit, irToType, toType, NULL);
  IrBasicBlock *largeEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = exit;
  IrInstruction *phi = newPhiInstruction(irToType);
  addPhiInput(phi, direct, smallEnd);
  addPhiInput(phi, biased, largeEnd);
  addInstruction(phi);
  phi->astType = toType;

  return phi;
}

static IrInstruction *translateCast(AstExpression *expr) {
  assert(expr->op == E_CAST);
  TypeRef *fromType = expr->castExpr.argument->type;
  TypeRef *toType = expr->castExpr.type;

  enum IrTypeKind irFromType = typeRefToIrType(fromType);
  enum IrTypeKind irToType = typeRefToIrType(toType);

  IrInstruction *src = translateRValue(expr->castExpr.argument);

  // isRealIrType and not isFloatIrType: long double needs this as much as the
  // SSE types do - more, since x87 has no unsigned form of anything either -
  // and it is not an isFloatIrType, having no register class of its own.
  if (isRealIrType(irToType) && isWideUnsigned(irFromType)) {
    return translateWideUnsignedToFloat(src, irFromType, irToType, toType, expr);
  }

  if (isRealIrType(irFromType) && isWideUnsigned(irToType)) {
    return translateFloatToWideUnsigned(src, irFromType, irToType, toType, expr);
  }

  return addCastInstr(src, irFromType, irToType, toType, expr);
}

// Whether an instruction is already known to yield exactly 0 or 1, so that
// comparing it against zero once more would only restate what it computed. The
// relational and equality operators are defined to yield one or the other
// (C99 6.5.8p6, 6.5.9p3) and so is logical negation (6.5.3.3p5); so is the phi
// translateLogicalExpression builds, which is what keeps a chain like
// 'a || (b && c)' down to one compare per operand that actually needs one
// rather than one per nesting level. Nothing folds the redundant compare later
// - neither scp nor gvn rewrites 'x != 0' to 'x' - so it has to not be emitted.
static Boolean yieldsBoolean(const IrInstruction *instr) {
  switch (instr->kind) {
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
    return TRUE;
  case IR_PHI:
    // The join of a nested '&&' / '||', tagged with the expression it came
    // from below. Any other phi carries whatever its operands did.
    return instr->meta.astExpr != NULL &&
           (instr->meta.astExpr->op == EB_ANDAND ||
            instr->meta.astExpr->op == EB_OROR);
  default:
    return FALSE;
  }
}

static IrInstruction *translateLogicalExpression(AstExpression *expr) {
  assert(expr->op == EB_ANDAND || expr->op == EB_OROR);

  Boolean isAndAnd = expr->op == EB_ANDAND;
  IrInstruction *leftOp = translateRValue(expr->binaryExpr.left);

  // The left operand is only ever branched on - the phi below never carries it
  // - so it has to be testable rather than 0 or 1. A float is neither: 'test'
  // on an xmm register is not an instruction, and the branch would read flags
  // nothing set. The parser wraps every other controlling expression in a
  // '!= 0' of its own (an 'if', 'while' or ternary on a double arrives here
  // already compared); the operands of '&&' and '||' are the one place it does
  // not, so the comparison has to be built here. An integer is left alone for
  // yieldsBoolean's reason: nothing folds a redundant compare away later.
  if (isRealType(expr->binaryExpr.left->type)) {
    IrInstruction *zero = createFloatConstant(leftOp->type, 0.0);
    leftOp = addBinaryOpeartion(IR_E_FNE, leftOp, zero, IR_BOOL, NULL, NULL);
  }

  IrBasicBlock *fst = ctx->currentBB;
  IrBasicBlock *scnd = newBasicBlock(isAndAnd ? "< && >" : "< || >");
  IrBasicBlock *exit = newBasicBlock(isAndAnd ? "< &&-exit>" : "< ||-exit>");

  IrInstruction *cond =
      newCondBranch(leftOp, isAndAnd ? scnd : exit, isAndAnd ? exit : scnd);
  addSuccessor(ctx->currentBB, scnd);
  addSuccessor(ctx->currentBB, exit);
  termintateBlock(cond);

  ctx->currentBB = scnd;
  IrInstruction *rightOp = translateRValue(expr->binaryExpr.right);

  // '&&' and '||' yield int 0 or 1, never the operand itself (C99 6.5.13p3,
  // 6.5.14p3), so the surviving operand has to be compared against zero rather
  // than carried into the phi as it stands. Feeding the raw value through was
  // invisible for as long as the result was only ever branched on - anything
  // non-zero is true - and wrong the moment it is used as a value: 'x || y'
  // with x == 7 returned 7. It also left the phi typed IR_BOOL while every
  // other expression of type int is IR_I32, which is what made a ternary with
  // one logical arm ('c ? 0 : (a || b)') fail translateTernary's
  // same-type assertion below.
  enum IrTypeKind irType = typeRefToIrType(expr->type);
  IrInstruction *rightBool = rightOp;

  if (!yieldsBoolean(rightOp)) {
    Boolean isFloatOperand = isRealType(expr->binaryExpr.right->type);
    // The zero matches the operand's machine type, which for a string literal
    // or a symbol is IR_PTR like any other address - comparing one against
    // zero is an ordinary null-pointer test. 'flag && "text"' asked for an
    // integer zero of a type that was not a machine type at all.
    enum IrTypeKind zeroType = rightOp->type;
    IrInstruction *zero = isFloatOperand
                              ? createFloatConstant(zeroType, 0.0)
                              : createIntegerConstant(zeroType, 0);
    rightBool =
        addBinaryOpeartion(isFloatOperand ? IR_E_FNE : IR_E_NE, rightOp, zero,
                           irType, expr->type, expr);
  }

  // Where the right operand *ends*, which is not where it began if it split
  // the block itself - a nested && / || or a ternary leaves translation in the
  // exit block it created rather than in 'scnd'. gotoToBlock() takes the edge
  // from ctx->currentBB, so that block, not 'scnd', is the predecessor of
  // 'exit', and it is the one the phi has to name: stage 0 pairs a phi's
  // incoming blocks against the block's predecessor list by position and
  // asserts they agree (destroyPhisOfBlock, src/ir/codegen/prepare.c).
  IrBasicBlock *scndEnd = ctx->currentBB;
  gotoToBlock(exit);

  ctx->currentBB = exit;
  IrInstruction *phi = newPhiInstruction(irType);
  // Reaching the join without evaluating the right operand means the left one
  // already decided the answer: false for '&&', true for '||'. The left
  // operand's own value never reaches here.
  addPhiInput(phi, createIntegerConstant(irType, isAndAnd ? 0 : 1), fst);
  addPhiInput(phi, rightBool, scndEnd);

  addInstruction(phi);

  phi->meta.astExpr = expr;
  phi->astType = expr->type;

  return phi;
}

static enum IrIntructionKind getBinaryArith(ExpressionType op,
                                            Boolean isFloatOperand) {
  // TODO: fix address arith

  enum IrIntructionKind k = IR_BAD;

  switch (op) {
  case EB_ADD:
    k = isFloatOperand ? IR_E_FADD : IR_E_ADD;
    break;
  case EB_SUB:
    k = isFloatOperand ? IR_E_FSUB : IR_E_SUB;
    break;
  case EB_MUL:
    k = isFloatOperand ? IR_E_FMUL : IR_E_MUL;
    break;
  case EB_DIV:
    k = isFloatOperand ? IR_E_FDIV : IR_E_DIV;
    break;
  // Like the bitwise operators below: C's '%' takes integer operands only, so
  // sema has already rejected the float case and IR_E_FMOD has no producer.
  case EB_MOD:
    assert(!isFloatOperand);
    k = IR_E_MOD;
    break;
  case EB_LHS:
    assert(!isFloatOperand);
    k = IR_E_SHL;
    break;
  case EB_RHS:
    assert(!isFloatOperand);
    k = IR_E_SHR;
    break;
  case EB_AND:
    assert(!isFloatOperand);
    k = IR_E_AND;
    break;
  case EB_OR:
    assert(!isFloatOperand);
    k = IR_E_OR;
    break;
  case EB_XOR:
    assert(!isFloatOperand);
    k = IR_E_XOR;
    break;
  case EB_EQ:
    k = isFloatOperand ? IR_E_FEQ : IR_E_EQ;
    break;
  case EB_NE:
    k = isFloatOperand ? IR_E_FNE : IR_E_NE;
    break;
  case EB_LT:
    k = isFloatOperand ? IR_E_FLT : IR_E_LT;
    break;
  case EB_GT:
    k = isFloatOperand ? IR_E_FGT : IR_E_GT;
    break;
  case EB_LE:
    k = isFloatOperand ? IR_E_FLE : IR_E_LE;
    break;
  case EB_GE:
    k = isFloatOperand ? IR_E_FGE : IR_E_GE;
    break;
  default:
    unreachable("wtf");
  }

  return k;
}

static ExpressionType assignArithToArith(ExpressionType op) {
  switch (op) {
  case EB_ASG_ADD:
    return EB_ADD;
  case EB_ASG_SUB:
    return EB_SUB;
  case EB_ASG_MUL:
    return EB_MUL;
  case EB_ASG_DIV:
    return EB_DIV;
  case EB_ASG_MOD:
    return EB_MOD;
  case EB_ASG_SHL:
    return EB_LHS;
  case EB_ASG_SHR:
    return EB_RHS;
  case EB_ASG_AND:
    return EB_AND;
  case EB_ASG_XOR:
    return EB_XOR;
  case EB_ASG_OR:
    return EB_OR;
  default:
    unreachable("WTF?");
  }

  return E_NUM_OF_OPS;
}

// 'value' as a value of 'type', widening it if it is narrower.
//
// An operand used at more than its own width reads bytes that nothing wrote,
// and selection is not the place to invent the conversion: what goes in those
// bytes follows from the source's signedness, which is a fact about the
// expression and not about the machine. docs/ir-codegen-design.md section 10.
static IrInstruction *widenOperand(IrInstruction *value, enum IrTypeKind type,
                                   AstExpression *expr) {
  if (irTypeMachineSize(value->type) >= irTypeMachineSize(type)) {
    return value;
  }

  IrInstruction *cast = newInstruction(IR_E_BITCAST, type);
  cast->info.fromCastType = value->type;
  cast->meta.astExpr = expr;
  addInstructionInput(cast, value);
  addInstruction(cast);

  return cast;
}

static IrInstruction *translateBinary(AstExpression *expr) {
  assert(isBinary(expr->op));

  IrInstruction *leftOp = translateRValue(expr->binaryExpr.left);
  IrInstruction *rightOp = translateRValue(expr->binaryExpr.right);

  enum IrTypeKind type = typeRefToIrType(expr->type);
  Boolean isFloatOperand = isRealType(expr->binaryExpr.left->type);
  enum IrIntructionKind k = getBinaryArith(expr->op, isFloatOperand);

  assert(k != IR_BAD);

  assert(leftOp != NULL);
  assert(rightOp != NULL);

  // 'p == 0' reaches here as a pointer against an 'int' zero: a null pointer
  // constant is one by C99 6.3.2.3p3 whatever it is spelled as, and sema
  // leaves the conversion to the pointer type implicit. Comparing the two
  // would read eight bytes of a four-byte value, so the narrow side is
  // widened, by its own signedness, before the comparison sees it.
  if (!isFloatOperand && isIntegerComparisonKind(k)) {
    leftOp = widenOperand(leftOp, rightOp->type, expr);
    rightOp = widenOperand(rightOp, leftOp->type, expr);
  } else if (isIntegerLikeIrType(type)) {
    // The same for an operand of the operation itself. Pointer arithmetic is
    // where it happens: 'p + i' is desugared in the parser into a scaling of
    // 'i' at pointer width, and the multiplication it builds keeps the index's
    // own 'int' type on one side. A shift's count is not a value of the
    // shifted type, so it is left alone.
    leftOp = widenOperand(leftOp, type, expr);
    if (k != IR_E_SHL && k != IR_E_SHR) {
      rightOp = widenOperand(rightOp, type, expr);
    }
  }

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

  IrInstruction *lvalue = translateLValue(assignee);
  IrInstruction *rvalue = translateRValue(value);

  // TODO: heavy_copy_1 = heavy_copy_2 = heavy_struct

  if (isCompositeType(value->type)) {
    generateCompositeCopy(value->type, rvalue, lvalue, expr);
  } else {
    // The assignee's type, not the value's: what decides whether this is a
    // read-modify-write of a storage unit is where the value is going, and the
    // right hand side of 'p.field = 1' is an ordinary int.
    if (assignee->type->kind == TR_BITFIELD) {
      enum IrTypeKind irMemType =
          typeRefToIrType(assignee->type->bitFieldDesc.storageType);
      IrInstruction *storageOp = addLoadInstr(irMemType, lvalue, expr);
      rvalue = encodeBitField(assignee->type, storageOp, rvalue);
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
  if (!isFloat && isIntegerLikeIrType(valueType)) {
    lhs = widenOperand(lhs, valueType, expr);
    if (ik != IR_E_SHL && ik != IR_E_SHR) {
      rvalue = widenOperand(rvalue, valueType, expr);
    }
  }

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

  // The address `*E` designates is the *value* of E, so the operand is always
  // evaluated as an rvalue, no matter how the dereference itself is consumed.
  // (For flat operands - arrays/structs - the rvalue *is* the address, so
  // translateRValue does not insert a load and this stays a no-op.)
  IrInstruction *lvalue = translateRValue(expr->unaryExpr.argument);
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
  case EU_PLUS:
    result = arg;
    break;
  case EU_MINUS: {
    IrInstruction *zeroConst = isFloat ? createFloatConstant(irType, 0.0)
                                       : createIntegerConstant(irType, 0);
    enum IrIntructionKind op = isFloat ? IR_E_FSUB : IR_E_SUB;
    result = newInstruction(op, irType);
    addInstructionInput(result, zeroConst);
    addInstructionInput(result, arg);
    addInstruction(result);
    break;
  }
  case EU_EXL:
    // '!x' is 'x == 0', and for a floating operand that comparison has to be a
    // floating one: IR_U_NOT selects an integer 'test', which given an xmm
    // operand is not an instruction that exists. The same choice
    // translateLogicalExpression makes for the operands of '&&' and '||'.
    if (isRealType(expr->unaryExpr.argument->type)) {
      IrInstruction *zero = createFloatConstant(arg->type, 0.0);
      result = addBinaryOpeartion(IR_E_FEQ, arg, zero, irType, type, expr);
      break;
    }
    exl = TRUE;
  case EU_TILDA: {
    assert(!isFloat);
    enum IrIntructionKind op = exl ? IR_U_NOT : IR_U_BNOT;
    result = newInstruction(op, irType);
    addInstructionInput(result, arg);
    addInstruction(result);
    break;
  }
  default:
    unreachable("wtf?");
  }

  assert(result != NULL);
  result->astType = type;
  result->meta.astExpr = expr;

  return result;
}

static IrInstruction *handleMemoryMode(IrInstruction *ptr, TypeRef *valueType,
                                       AstExpression *expr) {
  if (ctx->addressTM == IR_TM_LVALUE || isFlatType(valueType))
    return ptr;

  IrInstruction *loadInstr =
      addLoadInstr(typeRefToIrType(valueType), ptr, expr);
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
    IrInstruction *elementSizeOp =
        computeVLAElementType(vlaElementType->vlaDescriptor.elementType);
    AstExpression *sizeExpr = vlaElementType->vlaDescriptor.sizeExpression;
    assert(sizeExpr != NULL);
    // Cache at the array declaration
    IrInstruction *arraySize =
        widenOperand(translateRValue(sizeExpr), IR_U64, sizeExpr);
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
    pointerType = makePointedType(ctx->pctx, arrayType->flags.storage,
                                  arrayType->arrayTypeDesc.elementType);
  } else if (arrayType->kind == TR_VLA) {
    pointerType = makePointedType(ctx->pctx, arrayType->flags.storage,
                                  arrayType->vlaDescriptor.elementType);
  } else {
    pointerType = arrayType;
  }

  TypeRef *elementType = pointerType->pointed;
  int32_t indexOrigSize = computeTypeSize(index->type);
  TypeRef *indexType = makePrimitiveType(
      ctx->pctx, isUnsignedType(index->type) ? T_U8 : T_S8, 0);

  Boolean isFlat =
      (base->type->kind == TR_ARRAY /*|| base->type->kind == TR_VLA*/) &&
      base->op != E_CONST;

  const enum IrTypeKind indexIrType = typeRefToIrType(indexType);

  IrInstruction *baseInstr = translateRValue(base);
  // Widened before it is scaled, not after: the scaling happens at the width
  // of an address, and an 'int' index only fills half of one. Which half the
  // rest is - the sign of a negative index or zeroes - is what the index's own
  // type says, and nothing below this point still knows it.
  IrInstruction *indexInstr =
      widenOperand(translateRValue(index), indexIrType, index);

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
        IrInstruction *elementSizeOpScale =
            createIntegerConstant(IR_I32, log2Integer(elementSize));
        scaledIndexOp = newInstruction(IR_E_SHL, indexIrType);
        addInstructionInput(scaledIndexOp, indexInstr);
        addInstructionInput(scaledIndexOp, elementSizeOpScale);
      } else {
        IrInstruction *elementSizeOp =
            createIntegerConstant(indexIrType, elementSize);
        scaledIndexOp = newInstruction(IR_E_MUL, indexIrType);
        addInstructionInput(scaledIndexOp, indexInstr);
        addInstructionInput(scaledIndexOp, elementSizeOp);
      }
      addInstruction(scaledIndexOp);
      scaledIndexOp->meta.astExpr = expr;
      scaledIndexOp->astType = indexType;
    } else if (elementSize == 0) {
      // An empty struct, which this frontend accepts as GCC does. Every
      // element of an array of them is at the same address, so the index
      // scales to nothing at all rather than to itself.
      scaledIndexOp = createIntegerConstant(IR_I64, 0);
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

  IrInstruction *gepInstr =
      newGEPInstruction(baseInstr, scaledIndexOp, elementType);
  gepInstr->astType = pointerType;
  gepInstr->meta.astExpr = expr;
  gepInstr->info.gep.indexInstr = indexInstr;
  addInstruction(gepInstr);

  // A row of a multidimensional VLA is an address and nothing else - there is
  // no pointer stored anywhere to load, unlike the VLA variable itself, whose
  // slot does hold one. isFlatType() cannot say so: it answers about the type
  // alone, and a VLA is a flat object here and a pointer variable there. The
  // legacy pipeline draws the same line in the same place, by not wrapping the
  // sum in a dereference (cannonizeArrayAccess, src/cannonization.c).
  if (elementType->kind == TR_VLA) {
    return gepInstr;
  }

  return handleMemoryMode(gepInstr, elementType, expr);
}

static IrInstruction *translateFieldAccess(AstExpression *expr, Boolean isDot) {
  AstExpression *receiverExpr = expr->fieldExpr.recevier;
  IrInstruction *receiver =
      isDot ? translateLValue(receiverExpr) : translateRValue(receiverExpr);

  int64_t memberOffset = effectiveMemberOffset(expr->fieldExpr.member);
  IrInstruction *memberOffsetOp = createIntegerConstant(IR_I64, memberOffset);
  TypeRef *memberType = expr->fieldExpr.member->type;

  IrInstruction *gepInstr =
      newGEPInstruction(receiver, memberOffsetOp, memberType);
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

  return translateFieldAccess(expr, /* isDOT = */ FALSE);
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

  TypeRef *type = expr->type;
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

  // The *old* value, which is the whole difference between 'i++' and '++i'.
  // Returning the operation's result instead made 'if (i++ == 3)' compare
  // i + 1, and the store above is what carries the increment.
  return oldValue;
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

  default:
    unreachable("unexpcted expression op");
  }
  return NULL;
}

// -============================ statements =============================-

static Boolean translateStatement(AstStatement *stmt) {
  switch (stmt->statementKind) {
  case SK_BLOCK:
    return translateBlock(stmt);
  case SK_DECLARATION:
    return translateDeclaration(stmt->declStmt.declaration);
  case SK_EMPTY:
    return FALSE;
  case SK_EXPR_STMT:
    translateExpression(stmt->exprStmt.expression);
    return FALSE;
  case SK_LABEL:
    return translateLabel(stmt);
  case SK_GOTO_L:
    return translateGotoLabel(stmt);
  case SK_GOTO_P:
    return translateGotoPtr(stmt);
  case SK_RETURN:
    return translateReturn(stmt);
  case SK_BREAK:
    return translateBreak(stmt);
  case SK_CONTINUE:
    return translateContinue(stmt);
  case SK_IF:
    return translateIf(stmt);
  case SK_SWITCH:
    return translateSwitch(stmt);
  case SK_WHILE:
    return translateWhile(stmt);
  case SK_DO_WHILE:
    return translateDoWhile(stmt);
  case SK_FOR:
    return translateFor(stmt);
  default:
    unreachable("Unknown statement kind");
    return TRUE;
  }
}

static Boolean translateBlock(AstStatement *block) {

  IrBasicBlock *bb = ctx->currentBB;

  if (bb == NULL ||
      bb->term != NULL) { // emit into existed block if it not terminated
    bb = updateBlock();
    bb->ast = block;
  }

  Boolean opensScope = blockDeclaresVla(block);
  Boolean opened = FALSE;

  AstStatementList *stmt = block->block.stmts;
  Boolean terminated = FALSE;

  while (stmt != NULL) {
    // At the declaration and not at the head of the block: a label may sit
    // above the declaration and be jumped to from outside - legal C, the array
    // not being in scope there - and a save taken at the head is skipped by
    // exactly that jump while the restore below still runs.
    if (opensScope && !opened && declaresVla(stmt->stmt)) {
      opened = enterStackScope(TRUE);
    }

    terminated |= translateStatement(stmt->stmt);
    stmt = stmt->next;
  }

  leaveStackScope(opened);

  return terminated;
}

// A variable declared inside a function but living in static storage: a local
// 'static', or a local 'extern' declaration of something defined elsewhere.
//
// There is nothing to translate - a reference to one is a symbol constant like
// any other global's, and its initializer is a compile-time constant the data
// section carries rather than code. What there is to do is remember it, so
// that whoever emits this function also emits its storage; see
// IrFunction.staticLocals.
static void translateGlobalVariable(AstValueDeclaration *v) {
  assert(!v->flags.bits.isLocal && "Should be non-local storaged variable");

  // An 'extern' declaration defines nothing; the definition is another
  // translation unit's, and the linker is what connects them.
  if (v->flags.bits.isExternal)
    return;

  // A file-scope variable arrives here too, from the translation unit walk in
  // translateAstToIr(), and that one is already emitted by
  // generateCodeForFile(). Only a declaration inside a function - which is
  // what having a current function means - needs listing.
  if (ctx->currentFunc == NULL) {
    return;
  }

  addToVector(&ctx->currentFunc->staticLocals, (intptr_t)v);
}

// Emits the alloca backing a local into whatever block is current, and records
// it in the local map. 'size' is the aligned slot size, or -1 for a VLA, whose
// size is 'sizeInstr' rather than a constant.
//
// Callers decide the block, and for everything but a VLA that block is the
// function's entry (see buildInitialIr). C99 6.2.4p6 puts an automatic
// object's lifetime over the whole block it is declared in, not just the part
// after the declaration, so the point where the declaration textually sits is
// not where its storage begins - and a 'goto' into the middle of a block makes
// the difference observable. Jumping past a declaration leaves the block
// holding its alloca unreachable while the loads and stores after the label
// stay live, so an alloca emitted in place ends up as a value defined in dead
// code and used from live code. No later pass can repair that: SSA
// construction needs the def to dominate its uses, and cleanupUnreachableBlock
// (src/ir/dce.c) waits for such a block to go empty, which it never does.
// The entry block dominates everything, so allocating there is both what the
// standard describes and what keeps the IR well-formed.
//
// A VLA is the exception and stays where it is declared: its size is computed
// at run time, and C99 6.8.6.1p1 forbids jumping into its scope anyway.
static IrInstruction *createLocalSlot(AstValueDeclaration *v,
                                      IrInstruction *sizeInstr, size_t size) {
  assert(v->flags.bits.isLocal);
  assert(v->index2 >= 0);

  LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
  assert(lvi->stackSlot == NULL && "double-allocated variable");

  TypeRef *astType = v->type;
  IrInstruction *stackSlot = NULL;

  if (astType->kind == TR_VLA) {
    // A VLA is a pointer variable. Its object is carved out of the stack where
    // the declaration sits, and what the variable itself holds is the address
    // of that object - which is what every read of the name loads, here and in
    // the legacy backend both. So it takes two allocations rather than one: a
    // dynamically sized block for the object, and a fixed word pointing at it.
    //
    // Nothing wrote that word before. The read side has always loaded it, so a
    // VLA read back whatever its own first eight bytes happened to hold; it
    // went unnoticed because a dynamically sized allocation was refused by
    // selection and the function fell back before any of this ran.
    IrInstruction *memory = createAllocaInstr(sizeInstr);

    memory->info.alloca.sizeInstr = sizeInstr;
    memory->info.alloca.valueType = typeRefToIrType(astType);
    memory->astType = makePointedType(ctx->pctx, 0, astType);
    // Both carry the declaration, because both are part of what 'v' is: the
    // frame object naming the storage, and the word naming the variable. Only
    // one of them usually survives - mem2reg has no trouble with a pointer
    // that is only ever stored once and loaded.
    memory->info.alloca.v = v;

    stackSlot = createAllocaSlot(sizeof(intptr_t));
    addStoreInstr(stackSlot, memory, NULL);
  } else {
    stackSlot = createAllocaInstr(sizeInstr);
    stackSlot->info.alloca.stackSize = size;
  }

  stackSlot->info.alloca.v = v;
  stackSlot->info.alloca.valueType = typeRefToIrType(astType);
  stackSlot->astType = makePointedType(ctx->pctx, 0, astType);

  lvi->declaration = v;
  lvi->stackSlot = stackSlot;

  return stackSlot;
}

// The aligned size of a local's slot. Not meaningful for a VLA.
static size_t localSlotSize(const AstValueDeclaration *v) {
  assert(v->type->kind != TR_VLA);
  return ALIGN_SIZE(computeTypeSize(v->type), sizeof(intptr_t));
}

static void translateLocalDeclaration(AstValueDeclaration *v) {
  assert(v->flags.bits.isLocal);
  assert(v->index2 >= 0);

  trace("Translate local variable '%s'..., next = %p, initizlier = %p..\n",
        v->name, v->next, v->initializer);

  LocalValueInfo *lvi = &ctx->localOperandMap[v->index2];
  TypeRef *astType = v->type;
  AstInitializer *init = v->initializer;

  IrInstruction *stackSlot = lvi->stackSlot;
  size_t size = -1;

  if (stackSlot == NULL) {
    // Only a VLA gets this far without a slot - everything else was allocated
    // in the entry block before the body was translated.
    assert(astType->kind == TR_VLA);
    assert(init != NULL);
    assert(init->kind == IK_EXPRESSION);
    stackSlot = createLocalSlot(v, translateExpression(init->expression), size);
    init = NULL;
  } else {
    size = localSlotSize(v);
  }

  if (init) {
    assert(size != -1);
    trace(" translate initializer for variable '%s' (%c%u)\n", v->name, '%',
          stackSlot->id);
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
      // assert(varDecl->flags.bits.isStatic);
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
  IrBasicBlock *elseBB =
      elseStmt != NULL ? newBasicBlock("<if_else>") : continueBB;

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

// Whether this statement is a declaration of a variable-length array - the one
// thing that carves storage out of the stack and, by C99 6.2.4p6, owes it back
// at the end of the block that declared it.
//
// A call to alloca() deliberately does not count. Its storage lives until the
// function returns, not until the end of the block, so a scope must not
// reclaim it - and gcc agrees: an alloca() in a loop grows the frame every
// time round and the first block is still readable at the end.
static Boolean declaresVla(const AstStatement *stmt) {
  if (stmt == NULL || stmt->statementKind != SK_DECLARATION) {
    return FALSE;
  }

  const AstDeclaration *d = stmt->declStmt.declaration;
  return d->kind == DK_VAR && d->variableDeclaration->flags.bits.isLocal &&
         d->variableDeclaration->type->kind == TR_VLA;
}

// A block owes storage back when it declares a variable-length array of its
// own. Nested blocks are not looked into: each one answers for itself, which
// is what makes the depth of the scope stack mean the same thing here and in
// the pre-pass that measures where a label sits.
//
// Only a block and a 'for' initializer can hold a declaration at all - in C a
// declaration is not a statement - so those are the only two shapes that open
// a scope.
static Boolean blockDeclaresVla(const AstStatement *block) {
  assert(block->statementKind == SK_BLOCK);

  // The function's own body is the one block that never needs a mark. The
  // only ways out of it are a 'return' and falling off the end, both of which
  // land in the epilogue, and 'leave' puts rsp back from the frame pointer
  // there - so a save taken here would only ever be dead.
  if (block == ctx->currentFunc->ast->body) {
    return FALSE;
  }

  for (const AstStatementList *s = block->block.stmts; s != NULL; s = s->next) {
    if (declaresVla(s->stmt)) {
      return TRUE;
    }
  }

  return FALSE;
}

static Boolean forInitDeclaresVla(const AstStatement *stmt) {
  assert(stmt->statementKind == SK_FOR);

  for (const AstStatementList *s = stmt->forStmt.initial; s != NULL;
       s = s->next) {
    if (declaresVla(s->stmt)) {
      return TRUE;
    }
  }

  return FALSE;
}

// Opens a scope: the stack pointer as the scope found it, taken at the
// declaration that opens it rather than at the head of the block. Every path
// that can reach a restore crosses the declaration, so the save dominates all
// of them - landing below the declaration without having crossed it is what
// C99 6.8.6.1p1 forbids and verifyVlaJumps rejects.
static Boolean enterStackScope(Boolean opens) {
  if (!opens) {
    return FALSE;
  }

  IrInstruction *save = newInstruction(IR_STACK_SAVE, IR_PTR);
  addInstruction(save);
  pushToStack(&ctx->stackScopes, (intptr_t)save);
  return TRUE;
}

// Puts the stack pointer back to where scope 'depth' was entered, which undoes
// that scope and everything nested inside it. One restore however many scopes
// are being left at once: the outermost of them was saved before all the
// others, so its mark is the one they all sit above.
static void restoreStackScopes(size_t depth) {
  if (ctx->stackScopes.size <= depth) {
    return;
  }

  // Nothing runs after a terminator, and the jump this restore belongs in
  // front of will not be emitted either.
  if (ctx->currentBB == NULL || ctx->currentBB->term != NULL) {
    return;
  }

  IrInstruction *save = (IrInstruction *)getFromVector(&ctx->stackScopes, depth);
  IrInstruction *restore = newInstruction(IR_STACK_RESTORE, IR_VOID);
  addInstructionInput(restore, save);
  addInstruction(restore);
}

// Leaves by falling off the end, which is the one edge out of a scope that is
// not a jump.
static void leaveStackScope(Boolean opened) {
  if (!opened) {
    return;
  }

  assert(ctx->stackScopes.size > 0);
  restoreStackScopes(ctx->stackScopes.size - 1);
  popOffStack(&ctx->stackScopes, 1);
}

// Where a 'goto' target sits, as a depth into the scope stack. A valid program
// only ever jumps outwards or sideways past whole scopes (C99 6.8.6.1p1), so
// the label's depth is a prefix of the jump's and counting is enough to say
// which scopes are being left.
static size_t labelScopeDepth(const char *label) {
  intptr_t biased = getFromHashMap(ctx->labelScopeMap, (intptr_t)label);

  // A label the pre-pass never saw is one no valid program jumps to. Claiming
  // the current depth restores nothing, which costs stack rather than
  // correctness.
  return biased == 0 ? ctx->stackScopes.size : (size_t)(biased - 1);
}

// The same question for a 'goto *', whose target is a value rather than a
// name. Every label it can reach is one whose address was taken, and the
// deepest of them is the only depth that is honest for all of them: restoring
// to it is exact for that one and hands back too little for the shallower
// ones, whereas going by any shallower target would free a scope a deeper one
// is still entitled to read.
static size_t indirectTargetScopeDepth(void) {
  if (ctx->referencedBlocks.size == 0) {
    return ctx->stackScopes.size;
  }

  size_t depth = 0;

  for (uint32_t idx = 0; idx < ctx->referencedBlocks.size; ++idx) {
    const IrBasicBlock *b = getBlockFromVector(&ctx->referencedBlocks, idx);
    intptr_t biased = getFromHashMap(ctx->labelScopeMap, (intptr_t)b->name);

    // A target the pre-pass never measured: restore nothing rather than guess.
    if (biased == 0) {
      return ctx->stackScopes.size;
    }

    if ((size_t)(biased - 1) > depth) {
      depth = (size_t)(biased - 1);
    }
  }

  return depth;
}

// Measures every label in the function against the same scope rule translation
// applies, before translation starts - a 'goto' is routinely written above the
// label it names, and it has to know then how far out it is jumping.
static void collectLabelScopeDepths(const AstStatement *stmt, size_t depth) {
  if (stmt == NULL) {
    return;
  }

  switch (stmt->statementKind) {
  case SK_BLOCK: {
    // Positional, exactly as translateBlock is: the scope opens at the
    // declaration, so a label above it is still outside. Jumping to one below
    // it is what verifyVlaJumps rejects, so the two walks only ever have to
    // agree about the labels a valid program can reach.
    Boolean scoped = blockDeclaresVla(stmt);
    Boolean opened = FALSE;
    size_t inner = depth;

    for (const AstStatementList *s = stmt->block.stmts; s != NULL; s = s->next) {
      if (scoped && !opened && declaresVla(s->stmt)) {
        inner = depth + 1;
        opened = TRUE;
      }
      collectLabelScopeDepths(s->stmt, inner);
    }
    return;
  }
  case SK_LABEL:
    if (stmt->labelStmt.kind == LK_LABEL) {
      putToHashMap(ctx->labelScopeMap, (intptr_t)stmt->labelStmt.label,
                   (intptr_t)(depth + 1));
    }
    collectLabelScopeDepths(stmt->labelStmt.body, depth);
    return;
  case SK_IF:
    collectLabelScopeDepths(stmt->ifStmt.thenBranch, depth);
    collectLabelScopeDepths(stmt->ifStmt.elseBranch, depth);
    return;
  case SK_SWITCH:
    collectLabelScopeDepths(stmt->switchStmt.body, depth);
    return;
  case SK_WHILE:
  case SK_DO_WHILE:
    collectLabelScopeDepths(stmt->loopStmt.body, depth);
    return;
  case SK_FOR:
    collectLabelScopeDepths(stmt->forStmt.body,
                            forInitDeclaresVla(stmt) ? depth + 1 : depth);
    return;
  default:
    return;
  }
}

static Boolean translateWhile(AstStatement *stmt) {
  assert(stmt->statementKind == SK_WHILE);

  IrBasicBlock *oldBreakBB = ctx->breakBB;
  IrBasicBlock *oldContinueBB = ctx->continueBB;

  AstExpression *condition = stmt->loopStmt.condition;
  AstStatement *body = stmt->loopStmt.body;

  size_t oldBreakDepth = ctx->breakScopeDepth;
  size_t oldContinueDepth = ctx->continueScopeDepth;
  ctx->breakScopeDepth = ctx->continueScopeDepth = ctx->stackScopes.size;

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
  ctx->continueScopeDepth = oldContinueDepth;
  ctx->breakScopeDepth = oldBreakDepth;
  return FALSE;
}

static Boolean translateDoWhile(AstStatement *stmt) {
  assert(stmt->statementKind == SK_DO_WHILE);

  IrBasicBlock *oldBreakBB = ctx->breakBB;
  IrBasicBlock *oldContinueBB = ctx->continueBB;

  AstStatement *body = stmt->loopStmt.body;
  AstExpression *condition = stmt->loopStmt.condition;

  size_t oldBreakDepth = ctx->breakScopeDepth;
  size_t oldContinueDepth = ctx->continueScopeDepth;
  ctx->breakScopeDepth = ctx->continueScopeDepth = ctx->stackScopes.size;

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
  ctx->continueScopeDepth = oldContinueDepth;
  ctx->breakScopeDepth = oldBreakDepth;
  return FALSE;
}

static Boolean translateFor(AstStatement *stmt) {
  assert(stmt->statementKind == SK_FOR);

  AstStatementList *decl = stmt->forStmt.initial;
  AstExpression *condition = stmt->forStmt.condition;
  AstExpression *modifier = stmt->forStmt.modifier;
  AstStatement *body = stmt->forStmt.body;

  // A VLA in the initializer clause is scoped to the whole statement, not to
  // an iteration of it: it is allocated once, before the mark the loop's own
  // jumps restore to, and given back where the 'for' ends.
  Boolean scope = enterStackScope(forInitDeclaresVla(stmt));

  while (decl != NULL) {
    translateStatement(decl->stmt);
    decl = decl->next;
  }

  IrBasicBlock *oldBreakBB = ctx->breakBB;
  IrBasicBlock *oldContinueBB = ctx->continueBB;
  size_t oldBreakDepth = ctx->breakScopeDepth;
  size_t oldContinueDepth = ctx->continueScopeDepth;
  ctx->breakScopeDepth = ctx->continueScopeDepth = ctx->stackScopes.size;

  IrBasicBlock *loopHead = newBasicBlock("<for_head>");
  IrBasicBlock *loopBody = newBasicBlock("<for_body>");
  IrBasicBlock *loopExit = newBasicBlock("<for_exit>");
  IrBasicBlock *modifierBB =
      modifier != NULL ? newBasicBlock("<for_mod>") : NULL;

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
  ctx->continueScopeDepth = oldContinueDepth;
  ctx->breakScopeDepth = oldBreakDepth;

  leaveStackScope(scope);
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

    // Nothing goes back, but the expression still runs: 'return f();' with a
    // zero-sized result is a call like any other.
    if (!isTypeRequiresReturnValue(expr->type)) {
      jumpToBlock(ctx->currentFunc->exit, stmt);
      return TRUE;
    }

    assert(returnSlot != NULL);
    if (isCompositeType(expr->type)) {
      IrInstruction *dst = returnSlot;

      // The slot holds the caller's buffer pointer rather than the value, so
      // the copy goes through it - which is also what leaves the result where
      // the caller is looking for it, with no second copy at the exit.
      if (returnsThroughHiddenPointer(expr->type)) {
        dst = addLoadInstr(IR_PTR, returnSlot, NULL);
      }

      copyInstr = generateCompositeCopy(expr->type, returnValue, dst, NULL);
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

  restoreStackScopes(ctx->breakScopeDepth);
  jumpToBlock(ctx->breakBB, stmt);
  return TRUE;
}

static Boolean translateContinue(AstStatement *stmt) {
  assert(stmt->statementKind == SK_CONTINUE);
  assert(ctx->continueBB != NULL);

  restoreStackScopes(ctx->continueScopeDepth);
  jumpToBlock(ctx->continueBB, stmt);
  return TRUE;
}

static IrBasicBlock *getOrCreateLabelBlock(const char *labelName) {
  HashMap *labelMap = ctx->labelMap;
  IrBasicBlock *block =
      (IrBasicBlock *)getFromHashMap(labelMap, (intptr_t)labelName);
  if (block != NULL)
    return block;

  block = newBasicBlock(labelName);
  putToHashMap(labelMap, (intptr_t)labelName, (intptr_t)block);
  return block;
}

static Boolean translateGotoLabel(AstStatement *stmt) {
  assert(stmt->statementKind == SK_GOTO_L);

  IrBasicBlock *labelBlock = getOrCreateLabelBlock(stmt->jumpStmt.label);

  restoreStackScopes(labelScopeDepth(stmt->jumpStmt.label));
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

  // After the target has been computed, not before: the expression naming it
  // may well be read out of the storage this hands back.
  restoreStackScopes(indirectTargetScopeDepth());

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

static unsigned walkCaseLabels(AstStatement *body, CaseBlock *caseBlocks,
                               unsigned idx) {
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
  case SK_DECLARATION:
    break;
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
    case LK_LABEL:
      return walkCaseLabels(body->labelStmt.body, caseBlocks, idx);
    case LK_CASE:
      caseBlocks[idx++].caseConst = body->labelStmt.caseConst;
      return walkCaseLabels(body->labelStmt.body, caseBlocks, idx) + 1;
    }

    break;
  default:
    unreachable("Unknown statement kind");
  }

  return 0;
}

static Boolean translateSwitch(AstStatement *stmt) {
  assert(stmt->statementKind == SK_SWITCH);

  IrBasicBlock *oldBreakBB = ctx->breakBB;
  IrBasicBlock *oldDefaultCaseBB = ctx->defaultCaseBB;

  SwitchTable *oldSwitchTable = ctx->switchTable;

  SwitchTable *switchTable = areanAllocate(
      ctx->irArena,
      sizeof(SwitchTable) + stmt->switchStmt.caseCount * sizeof(CaseBlock));
  CaseBlock *caseBlocks = (CaseBlock *)(&switchTable[1]);

  switchTable->caseCount = stmt->switchStmt.caseCount;
  switchTable->caseBlocks = caseBlocks;

  memset(caseBlocks, 0, sizeof(CaseBlock) * switchTable->caseCount);

  IrBasicBlock *switchExitBB = newBasicBlock("<switch_exit>");
  IrBasicBlock *defaultBB = stmt->switchStmt.hasDefault
                                ? newBasicBlock("<default_case>")
                                : switchExitBB;

  size_t oldBreakDepth = ctx->breakScopeDepth;
  ctx->breakScopeDepth = ctx->stackScopes.size;

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

  switchTable->defaultBB = defaultBB;
  addSuccessor(ctx->currentBB, defaultBB);
  termintateBlock(tableBranch);

  IrBasicBlock *switchBody = newBasicBlock("<switch_body>");
  ctx->currentBB = switchBody;
  translateStatement(stmt->switchStmt.body);

  jumpToBlock(switchExitBB, stmt);
  ctx->currentBB = switchExitBB;

  ctx->switchTable = oldSwitchTable;
  ctx->breakBB = oldBreakBB;
  ctx->breakScopeDepth = oldBreakDepth;
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
    for (const AstInitializerList *inits = init->initializerList; inits != NULL;
         inits = inits->next) {
      collectRerenecedLabelsInit(inits->initializer);
    }
    break;
  default:
    unreachable("Unexpected initializer type");
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
    return;
  case E_VA_ARG:
    return collectTranslationInfoExpr(expr->vaArg.va_list);
  case E_COMPOUND:
    return collectRerenecedLabelsInit(expr->compound);
  case E_CALL:
    collectTranslationInfoExpr(expr->callExpr.callee);
    for (const AstExpressionList *arg = expr->callExpr.arguments; arg;
         arg = arg->next) {
      collectTranslationInfoExpr(arg->expression);
    }
    return;
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
          assert(!vd->flags.bits.isRegister &&
                 "This should be verified during Sema analysis");
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

  default:
    unreachable("unexpected expression op");
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
      const AstValueDeclaration *v =
          stmt->declStmt.declaration->variableDeclaration;
      if (v->initializer && v->flags.bits.isLocal) {
        return collectRerenecedLabelsInit(v->initializer);
      }
    }
    return;
  case SK_BREAK:
  case SK_CONTINUE:
  case SK_GOTO_L:
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
  default:
    unreachable("Unknown statement kind");
  }
}

static void collectTranslationInfo(const AstStatement *body) {
  assert(ctx->labelMap != NULL &&
         "Label map need to be allocated at this point");
  assert(ctx->localOperandMap != NULL &&
         "Local Operand map need to be allocated at this point");

  collectTranslationInfoStmt(body);
  collectLabelScopeDepths(body, 0);
}

static void generateExitBlock(IrFunction *func, TypeRef *returnType) {
  ctx->currentBB = func->exit;

  IrInstruction *ret = newInstruction(IR_RET, IR_VOID);
  if (isTypeRequiresReturnValue(returnType)) {
    assert(func->retOperand != NULL);

    // Whatever the return type, the slot holds what the ABI hands back and the
    // exit block reads it out. For a large composite that is the caller's
    // buffer pointer, which is returned as well as written through; for a
    // small one it is the eightbyte the value travels in, so it is read as
    // IR_P_AGG rather than as the struct it spells.
    TypeRef *slotType = returnType;
    enum IrTypeKind valueType = IR_P_AGG;

    if (returnsThroughHiddenPointer(returnType)) {
      slotType = makePointedType(ctx->pctx, 0, returnType);
      valueType = IR_PTR;
    } else if (!isCompositeType(returnType)) {
      valueType = typeRefToIrType(returnType);
    }

    IrInstruction *retValue = addLoadInstr(valueType, func->retOperand, NULL);
    retValue->astType = slotType;
    addInstructionInput(ret, retValue);
  }

  termintateBlock(ret);
}

static void initializeParamterLocal(IrBasicBlock *entryBB,
                                    IrInstruction *stackPtrOp,
                                    ParamtersABIInfo *paramInfo) {

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
    stackSlot->astType =
        makePointedType(ctx->pctx, astType->flags.storage, astType);
    lvi->stackSlot = stackSlot;

    IrInstruction *regInstr = newPhysRegister(type, paramInfo->loc.physReg);
    addInstruction(regInstr);

    addStoreInstr(stackSlot, regInstr, NULL);
    stackSlot->info.alloca.v = param;
  } else {
    param->index2 = paramIndex;
    lvi->declaration = param;

    IrInstruction *offset =
        createIntegerConstant(IR_I64, paramInfo->loc.stackOffset);
    IrInstruction *addInstr = newInstruction(IR_E_ADD, IR_P_AGG);
    addInstructionInput(addInstr, stackPtrOp);
    addInstructionInput(addInstr, offset);
    addInstruction(addInstr);
    lvi->stackSlot = addInstr;
    lvi->frameOffset = paramInfo->loc.stackOffset;
  }
}

// Parameter classification itself lives in the TargetDescriptor now (see
// classifyParametersGeneric in src/ir/target.c); this only wires the result
// up to the LocalValueInfo slots the rest of the translation indexes by.
static uint32_t computeParametersABIInfo(AstFunctionDeclaration *declaration,
                                         ParamtersABIInfo *infos,
                                         size_t numberOfParams,
                                         LocalValueInfo *lvis,
                                         ParametersABISummary *summary) {

  ctx->target->classifyParameters(ctx->target, declaration, infos,
                                  numberOfParams, summary);

  for (uint32_t idx = 0; idx < numberOfParams; ++idx) {
    infos[idx].lvi = &lvis[idx];
  }

  return numberOfParams;
}

static size_t generateVaArea(AstValueDeclaration *va_area,
                             const ParametersABISummary *summary,
                             IrInstruction *stackPtrOp, LocalValueInfo *infos,
                             size_t idx) {
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

  // These have to be the member offsets of __va_elem as sdk/include/stdarg.h
  // declares it - {unsigned gp_offset; unsigned fp_offset; void
  // *overflow_arg_area; const void *reg_save_area;}, so 0/4/8/16 - because
  // this only writes the area and translateVaArg() reads it back through
  // findStructualMember() on the real struct. The two sides disagreeing is
  // silent: va_arg just picks up the wrong field.
  //
  // fp_offset_off used to be initialized from itself rather than from
  // gp_offset_off, so it took whatever the stack held; that fed the two
  // offsets below it and made the emitted IR differ between runs of the same
  // input. overflow_arg_area also advanced by a pointer rather than by the
  // second uint32_t that actually precedes it.
  int32_t gp_offset_off = 0;
  int32_t fp_offset_off = gp_offset_off + sizeof(uint32_t);
  int32_t overflow_arg_area_ptr_off = fp_offset_off + sizeof(uint32_t);
  int32_t reg_save_area_ptr_off = overflow_arg_area_ptr_off + dataSize;

  int32_t gp_va_area = ALIGN_SIZE(reg_save_area_ptr_off + dataSize, dataSize);
  int32_t fp_va_area =
      ALIGN_SIZE(gp_va_area + R_PARAM_COUNT * dataSize, dataSize);
  int32_t reg_save_area_offset = gp_va_area;

  IrInstruction *gp_offset_off_i = createIntegerConstant(IR_I64, gp_offset_off);

  // makePrimitiveType takes (id, flags); these five passed them the other way
  // round, so every type here was 'void' carrying a type id as its flags. It
  // showed up only as nonsense in the IR dump - a GEP's underlying type is not
  // what decides the width of the access - but the dump is how this area gets
  // checked.
  TypeRef *u32Type = makePrimitiveType(ctx->pctx, T_U4, 0);
  TypeRef *voidType = makePrimitiveType(ctx->pctx, T_VOID, 0);
  TypeRef *voidPtrType = makePointedType(ctx->pctx, 0, voidType);
  TypeRef *uintptrType = makePrimitiveType(ctx->pctx, T_U8, 0);
  TypeRef *floatType = makePrimitiveType(ctx->pctx, T_F8, 0);
  enum IrTypeKind iru32Type = IR_U32;

  // va_elem->reg_save_area = reg_save_area_begin
  IrInstruction *reg_save_area_off =
      createIntegerConstant(IR_I64, reg_save_area_offset);
  IrInstruction *reg_save_area_begin_ptr =
      newGEPInstruction(vaAreaSlot, reg_save_area_off, uintptrType);
  addInstruction(reg_save_area_begin_ptr);

  IrInstruction *reg_save_area_slot_off =
      createIntegerConstant(IR_I64, reg_save_area_ptr_off);
  IrInstruction *reg_save_area_slot_ptr =
      newGEPInstruction(vaAreaSlot, reg_save_area_slot_off, voidPtrType);
  addInstruction(reg_save_area_slot_ptr);
  addStoreInstr(reg_save_area_slot_ptr, reg_save_area_begin_ptr, NULL);

  // va_elem->gp_offset / fp_offset - where va_arg starts reading, which is past
  // whatever the named parameters already took. Both count from reg_save_area,
  // so the floating-point one starts beyond the whole integer half;
  // translateVaArg's bounds (R_PARAM_COUNT and R_PARAM_COUNT +
  // R_FP_PARAM_COUNT) are the far end of that same measurement.
  IrInstruction *gp_offset_ptr =
      newGEPInstruction(vaAreaSlot, gp_offset_off_i, u32Type);
  addInstruction(gp_offset_ptr);
  addStoreInstr(
      gp_offset_ptr,
      createIntegerConstant(iru32Type, summary->intRegParams * dataSize), NULL);

  IrInstruction *fp_offset_ptr = newGEPInstruction(
      vaAreaSlot, createIntegerConstant(IR_I64, fp_offset_off), u32Type);
  addInstruction(fp_offset_ptr);
  addStoreInstr(fp_offset_ptr,
                createIntegerConstant(iru32Type,
                                      (fp_va_area - gp_va_area) +
                                          summary->fpRegParams * dataSize),
                NULL);

  // va_elem->overflow_arg_area = the first unnamed argument the caller left on
  // the stack. That is above the frame pointer, so it is spelled the way an
  // incoming stack parameter is - an offset off the stack operand, which
  // layoutIncomingParameters turns into a frame object - and takes a local slot
  // of its own to be found through. It used to be an offset into this frame's
  // own va area, which is a different piece of memory entirely.
  IrInstruction *stack_param_begin_ptr = newInstruction(IR_E_ADD, IR_P_AGG);
  addInstructionInput(stack_param_begin_ptr, stackPtrOp);
  addInstructionInput(stack_param_begin_ptr,
                      createIntegerConstant(IR_I64, summary->stackParamOffset));
  addInstruction(stack_param_begin_ptr);

  infos[idx + 1].stackSlot = stack_param_begin_ptr;
  infos[idx + 1].frameOffset = summary->stackParamOffset;
  infos[idx + 1].name = "<va_overflow_area>";

  IrInstruction *overflow_arg_area_slot_off =
      createIntegerConstant(IR_I64, overflow_arg_area_ptr_off);
  IrInstruction *overflow_arg_area_slot_ptr =
      newGEPInstruction(vaAreaSlot, overflow_arg_area_slot_off, voidPtrType);
  addInstruction(overflow_arg_area_slot_ptr);

  addStoreInstr(overflow_arg_area_slot_ptr, stack_param_begin_ptr, NULL);

  // The stores this whole area existed without. Every argument register is
  // written, including the ones a named parameter already used: the two offsets
  // above are what stop va_arg from reading those, so leaving holes would save
  // nothing and make the area's contents depend on the signature.
  //
  // The layout is x86's - so are the sizes above, and so is the 4+6+8 the
  // parser gives __va_area__ - which is why the target's own register counts
  // are asserted to cover it rather than driving it.
  assert(ctx->target->intArgRegCount >= R_PARAM_COUNT &&
         ctx->target->fpArgRegCount >= R_FP_PARAM_COUNT);

  for (uint32_t k = 0; k < R_PARAM_COUNT; ++k) {
    IrInstruction *slot = newGEPInstruction(
        vaAreaSlot, createIntegerConstant(IR_I64, gp_va_area + k * dataSize),
        uintptrType);
    addInstruction(slot);
    IrInstruction *reg = newPhysRegister(IR_I64, ctx->target->intArgRegs[k]);
    addInstruction(reg);
    addStoreInstr(slot, reg, NULL);
  }

  for (uint32_t k = 0; k < R_FP_PARAM_COUNT; ++k) {
    IrInstruction *slot = newGEPInstruction(
        vaAreaSlot, createIntegerConstant(IR_I64, fp_va_area + k * dataSize),
        floatType);
    addInstruction(slot);
    IrInstruction *reg = newPhysRegister(IR_F64, ctx->target->fpArgRegs[k]);
    addInstruction(reg);
    addStoreInstr(slot, reg, NULL);
  }

  return idx + 2;
}

static uint32_t buildInitialIr(IrFunction *func,
                               AstFunctionDefinition *function) {
  AstFunctionDeclaration *declaration = function->declaration;
  AstValueDeclaration *local = function->locals;
  size_t numOfParams = 0;
  for (AstValueDeclaration *param = declaration->parameters; param != NULL;
       param = param->next, ++numOfParams)
    ;

  declaration->parameterCount = numOfParams;

  size_t numOfLocals = 0;
  for (AstValueDeclaration *local = function->locals; local != NULL;
       local = local->next, ++numOfLocals)
    ;

  size_t numOfReturnSlots = 0;
  if (isTypeRequiresReturnValue(declaration->returnType)) {
    numOfReturnSlots = 1;
  }

  // Two: the register save area itself, and an anchor for the overflow area,
  // which is above the frame pointer and so is a slot of its own the way an
  // incoming stack parameter is. See generateVaArea.
  size_t numOfVariadicSlots = 0;
  if (declaration->isVariadic) {
    numOfVariadicSlots = 2;
  }

  const size_t numOfLocalSlots =
      numOfParams + numOfLocals + numOfReturnSlots + numOfVariadicSlots;
  func->numOfLocalSlots = numOfLocalSlots;

  LocalValueInfo *localOperandsMap =
      areanAllocate(ctx->irArena, numOfLocalSlots * sizeof(LocalValueInfo));
  ctx->localOperandMap = localOperandsMap;
  func->localOperandMap = localOperandsMap;

  int32_t frameOffset = 0;

  ParamtersABIInfo *paramABIInfo =
      heapAllocate(numOfParams * sizeof(ParamtersABIInfo));

  IrInstruction *stackPtrOp = ctx->stackOp =
      newPhysRegister(IR_PTR, ctx->target->sp);
  addInstructionHead(func->entry, stackPtrOp);

  ParametersABISummary abiSummary = {0};
  computeParametersABIInfo(declaration, paramABIInfo, numOfParams,
                           localOperandsMap, &abiSummary);
  size_t idx = 0;
  for (AstValueDeclaration *param = declaration->parameters; param != NULL;
       param = param->next, ++idx) {
    ParamtersABIInfo *pi = &paramABIInfo[idx];
    initializeParamterLocal(func->entry, stackPtrOp, pi);
    frameOffset += alignSize(computeTypeSize(param->type), sizeof(intptr_t));
  }

  trace("idx = %lu, param count = %lu\n", idx, numOfParams);
  assert(idx == numOfParams);

  for (local = function->locals; local != NULL; local = local->next, ++idx) {
    local->index2 = idx;
  }

  trace("idx = %lu (%lu), numOfLocals = %lu\n", idx, idx - numOfParams,
        numOfLocals);
  assert((idx - numOfParams) == numOfLocals);

  if (numOfReturnSlots) {
    TypeRef *returnType = declaration->returnType;
    ctx->currentBB = func->entry;

    if (returnsThroughHiddenPointer(returnType)) {
      // Nothing local holds the value: the caller allocated the buffer and
      // passed its address in the first integer argument register (see
      // classifyParametersGeneric, which reserves that register), and each
      // 'return' writes straight through it. So the slot is a pointer, filled
      // on entry the same way a register parameter is.
      IrInstruction *bufferSlot = func->retOperand =
          createAllocaSlot(sizeof(intptr_t));
      bufferSlot->astType = makePointedType(
          ctx->pctx, 0, makePointedType(ctx->pctx, 0, returnType));
      bufferSlot->info.alloca.valueType = IR_PTR;

      IrInstruction *bufferReg =
          newPhysRegister(IR_PTR, ctx->target->intArgRegs[0]);
      addInstruction(bufferReg);
      addStoreInstr(bufferSlot, bufferReg, NULL);

      localOperandsMap[idx++].stackSlot = bufferSlot;
    } else {
      IrInstruction *returnStackSlot = func->retOperand =
          createAllocaSlot(computeTypeSize(returnType));
      returnStackSlot->astType = makePointedType(ctx->pctx, 0, returnType);
      returnStackSlot->info.alloca.valueType = typeRefToIrType(returnType);
      localOperandsMap[idx++].stackSlot = returnStackSlot;
    }
  }

  if (numOfVariadicSlots) {
    AstValueDeclaration *va_area = function->va_area;
    ctx->currentBB = func->entry;
    idx = generateVaArea(va_area, &abiSummary, stackPtrOp, localOperandsMap,
                         idx);
  }

  // Every fixed-size local is allocated here, in the entry block, rather than
  // where its declaration sits in the body - see createLocalSlot for why.
  // Walking function->locals gives the frame a layout that depends only on
  // what the function declares, not on how its blocks end up linked, which is
  // what layoutFrame() already wants of it; it is the same order the slot
  // indices above were assigned in (the parser prepends, so it runs backwards
  // through the source).
  //
  // A VLA is left out: translateLocalDeclaration reaches it once its size
  // expression has been translated, in the block the declaration sits in.
  ctx->currentBB = func->entry;
  for (AstValueDeclaration *l = function->locals; l != NULL; l = l->next) {
    if (l->type->kind == TR_VLA)
      continue;
    size_t size = localSlotSize(l);
    createLocalSlot(l, createIntegerConstant(IR_U64, size), size);
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

  func->phases.initalIr = 1;

  return 0;
}

static void dumpRequestedPhase(IrFunction *func, enum IrDumpPhase phase, const char *name) {
  if (ctx->irDumpStream && (ctx->irDumpPhases & phase)) {
    dumpIrFunctionPhase(ctx->irDumpStream, func, name);
  }
}

static IrFunction *translateFunction(AstFunctionDefinition *function) {
  resetIrContext(ctx);
  IrFunction *func = newIrFunction(function);

  buildInitialIr(func, function);
  assert(func->numOfBlocks == ctx->bbCnt);
  dumpRequestedPhase(func, IR_DUMP_PHASE_INITIAL, "initial");

  buildSSA(func);
  assert(func->numOfBlocks == ctx->bbCnt);
  dumpRequestedPhase(func, IR_DUMP_PHASE_SSA, "ssa");

  scp(func);
  assert(func->numOfBlocks == ctx->bbCnt);
  dumpRequestedPhase(func, IR_DUMP_PHASE_SCP, "scp");

  gvn(func);
  dumpRequestedPhase(func, IR_DUMP_PHASE_GVN, "gvn");

  dce(func);
  dumpRequestedPhase(func, IR_DUMP_PHASE_DCE, "dce");

  // Machine IR is built unconditionally rather than only when it is dumped:
  // it is on the way to being the only way this pipeline produces code, and a
  // path that runs only under a debug flag is a path that rots.
  func->machine = prepareMachineFunction(func);
  if (ctx->irDumpStream && (ctx->irDumpPhases & IR_DUMP_PHASE_MIR)) {
    dumpMachineFunctionPhase(ctx->irDumpStream, func->machine, "mir");
  }

  selectInstructions(func->machine);
  if (ctx->irDumpStream && (ctx->irDumpPhases & IR_DUMP_PHASE_ISEL)) {
    dumpMachineFunctionPhase(ctx->irDumpStream, func->machine, "isel");
  }

  allocateRegisters(func->machine);
  if (ctx->irDumpStream && (ctx->irDumpPhases & IR_DUMP_PHASE_RA)) {
    dumpMachineFunctionPhase(ctx->irDumpStream, func->machine, "ra");
  }

  return func;
}
