#include <assert.h>

#include "_elf.h"
#include "codegen.h"
#include "mem.h"
#include "parser.h"
#include "sema.h"
#include "instructions_riscv64.h"

const static size_t RISCV64_STACK_ALIGNMENT = 2 * sizeof(uint64_t);
const static size_t RISCV64_REGISTER_AGGREGATE_SIZE = 2 * sizeof(uint64_t);
const static size_t RISCV64_STACK_SLOT_SIZE = sizeof(uint64_t);


const static enum XRegister XArgumentRegs[] = { X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7 };
const static enum FRegister FArgumentRegs[] = { F_A0, F_A1, F_A2, F_A3, F_A4, F_A5, F_A6, F_A7 };

const static enum XRegister calleeSavedXRegs[] = { X_S0, X_S1, X_S2, X_S3, X_S4, X_S5,
                                                   X_S6, X_S7, X_S8, X_S9, X_S10, X_S11
                                                 };

const static enum FRegister calleeSavedFRegs[] = { F_S0, F_S1, F_S2, F_S3, F_S4, F_S5,
                                                   F_S6, F_S7, F_S8, F_S9, F_S10, F_S11
                                                 };


static Boolean generateBlock(GeneratedFunction *f, AstBlock *block);
static Boolean generateStatement(GeneratedFunction *f, AstStatement *stmt);
static void generateExpression(GeneratedFunction *f, AstExpression *expression);

#define CALLEE_SAVED_XREGS_COUNT 12u
#define CALLEE_SAVED_FREGS_COUNT 12u

int32_t isUsedX(enum XRegister reg) {
    return 0;
}

int32_t isUsedF(enum FRegister reg) {
    return 0;
}

static size_t allocateFrame(GeneratedFunction *g, AstFunctionDefinition *f) {
    AstValueDeclaration *param = f->declaration->parameters;
    AstValueDeclaration *local = f->locals;
    TypeRef *returnType = f->declaration->returnType;

    unsigned intRegParams = 0;
    unsigned fpRegParams = 0;

    int32_t baseOffset = 0; // from rbp;
    int32_t callerOffset = 0;
    size_t frameSize = RISCV64_STACK_SLOT_SIZE; // RA
    // sd ra, -baseOffset(sp)
    int32_t returnAddressOffset = baseOffset;

    int32_t structBufferOffset = 0;
    if (f->returnStructBuffer) {
        size_t aligned = ALIGN_SIZE(f->returnStructBuffer, RISCV64_STACK_SLOT_SIZE);
        baseOffset += aligned;
        frameSize += aligned;
        structBufferOffset = baseOffset;
    }

//    if (f->useAlloca) {
    baseOffset += RISCV64_STACK_SLOT_SIZE;
    frameSize += RISCV64_STACK_SLOT_SIZE;
    int32_t allocaOffset = baseOffset;
    // sd zero, -baseOffset(sp)
//    }

    int32_t returnAggregateBufferOffset = 0;
    if (isCompositeType(returnType) && computeTypeSize(returnType) > RISCV64_REGISTER_AGGREGATE_SIZE) {
        baseOffset += RISCV64_STACK_SLOT_SIZE;
        frameSize += RISCV64_STACK_SLOT_SIZE;
        returnAggregateBufferOffset = baseOffset;
        // sd XArgumentRegs[intRegParams++], -baseOffset(sp)
    }

    for (; param; param = param->next) {
      TypeRef *paramType = param->type;
      GeneratedVariable *gp = allocateGenVarialbe(g->context, param);
      size_t size = max(computeTypeSize(paramType), RISCV64_STACK_SLOT_SIZE);
      size_t align = max(typeAlignment(paramType), RISCV64_STACK_SLOT_SIZE);

      if (isCompositeType(paramType)) {
          if (size > RISCV64_REGISTER_AGGREGATE_SIZE) {
//              int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
//              gp->baseOffset = alignedOffset;
//              stackParamOffset = alignedOffset + size;
          } else {

          }
      } else if (isRealType(paramType)) {
          if (size <= sizeof (double)) { // fp16 / float / double
//              callerOffset += RISCV64_STACK_SLOT_SIZE;
              if (fpRegParams < F_FP_PARAM_COUNT) { // in reg
                frameSize += RISCV64_STACK_SLOT_SIZE;
                baseOffset += RISCV64_STACK_SLOT_SIZE;
                gp->baseOffset = baseOffset;
                // fsd FArgumentRegs[fpRegParams++], -baseOffset(sp)
              } else { // in stack
                callerOffset += RISCV64_STACK_SLOT_SIZE;
                gp->baseOffset = -callerOffset;
              }
          } else { // long double
               if ((fpRegParams + 1) < F_FP_PARAM_COUNT) { // fully fits into regs
                frameSize += 2 * RISCV64_STACK_SLOT_SIZE;
                baseOffset += RISCV64_STACK_SLOT_SIZE;
                // fsd FArgumentRegs[fpRegParams++], -baseOffset(sp)
                baseOffset += RISCV64_STACK_SLOT_SIZE;
                // fsd FArgumentRegs[fpRegParams++], -baseOffset(sp)
               } else if (fpRegParams < F_FP_PARAM_COUNT) { // lo in reg, hi in stack
                enum FRegister reg = FArgumentRegs[fpRegParams++];
                frameSize += 2 * RISCV64_STACK_SLOT_SIZE;

                baseOffset += RISCV64_STACK_SLOT_SIZE;
                // fsd reg, -baseOffset(sp)

                callerOffset += RISCV64_STACK_SLOT_SIZE;
                baseOffset += RISCV64_STACK_SLOT_SIZE;
                // fld reg, callerOffset(sp)
                // fsd reg, -baseOffset(sp)
               } else { // in stack
                 callerOffset += 2 * RISCV64_STACK_SLOT_SIZE;
                 gp->baseOffset = -callerOffset;
               }
          }
      } else {
          if (intRegParams < X_GP_PARAM_COUNT) {
              baseOffset += RISCV64_STACK_SLOT_SIZE;
              frameSize += RISCV64_STACK_SLOT_SIZE;
//              baseOffset = ALIGN_SIZE(baseOffset, align);
              gp->baseOffset = baseOffset;
              // sd XArgumentRegs[intRegParams++], -baseOffset(sp)
          } else {
              callerOffset += RISCV64_STACK_SLOT_SIZE;
              gp->baseOffset = -callerOffset;
//              int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
//              gp->baseOffset = alignedOffset;
//              stackParamOffset = alignedOffset + size;
          }
      }
    }


    assert(CALLEE_SAVED_XREGS_COUNT == (sizeof calleeSavedXRegs / sizeof calleeSavedXRegs[0]));
    assert(CALLEE_SAVED_FREGS_COUNT == (sizeof calleeSavedFRegs / sizeof calleeSavedFRegs[0]));

//    int32_t xSpillOffset[CALLEE_SAVED_XREGS_COUNT] = {0};
//    int32_t fSpillOffset[CALLEE_SAVED_FREGS_COUNT] = {0};

    int32_t calleeSaveArea = baseOffset;
    int32_t xCalleeSavedArea = baseOffset;
    int32_t i;
    for (i = 0; i < CALLEE_SAVED_XREGS_COUNT; ++i) {
        if (isUsedX(calleeSavedXRegs[i])) {
           frameSize += RISCV64_STACK_SLOT_SIZE;
           baseOffset += RISCV64_STACK_SLOT_SIZE;
           // sd calleeSavedXRegs[i], -baseOffset(sp)
        }
    }

    int32_t fCalleeSavedArea = baseOffset;

    for (i = 0; i < CALLEE_SAVED_FREGS_COUNT; ++i) {
        if (isUsedF(calleeSavedFRegs[i])) {
           frameSize += RISCV64_STACK_SLOT_SIZE;
           baseOffset += RISCV64_STACK_SLOT_SIZE;
           // fsd calleeSavedFRegs[i], -baseOffset(sp)
        }
    }


    returnAddressOffset = frameSize - returnAddressOffset - RISCV64_STACK_SLOT_SIZE;
    structBufferOffset = frameSize - structBufferOffset - RISCV64_STACK_SLOT_SIZE;
    allocaOffset = frameSize - allocaOffset - RISCV64_STACK_SLOT_SIZE;
    returnAggregateBufferOffset = frameSize - returnAggregateBufferOffset - RISCV64_STACK_SLOT_SIZE;
    for (; param; param = param->next) {
        GeneratedVariable *gp = param->gen;
        assert(gp != NULL);
        gp->baseOffset = frameSize - gp->baseOffset - RISCV64_STACK_SLOT_SIZE;
    }

    g->frameSize = frameSize;
    g->allocaOffset = allocaOffset;
    g->structBufferOffset = structBufferOffset;
    g->returnStructAddressOffset = returnAddressOffset;

    g->savedRegOffset = calleeSaveArea;

    // addi sp, sp, -frameSize

    return frameSize;
}

static GeneratedVariable *generateVaribale_riscv64(GenerationContext *ctx, AstValueDeclaration *d) {
  if (d->flags.bits.isExternal) return NULL; // no declaration is needed

  Section *section = NULL;

  if (d->initializer) {
      if (hasRelocationsInit(d->initializer)) {
          section = d->flags.bits.isConst ? ctx->rodataLocal : ctx->dataLocal;
      } else {
          section = d->flags.bits.isConst ? ctx->rodata : ctx->data;
      }
  } else {
      section = ctx->bss;
  }

  int32_t align = typeAlignment(d->type);
  alignSection(section, align);

  size_t objectSize = computeTypeSize(d->type);

  ptrdiff_t offset = section->pc - section->start;

  if (d->initializer) {
    fillInitializer(ctx, section, d->initializer, offset, objectSize);
  } else {
    size_t filled = 0;
    while (filled < objectSize) {
        emitSectionByte(section, 0x00);
        ++filled;
    }
  }

  GeneratedVariable *v = allocateGenVarialbe(ctx, d);
  v->section = section;
  v->sectionOffset = offset;
  v->size = objectSize;
  return v;
}

static void emitConst(GeneratedFunction *f, AstConst *_const, TypeId tid) {
  // movq #const, %rax
  int64_t c = 0;
  switch (_const->op) {
  case CK_FLOAT_CONST: {
//        Address addr = { 0 };
//        if (emitFloatConst(f, _const, tid, &addr)) {
//          switch (tid) {
//            case T_F4: emitMovfpAR(f, &addr, R_FACC, 4); break;
//            case T_F8: emitMovfpAR(f, &addr, R_FACC, 8); break;
//            case T_F10: emitFPLoad(f, &addr, T_F10); break;
//            default: unreachable("Unknown FP type ID");
//          }
//        }
        break;
      }
  case CK_INT_CONST:
      emitLoadImmediate(f, _const->i, X_ACC);
      break;
  case CK_STRING_LITERAL: {
//        GenerationContext *ctx = f->context;
//        Section *rodata = ctx->rodata;
//        ptrdiff_t literalSectionOffset = emitStringWithEscaping(ctx, rodata, _const);

//        Relocation *reloc = allocateRelocation(ctx);
//        reloc->applySection = f->section;
//        reloc->kind = RK_RIP;
//        reloc->sectionData.dataSection= rodata;
//        reloc->sectionData.dataSectionOffset = literalSectionOffset;
//        reloc->next = f->section->reloc;
//        f->section->reloc = reloc;

//        Address addr = { R_RIP, R_BAD, 0, 0, reloc };

//        emitLea(f, &addr, R_ACC);

        break;
    }
  }
}

static void generateExpression(GeneratedFunction *f, AstExpression *expression) {
//  Address addr = { 0 };
  TypeId typeId = typeToId(expression->type);
  switch (expression->op) {
    case E_PAREN:
      generateExpression(f, expression->parened);
      break;
    case E_BLOCK:
      generateStatement(f, expression->block);
      break;
    case E_CONST:
      emitConst(f, &expression->constExpr, typeId);
      break;
    case E_VA_ARG:
//      generateVaArg(f, expression);
      break;
    case E_NAMEREF:
//      translateAddress(f, expression, &addr);
//      emitLea(f, &addr, R_ACC);
      break;
    case E_COMPOUND:
//      generateCompoundExpression(f, expression);
      break;
    case E_CALL:
//      generateCall(f, expression);
      break;
    case E_TERNARY: {
//        AstTernaryExpression *ternary = &expression->ternaryExpr;
//        struct Label elseLabel = { 0 }, endLabel = { 0 };
//        assert(ternary->condition);
//        enum JumpCondition cc = generateCondition(f, ternary->condition, TRUE);

//        emitCondJump(f, &elseLabel, cc, FALSE);

//        assert(ternary->ifTrue);
//        generateExpression(f, ternary->ifTrue);
//        emitJumpTo(f, &endLabel, FALSE);

//        assert(ternary->ifFalse);
//        bindLabel(f, &elseLabel);
//        generateExpression(f, ternary->ifFalse);

//        bindLabel(f, &endLabel);
      }
      break;
    case E_BIT_EXTEND:
//      generateBitExtend(f, expression);
      break;
    case E_CAST:
//      generateCast(f, &expression->castExpr);
      break;
    case EB_ADD:
    case EB_SUB:
    case EB_LHS: /** << */
    case EB_RHS: /** >> */
    case EB_AND:
    case EB_OR:
    case EB_XOR:
    case EB_MUL:
//      generateBinary(f, expression);
      break;
    case EB_DIV:
    case EB_MOD:
//      generateDiv(f, expression);
      break;
    case EB_ANDAND:
    case EB_OROR:
//      generateLogicalBinary(f, expression);
      break;
    case EB_EQ:
    case EB_NE:
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE: {
//        enum JumpCondition cc = generateCondition(f, expression, FALSE);
//        emitSetccR(f, cc, R_ACC);
//        emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
      }
      break;
    case EB_ASSIGN:
    case EB_ASG_MUL:
    case EB_ASG_ADD:
    case EB_ASG_SUB:
    case EB_ASG_SHL:
    case EB_ASG_SHR:
    case EB_ASG_AND:
    case EB_ASG_XOR:
    case EB_ASG_OR:
//      generateAssign(f, expression);
      // TODO:
      break;
    case EB_ASG_DIV:
    case EB_ASG_MOD:
//      generateAssignDiv(f, expression);
      break;
    case EB_COMMA:
      generateExpression(f, expression->binaryExpr.left);
      generateExpression(f, expression->binaryExpr.right);
      break;
    case EU_REF:
//      translateAddress(f, expression->unaryExpr.argument, &addr);
//      emitLea(f, &addr, R_ACC);
      break;
    case EU_DEREF:
//      translateAddress(f, expression->unaryExpr.argument, &addr);
//      if (isFlatType(expression->type) || expression->type->kind == TR_FUNCTION) {
//        if (!(addr.base == R_ACC && addr.index == R_BAD && addr.imm == 0)) {
//          emitLea(f, &addr, R_ACC);
//        }
//      } else {
//        emitLoad(f, &addr, R_ACC, typeId);
//      }
      break;
    case EU_PLUS:
      generateExpression(f, expression->unaryExpr.argument);
      break;
    case EU_MINUS:
      generateExpression(f, expression->unaryExpr.argument);
//      if (isRealType(expression->type)) {
//          emitFPNeg(f, expression->type->descriptorDesc->typeId);
//      } else {
//          emitNegR(f, R_ACC, typeIdSize(typeId));
//      }
      break;
    case EU_TILDA:
      generateExpression(f, expression->unaryExpr.argument);
//      emitBitwiseNotR(f, R_ACC, computeTypeSize(expression->type));
      break;
    case EU_EXL:
      generateExpression(f, expression->unaryExpr.argument);
//      emitNot(f, R_ACC, computeTypeSize(expression->unaryExpr.argument->type));
      break;

    case E_LABEL_REF: {
//      GenerationContext *ctx = f->context;
//      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)expression->label);
//      if (l == NULL) {
//          l = allocateLabel(ctx);
//          putToHashMap(ctx->labelMap, (intptr_t)expression->label, (intptr_t)l);
//      }
//      Address addr = { R_RIP, R_BAD, 0, 0, NULL, l };
//      emitLea(f, &addr, R_ACC);
      break;

    }

    default: unreachable("unexpcted expression op");
  }
}

static Boolean generateStatement(GeneratedFunction *f, AstStatement *stmt) {

  GenerationContext *ctx = f->context;

  switch (stmt->statementKind) {
  case SK_BLOCK:
      return generateBlock(f, &stmt->block);
  /*case SK_DECLARATION: {
      AstDeclaration *d = stmt->declStmt.declaration;
      if (d->kind == DK_VAR) {
        AstValueDeclaration *v = d->variableDeclaration;
        assert(v->kind == VD_VARIABLE);
        const Symbol *s = v->symbol;
        assert(s);
        size_t typeSize = computeTypeSize(v->type);
        size_t align = min(max(4, typeSize), sizeof(intptr_t));

        if (v->flags.bits.isLocal) {
            assert(v->gen);
            if (v->type->kind == TR_VLA) {
                allocateVLAMemory(f, v->gen, v->initializer, v->type);
            } else {
              if (v->initializer) {
                  emitLocalInitializer(f, v->type, v->gen->baseOffset, v->initializer);
              }
            }
        } else {
          assert(v->flags.bits.isStatic);
          GeneratedVariable *gv = generateVaribale_riscv64(ctx, v);
          v->gen = gv;
          gv->next = ctx->file->staticVariables;
          ctx->file->staticVariables = gv;
        }
      }
      break;
  }*/
  case SK_EMPTY: break;
  case SK_EXPR_STMT:
      generateExpression(f, stmt->exprStmt.expression);
//      if (typeToId(stmt->exprStmt.expression->type) == T_F10) {
//          // clean up FP processor stack
//          emitFPPop(f, 0);
//      }
      break;
  case SK_LABEL:
//      return generateLabel(f, &stmt->labelStmt);
  case SK_GOTO_L: {
//      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label);
//      if (l == NULL) {
//          l = allocateLabel(ctx);
//          putToHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label, (intptr_t)l);
//      }
//      emitJumpTo(f, l, FALSE);
//      break;
  }
  case SK_GOTO_P: {
//      generateExpression(f, stmt->jumpStmt.expression);
//      emitJumpByReg(f, R_ACC);
//      break;
  }
  case SK_RETURN: {
//      AstExpression *retExpr = stmt->jumpStmt.expression;
//      if (retExpr) {
//          if (isCompositeType(retExpr->type)) {
//            Address src = { 0 };
//            assert(retExpr->op == EU_DEREF);
//            translateAddress(f, retExpr->unaryExpr.argument, &src);

//            size_t retSize = computeTypeSize(retExpr->type);
//            if (retSize > sizeof(intptr_t)) {
//              Address addr = { R_EBP, R_BAD, 0, f->returnStructAddressOffset, NULL, NULL };
//              emitMoveAR(f, &addr, R_EDI, sizeof(intptr_t));

//              Address dst = { R_EDI, R_BAD, 0, 0, NULL, NULL };
//              copyStructTo(f, retExpr->type, &src, &dst);
//              emitMoveRR(f, R_EDI, R_ACC, sizeof(intptr_t));
//            } else {
//              emitMoveAR(f, &src, R_ACC, retSize);
//            }
//          } else {
//            generateExpression(f, retExpr);
//          }
//      }
//      emitLeave(f);
//      emitRet(f, 0);
//      return TRUE;
      }
  case SK_BREAK:
      assert(f->context->breakLabel);
//      emitJumpTo(f, f->context->breakLabel, FALSE);
      break;
  case SK_CONTINUE:
      assert(ctx->continueLabel);
//      emitJumpTo(f, ctx->continueLabel, FALSE);
      break;
  case SK_IF:
//      generateIfStatement(f, &stmt->ifStmt);
      break;
  case SK_SWITCH:
//      generateSwitchStatement(f, &stmt->switchStmt);
      break;
  case SK_WHILE:
  case SK_DO_WHILE:
//      generateLoopStatement(f, &stmt->loopStmt, stmt->statementKind == SK_DO_WHILE);
      break;
  case SK_FOR:
//      generateForStatement(f, &stmt->forStmt);
      break;
  default:
      unreachable("Unreachable");
      break;
  }
  return FALSE;
}

static Boolean generateBlock(GeneratedFunction *f, AstBlock *block) {

  AstStatementList *stmt = block->stmts;
  Boolean lastIsRet = FALSE;

  while (stmt) {
    lastIsRet = generateStatement(f, stmt->stmt);
    stmt = stmt->next;
  }

  return lastIsRet;
}

static GeneratedFunction *generateFunction_riscv64(GenerationContext *ctx, AstFunctionDefinition *f) {
  HashMap *labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
  ctx->labelMap = labelMap;

  assert(f->body->statementKind == SK_BLOCK);

  GeneratedFunction *gen = allocateGenFunction(ctx);

  gen->symbol = f->declaration->symbol;
  gen->name = f->declaration->name;

//  pushFrame(gen);

  size_t frameSize = allocateFrame(gen, f);

  size_t delta = frameSize;

//  if (frameSize)
//    emitArithConst(gen, OP_SUB, R_ESP, delta, T_S8);

//  Address addr = { R_EBP, R_BAD, 0, gen->allocaOffset, NULL, NULL };
//  emitMoveRA(gen, R_ESP, &addr, sizeof(intptr_t));

  gen->stackOffset = 0;
  Boolean lastIsRet = generateBlock(gen, &f->body->block);
  assert(gen->stackOffset == 0);

  if (!lastIsRet) {
    TypeRef * returnType = f->declaration->returnType;
    size_t returnTypeSize = computeTypeSize(returnType);
    if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
//        Address addr = { R_EBP, R_BAD, 0, gen->returnStructAddressOffset, NULL, NULL };
//        emitMoveAR(gen, &addr, R_EAX, sizeof(intptr_t));
    }

//    popFrame(gen);
//    emitReturn(gen);
  }

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  ctx->labelMap = NULL;
  releaseHashMap(labelMap);

  if (ctx->parserContext->config->asmDump) {
    fprintf(stdout, "<<< %s >>>\n", f->declaration->name);
    address b = gen->section->start + gen->sectionOffset;
    address e = gen->section->pc;
//    disassemble(stdout, b, e - b);

    fprintf(stdout, "<<< bytes >>>\n");

    while (b != e) {
        fprintf(stdout, "%.2x ", *b++);
    }

    fprintf(stdout, "\n<<<>>>\n");
  }

  return gen;
}

void initArchCodegen_riscv64(ArchCodegen *cg) {
    cg->generateFunction = &generateFunction_riscv64;
    cg->generateVaribale = &generateVaribale_riscv64;
}
