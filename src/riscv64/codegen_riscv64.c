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


#define CALLEE_SAVED_XREGS_COUNT 12u
#define CALLEE_SAVED_FREGS_COUNT 12u

int32_t isUsedX(enum XRegister reg) {
    return 0;
}

int32_t isUsedF(enum FRegister reg) {
    return 0;
}

static size_t allocateLocalSlots(GeneratedFunction *g, AstFunctionDefinition *f) {
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

static GeneratedFunction *generateFunction_riscv64(GenerationContext *ctx, AstFunctionDefinition *f) {
  HashMap *labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
  ctx->labelMap = labelMap;

  assert(f->body->statementKind == SK_BLOCK);

  GeneratedFunction *gen = allocateGenFunction(ctx);

  gen->symbol = f->declaration->symbol;
  gen->name = f->declaration->name;

//  pushFrame(gen);

  size_t frameSize = allocateLocalSlots(gen, f);

  size_t delta = frameSize;

//  if (frameSize)
//    emitArithConst(gen, OP_SUB, R_ESP, delta, T_S8);

//  Address addr = { R_EBP, R_BAD, 0, gen->allocaOffset, NULL, NULL };
//  emitMoveRA(gen, R_ESP, &addr, sizeof(intptr_t));

//  gen->stackOffset = 0;
//  Boolean lastIsRet = generateBlock(gen, &f->body->block);
//  assert(gen->stackOffset == 0);

//  if (!lastIsRet) {
//    TypeRef * returnType = f->declaration->returnType;
//    size_t returnTypeSize = computeTypeSize(returnType);
//    if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
//        Address addr = { R_EBP, R_BAD, 0, gen->returnStructAddressOffset, NULL, NULL };
//        emitMoveAR(gen, &addr, R_EAX, sizeof(intptr_t));
//    }

//    popFrame(gen);
//    emitReturn(gen);
//  }

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
}
