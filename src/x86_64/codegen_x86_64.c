
#include <assert.h>

#include <udis86.h>
#include <alloca.h>

#include "_elf.h"
#include "codegen.h"
#include "mem.h"
#include "parser.h"
#include "sema.h"
#include "instructions_x86_64.h"

static void disassemble(FILE *output, uint8_t *buffer, size_t size) {
  ud_t ud_obj;

  ud_init(&ud_obj);
  ud_set_input_buffer(&ud_obj, buffer, size);
  ud_set_mode(&ud_obj, 64);
  ud_set_syntax(&ud_obj, UD_SYN_INTEL);

  while (ud_disassemble(&ud_obj)) {
      uint64_t offset = ud_insn_off(&ud_obj);
      fprintf(output, "<%08lx>\t", offset);

      unsigned c = 0, i = 0;
      const char *hex = ud_insn_hex(&ud_obj);

      while (hex[i]) {
          fputc(hex[i++], output);
          fputc(hex[i++], output);
          ++c;
      }

      while (c < 10) {
          fputc(' ', output);
          fputc(' ', output);
          ++c;
      }

      fprintf(output, "%s\n", ud_insn_asm(&ud_obj));
  }
  fflush(output);
}

static struct Label *allocateLabel(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (struct Label));
}

void emitPushRegF(GeneratedFunction *f, enum Registers r) {
  int size = sizeof(double);
  Address addr = { 0 };
  addr.base = R_ESP;
  addr.index = R_BAD;
  addr.imm = -size;

  f->stackOffset += size;

  emitMovfpRA(f, r, &addr, size);
  emitArithConst(f, OP_SUB, R_ESP, size, T_S8);
}

void emitPopRegF(GeneratedFunction *f, enum Registers r) {
  int size = sizeof(double);
  Address addr = { 0 };
  addr.base = R_ESP;
  addr.index = R_BAD;
  addr.imm = 0;

  f->stackOffset -= size;

  emitMovfpAR(f, &addr, r, size);
  emitArithConst(f, OP_ADD, R_ESP, size, T_S8);
}

static Boolean isBinOp(ExpressionType op) {
  switch (op) {
    case EB_ADD:
    case EB_SUB:
    case EB_MUL:
    case EB_DIV:
    case EB_MOD:
    case EB_LHS:
    case EB_RHS:
    case EB_AND:
    case EB_XOR:
    case EB_OR:
    case EB_ANDAND:
    case EB_OROR:
    case EB_EQ:
    case EB_NE:
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE:
      return TRUE;
    default:
      return FALSE;
    }
}

static enum Opcodes selectByType(TypeRef *type, enum Opcodes p, enum Opcodes f, enum Opcodes d, enum Opcodes i, enum Opcodes l) {
  if (isPointerLikeType(type)) return p;
  if (type->kind == TR_VALUE) {
      switch (type->descriptorDesc->typeId) {
        case T_F8: return d;
        case T_F4: return f;
        case T_S8:
        case T_U8: return l;
        default: return i;
      }
  }

  unreachable("unexpected type");
}

static enum Opcodes selectOpcode(ExpressionType astOp, TypeRef *type) {
  TypeId id = T_ERROR;

  switch (astOp) {
    case EB_ADD:
      return isRealType(type) ? OP_FADD : OP_ADD;
    case EB_SUB:
      return isRealType(type) ? OP_FSUB : OP_SUB;
    case EB_MUL:
      return isRealType(type) ? OP_FMUL : OP_SMUL;
    case EB_MOD:
      assert(isRealType(type));
      return OP_FMOD;
    case EB_DIV:
      assert(isRealType(type));
      return OP_FDIV;
    case EB_LHS: return OP_SHL;
    case EB_RHS: return isUnsignedType(type) ? OP_SHR : OP_SAR;
    case EB_AND: return OP_AND;
    case EB_OR: return OP_OR;
    case EB_XOR: return OP_XOR;

    default: unreachable("Unknown expression op");
  }
}

static enum Opcodes selectAssignOpcode(ExpressionType astOp, TypeRef *type) {
  switch (astOp) {
    case EB_ASG_ADD: return selectOpcode(EB_ADD, type);
    case EB_ASG_SUB: return selectOpcode(EB_SUB, type);
    case EB_ASG_MUL: return selectOpcode(EB_MUL, type);
    case EB_ASG_DIV: return selectOpcode(EB_DIV, type);
    case EB_ASG_MOD: return selectOpcode(EB_MOD, type);
    case EB_ASG_SHL: return selectOpcode(EB_LHS, type);
    case EB_ASG_SHR: return selectOpcode(EB_RHS, type);
    case EB_ASG_AND: return selectOpcode(EB_AND, type);
    case EB_ASG_XOR: return selectOpcode(EB_XOR, type);
    case EB_ASG_OR: return selectOpcode(EB_OR, type);
    default: unreachable("Unknown expression op");
    }
}

static void emitFloatIntoSection(Section *s, TypeId tid, long double v) {
  if (tid == T_F4) {
      FloatBytes fb; fb.f = (float)v;
      emitSectionByte(s, fb.bytes[0]);
      emitSectionByte(s, fb.bytes[1]);
      emitSectionByte(s, fb.bytes[2]);
      emitSectionByte(s, fb.bytes[3]);
  } else if (tid == T_F8) {
      DoubleBytes db; db.d = (double)v;
      emitSectionByte(s, db.bytes[0]);
      emitSectionByte(s, db.bytes[1]);
      emitSectionByte(s, db.bytes[2]);
      emitSectionByte(s, db.bytes[3]);
      emitSectionByte(s, db.bytes[4]);
      emitSectionByte(s, db.bytes[5]);
      emitSectionByte(s, db.bytes[6]);
      emitSectionByte(s, db.bytes[7]);
   } else {
      assert(tid == T_F10);
      LongDoubleBytes ldb = { 0 }; ldb.ld = v;
      emitSectionByte(s, (uint8_t)(ldb.bytes[0]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[1]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[2]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[3]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[4]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[5]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[6]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[7]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[8]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[9]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[10]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[11]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[12]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[13]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[14]));
      emitSectionByte(s, (uint8_t)(ldb.bytes[15]));
   }
}


static void emitIntIntoSection(Section *s, uint64_t v, size_t size) {
  emitSectionByte(s, (uint8_t)(v));
  if (size > 1) {
      emitSectionByte(s, (uint8_t)(v >> 8));
  }
  if (size > 2) {
      emitSectionByte(s, (uint8_t)(v >> 16));
      emitSectionByte(s, (uint8_t)(v >> 24));
  }
  if (size > 4) {
      emitSectionByte(s, (uint8_t)(v >> 32));
      emitSectionByte(s, (uint8_t)(v >> 40));
      emitSectionByte(s, (uint8_t)(v >> 48));
      emitSectionByte(s, (uint8_t)(v >> 56));
  }
}


static Boolean maybeSpecialConst(GeneratedFunction *f, AstConst *_const, TypeId tid) {
  if (tid == T_F10) {
      // check for special constants

      long double ld = _const->f;
      LongDoubleBytes ldb;
      ldb.qwords[0] = ldb.qwords[1] = 0;
      ldb.ld = ld;

      if (ldb.qwords[0] == 0x0UL && ldb.qwords[1] == 0x0UL) { // +0.0
          emitFPnoArg(f, 0xEE);
          return TRUE;
      } else if (ldb.qwords[0] == 0x8000000000000000UL && ldb.qwords[1] == 0x3fffUL) { // +1.0
          emitFPnoArg(f, 0xE8);
          return TRUE;
      } else if (ldb.qwords[0] == 0xc90fdaa22168c235UL && ldb.qwords[1] == 0x4000) { // pi
          emitFPnoArg(f, 0xEB);
          return TRUE;
      } else if (ldb.qwords[0] == 0xd49a784bcd1b8afe && ldb.qwords[1] == 0x4000) { // lb(10)
          emitFPnoArg(f, 0xE9);
          return TRUE;
      } else if (ldb.qwords[0] == 0xb8aa3b295c17f0bcUL && ldb.qwords[1] == 0x3fff) { // lb(e)
          emitFPnoArg(f, 0xEA);
          return TRUE;
      } else if (ldb.qwords[0] == 0x9a209a84fbcff799UL && ldb.qwords[1] == 0x3ffd) { // lg(2)
          emitFPnoArg(f, 0xEC);
          return TRUE;
      } else if (ldb.qwords[0] == 0xb17217f7d1cf79acUL && ldb.qwords[1] == 0x3ffe) { // ln(2)
          emitFPnoArg(f, 0xED);
          return TRUE;
      }
  }
  return FALSE;
}

HashMap *floatCache(GenerationContext *ctx, TypeId tid) {
  switch (tid) {
    case T_F4: return ctx->constCache.f4ConstMap; break;
    case T_F8: return ctx->constCache.f8ConstMap; break;
    case T_F10: return ctx->constCache.f10ConstMap; break;
    default: unreachable("Unexpected float type");
  }
}

static ptrdiff_t checkFloatConstCache(GenerationContext *ctx, AstConst *_const, TypeId tid) {

  HashMap *cache = floatCache(ctx, tid);

  return (ptrdiff_t)getFromHashMap(cache, (intptr_t)&_const->f);
}

static Boolean emitFloatConst(GeneratedFunction *f, AstConst *_const, TypeId tid, Address *addr) {
  GenerationContext *ctx = f->context;
  assert(_const->op == CK_FLOAT_CONST);
  Section *rodata = ctx->rodata;
  ptrdiff_t offset = rodata->pc - rodata->start;
  size_t size = typeIdSize(tid);
  ptrdiff_t alligned = ALIGN_SIZE(offset, size);

  if (maybeSpecialConst(f, _const, tid)) {
      return FALSE;
  }

  ptrdiff_t cached = checkFloatConstCache(ctx, _const, tid);

  if (cached) {
      offset = cached - 1;
  } else {
      while (offset < alligned) {
          emitSectionByte(rodata, 0x00);
          ++offset;
      }

      offset = rodata->pc - rodata->start;
      emitFloatIntoSection(rodata, tid, _const->f);

      HashMap *cache = floatCache(ctx, tid);
      putToHashMap(cache, (intptr_t)&_const->f, offset + 1);
  }

  Relocation *reloc = allocateRelocation(ctx);
  reloc->kind = RK_RIP;
  reloc->applySection = f->section;
  reloc->sectionData.dataSection = rodata;
  reloc->sectionData.dataSectionOffset = offset;
  reloc->next = f->section->reloc;
  f->section->reloc = reloc;

  addr->base = R_RIP;
  addr->index = R_BAD;
  addr->reloc = reloc;
  addr->scale = addr->imm = 0;

  return TRUE;
}

static void emitConst(GeneratedFunction *f, AstConst *_const, TypeId tid) {
  // movq #const, %rax
  int64_t c = 0;
  switch (_const->op) {
  case CK_FLOAT_CONST: {
        Address addr = { 0 };
        if (emitFloatConst(f, _const, tid, &addr)) {
          switch (tid) {
            case T_F4: emitMovfpAR(f, &addr, R_FACC, 4); break;
            case T_F8: emitMovfpAR(f, &addr, R_FACC, 8); break;
            case T_F10: emitFPLoad(f, &addr, T_F10); break;
            default: unreachable("Unknown FP type ID");
          }
        }
        break;
      }
  case CK_INT_CONST:
      c = _const->i;
      emitMoveCR(f, c, R_ACC, tid);
      break;
  case CK_STRING_LITERAL: {
        GenerationContext *ctx = f->context;
        Section *rodata = ctx->rodata;
        ptrdiff_t literalSectionOffset = emitStringWithEscaping(ctx, rodata, _const);

        Relocation *reloc = allocateRelocation(ctx);
        reloc->applySection = f->section;
        reloc->kind = RK_RIP;
        reloc->sectionData.dataSection= rodata;
        reloc->sectionData.dataSectionOffset = literalSectionOffset;
        reloc->next = f->section->reloc;
        f->section->reloc = reloc;

        Address addr = { R_RIP, R_BAD, 0, 0, reloc };

        emitLea(f, &addr, R_ACC);

        break;
    }
  }
}

static void bindLabel(GeneratedFunction *f, struct Label *l) {
  l->label_cp = f->section->pc - f->section->start;
  l->binded = 1;
  struct LabelJump *jump = l->jumps;
  while (jump) {
      patchJumpTo(f, jump->instruction_cp, jump->instSize, l->label_cp);
      jump = jump->next;
  }
  l->jumps = NULL;
  struct LabelRef *ref = l->refs;

  while (ref) {
    patchRefTo(f, ref->offset_cp, l->label_cp);
    ref = ref->next;
  }
  l->refs = NULL;
}

static void emitLoad(GeneratedFunction *f, Address *from, enum Registers to, TypeId typeId) {
  switch (typeId) {
  case T_BOOL: emitMovxxAR(f, 0xB6, from, to); break;
  case T_S1: emitMovxxAR(f, 0xBE, from, to); break;
  case T_S2: emitMovxxAR(f, 0xBF, from, to); break;
  case T_S4: emitMoveAR(f, from, to, sizeof(int32_t)); break;
  case T_S8: emitMoveAR(f, from, to, sizeof(int64_t)); break;
  case T_U1: emitMovxxAR(f, 0xB6, from, to); break;
  case T_U2: emitMovxxAR(f, 0xB7, from, to); break;
  case T_U4: emitMoveAR(f, from, to, sizeof(uint32_t)); break;
  case T_U8: emitMoveAR(f, from, to, sizeof(uint64_t)); break;
  case T_F4: emitMovfpAR(f, from, to, sizeof(float)); break;
  case T_F8: emitMovfpAR(f, from, to, sizeof(double)); break;
  case T_F10: emitFPLoad(f, from, T_F10); break;// to st(0)
  default: unreachable("Unknown memory slot type");
  }
}

static void emitStore(GeneratedFunction *f, enum Registers from, Address *to, TypeId typeId) {
  switch (typeId) {
  case T_BOOL:
  case T_S1:
  case T_U1: emitMoveRA(f, from, to, sizeof(uint8_t)); break;
  case T_S2:
  case T_U2: emitMoveRA(f, from, to, sizeof(uint16_t)); break;
  case T_S4:
  case T_U4: emitMoveRA(f, from, to, sizeof(uint32_t)); break;
  case T_S8:
  case T_U8: emitMoveRA(f, from, to, sizeof(uint64_t)); break;
  case T_F4: emitMovfpRA(f, from, to, sizeof(float)); break;
  case T_F8: emitMovfpRA(f, from, to, sizeof(double)); break;
  case T_F10: assert(from == R_BAD); emitFPStore(f, to, T_F10); break;// from st(0)
  default: unreachable("Unknown memory slot type");
  }
}

static void storeBitField(GeneratedFunction *f, TypeRef *t, enum Registers from, Address *addr);
static void generateExpression(GeneratedFunction *f, AstExpression *expression);
static enum JumpCondition generateCondition(GeneratedFunction *f, AstExpression *cond, Boolean invertion);
static void translateAddress(GeneratedFunction *f, AstExpression *expression, Address *addr);
static Boolean generateStatement(GeneratedFunction *f, AstStatement *stmt);
static Boolean generateBlock(GeneratedFunction *f, AstBlock *block);

static void emitSymbolCall(GeneratedFunction *f, Symbol *s) {
  Relocation *newReloc = allocateRelocation(f->context);
  newReloc->applySection = f->section;
  newReloc->symbolData.symbolName = s->name;
  newReloc->symbolData.symbol = s;
  newReloc->kind = RK_SYMBOL;
  newReloc->next = f->section->reloc;
  f->section->reloc = newReloc;

  emitCallLiteral(f, newReloc);
}

static void copyStructTo(GeneratedFunction *f, TypeRef *type, Address *src, Address *dst) {

  assert(isCompositeType(type));

  int32_t align = type->descriptorDesc->typeDefinition->align;
  int32_t size = computeTypeSize(type);
  int32_t copied = 0;

  while (copied < size) {
      int32_t chunkSize;
      int32_t left = size - copied;

      if (left >= 8) chunkSize = sizeof(int64_t);
      else if (left >= 4) chunkSize = sizeof(int32_t);
      else if (left >= 2) chunkSize = sizeof(int16_t);
      else chunkSize = sizeof(int8_t);

      chunkSize = min(align, chunkSize);

      emitMoveAR(f, src, R_TMP, chunkSize);
      emitMoveRA(f, R_TMP, dst, chunkSize);

      src->imm += chunkSize;
      dst->imm += chunkSize;
      copied += chunkSize;
  }
}

static size_t emitInitializerImpl(GeneratedFunction *f, int32_t typeSize, Address *dst, AstInitializer *initializer, Boolean skipNull) {
  size_t emitted = 0;

  switch (initializer->kind) {
  case IK_EXPRESSION: {
      AstExpression *expr = initializer->expression;
      size_t exprSize = computeTypeSize(expr->type);
      TypeRef *slotType = initializer->slotType;
      size_t slotSize = computeTypeSize(slotType);
      int32_t offset = initializer->offset;
      Address addr = *dst;
      addr.imm += offset;

      if (skipNull && isNullConst(initializer->expression)) {
          return offset + slotSize;
      }

      AstExpression *expression = initializer->expression;

      if (expression->op == E_COMPOUND) {
          emitInitializerImpl(f, slotSize, &addr, expression->compound, skipNull);
          return offset + slotSize;
      }

      generateExpression(f, initializer->expression);

      if ((offset + slotSize) <= typeSize) {
        if (isRealType(slotType)) {
            TypeId sid = slotType->descriptorDesc->typeId;
            if (sid == T_F10) {
              emitFPStore(f, &addr, T_F10);
            } else {
              emitMovfpRA(f, R_FACC, &addr, slotSize);
            }
        } else if (isCompositeType(slotType)) {
            Address src = { R_ACC, R_BAD, 0, 0 };
            copyStructTo(f, expr->type, &src, &addr);
        } else if (slotType->kind == TR_BITFIELD) {
          storeBitField(f, slotType, R_ACC, &addr);
          // TODO:
        } else {
          emitMoveRA(f, R_ACC, &addr, slotSize);
        }
      }
      return offset + slotSize;
    }
    break;
    case IK_LIST: {
        AstInitializerList *inits = initializer->initializerList;
        if (isUnionType(initializer->slotType) && initializer->state == IS_INIT) {
          for (; inits; inits = inits->next) {
            if (inits->initializer->state == IS_INIT) {
              emitted = emitInitializerImpl(f, typeSize, dst, inits->initializer, skipNull);
              break;
            }
          }
        } else {
          while (inits) {
            emitted = emitInitializerImpl(f, typeSize, dst, inits->initializer, skipNull);
            inits = inits->next;
          }
        }
    }
    return emitted;

  default: unreachable("Unknown init kind");

  }

  return 0;
}

static void emitLocalInitializer(GeneratedFunction *f, TypeRef* type, int32_t frameOffset, AstInitializer *initializer) {
  Address addr = { R_EBP, R_BAD, 0, frameOffset };

  size_t typeSize = computeTypeSize(type);
  int32_t align = typeAlignment(type);

  if (typeSize >= 16) {
      emitLea(f, &addr, R_ARG_0);
      emitArithRR(f, OP_XOR, R_ARG_1, R_ARG_1, sizeof (intptr_t));
      emitMoveCR(f, typeSize, R_ARG_2, T_U8);
      emitSymbolCall(f, f->context->memsetSymbol);
      emitInitializerImpl(f, typeSize, &addr, initializer, TRUE);
  } else {

    size_t emitted = emitInitializerImpl(f, typeSize, &addr, initializer, FALSE);

    if (isCompositeType(type) && emitted < typeSize) {
        addr.imm += emitted;
        emitArithRR(f, OP_XOR, R_ACC, R_ACC, sizeof (intptr_t));
        int32_t delta1 = ALIGN_SIZE(emitted, sizeof (intptr_t));
        while (emitted < delta1 && emitted < typeSize) {
            emitMoveRA(f, R_ACC, &addr, sizeof(uint8_t));
            addr.imm += sizeof(uint8_t);
            emitted += sizeof(uint8_t);
        }

        while (emitted < typeSize) {
          emitMoveRA(f, R_ACC, &addr, sizeof(intptr_t));
          addr.imm += sizeof(intptr_t);
          emitted += sizeof(intptr_t);
        }
    }
  }
}

static Symbol *extractSymbol(AstExpression *expr) {
  switch (expr->op) {
  case E_NAMEREF: return expr->nameRefExpr.s;
  case E_CAST: return extractSymbol(expr->castExpr.argument);
  case E_PAREN: return extractSymbol(expr->parened);
  case EU_REF: return extractSymbol(expr->unaryExpr.argument);
  default: return NULL;
  }
}

static GeneratedVariable *generateVaribale_x86_64(GenerationContext *ctx, AstValueDeclaration *d) {
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

static void generateBitExtend(GeneratedFunction *f, AstExpression *extend) {
  unsigned w = extend->extendExpr.w;
  Boolean isU = extend->extendExpr.isUnsigned;
  TypeId id = typeToId(extend->type);

  generateExpression(f, extend->extendExpr.argument);

  uint8_t opcode = 0;

  if (w <= 16) {
    if (w <= 8) {
        opcode = isU ? 0xB6 : 0xBE;
    } else if (w <= 16) {
        opcode = isU ? 0xB7 : 0xBF;
    }
    emitMovxxRR(f, opcode, R_ACC, R_ACC);
  }
}

static void generateU8toF8(GeneratedFunction *f, enum Registers from, enum Registers to) {
  /**
   *        test    rax, rax
   *        js      .L133
   *        pxor    xmm0, xmm0
   *        cvtsi2sd        xmm0, rax
   *        jmp     .L135
   * .L133:
   *        mov     rdx, rax
   *        shr     rdx
   *        and     eax, 1
   *        or      rdx, rax
   *        pxor    xmm0, xmm0
   *        cvtsi2sd        xmm0, rdx
   *        addsd   xmm0, xmm0
   * .L135:
   *
   **/

  struct Label l1 = { 0 }, l2 = { 0 };

  emitTestRR(f, from, from, 8);
  emitCondJump(f, &l1, JC_SIGN, TRUE);

  emitArithRR(f, OP_PXOR, to, to, 8);
  emitConvertFP(f, 0xF2, 0x2A, from, to, TRUE);
  emitJumpTo(f, &l2, TRUE);

  bindLabel(f, &l1);
  emitMoveRR(f, from, R_TMP, 8);
  emitArithConst(f, OP_SHR, R_TMP, 1, T_U8);
  emitArithConst(f, OP_AND, from, 1, T_U4);
  emitArithRR(f, OP_OR, R_TMP, from, 8);
  emitArithRR(f, OP_PXOR, to, to, 8);
  emitConvertFP(f, 0xF2, 0x2A, R_TMP, to, TRUE);
  emitArithRR(f, OP_FADD, to, to, 8);

  bindLabel(f, &l2);
}

static void generateU8toF10(GeneratedFunction *f, enum Registers from) {
  /**
   *       fild    QWORD PTR [rbp-8]
   *       cmp     QWORD PTR [rbp-8], 0
   *       jns     .L152

   *       fld     TBYTE PTR .MAX_ULONG
   *       faddp   st(1), st
   * .L152:
   *
   */

  Address tos = { R_ESP, R_BAD };
  struct Label l = { 0 };

  emitPushReg(f, from);
  emitFPIntLoad(f, &tos, 8);
  emitTestRR(f, from, from, 8);
  emitCondJump(f, &l, JC_NOT_SIGN, TRUE);

  AstConst cv = { 0 };
  cv.op = CK_FLOAT_CONST;
  cv.f = 18446744073709551616.0;
  Address magic = { 0 };
  emitFloatConst(f, &cv, T_F10, &magic);
  emitFPLoad(f, &magic, T_F10);
  emitFPArith(f, OP_FADD, 1, TRUE);

  bindLabel(f, &l);
  emitPopReg(f, from);
}

static void generateF8toU8(GeneratedFunction *f, enum Registers from, enum Registers to) {
  /**
   *       comisd  xmm0, QWORD PTR .LC9[rip]
   *       jnb     .L139
   *       movsd   xmm0, QWORD PTR [rbp-8]
   *       cvttsd2si       rax, xmm0
   *       jmp     .L140
   * .L139:
   *       movsd   xmm0, QWORD PTR [rbp-8]
   *       movsd   xmm1, QWORD PTR .LC9[rip]
   *       subsd   xmm0, xmm1
   *       cvttsd2si       rax, xmm0
   *       movabs  rdx, -9223372036854775808
   *       xor     rax, rdx
   * .L140:
   */

  struct Label l1 = { 0 }, l2 = { 0 };

  emitMoveCR(f, 0x43e0000000000000L, R_TMP, T_S8);
  emitMovdq(f, 0x66, 0x0F, 0x6E, R_TMP, R_FTMP, TRUE);
  emitArithRR(f, OP_FOCMP, from, R_FTMP, 8);
  emitCondJump(f, &l1, JC_A_E, TRUE);

  emitConvertFP(f, 0xF2, 0x2C, from, to, TRUE);
  emitJumpTo(f, &l2, TRUE);

  bindLabel(f, &l1);
  emitArithRR(f, OP_FSUB, from, R_FTMP, 8);
  emitConvertFP(f, 0xF2, 0x2C, from, to, TRUE);
  emitMoveCR(f, -9223372036854775808UL, R_TMP, T_U8);
  emitArithRR(f, OP_XOR, to, R_TMP, 8);

  bindLabel(f, &l2);
}

static void generateF10toInt(GeneratedFunction *f, enum Registers to, TypeId tid) {
  Address oldCtrl = { R_ESP, R_BAD, 0, -4 };
  Address newCtrl = { R_ESP, R_BAD, 0, -2 };
  Address result = { R_ESP, R_BAD, 0, -16 };

  int32_t size = 0;
  switch (tid) {
    case T_BOOL:
    case T_S1: tid = T_U1;
    case T_U1:
    case T_U2: size = 2; break;
    case T_S2: tid = T_U2;
    case T_U4: size = 4; break;
    case T_S8:
    case T_S4: size = 8; break;
    default: unreachable("Unexpected integer type ID");
  }

  emitFPnoArgMem(f, &oldCtrl, 7); // fnstcw
  emitMovxxAR(f, 0xB7, &oldCtrl, R_ACC); // movzwx
  emitArithConst(f, OP_OR, R_ACC, 0x0C00, T_U2); // or 0x0c00
  emitStore(f, R_ACC, &newCtrl, T_U2);
  emitFPnoArgMem(f, &newCtrl, 5); // fldcw
  emitFPIntStore(f, &result, size); // fistp
  emitFPnoArgMem(f, &oldCtrl, 5); // fldcw

  emitLoad(f, &result, R_ACC, tid);
}

static void generateF10toU8(GeneratedFunction *f, enum Registers to) {

  /**
   *        fld     TBYTE PTR .LC9[rip]
   *        fxch    st(1)
   *        fcomi   st, st(1)
   *        jnb     .L175
   *        fnstcw  WORD PTR [rbp-2]
   *        movzx   eax, WORD PTR [rbp-2]
   *        or      ah, 12
   *        mov     WORD PTR [rbp-4], ax
   *        fldcw   WORD PTR [rbp-4]
   *        fistp   QWORD PTR [rbp-16]
   *        fldcw   WORD PTR [rbp-2]
   *        mov     rdx, QWORD PTR [rbp-16]
   *        jmp     .L176
   * .L1:
   *        fxch    st(1)
   *        fsubp   st(1), st
   *        fnstcw  WORD PTR [rbp-2]
   *        movzx   eax, WORD PTR [rbp-2]
   *        or      ah, 12
   *        mov     WORD PTR [rbp-4], ax
   *        fldcw   WORD PTR [rbp-4]
   *        fistp   QWORD PTR [rbp-16]
   *        fldcw   WORD PTR [rbp-2]
   *        mov     rdx, QWORD PTR [rbp-16]
   *        movabs  rax, -9223372036854775808
   *        xor     rdx, rax
   * .L2:
   */


  Address oldCtrl = { R_ESP, R_BAD, 0, -4 };
  Address newCtrl = { R_ESP, R_BAD, 0, -2 };
  Address result = { R_ESP, R_BAD, 0, -16 };

  struct Label l1 = { 0 }, l2 = { 0 };

  AstConst cv = { 0 };
  cv.op = CK_FLOAT_CONST;
  cv.f = 18446744073709551616.0;
  Address magic = { 0 };
  emitFloatConst(f, &cv, T_F10, &magic);

  emitFPLoad(f, &magic, T_F10);
  emitFPnoArg(f, 0xC9); // xchg

  emitFPArith(f, OP_FOCMP, 1, FALSE);
  emitCondJump(f, &l1, JC_A_E, TRUE);

  emitFPnoArgMem(f, &oldCtrl, 7); // fnstcw
  emitMovxxAR(f, 0xB7, &oldCtrl, R_ACC); // movzwx
  emitArithConst(f, OP_OR, R_ACC, 0x0C00, T_U2); // or 0x0c00
  emitStore(f, R_ACC, &newCtrl, T_U2);
  emitFPnoArgMem(f, &newCtrl, 5); // fldcw
  emitFPIntStore(f, &result, 8); // fistp
  emitFPnoArgMem(f, &oldCtrl, 5); // fldcw
  emitLoad(f, &result, R_ACC, T_U8);
  emitJumpTo(f, &l2, TRUE);

  bindLabel(f, &l1);
  emitFPnoArg(f, 0xC9); // xchg
  emitFPArith(f, OP_FSUB, 1, TRUE);
  emitFPnoArgMem(f, &oldCtrl, 7); // fnstcw
  emitMovxxAR(f, 0xB7, &oldCtrl, R_ACC); // movzwx
  emitArithConst(f, OP_OR, R_ACC, 0x0C00, T_U2); // or 0x0c00
  emitStore(f, R_ACC, &newCtrl, T_U2);
  emitFPnoArgMem(f, &newCtrl, 5); // fldcw
  emitFPIntStore(f, &result, 8); // fistp
  emitFPnoArgMem(f, &oldCtrl, 5); // fldcw
  emitLoad(f, &result, R_TMP, T_U8);
  emitMoveCR(f, -9223372036854775808UL, R_ACC, T_S8);
  emitArithRR(f, OP_XOR, R_ACC, R_TMP, 8);

  bindLabel(f, &l2);
  emitFPPop(f, 0);
}

static void generateCast(GeneratedFunction *f, AstCastExpression *cast) {
    TypeRef *fromType = cast->argument->type;
    TypeRef *toType = cast->type;

    TypeId fromTypeId = typeToId(fromType);
    TypeId toTypeId = typeToId(toType);

    generateExpression(f, cast->argument);

    int32_t boolCastSize = -1;

    Address tos = { R_ESP, R_BAD };

    switch (fromTypeId) {
    case T_S1:
      switch (toTypeId) {
        case T_BOOL: boolCastSize = 1;
        case T_S1: break;
        case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
        case T_S4: break;
        case T_S8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
        case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
        case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;  // movzwx
        case T_U4: break;
        case T_U8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
        case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2ss eax, xmm0
        case T_F8: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2sd eax, xmm0
        case T_F10:
          emitConvertWDQ(f, 0x98, 4);
          emitPushReg(f, R_ACC);
          emitFPIntLoad(f, &tos, 2);
          emitPopReg(f, R_ACC);
          break;
        default: unreachable("unexpected type");
      }
      break;
    case T_S2:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsb
          case T_BOOL: boolCastSize = 2;
          case T_S2: break;
          case T_S4: break;
          case T_S8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break; // movzbx
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break; // movzwx
          case T_U4: break;
          case T_U8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2ss eax, xmm0
          case T_F8: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2sd eax, xmm0
          case T_F10:
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 2);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_S4:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_BOOL: boolCastSize = 4;
          case T_S4: break;
          case T_S8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;  // movzwx
          case T_U4: break;
          case T_U8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2ss eax, xmm0
          case T_F8: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2sd eax, xmm0
          case T_F10:
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 4);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_S8:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_S4: break;
          case T_BOOL: boolCastSize = 8;
          case T_S8: break;
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;  // movzwx
          case T_U4: break;
          case T_U8: break;
          case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, TRUE); break; // cvtsi2ss rax, xmm0
          case T_F8: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, TRUE); break; // cvtsi2sd rax, xmm0
          case T_F10:
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 8);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_BOOL:
    case T_U1:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_S4: break;
          case T_S8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_BOOL: boolCastSize = 1;
          case T_U1: break;
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break; // movzbx
          case T_U4: break;
          case T_U8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_F4: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2ss eax, xmm0
          case T_F8: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2sd eax, xmm0
          case T_F10:
            emitMovxxRR(f, 0xB7, R_ACC, R_ACC);
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 2);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_U2:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_S4: break;
          case T_S8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
          case T_BOOL: boolCastSize = 2;
          case T_U2: break;
          case T_U4: break;
          case T_U8: emitConvertWDQ(f, 0x98, 8); break; // cdqe
          case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2ss eax, xmm0
          case T_F8: emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); break; // cvtsi2sd eax, xmm0
          case T_F10:
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 2);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_U4:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_S4: break;
          case T_S8: emitMoveRR(f, R_ACC, R_ACC, sizeof(int32_t)); break; // mov
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;  // movzwx
          case T_BOOL: boolCastSize = 4;
          case T_U4: break;
          case T_U8: emitMoveRR(f, R_ACC, R_ACC, 4); break; // mov
          case T_F4:
            emitMoveRR(f, R_ACC, R_ACC, sizeof(int32_t));
            emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, FALSE); // cvtsi2ss eax, xmm0
            break;
          case T_F8:
            emitMoveRR(f, R_ACC, R_ACC, sizeof(int32_t));
            emitConvertFP(f, 0xF2, 0x2A, R_ACC, R_FACC, FALSE); // cvtsi2ss eax, xmm0
            break;
          case T_F10:
            emitPushReg(f, R_ACC);
            emitFPIntLoad(f, &tos, 4);
            emitPopReg(f, R_ACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_U8:
        switch (toTypeId) {
          case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break; // movsbx
          case T_S2: emitConvertWDQ(f, 0x98, 4); break; // cwde
          case T_S4: break;
          case T_S8: break;
          case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;  // movzbx
          case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;  // movzwx
          case T_U4: break;
          case T_BOOL: boolCastSize = 8;
          case T_U8: break;
          case T_F4: emitConvertFP(f, 0xF3, 0x2A, R_ACC, R_FACC, TRUE); break; // cvtsi2ss rax, xmm0
          case T_F8: generateU8toF8(f, R_ACC, R_FACC); break;
          case T_F10: generateU8toF10(f, R_ACC); break;
          default: unreachable("unexpected type");
        }
        break;
    case T_F4:
        switch (toTypeId) {
          case T_S1:
            emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax, xmm0
            emitMovxxRR(f, 0xBE, R_ACC, R_ACC); // movsbx
            break;
          case T_S2:
            emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax, xmm0
            emitConvertWDQ(f, 0x98, 4); // cwde
            break;
          case T_S4: emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); break; // cvttss2si eax, xmm0
          case T_S8: emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, TRUE); break; // cvttss2si eax, xmm0
          case T_BOOL: boolCastSize = 1;
          case T_U1:
            emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE);  // cvttss2si eax, xmm0
            emitMovxxRR(f, 0xB6, R_ACC, R_ACC); // movzbx
            break;
          case T_U2:
            emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax, xmm0
            emitMovxxRR(f, 0xB7, R_ACC, R_ACC); // movzwx
            break;
          case T_U4: emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); break; // cvttss2si eax, xmm0
          case T_U8: emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, TRUE); break; // cvttss2si eax, xmm0
          case T_F4: break;
          case T_F8: emitConvertFP(f, 0xF3, 0x5A, R_FACC, R_FACC, FALSE); // cvtss2sd xmm0, xmm0
          case T_F10:
            emitPushRegF(f, R_FACC);
            emitFPLoad(f, &tos, T_F4);
            emitPopRegF(f, R_FACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_F8:
        switch (toTypeId) {
          case T_S1:
            emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax, xmm0
            emitMovxxRR(f, 0xBE, R_ACC, R_ACC); // movsx
            break;
          case T_S2:
            emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax, xmm0
            emitConvertWDQ(f, 0x98, 4); // cwde
            break;
          case T_S4:
            emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE);
            break; // cvttsd2si eax, xmm0
          case T_S8: emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, TRUE); break; // cvttsd2si eax, xmm0
          case T_BOOL: boolCastSize = 1;
          case T_U1:
            emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax, xmm0
            emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
            break;
          case T_U2:
            emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            emitMovxxRR(f, 0xB7, R_ACC, R_ACC); // movzwx
            break;
          case T_U4: emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); break; // cvttsd2si eax,xmm0
          case T_U8: generateF8toU8(f, R_FACC, R_ACC); break;
          case T_F4: emitConvertFP(f, 0xF2, 0x5A, R_FACC, R_FACC, FALSE); break; // cvtss2sd xmm0,xmm0
          case T_F8: break;
          case T_F10:
            emitPushRegF(f, R_FACC);
            emitFPLoad(f, &tos, T_F8);
            emitPopRegF(f, R_FACC);
            break;
          default: unreachable("unexpected type");
        }
        break;
    case T_F10:
        switch (toTypeId) {
          case T_BOOL:
            boolCastSize = 1;
            generateF10toInt(f, R_ACC, T_U1);
            break;
          case T_S1:
          case T_S2:
          case T_S4:
          case T_S8:
          case T_U1:
          case T_U2:
          case T_U4: generateF10toInt(f, R_ACC, toTypeId); break;
          case T_U8: generateF10toU8(f, R_ACC); break;
          case T_F4:
          case T_F8:
            tos.imm = -8;
            emitFPStore(f, &tos, toTypeId);
            emitLoad(f, &tos, R_FACC, toTypeId);
            break;
          case T_F10: break;
          default: unreachable("unexpected type");
        }
        break;
    default:
        unreachable("unexpected type");
    }

    if (toTypeId == T_BOOL) {
      emitTestRR(f, R_ACC, R_ACC, boolCastSize);
      emitSetccR(f, JC_NOT_ZERO, R_ACC);
    }
}

static void generateBinary(GeneratedFunction *f, AstExpression *binOp) {
  assert(isBinOp(binOp->op));
  AstExpression *left = binOp->binaryExpr.left;
  assert(left);

  size_t opSize = computeTypeSize(binOp->type);
  Boolean isFP = isRealType(left->type);

  // TODO: optimize memory operations
  generateExpression(f, left);

  AstExpression *right = binOp->binaryExpr.right;
  assert(right);
  assert(isFP == isRealType(right->type));

  TypeId tid = typeToId(binOp->type);
  TypeId lid = typeToId(left->type);
  TypeId rid = typeToId(right->type);

  enum Opcodes opcode = selectOpcode(binOp->op, binOp->type);

  if (rid == T_F10) {
      // TODO: probably it worth to be poped and pushed after evaluation to FP stack
      generateExpression(f, right);
      emitFPArith(f, opcode, 1, TRUE);
  } else if (right->op == E_CONST) {
      if (rid == T_F4 || rid == T_F8) {
        Address addr = { 0 };
        emitFloatConst(f, &right->constExpr, rid, &addr);
        emitArithAR(f, opcode, R_FACC, &addr, opSize);
      } else {
        uint64_t cnst = right->constExpr.i;
        emitArithConst(f, opcode, R_ACC, cnst, tid);
      }
  } else {
    if (isFP) {
      emitPushRegF(f, R_FACC);
    } else {
      emitPushReg(f, R_ACC); // save result
    }

    if (right->op == EU_DEREF && !isFlatType(right->type) && !isShiftOp(binOp->op) && lid == rid) {
      Address addr = { 0 };
      translateAddress(f, right->unaryExpr.argument, &addr);

      if (addr.base == R_ACC && !isFP) {
          emitMoveRR(f, R_ACC, R_TMP, sizeof(intptr_t));
          addr.base = R_TMP;
      }
      if (addr.index == R_ACC && !isFP) {
          assert(addr.base != R_TMP);
          emitMoveRR(f, R_ACC, R_TMP, sizeof(intptr_t));
          addr.index = R_TMP;
      }

      if (isFP) {
        emitPopRegF(f, R_FACC);
      } else {
        emitPopReg(f, R_ACC); // saved result
      }

      emitArithAR(f, opcode, isFP ? R_FACC : R_ACC, &addr, opSize);
    } else {
      generateExpression(f, right);

      if (isFP) {
        emitMovfpRR(f, R_FACC, R_FTMP, opSize);
        emitPopRegF(f, R_FACC);
        emitArithRR(f, opcode, R_FACC, R_FTMP, opSize);
      } else {
        emitMoveRR(f, R_ACC, R_ECX, opSize); // ECX becouse of shift instructions
        emitPopReg(f, R_ACC);
        emitArithRR(f, opcode, R_ACC, R_ECX, opSize);
      }
    }
  }
}

static void generateDiv(GeneratedFunction *f, AstExpression *binOp) {
  TypeRef *type = binOp->type;
  if (isRealType(type)) return generateBinary(f, binOp);

  Boolean isMod = binOp->op == EB_MOD;
  size_t opSize = computeTypeSize(type);

  Boolean isU = isUnsignedType(type);
  AstExpression *left = binOp->binaryExpr.left;
  Boolean isLU = isUnsignedType(left->type);

  generateExpression(f, left);

  emitPushReg(f, R_ACC);

  AstExpression *right = binOp->binaryExpr.right;
  Boolean isRU = isUnsignedType(right->type);

  TypeId lid = typeToId(left->type);
  TypeId rid = typeToId(right->type);

  enum Opcodes opcode;
  if (right->op == EU_DEREF && lid == rid) {
      Address addr = { 0 };
      translateAddress(f, right->unaryExpr.argument, &addr);
      emitPopReg(f, R_ACC);

      if (isU) {
        emitArithRR(f, OP_XOR, R_EDX, R_EDX, opSize);
        opcode = OP_UDIV;
      } else {
        emitConvertWDQ(f, 0x99, opSize);
        opcode = OP_SDIV;
      }
      emitArithAR(f, OP_SDIV, R_ACC, &addr, opSize);
  } else {
      generateExpression(f, right);
      emitMoveRR(f, R_ACC, R_TMP2, opSize);
      emitPopReg(f, R_ACC);

      if (isU) {
        emitArithRR(f, OP_XOR, R_EDX, R_EDX, opSize);
        opcode = OP_UDIV;
      } else {
        emitConvertWDQ(f, 0x99, opSize);
        opcode = OP_SDIV;
      }

      emitArithRR(f, opcode, R_ACC, R_TMP2, opSize);
  }

  if (isMod) {
      emitMoveRR(f, R_EDX, R_ACC, opSize);
  }
}

static void generateLogicalBinary(GeneratedFunction *f, AstExpression *binOp) {
  assert(binOp->op == EB_ANDAND || binOp->op == EB_OROR);

  enum JumpCondition cc = generateCondition(f, binOp->binaryExpr.left, binOp->op == EB_ANDAND);

  struct Label elseB = { 0 }, endB = { 0 };

  emitCondJump(f, &elseB, cc, FALSE);

  cc = generateCondition(f, binOp->binaryExpr.right, FALSE);

  emitSetccR(f, cc, R_ACC);
  emitMovxxRR(f, 0xB6, R_ACC, R_ACC);

  emitJumpTo(f, &endB, TRUE);
  bindLabel(f, &elseB);
  emitMoveCR(f, binOp->op == EB_ANDAND ? 0 : 1, R_ACC, T_S4);
  bindLabel(f, &endB);
}

static void loadBitField(GeneratedFunction *f, TypeRef *t, Address *addr, enum Registers to) {
  assert(t->kind == TR_BITFIELD);

  int s = t->bitFieldDesc.offset;
  int w = t->bitFieldDesc.width;

  TypeRef *storageType = t->bitFieldDesc.storageType;
  TypeId tid = typeToId(storageType);
  size_t size = computeTypeSize(storageType);
  size_t W = size * 8;

  emitLoad(f, addr, to, typeToId(storageType));

  uint64_t l = W - (w + s);
  uint64_t r = W - w;

  Boolean isU = isUnsignedType(storageType);

  emitArithConst(f, OP_SHL, to, l, tid);
  emitArithConst(f, isU ? OP_SHR : OP_SAR, to, r, tid);

  if (size < 4) {
      uint8_t opcode = 0;
      if (size == 1) {
          opcode = isU ? 0xB6 : 0xBE;
      }

      if (size == 2) {
          opcode = isU ? 0xB7 : 0xBF;
      }
      emitMovxxRR(f, opcode, to, to);
  }
}

static void storeBitField(GeneratedFunction *f, TypeRef *t, enum Registers from, Address *addr) {
  assert(t->kind == TR_BITFIELD);

  int s = t->bitFieldDesc.offset;
  int w = t->bitFieldDesc.width;

  TypeRef *storageType = t->bitFieldDesc.storageType;
  int size = computeTypeSize(storageType);

  TypeId storageTypeId = typeToId(storageType);

  emitLoad(f, addr, R_TMP, storageTypeId);


  emitArithConst(f, OP_AND, from, ~(~0LLu << w), storageTypeId);
  if (s != 0) {
      emitArithConst(f, OP_SHL, from, s, storageTypeId);
  }

  uint64_t mask = ~(~(~0LLu << w) << s);
  emitArithConst(f, OP_AND, R_TMP, mask, storageTypeId);

  emitArithRR(f, OP_OR, R_TMP, from, size);

  emitStore(f, R_TMP, addr, storageTypeId);

  if (s) {
      emitArithConst(f, OP_SHR, from, s, storageTypeId);
  }
}

static void localVarAddress(const Symbol *s, Address *addr) {
  addr->base = R_EBP;
  addr->index = R_BAD;
  addr->imm = s->variableDesc->gen->baseOffset;
}

static void leaRelocatable(GeneratedFunction *f, Address *addr, enum Registers r) {
  if (addr->reloc) {
      emitLea(f, addr, r);
      addr->base = r;
      addr->index = R_BAD;
      addr->scale = addr->imm = 0;
      addr->reloc = NULL;
  }
}

static void translateAddress(GeneratedFunction *f, AstExpression *expression, Address *addr) {

  if (expression->op == E_COMPOUND && isScalarType(expression->type)) {
    generateExpression(f, expression);
    addr->base = R_ACC;
    addr->index = R_BAD;
    addr->scale = addr->imm = 0;
  } else if (expression->op == E_NAMEREF) {
    Symbol *s = expression->nameRefExpr.s;
    if (s->kind == ValueSymbol && s->variableDesc->flags.bits.isLocal) {
      localVarAddress(s, addr);
    } else {
      Relocation *reloc = allocateRelocation(f->context);
      addr->base = R_RIP;
      addr->index = R_BAD;
      addr->scale = addr->imm = 0;
      reloc->symbolData.symbol = s;
      reloc->symbolData.symbolName = s->name;
      reloc->kind = RK_SYMBOL;
      reloc->applySection = f->section;
      reloc->next = f->section->reloc;
      f->section->reloc = reloc;

      addr->reloc = reloc;
    }
  } else if (expression->op == EB_ADD) {
    // [base + index * scale + imm]
    AstExpression *l = expression->binaryExpr.left;
    AstExpression *r = expression->binaryExpr.right;

    if (l->op == EB_ADD) {
      // [(base + index * scale) + imm]
      // TODO
      generateExpression(f, l);
      emitPushReg(f, R_ACC);
      generateExpression(f, r);
      emitPopReg(f, R_EDI);
      addr->base = R_EDI;
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else if (r->op == EB_ADD) {
      // [base + (index * scale + imm)]
      generateExpression(f, l);
      emitPushReg(f, R_ACC);
      generateExpression(f, r);
      emitPopReg(f, R_EDI);
      addr->base = R_EDI;
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else {
        if (r->op == E_CONST && r->constExpr.op != CK_STRING_LITERAL) {
            // [expr + imm]
            generateExpression(f, l);
            emitMoveRR(f, R_ACC, R_EDI, sizeof(intptr_t));
            addr->base = R_EDI;
            addr->index = R_BAD;
            addr->imm += r->constExpr.i;
        } else if (r->op == EB_MUL || l->op == EB_MUL) {
            // [expr + index * scale] || [index * scale + expr]
            AstExpression *idxs = r->op == EB_MUL ? r : l;
            AstExpression *expr = idxs == r ? l : r;
            generateExpression(f, expr);
            AstExpression *ml = idxs->binaryExpr.left;
            AstExpression *mr = idxs->binaryExpr.right;
            if (mr->op == E_CONST) {
                int64_t d = mr->constExpr.i;
                if (d == 0) {
                    addr->scale = 0;
                    addr->index = R_BAD;
                    addr->base = R_ACC;
                } else if (d == 1 || d == 2 || d == 4 || d == 8) {
                  emitPushReg(f, R_ACC);
                  generateExpression(f, ml);
                  switch (d) {
                  case 1: addr->scale = 0; break;
                  case 2: addr->scale = 1; break;
                  case 4: addr->scale = 2; break;
                  case 8: addr->scale = 3; break;
                  }
                  emitPopReg(f, R_EDI);
                  addr->base = R_EDI;
                  emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
                  addr->index = R_ECX;
                } else {
                  emitPushReg(f, R_ACC);
                  generateExpression(f, idxs);
                  emitPopReg(f, R_EDI);
                  addr->base = R_EDI;
                  addr->scale = 0;
                  addr->index = R_ACC;
                }
            } else {
                generateExpression(f, idxs);
                emitPopReg(f, R_EDI);
                addr->base = R_EDI;
                addr->scale = 0;
                addr->index = R_ACC;
            }
        } else {
            // [expr1 + expr2]
            generateExpression(f, l);
            emitPushReg(f, R_ACC);
            generateExpression(f, r);
            emitPopReg(f, R_EDI);
            addr->base = R_EDI;
            addr->index = R_ACC;
        }
    }

  } else {
    generateExpression(f, expression);
    addr->base = R_ACC;
    addr->index = R_BAD;
  }
}

static Boolean isShiftLikeOp(ExpressionType op) {
  switch (op) {
  case EB_ASG_SHR:
  case EB_ASG_SHL:
  case EB_LHS:
  case EB_RHS:
      return TRUE;
  }

  return FALSE;
}

static void generateAssign(GeneratedFunction *f, AstExpression *expression) {
  AstExpression *lvalue = expression->binaryExpr.left;
  AstExpression *rvalue = expression->binaryExpr.right;
  ExpressionType op = expression->op;

  generateExpression(f, rvalue);

  TypeRef *lType = lvalue->type;
  TypeRef *rType = rvalue->type;
  Address addr = { 0 };

  AstExpression *addrExpr = lvalue;
  Boolean saved_acc = FALSE;
  TypeId lTypeId = typeToId(lType);
  TypeId rTypeId = typeToId(rType);

  Boolean isFP = lTypeId == T_F4 || lTypeId == T_F8;

  size_t typeSize = computeTypeSize(lType);

  if (!((addrExpr->op == E_NAMEREF || addrExpr->op == E_CONST) && op == EB_ASSIGN) && lTypeId != T_F10) {
    if (isFP) {
      emitPushRegF(f, R_FACC);
    } else {
      emitPushReg(f, R_ACC); // save result
    }
    saved_acc = TRUE;
  }

  assert(addrExpr->op == EU_DEREF);
  translateAddress(f, addrExpr->unaryExpr.argument, &addr);

  if (op == EB_ASSIGN) {
      // a = b

      if (lTypeId == T_F10) {
          leaRelocatable(f, &addr, R_ACC);
          emitStore(f, R_BAD, &addr, lTypeId);
          emitLoad(f, &addr, R_BAD, T_F10);
      } else if (lType->kind == TR_BITFIELD) {
          TypeRef *storageType = lType->bitFieldDesc.storageType;

          emitLea(f, &addr, R_EDI);

          addr.base = R_EDI;
          addr.index = R_BAD;
          addr.imm = 0;

          if (saved_acc) {
              emitPopReg(f, R_ACC);
          }
          storeBitField(f, lType, R_ACC, &addr);
      } else {
          if (isCompositeType(lType)) {
            emitPopReg(f, R_TMP2); // load result

            Address src = { 0 };
            src.base = R_TMP2;
            src.index = R_BAD;

            leaRelocatable(f, &addr, R_ACC);

            copyStructTo(f, lType, &src, &addr);
          } else {
            if (isFP) {
                emitPopRegF(f, R_FACC);
                emitStore(f, R_FACC, &addr, rTypeId);
            } else {
                enum Registers resultReg = R_BAD;

                if (addr.base != R_ACC && addr.index != R_ACC) {
                    resultReg = R_ACC;
                } else if (addr.base == R_ACC) {
                    switch (addr.index) {
                    case R_BAD:
                    case R_EDI:
                    case R_EDX: resultReg = R_ECX; break;
                    case R_ECX: resultReg = R_EDI; break;
                    default: unreachable("Cannot pick a register");
                    }
                } else {
                    assert(addr.index == R_ACC);
                    switch (addr.base) {
                    case R_BAD:
                    case R_EDI:
                    case R_EDX: resultReg = R_ECX; break;
                    case R_ECX: resultReg = R_EDI; break;
                    default: unreachable("Cannot pick a register");
                    }
                }

                assert(resultReg != R_BAD);

                emitPopReg(f, resultReg);

                emitStore(f, resultReg, &addr, rTypeId);

                if (resultReg != R_ACC) {
                    emitMoveRR(f, resultReg, R_ACC, typeSize);
                }
            }
          }
      }
  } else {
    enum Opcodes opcode = selectAssignOpcode(op, lType);
    if (lTypeId == T_F10) {
        leaRelocatable(f, &addr, R_EDI);
        emitLoad(f, &addr, R_BAD, T_F10);
        emitFPnoArg(f, 0xC9); // xchg
        emitFPArith(f, opcode, 1, TRUE);
        emitStore(f, R_BAD, &addr, T_F10);
        emitLoad(f, &addr, R_BAD, T_F10);
    } else if (lType->kind == TR_BITFIELD) {
        TypeRef *storageType = lType->bitFieldDesc.storageType;

        emitLea(f, &addr, R_EDI);

        addr.base = R_EDI;
        addr.index = R_BAD;
        addr.imm = 0;

        loadBitField(f, lType, &addr, R_ACC);
        emitPopReg(f, R_TMP);

        emitArithRR(f, selectAssignOpcode(op, storageType), R_ACC, R_TMP, typeSize);

        storeBitField(f, lType, R_ACC, &addr);

    } else {
        if (isFP) {
            emitLoad(f, &addr, R_FACC, lTypeId);
            emitPopRegF(f, R_FTMP);
            emitArithRR(f, opcode, R_FACC, R_FTMP, typeSize);
            emitStore(f, R_FACC, &addr, rTypeId);
        } else {
            leaRelocatable(f, &addr, R_EDI);

            if (addr.base == R_ACC || addr.index == R_ACC) {
                emitLea(f, &addr, R_EDI);
                addr.base = R_EDI;
                addr.index = R_BAD;
                addr.imm = addr.scale = 0;
            }

            emitLoad(f, &addr, R_ACC, lTypeId);

            enum Registers reg = R_TMP;

            if (isShiftLikeOp(op)) {
                emitMoveRR(f, R_ECX, R_TMP2, sizeof(intptr_t));
                reg = R_ECX;
            }

            emitPopReg(f, reg);
            emitArithRR(f, opcode, R_ACC, reg, typeSize);
            emitStore(f, R_ACC, &addr, rTypeId);
            if (reg != R_TMP2) {
               emitMoveRR(f, R_TMP2, R_ECX, sizeof(intptr_t));
            }
        }
    }
  }
}

static void generateAssignDiv(GeneratedFunction *f, AstExpression *expression) {
  assert(expression->op == EB_ASG_DIV || EB_ASG_MOD);

  TypeRef *type = expression->type;

  if (isRealType(type)) return generateAssign(f, expression);

  AstExpression *lvalue = expression->binaryExpr.left;
  AstExpression *rvalue = expression->binaryExpr.right;

  generateExpression(f, rvalue);

  emitPushReg(f, R_ACC);

  TypeRef *lType = lvalue->type;
  TypeRef *rType = rvalue->type;
  Address addr = { 0 };

  AstExpression *addrExpr = lvalue;
  Boolean saved_acc = FALSE;
  TypeId lTypeId = typeToId(lType);
  TypeId rTypeId = typeToId(rType);

  size_t typeSize = computeTypeSize(type);
  Boolean isU = isUnsignedType(type);

  assert(lvalue->op == EU_DEREF);
  translateAddress(f, lvalue->unaryExpr.argument, &addr);
  leaRelocatable(f, &addr, R_EDI);

  enum Opcodes opcode;
  if (lType->kind == TR_BITFIELD) {
      TypeRef *storageType = lType->bitFieldDesc.storageType;

      loadBitField(f, lType, &addr, R_ACC);
      emitPopReg(f, R_TMP);

      if (isU) {
        emitArithRR(f, OP_XOR, R_EDX, R_EDX, typeSize);
        opcode = OP_UDIV;
      } else {
        emitConvertWDQ(f, 0x99, typeSize);
        opcode = OP_SDIV;
      }

      emitArithRR(f, opcode, R_ACC, R_TMP2, typeSize);

      enum Registers result = R_ACC;

      if (expression->op == EB_ASG_MOD) {
          emitMoveRR(f, R_EDX, R_ACC, typeSize);
          result = R_EDX;
      }

      storeBitField(f, lType, result, &addr);
  } else {
      emitLoad(f, &addr, R_ACC, rTypeId);
      emitPopReg(f, R_TMP2);

      if (isU) {
        emitArithRR(f, OP_XOR, R_EDX, R_EDX, typeSize);
        opcode = OP_UDIV;
      } else {
        emitConvertWDQ(f, 0x99, typeSize);
        opcode = OP_SDIV;
      }

      emitArithRR(f, opcode, R_ACC, R_TMP2, typeSize);

      enum Registers result = R_ACC;

      if (expression->op == EB_ASG_MOD) {
          emitMoveRR(f, R_EDX, R_ACC, typeSize);
          result = R_EDX;
      }

      emitStore(f, result, &addr, rTypeId);
  }
}

static enum Opcodes selectIncDecOpcode(ExpressionType astOp, TypeRef *type) {
  size_t size = computeTypeSize(type);
  if (astOp == EU_POST_DEC || astOp == EU_PRE_DEC) {
      return OP_SUB;
  } else {
      assert(astOp == EU_POST_INC || astOp == EU_PRE_INC);
      return OP_ADD;
  }
}

static void generateAllocaImpl(GeneratedFunction *f, AstExpression *size) {
  // alloca algo consts of three steps:
  // 1. move existing stack down on allocating delta
  // 2. set new rsp
  // 3. save new alloca stack border which is also a result of alloca

  // callee-saved regs: R_EBX, R_R12, R_R13, R_R14, R_R15
  const int32_t dataSize = sizeof(intptr_t);

  generateExpression(f, size);

  int32_t alignment = 2 * dataSize;

  // size is in R_ACC
  emitArithConst(f, OP_ADD, R_ACC, alignment - 1, T_S8);
  emitArithConst(f, OP_AND, R_ACC, ~(alignment - 1), T_S8);

  // aligned size is in R_ACC

  // move existing stack down
  // R_EAX, R_ECX, R_EDX, R_ESI, R_EDI, R_R8, R_R9, R_R10, R_R11

  enum Registers delta = R_EAX;
  enum Registers r_sab = R_ECX; // R_ECX - Stack Alloca Border
  enum Registers to = R_EDX;
  enum Registers from = R_ESI;

  Address sabAddress = { R_EBP, R_BAD, 0, f->allocaOffset, NULL, NULL };
  emitMoveAR(f, &sabAddress, r_sab, dataSize);

  emitMoveRR(f, R_ESP, from, dataSize);
  emitMoveRR(f, R_ESP, to, dataSize);
  emitArithRR(f, OP_SUB, to, delta, dataSize);

  struct Label head = { 0 }, tail = { 0 };

  bindLabel(f, &head);
  // x < y -> cmp, x, y
  emitArithRR(f, OP_CMP, to, r_sab, dataSize);
  emitCondJump(f, &tail, JC_NOT_L, TRUE);

  Address fromAddr = { from, R_BAD, 0, 0, NULL, NULL };
  Address toAddr = { to, R_BAD, 0, 0, NULL, NULL };

  enum Registers tmp = R_EDI;
  emitMoveAR(f, &fromAddr, tmp, dataSize);
  emitMoveRA(f, tmp, &toAddr, dataSize);

  emitArithConst(f, OP_ADD, to, dataSize, T_S8);
  emitArithConst(f, OP_ADD, from, dataSize, T_S8);

  emitJumpTo(f, &head, TRUE);

  bindLabel(f, &tail);

  emitArithRR(f, OP_SUB, R_ESP, delta, dataSize);
  emitNegR(f, delta, dataSize);
  emitArithRR(f, OP_ADD, delta, r_sab, dataSize);
  emitMoveRA(f, R_ACC, &sabAddress, dataSize);
}

static Boolean generateAlloca(GeneratedFunction *f, AstExpression *expression) {
  AstExpression *callee = expression->callExpr.callee;

  if (callee->op != E_NAMEREF) return FALSE;
  if (strcmp("alloca", callee->nameRefExpr.s->name)) return FALSE;

  const int32_t dataSize = sizeof(intptr_t);
  AstExpressionList *args = expression->callExpr.arguments;
  assert(args->next == NULL);

  generateAllocaImpl(f, args->expression);

  return TRUE;
}

static const enum Registers intArgumentRegs[] = { R_ARG_0, R_ARG_1, R_ARG_2, R_ARG_3, R_ARG_4, R_ARG_5 };
static const enum Registers fpArgumentRegs[] = { R_XMM0, R_XMM1, R_XMM2, R_XMM3, R_XMM4, R_XMM5, R_XMM6, R_XMM7 };
static const int32_t smallStructSize = 2 * sizeof (intptr_t);

static void generateCall(GeneratedFunction *f, AstExpression *expression) {
  assert(expression->op == E_CALL);

  if (generateAlloca(f, expression)) return;

  AstExpression *callee = expression->callExpr.callee;
  AstExpressionList *args = expression->callExpr.arguments;
  TypeRef *type = expression->type;
  TypeRef *pcalleeType = callee->type;
  TypeRef *calleeType = pcalleeType->kind == TR_POINTED ? pcalleeType->pointed : pcalleeType;
//  assert(pcalleeType->kind == TR_POINTED);
  assert(calleeType->kind == TR_FUNCTION);
  TypeRef *returnType = calleeType->functionTypeDesc.returnType;

  unsigned offset = 0; // call frame start
  int r_offsets[R_PARAM_COUNT + R_FP_PARAM_COUNT] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  TypeRef *r_types[R_PARAM_COUNT + R_FP_PARAM_COUNT] = { NULL };

  unsigned frameOffset = f->frameSize; //ALIGN_SIZE(f->frameOffset + f->localsSize + f->argsSize, sizeof(intptr_t));
  assert(ALIGN_SIZE(frameOffset, 16) == frameOffset);

  unsigned idx = 0;

  unsigned returnTypeSize = computeTypeSize(returnType);

  unsigned firstIntRegArg = 0;
  unsigned lastIntRegArg = 0;

  unsigned lastFpRegArg = 0;

  unsigned intRegArgs = 0;
  unsigned fpRegArgs = 0;

  unsigned argsCount = 0;
  AstExpressionList *tmp = args;

  for (; tmp; tmp = tmp->next) ++argsCount;

  int32_t *offsets = alloca(argsCount * sizeof (int32_t));
  unsigned stackDelta = (f->stackOffset / 8) % 2 * 8;
  unsigned stackArgSize = 0;

  if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
    firstIntRegArg = 1;
  }

  unsigned count = 0;

  for (tmp = args; tmp; tmp = tmp->next) {
      args = tmp;

      TypeRef *t = tmp->expression->type;

      unsigned alignent = max(8, typeAlignment(t));
      unsigned argSize = max(8, computeTypeSize(t));

      ++count;

      if (isRealType(t)) {
        if (fpRegArgs < R_FP_PARAM_COUNT && argSize <= 8) {
          ++fpRegArgs;
          lastFpRegArg = count;
          continue;
        }
      } else if (!isCompositeType(t) || argSize <= sizeof(intptr_t)) {
        if (intRegArgs < R_PARAM_COUNT) {
          ++intRegArgs;
          lastIntRegArg = count;
          continue;
        }
      }

      stackArgSize = ALIGN_SIZE(stackArgSize, alignent);

      offsets[count - 1] = stackArgSize;

      stackArgSize += argSize;
  }

  unsigned alignedStackSize = ALIGN_SIZE(stackArgSize, 2 * sizeof(intptr_t)) + stackDelta;
  unsigned delta = alignedStackSize - stackArgSize;

  unsigned totalRegArg = intRegArgs + fpRegArgs;

  if (alignedStackSize) {
    emitArithConst(f, OP_SUB, R_ESP, alignedStackSize, T_S8);
    f->stackOffset += alignedStackSize;
  }

  int32_t stackBase = f->stackOffset;

  while (args) {
    AstExpression *arg = args->expression;
    TypeRef *argType = arg->type;

    unsigned alignent = max(8, typeAlignment(argType));
    unsigned argSize = max(8, computeTypeSize(argType));

    int32_t rspOffset = offsets[count - 1];

    Address dst = { R_ESP, R_BAD, 0, rspOffset, NULL, NULL };

    if (isCompositeType(argType) && argSize > sizeof(intptr_t)) {
      Address addr = { 0 };
      assert(arg->op == EU_DEREF);
      translateAddress(f, arg->unaryExpr.argument, &addr);
      leaRelocatable(f, &addr, R_ACC);
      dst.imm = rspOffset + (f->stackOffset - stackBase);
      copyStructTo(f, argType, &addr, &dst);
    } else {
      generateExpression(f, arg);
      dst.imm = rspOffset + (f->stackOffset - stackBase);

      if (isRealType(argType)) {
        if (argSize <= 8) {
          if (count <= lastFpRegArg) {
              r_offsets[totalRegArg - idx - 1] = rspOffset;
              r_types[totalRegArg - idx - 1] = argType;
              ++idx;
              emitPushRegF(f, R_FACC);
          } else {
              emitMovfpRA(f, R_FACC, &dst, argSize);
          }
        } else { // F10
          emitFPStore(f, &dst, T_F10);
        }
      } else {
        if (count <= lastIntRegArg) {
            r_offsets[totalRegArg - idx - 1] = rspOffset;
            r_types[totalRegArg - idx - 1] = argType;
            ++idx;
            if (isCompositeType(argType)) {
                Address addr = { R_ACC, R_BAD, 0, 0, NULL, NULL };
                emitMoveAR(f, &addr, R_ACC, argSize);
            }
            emitPushReg(f, R_ACC);
        } else {
            emitMoveRA(f, R_ACC, &dst, argSize);
        }
      }

    }
    --count;
    args = args->prev;
  }

  assert(count == 0);
  assert(idx <= totalRegArg);

  unsigned callFrameSize = offset;
  unsigned parametersAreaSize = callFrameSize - returnTypeSize;

  int i;

  Address saddr = { R_ESP, R_BAD, 0, 0, NULL, NULL };

  if (callee->op != E_NAMEREF) {
      generateExpression(f, callee);
      emitMoveRR(f, R_ACC, R_R10, sizeof(intptr_t));
  }

  unsigned ir = 0, fr = 0;

  if (isCompositeType(returnType) && returnTypeSize > sizeof (intptr_t)) {
      Address returnBuffer = { R_EBP, R_BAD, 0, f->structBufferOffset };
      emitLea(f, &returnBuffer, intArgumentRegs[ir++]);
  }

  for (i = 0; i < totalRegArg; ++i) {
      TypeRef *argType = r_types[i];
      size_t argSize = computeTypeSize(argType);
      if (isCompositeType(argType) && argSize > sizeof(intptr_t)) {
          saddr.imm = r_offsets[i] + f->stackOffset - stackBase;
          emitLea(f, &saddr, intArgumentRegs[ir++]);
      } else {
          if (isRealType(argType)) {
            if (argSize <= 8) {
              enum Registers fpArg = fpArgumentRegs[fr++];
              emitPopRegF(f, fpArg);
            }
          } else {
            emitPopReg(f, intArgumentRegs[ir++]);
          }
      }
  }

  if (calleeType->functionTypeDesc.isVariadic) {
    if (fpRegArgs) {
      emitMoveCR(f, fpRegArgs, R_ACC, T_S8);
    } else {
      emitArithRR(f, OP_XOR, R_ACC, R_ACC, sizeof(int64_t));
    }
  }

  if (callee->op == E_NAMEREF) {
    emitSymbolCall(f, callee->nameRefExpr.s);
  } else {
    emitCall(f, R_R10);
  }

  if (isCompositeType(returnType) && returnTypeSize <= sizeof(intptr_t)) {
      Address addr = { R_EBP, R_BAD, 0, f->structBufferOffset, NULL, NULL };
      emitMoveRA(f, R_ACC, &addr, sizeof(intptr_t));
      emitLea(f, &addr, R_ACC);
  } else if (isIntegerType(returnType)) {
      TypeId tid = typeToId(returnType);
      switch (tid) {
      case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC); break;
      case T_BOOL:
      case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC); break;
      case T_S2: emitMovxxRR(f, 0xBF, R_ACC, R_ACC); break;
      case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC); break;
      default: break;
      }
  }

  if (alignedStackSize) {
    f->stackOffset -= alignedStackSize;
    emitArithConst(f, OP_ADD, R_ESP, alignedStackSize, T_S8);
  }
}

static void generateVaArg(GeneratedFunction *f, AstExpression *expression) {
  generateExpression(f, expression->vaArg.va_list);

  TypeRef *vatype = expression->vaArg.argType;
  struct Label memLabl = { 0 }, doneLabl = { 0 };

  emitMoveRR(f, R_ACC, R_EDI, sizeof(intptr_t));

  TypeRef *valistType = expression->vaArg.va_list->type;
  Address valist_addr = { R_EDI, R_BAD, 0, 0, NULL, NULL };
  assert(is_va_list_Type(valistType));
  TypeDefiniton *vastruct = valistType->pointed->descriptorDesc->typeDefinition;
  const static int32_t dataSize = sizeof(intptr_t);

  /**
   * typedef struct {
   *   intptr_t gp_offset;
   *   intptr_t fp_offset;
   *   void *overflow_arg_area;
   *   const void *reg_save_area;
   * } __va_elem;
   */

  if (isRealType(vatype)) {
      int32_t fp_offset_off = memberOffset(vastruct, "fp_offset");
      valist_addr.imm = fp_offset_off;
      // R_ACC = va_list->fp_offset
      emitLoad(f, &valist_addr, R_TMP, T_U4);

      // va_list->fp_offset >= R_PARAM_COUNT + R_FP_PARAM_COUNT
      emitArithConst(f, OP_CMP, R_TMP, dataSize * (R_PARAM_COUNT + R_FP_PARAM_COUNT), T_U4);
      // if ( >= ) mem-load
      emitCondJump(f, &memLabl, JC_GE, TRUE);
      valist_addr.imm = memberOffset(vastruct, "reg_save_area");
      emitLoad(f, &valist_addr, R_ACC, T_U8);
      emitArithRR(f, OP_ADD, R_ACC, R_TMP, dataSize);
      emitArithConst(f, OP_ADD, R_TMP, dataSize, T_U4);
      valist_addr.imm = fp_offset_off;
      emitStore(f, R_TMP, &valist_addr, T_U4);
      emitJumpTo(f, &doneLabl, TRUE);
  } else if (isScalarType(vatype)) {
      int32_t gp_offset_off = memberOffset(vastruct, "gp_offset");
      valist_addr.imm = gp_offset_off;
      // R_ACC = va_list->fp_offset
      emitLoad(f, &valist_addr, R_TMP, T_U4);

      // va_list->gp_offset >= R_PARAM_COUNT
      emitArithConst(f, OP_CMP, R_TMP, dataSize * R_PARAM_COUNT, T_U4);
      // if ( >= ) mem-load
      emitCondJump(f, &memLabl, JC_GE, TRUE);

      valist_addr.imm = memberOffset(vastruct, "reg_save_area");
      emitLoad(f, &valist_addr, R_ACC, T_U8);

      emitArithRR(f, OP_ADD, R_ACC, R_TMP, dataSize);
      emitArithConst(f, OP_ADD, R_TMP, dataSize, T_U4);
      valist_addr.imm = gp_offset_off;
      emitStore(f, R_TMP, &valist_addr, T_U4);
      emitJumpTo(f, &doneLabl, TRUE);
  }

  bindLabel(f, &memLabl);

  int32_t ofa_area_off = memberOffset(vastruct, "overflow_arg_area");
  valist_addr.imm = ofa_area_off;
  emitLoad(f, &valist_addr, R_ACC, T_U8);
  int32_t align = typeAlignment(vatype);

  if (align > 8) {
    // (((len)+(align - 1)) & ~((align)-1))
    int32_t mask = ~(align - 1);
    emitArithConst(f, OP_ADD, R_ACC, align - 1, T_S8);
    emitArithConst(f, OP_AND, R_ACC, mask, T_S8);
  }

  int32_t argSize = max(8, computeTypeSize(vatype));

  emitMoveCR(f, ALIGN_SIZE(argSize, dataSize), R_TMP, T_S8);
  emitArithRR(f, OP_ADD, R_TMP, R_ACC, dataSize);
  emitStore(f, R_TMP, &valist_addr, T_U8);

  bindLabel(f, &doneLabl);
}

static void emitFPNeg(GeneratedFunction *f, TypeId id) {
  switch (id) {
   case T_F4:
      emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, FALSE);
      emitArithConst(f, OP_XOR, R_ACC, 1U << 31, T_U4);
      emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, FALSE);
      break;
   case T_F8:
      emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, TRUE);
      emitMoveCR(f, 1ULL << 63, R_TMP, T_U8);
      emitArithRR(f, OP_XOR, R_ACC, R_TMP, 8);
      emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, TRUE);
      break;
   case T_F10:
      emitFPnoArg(f, 0xE0);
      break;
   default:
      unreachable("unexpected FP type");
    }
}

static void generateCompoundExpression(GeneratedFunction *f, AstExpression *expression) {
  Address addr = { R_EBP, R_BAD, 0, f->structBufferOffset, NULL, NULL };

  TypeRef *type = expression->type;

  emitInitializerImpl(f, computeTypeSize(type), &addr, expression->compound, FALSE);

  if (isScalarType(type)) {
      TypeId tid = typeToId(type);
      enum Registers dst = tid == T_F10 ? R_BAD : tid >= T_F4 ? R_FACC : R_ACC;
      emitLoad(f, &addr, dst, tid);
  } else {
      emitLea(f, &addr, R_ACC);
  }
}

// result is in accamulator
static void generateExpression(GeneratedFunction *f, AstExpression *expression) {
  Address addr = { 0 };
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
      generateVaArg(f, expression);
      break;
    case E_NAMEREF:
      translateAddress(f, expression, &addr);
      emitLea(f, &addr, R_ACC);
      break;
    case E_COMPOUND:
      generateCompoundExpression(f, expression);
      break;
    case E_CALL:
      generateCall(f, expression);
      break;
    case E_TERNARY: {
        AstTernaryExpression *ternary = &expression->ternaryExpr;
        struct Label elseLabel = { 0 }, endLabel = { 0 };
        assert(ternary->condition);
        enum JumpCondition cc = generateCondition(f, ternary->condition, TRUE);

        emitCondJump(f, &elseLabel, cc, FALSE);

        assert(ternary->ifTrue);
        generateExpression(f, ternary->ifTrue);
        emitJumpTo(f, &endLabel, FALSE);

        assert(ternary->ifFalse);
        bindLabel(f, &elseLabel);
        generateExpression(f, ternary->ifFalse);

        bindLabel(f, &endLabel);
      }
      break;
    case E_BIT_EXTEND:
      generateBitExtend(f, expression);
      break;
    case E_CAST:
      generateCast(f, &expression->castExpr);
      break;
    case EB_ADD:
    case EB_SUB:
    case EB_LHS: /** << */
    case EB_RHS: /** >> */
    case EB_AND:
    case EB_OR:
    case EB_XOR:
    case EB_MUL:
      generateBinary(f, expression);
      break;
    case EB_DIV:
    case EB_MOD:
      generateDiv(f, expression);
      break;
    case EB_ANDAND:
    case EB_OROR:
      generateLogicalBinary(f, expression);
      break;
    case EB_EQ:
    case EB_NE:
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE: {
        enum JumpCondition cc = generateCondition(f, expression, FALSE);
        emitSetccR(f, cc, R_ACC);
        emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
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
      generateAssign(f, expression);
      // TODO:
      break;
    case EB_ASG_DIV:
    case EB_ASG_MOD:
      generateAssignDiv(f, expression);
      break;
    case EB_COMMA:
      generateExpression(f, expression->binaryExpr.left);
      generateExpression(f, expression->binaryExpr.right);
      break;
    case EU_REF:
      translateAddress(f, expression->unaryExpr.argument, &addr);
      emitLea(f, &addr, R_ACC);
      break;
    case EU_DEREF:
      translateAddress(f, expression->unaryExpr.argument, &addr);
      if (isFlatType(expression->type) || expression->type->kind == TR_FUNCTION) {
        if (!(addr.base == R_ACC && addr.index == R_BAD && addr.imm == 0)) {
          emitLea(f, &addr, R_ACC);
        }
      } else {
        emitLoad(f, &addr, R_ACC, typeId);
      }
      break;
    case EU_PLUS:
      generateExpression(f, expression->unaryExpr.argument);
      break;
    case EU_MINUS:
      generateExpression(f, expression->unaryExpr.argument);
      if (isRealType(expression->type)) {
          emitFPNeg(f, expression->type->descriptorDesc->typeId);
      } else {
          emitNegR(f, R_ACC, typeIdSize(typeId));
      }
      break;
    case EU_TILDA:
      generateExpression(f, expression->unaryExpr.argument);
      emitBitwiseNotR(f, R_ACC, computeTypeSize(expression->type));
      break;
    case EU_EXL:
      generateExpression(f, expression->unaryExpr.argument);
      emitNot(f, R_ACC, computeTypeSize(expression->unaryExpr.argument->type));
      break;

    case E_LABEL_REF: {
      GenerationContext *ctx = f->context;
      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)expression->label);
      if (l == NULL) {
          l = allocateLabel(ctx);
          putToHashMap(ctx->labelMap, (intptr_t)expression->label, (intptr_t)l);
      }
      Address addr = { R_RIP, R_BAD, 0, 0, NULL, l };
      emitLea(f, &addr, R_ACC);
      break;

    }

    default: unreachable("unexpcted expression op");
  }
}

static Boolean generateLabel(GeneratedFunction *f, AstLabelStatement *label) {
  GenerationContext *ctx = f->context;
  int i;
  switch (label->kind) {
    case LK_CASE:
      assert(ctx->caseLabels);
      struct CaseLabel *labels = ctx->caseLabels;
      for (i = 0; i < ctx->caseCount; ++i) {
          if (labels[i].caseConst == label->caseConst) {
              bindLabel(f, &labels[i].label);
              break;
          }
      }
      assert(i < ctx->caseCount);
      break;
    case LK_DEFAULT:
      assert(ctx->defaultLabel);
      bindLabel(f, ctx->defaultLabel);
      break;
    case LK_LABEL: {
      assert(label->label);
      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)label->label);
      if (l == NULL) {
        l = allocateLabel(ctx);
        putToHashMap(ctx->labelMap, (intptr_t)label->label, (intptr_t)l);
      }
      bindLabel(f, l);
    }
    break;
  }

  return generateStatement(f, label->body);
}

static void emitReturn(GeneratedFunction *f) {
  // movq $result, %rax
  // ret
  emitRet(f, 0);
}

static unsigned walkCaseLabels(AstStatement *body, struct CaseLabel *caseLabels, unsigned idx) {
  unsigned visited = 0;
  switch (body->statementKind) {
    case SK_BLOCK: {
        AstStatementList *stmts = body->block.stmts;
        while (stmts) {
            unsigned tmp = walkCaseLabels(stmts->stmt, caseLabels, idx);
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
      visited = walkCaseLabels(body->ifStmt.thenBranch, caseLabels, idx);
      idx += visited;
      if (body->ifStmt.elseBranch)
        visited += walkCaseLabels(body->ifStmt.elseBranch, caseLabels, idx);
      return visited;
    case SK_SWITCH:
      return 0; // stop
    case SK_WHILE:
    case SK_DO_WHILE:
      return walkCaseLabels(body->loopStmt.body, caseLabels, idx);
    case SK_FOR:
      return walkCaseLabels(body->forStmt.body, caseLabels, idx);
    case SK_LABEL:
      switch (body->labelStmt.kind) {
      case LK_DEFAULT:
      case LK_LABEL: return walkCaseLabels(body->labelStmt.body, caseLabels, idx);
      case LK_CASE:
          caseLabels[idx++].caseConst = body->labelStmt.caseConst;
          return walkCaseLabels(body->labelStmt.body, caseLabels, idx) + 1;
      }

      break;
    default: unreachable("Unknown statement kind");
  }

  return 0;

}

static void generateSwitchStatement(GeneratedFunction *f, AstSwitchStatement *stmt) {
  GenerationContext *ctx = f->context;
  struct Label *oldBreak = ctx->breakLabel;
  struct Label switchBreak = { 0 };
  ctx->breakLabel = &switchBreak;

  struct Label defaultLabel = { 0 };
  struct CaseLabel *caseLabels = alloca(sizeof(struct CaseLabel) * stmt->caseCount);
  memset(caseLabels, 0, sizeof(struct CaseLabel) * stmt->caseCount);

  struct Label *oldDefaultLabel = ctx->defaultLabel;
  struct CaseLabel *oldCaseLabels = ctx->caseLabels;
  unsigned oldCaseCount = ctx->caseCount;

  ctx->defaultLabel = &defaultLabel;
  ctx->caseLabels = caseLabels;
  ctx->caseCount = stmt->caseCount;

  unsigned visited = walkCaseLabels(stmt->body, caseLabels, 0);
  assert(visited == stmt->caseCount);


  AstExpression *condition = stmt->condition;
  assert(condition->type->kind == TR_VALUE);
  generateExpression(f, condition);

  size_t condTypeSize = computeTypeSize(condition->type);

  int i;

  for (i = 0; i < visited; ++i) {
      int64_t caseConst = caseLabels[i].caseConst;
      struct Label *caseLabel = &caseLabels[i].label;
      emitArithConst(f, OP_CMP, R_ACC, caseConst, typeToId(condition->type));
      emitCondJump(f, caseLabel, JC_EQ, FALSE);
  }

  if (stmt->hasDefault) {
      emitJumpTo(f, &defaultLabel, FALSE);
  } else {
      emitJumpTo(f, &switchBreak, FALSE);
  }

  generateStatement(f, stmt->body);

  bindLabel(f, &switchBreak);

  ctx->caseCount = oldCaseCount;
  ctx->caseLabels = oldCaseLabels;
  ctx->defaultLabel = oldDefaultLabel;
  ctx->breakLabel = oldBreak;
}

static enum JumpCondition generateFloatCondition(GeneratedFunction *f, AstExpression *left, AstExpression *right, ExpressionType op, Boolean invertion) {
  TypeId lid = typeToId(left->type);
  TypeId rid = typeToId(right->type);
  int32_t opSize = typeIdSize(lid);

  generateExpression(f, left);

  if (lid != T_F10) {
    emitPushRegF(f, R_FACC);
  }

  Address addr = { 0 };
  if (op == EB_EQ || op == EB_NE) {

      enum JumpCondition setcc = op == EB_EQ ? JC_NOT_PARITY : JC_PARITY;

      if (rid == T_F10) {
          generateExpression(f, right);
          emitFPArith(f, OP_FUCMP, 1, FALSE);
          emitSetccR(f, setcc, R_ACC);
          emitFPArith(f, OP_FUCMP, 1, TRUE);
          emitFPPop(f, 0);
      } else if (right->op == EU_DEREF) {
          translateAddress(f, right->unaryExpr.argument, &addr);
          if (addr.reloc) {
              emitLea(f, &addr, R_TMP);
              addr.base = R_TMP;
              addr.index = R_BAD;
              addr.imm = addr.scale = 0;
              addr.reloc = NULL;
          }
          emitPopRegF(f, R_FTMP2);
          emitArithAR(f, OP_FUCMP, R_FTMP2, &addr, opSize);
          emitSetccR(f, setcc, R_ACC);
          emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
          emitArithAR(f, OP_FUCMP, R_FTMP2, &addr, opSize);
      } else {
          generateExpression(f, right);
          emitPopRegF(f, R_FTMP2);
          emitArithRR(f, OP_FUCMP, R_FTMP2, R_FACC, opSize);
          emitSetccR(f, setcc, R_ACC);
          emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
          emitArithRR(f, OP_FUCMP, R_FTMP2, R_FACC, opSize);
      }

      struct Label l = { 0 };
      emitCondJump(f, &l, JC_EQ, TRUE);
      if (op == EB_EQ) {
          emitArithRR(f, OP_XOR, R_ACC, R_ACC, sizeof(int32_t));
      } else {
          emitMoveCR(f, 1, R_ACC, T_S4);
      }
      bindLabel(f, &l);

      emitTestRR(f, R_ACC, R_ACC, sizeof (int32_t));

      return invertion ? JC_ZERO : JC_NOT_ZERO;
  } else if (op == EB_LT || op == EB_LE) {
      if (rid == T_F10) {
          generateExpression(f, right);
          emitFPArith(f, OP_FOCMP, 1, TRUE);
          emitFPPop(f, 0);
      } else if (right->op == EU_DEREF) {
          translateAddress(f, right->unaryExpr.argument, &addr);
          emitPopReg(f, R_ACC);
          emitArithAR(f, OP_FOCMP, R_FACC, &addr, opSize);
      } else {
          generateExpression(f, right);
          emitPopReg(f, R_TMP);
          emitArithRR(f, OP_FOCMP, R_FTMP, R_FACC, opSize);
      }
      return op == EB_LT ? invertion ? JC_NOT_L : JC_L : invertion ? JC_NOT_LE : JC_LE;
  } else {
      generateExpression(f, right);
      if (rid == T_F10) {
        emitFPnoArg(f, 0xC9); // change st(0) with st(1)
        emitFPArith(f, OP_FOCMP, 1, TRUE);
        emitFPPop(f, 0);
      } else {
        emitPopReg(f, R_FTMP);
        emitArithRR(f, OP_FOCMP, R_FACC, R_FTMP, opSize);
      }
      return op == EB_GT ? invertion ? JC_NOT_G : JC_G : invertion ? JC_NOT_GE : JC_GE;
  }

}

static enum JumpCondition generateUnsignedCondition(GeneratedFunction *f, AstExpression *left, AstExpression *right, ExpressionType op, Boolean invertion) {
  enum JumpCondition cc = JC_BAD;
  Boolean swap = FALSE;
  switch (op) {
  case EB_GE: cc = invertion ? JC_BELOW : JC_A_E; break;
  case EB_LE: swap = TRUE; cc = invertion ? JC_BELOW : JC_A_E; break;
  case EB_LT: cc = invertion ? JC_A_E: JC_BELOW; break;
  case EB_GT: swap = TRUE; cc = invertion ? JC_A_E: JC_BELOW; break;
  case EB_NE: cc = invertion ? JC_EQ : JC_NE; break;
  case EB_EQ: cc = invertion ? JC_NE : JC_EQ; break;
  default: unreachable("Unepxected condition op");
  }

  TypeId lid = typeToId(left->type);
  TypeId rid = typeToId(right->type);
  int32_t opSize = typeIdSize(lid);

  generateExpression(f, left);

  if (right->op == E_CONST && !swap) {
    uint64_t cnst = right->constExpr.i;
    emitArithConst(f, OP_CMP, R_ACC, cnst, lid);
  } else {
    emitPushReg(f, R_ACC);

    if (right->op == EU_DEREF && !isFlatType(right->type) && lid == rid && !swap) {
        Address addr = { 0 };
        translateAddress(f, right->unaryExpr.argument, &addr);
        emitPopReg(f, R_TMP);
        emitArithAR(f, OP_CMP, R_TMP, &addr, opSize);
    } else {
        generateExpression(f, right);
        emitPopReg(f, R_TMP);
        // x op y -> y op x
        enum Registers l = swap ? R_ACC : R_TMP;
        enum Registers r = swap ? R_TMP : R_ACC;
        emitArithRR(f, OP_CMP, l, r, opSize);
    }
  }

  return cc;
}

static enum JumpCondition generateSignedCondition(GeneratedFunction *f, AstExpression *left, AstExpression *right, ExpressionType op, Boolean invertion) {
  enum JumpCondition cc = JC_BAD;
  switch (op) {
  case EB_GE: cc = invertion ? JC_NOT_GE : JC_GE; break;
  case EB_LE: cc = invertion ? JC_NOT_LE : JC_LE; break;
  case EB_LT: cc = invertion ? JC_NOT_L : JC_L; break;
  case EB_GT: cc = invertion ? JC_NOT_G : JC_G; break;
  case EB_NE: cc = invertion ? JC_EQ : JC_NE; break;
  case EB_EQ: cc = invertion ? JC_NE : JC_EQ; break;
  default: unreachable("Unepxected condition op");
  }

  TypeId lid = typeToId(left->type);
  TypeId rid = typeToId(right->type);
  int32_t opSize = typeIdSize(lid);

  generateExpression(f, left);

  if (right->op == E_CONST) {
    uint64_t cnst = right->constExpr.i;
    emitArithConst(f, OP_CMP, R_ACC, cnst, lid);
  } else {
    emitPushReg(f, R_ACC);

    assert(!isFlatType(right->type));

    if (right->op == EU_DEREF && lid == rid) {
        Address addr = { 0 };
        translateAddress(f, right->unaryExpr.argument, &addr);
        emitPopReg(f, R_TMP);
        emitArithAR(f, OP_CMP, R_TMP, &addr, opSize);
    } else {
        generateExpression(f, right);
        emitPopReg(f, R_TMP);
        // x op y -> y op x
        emitArithRR(f, OP_CMP, R_TMP, R_ACC, opSize);
    }
  }

  return cc;
}

static enum JumpCondition generateCondition(GeneratedFunction *f, AstExpression *cond, Boolean invertion) {

  switch (cond->op) {
    case EB_GE:
    case EB_LE:
    case EB_LT:
    case EB_GT:
    case EB_NE:
    case EB_EQ: {
      TypeId tid = typeToId(cond->binaryExpr.left->type);
      Boolean isFP = tid >= T_F4;
      Boolean isU = tid >= T_U1;
      if (tid >= T_F4) {
          return generateFloatCondition(f, cond->binaryExpr.left, cond->binaryExpr.right, cond->op, invertion);
      }
      if (tid >= T_U1) {
          return generateUnsignedCondition(f, cond->binaryExpr.left, cond->binaryExpr.right, cond->op, invertion);
      }

      return generateSignedCondition(f, cond->binaryExpr.left, cond->binaryExpr.right, cond->op, invertion);
    }
    default:
      if (cond->op == EU_EXL) {
        generateExpression(f, cond->unaryExpr.argument);
        emitTestRR(f, R_ACC, R_ACC, computeTypeSize(cond->type));
        return invertion ? JC_NOT_ZERO : JC_ZERO;
      } else {
        generateExpression(f, cond);
        emitTestRR(f, R_ACC, R_ACC, computeTypeSize(cond->type));
        return invertion ? JC_ZERO : JC_NOT_ZERO;
      }
  }
}

static void generateIfStatement(GeneratedFunction *f, AstIfStatement *stmt) {

  struct Label elseLabel = { 0 }, endLabel = { 0 };

  assert(stmt->condition);
  enum JumpCondition cc = generateCondition(f, stmt->condition, TRUE);

  if (stmt->elseBranch) {
    emitCondJump(f, &elseLabel, cc, FALSE);
  } else {
    emitCondJump(f, &endLabel, cc, FALSE);
  }

  assert(stmt->thenBranch);
  generateStatement(f, stmt->thenBranch);

  if (stmt->elseBranch) {
      emitJumpTo(f, &endLabel, FALSE);
      bindLabel(f, &elseLabel);
      generateStatement(f, stmt->elseBranch);
  }

  bindLabel(f, &endLabel);
}

static void generateLoopStatement(GeneratedFunction *f, AstLoopStatement *stmt, Boolean doLoop) {

  GenerationContext *ctx = f->context;
  struct Label loopHead = { 0 }, loopTail = { 0 }, continueLabel = { 0 };
  struct Label *oldBreak = ctx->breakLabel, *oldContinue = ctx->continueLabel;

  ctx->breakLabel = &loopTail;
  ctx->continueLabel = &continueLabel;

  bindLabel(f, &loopHead);

  assert(stmt->condition);

  if (!doLoop) {
      bindLabel(f, &continueLabel);
      enum JumpCondition cc = generateCondition(f, stmt->condition, TRUE);
      emitCondJump(f, &loopTail, cc, FALSE);
  }

  assert(stmt->body);
  generateStatement(f, stmt->body);

  if (doLoop) {
      bindLabel(f, &continueLabel);
      enum JumpCondition cc = generateCondition(f, stmt->condition, FALSE);
      emitCondJump(f, &loopHead, cc, FALSE);
  } else {
      emitJumpTo(f, &loopHead, FALSE);
  }

  bindLabel(f, &loopTail);

  ctx->breakLabel = oldBreak;
  ctx->continueLabel = oldContinue;
}

static void generateForStatement(GeneratedFunction *f, AstForStatement *stmt) {
  if (stmt->initial) {
      AstStatementList *stmts = stmt->initial;
      for (; stmts; stmts = stmts->next) {
        generateStatement(f, stmts->stmt);
      }
  }

  GenerationContext *ctx = f->context;

  struct Label loopHead = { 0 }, loopTail = { 0 }, continueLabel = { 0 };

  struct Label *oldBreak = ctx->breakLabel, *oldContinue = ctx->continueLabel;

  ctx->breakLabel = &loopTail;
  ctx->continueLabel = stmt->modifier ? &continueLabel : &loopHead;

  bindLabel(f, &loopHead);

  if (stmt->condition) {
    enum JumpCondition cc = generateCondition(f, stmt->condition, TRUE);
    emitCondJump(f, &loopTail, cc, FALSE);
  }

  assert(stmt->body);
  generateStatement(f, stmt->body);

  if (stmt->modifier) {
    bindLabel(f, &continueLabel);
    generateExpression(f, stmt->modifier);
  }

  emitJumpTo(f, &loopHead, FALSE);
  bindLabel(f, &loopTail);

  ctx->breakLabel = oldBreak;
  ctx->continueLabel = oldContinue;
}

static void allocateVLAMemory(GeneratedFunction *f, GeneratedVariable *v, AstInitializer *init, TypeRef *type) {
  assert(init && init->kind == IK_EXPRESSION);

  generateAllocaImpl(f, init->expression);

  Address addr = { R_EBP, R_BAD, 0, v->baseOffset };
  emitStore(f, R_ACC, &addr, T_U8);
}

static Boolean generateStatement(GeneratedFunction *f, AstStatement *stmt) {

  GenerationContext *ctx = f->context;

  switch (stmt->statementKind) {
  case SK_BLOCK:
      return generateBlock(f, &stmt->block);
  case SK_DECLARATION: {
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
          GeneratedVariable *gv = generateVaribale_x86_64(ctx, v);
          v->gen = gv;
          gv->next = ctx->file->staticVariables;
          ctx->file->staticVariables = gv;
        }
      }
      break;
  }
  case SK_EMPTY: break;
  case SK_EXPR_STMT:
      generateExpression(f, stmt->exprStmt.expression);
      if (typeToId(stmt->exprStmt.expression->type) == T_F10) {
          // clean up FP processor stack
          emitFPPop(f, 0);
      }
      break;
  case SK_LABEL:
      return generateLabel(f, &stmt->labelStmt);
  case SK_GOTO_L: {
      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label);
      if (l == NULL) {
          l = allocateLabel(ctx);
          putToHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label, (intptr_t)l);
      }
      emitJumpTo(f, l, FALSE);
      break;
  }
  case SK_GOTO_P: {
      generateExpression(f, stmt->jumpStmt.expression);
      emitJumpByReg(f, R_ACC);
      break;
  }
  case SK_RETURN: {
      AstExpression *retExpr = stmt->jumpStmt.expression;
      if (retExpr) {
          if (isCompositeType(retExpr->type)) {
            Address src = { 0 };
            assert(retExpr->op == EU_DEREF);
            translateAddress(f, retExpr->unaryExpr.argument, &src);

            size_t retSize = computeTypeSize(retExpr->type);
            if (retSize > sizeof(intptr_t)) {
              Address addr = { R_EBP, R_BAD, 0, f->returnStructAddressOffset, NULL, NULL };
              emitMoveAR(f, &addr, R_EDI, sizeof(intptr_t));

              Address dst = { R_EDI, R_BAD, 0, 0, NULL, NULL };
              copyStructTo(f, retExpr->type, &src, &dst);
              emitMoveRR(f, R_EDI, R_ACC, sizeof(intptr_t));
            } else {
              emitMoveAR(f, &src, R_ACC, retSize);
            }
          } else {
            generateExpression(f, retExpr);
          }
      }
      emitLeave(f);
      emitRet(f, 0);
      return TRUE;
      }
  case SK_BREAK:
      assert(f->context->breakLabel);
      emitJumpTo(f, f->context->breakLabel, FALSE);
      break;
  case SK_CONTINUE:
      assert(ctx->continueLabel);
      emitJumpTo(f, ctx->continueLabel, FALSE);
      break;
  case SK_IF:
      generateIfStatement(f, &stmt->ifStmt);
      break;
  case SK_SWITCH:
      generateSwitchStatement(f, &stmt->switchStmt);
      break;
  case SK_WHILE:
  case SK_DO_WHILE:
      generateLoopStatement(f, &stmt->loopStmt, stmt->statementKind == SK_DO_WHILE);
      break;
  case SK_FOR:
      generateForStatement(f, &stmt->forStmt);
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

static void pushFrame(GeneratedFunction *f) {

  // pushq %rbp
  emitPushReg(f, R_EBP);
  // movq %rsp, %rbp
  emitMoveRR(f, R_ESP, R_EBP, sizeof(intptr_t));
}

static void popFrame(GeneratedFunction *f) {
  // movq %rbp, %rsp
  // popq %rbp

  emitLeave(f);
}

static size_t allocateLocalSlots(GeneratedFunction *g, AstFunctionDefinition *f) {
  AstValueDeclaration *param = f->declaration->parameters;
  AstValueDeclaration *local = f->locals;
  TypeRef *returnType = f->declaration->returnType;

  unsigned intRegParams = 0;
  unsigned fpRegParams = 0;

  int32_t baseOffset = 0; // from rbp;
  int32_t stackParamOffset = sizeof(intptr_t) + sizeof(intptr_t); // rbp itself + return pc

  int32_t structBufferOffset = 0;
  if (f->returnStructBuffer) {
      baseOffset += ALIGN_SIZE(f->returnStructBuffer, 2 * sizeof(intptr_t));
      structBufferOffset = g->structBufferOffset = -baseOffset;
  }

  baseOffset += sizeof(intptr_t);
  int32_t allocaOffset = g->allocaOffset = -baseOffset;

  Address addr = { R_EBP, R_BAD, 0, 0, NULL, NULL };

  if (isCompositeType(returnType) && computeTypeSize(returnType) > sizeof(intptr_t)) {
      baseOffset += sizeof(intptr_t);
      g->returnStructAddressOffset = addr.imm = -baseOffset;
      emitMoveRA(g, intArgumentRegs[intRegParams++], &addr, sizeof(intptr_t));
  }

  for (; param; param = param->next) {
    TypeRef *paramType = param->type;
    GeneratedVariable *gp = allocateGenVarialbe(g->context, param);
    size_t size = max(computeTypeSize(paramType), sizeof(intptr_t));
    size_t align = max(typeAlignment(paramType), sizeof(intptr_t));

    if (isCompositeType(paramType) && size > sizeof(intptr_t)) {
        int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
        gp->baseOffset = alignedOffset;
        stackParamOffset = alignedOffset + size;
    } else if (isRealType(paramType)) {
        if (fpRegParams < R_FP_PARAM_COUNT && size <= 8) {
            baseOffset += size;
            baseOffset = ALIGN_SIZE(baseOffset, align);
            gp->baseOffset = addr.imm = -baseOffset;
            emitMovfpRA(g, fpArgumentRegs[fpRegParams++], &addr, size);
        } else {
            int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
            gp->baseOffset = alignedOffset;
            stackParamOffset = alignedOffset + size;
        }
    } else {
        if (intRegParams < R_PARAM_COUNT) {
            baseOffset += size;
            baseOffset = ALIGN_SIZE(baseOffset, align);
            gp->baseOffset = addr.imm = -baseOffset;
            emitMoveRA(g, intArgumentRegs[intRegParams++], &addr, size);
        } else {
            int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
            gp->baseOffset = alignedOffset;
            stackParamOffset = alignedOffset + size;
        }
    }
  }

  for (; local; local = local->next) {
      TypeRef *localType = local->type;
      GeneratedVariable *gp = allocateGenVarialbe(g->context, local);
      size_t size = computeTypeSize(localType);
      size_t align = typeAlignment(localType);

      baseOffset += size;
      baseOffset = ALIGN_SIZE(baseOffset, align);
      gp->baseOffset = -baseOffset;
  }

  if (f->declaration->isVariadic) {
      const static int32_t dataSize = sizeof(intptr_t);
      AstValueDeclaration *va_area = f->va_area;
      assert(va_area);
      int32_t gap = baseOffset += dataSize;
      int32_t reg_save_area_ptr_off = baseOffset += dataSize;
      int32_t overflow_arg_area_off = baseOffset += dataSize;
      int32_t fp_offset_off = baseOffset += sizeof(uint32_t);
      int32_t gp_offset_off = baseOffset += sizeof(uint32_t);

      va_area->gen = allocateGenVarialbe(g->context, va_area);
      va_area->gen->baseOffset = -gp_offset_off;

      int32_t fp_va_area = baseOffset += R_FP_PARAM_COUNT * dataSize;

      int32_t gp_va_area = baseOffset += R_PARAM_COUNT * dataSize;

      Address addr = { R_EBP, R_BAD, 0, 0, NULL, NULL };

      // fp
      addr.imm = -(fp_va_area - 0 * dataSize);
      emitMovfpRA(g, R_XMM0, &addr, dataSize);

      addr.imm = -(fp_va_area - 1 * dataSize);
      emitMovfpRA(g, R_XMM1, &addr, dataSize);

      addr.imm = -(fp_va_area - 2 * dataSize);
      emitMovfpRA(g, R_XMM2, &addr, dataSize);

      addr.imm = -(fp_va_area - 3 * dataSize);
      emitMovfpRA(g, R_XMM3, &addr, dataSize);

      addr.imm = -(fp_va_area - 4 * dataSize);
      emitMovfpRA(g, R_XMM4, &addr, dataSize);

      addr.imm = -(fp_va_area - 5 * dataSize);
      emitMovfpRA(g, R_XMM5, &addr, dataSize);

      addr.imm = -(fp_va_area - 6 * dataSize);
      emitMovfpRA(g, R_XMM6, &addr, dataSize);

      addr.imm = -(fp_va_area - 7 * dataSize);
      emitMovfpRA(g, R_XMM7, &addr, dataSize);

      // gp
      addr.imm = -(gp_va_area - 0 * dataSize);
      emitMoveRA(g, R_ARG_0, &addr, dataSize);

      addr.imm = -(gp_va_area - 1 * dataSize);
      emitMoveRA(g, R_ARG_1, &addr, dataSize);

      addr.imm = -(gp_va_area - 2 * dataSize);
      emitMoveRA(g, R_ARG_2, &addr, dataSize);

      addr.imm = -(gp_va_area - 3 * dataSize);
      emitMoveRA(g, R_ARG_3, &addr, dataSize);

      addr.imm = -(gp_va_area - 4 * dataSize);
      emitMoveRA(g, R_ARG_4, &addr, dataSize);

      addr.imm = -(gp_va_area - 5 * dataSize);
      emitMoveRA(g, R_ARG_5, &addr, dataSize);


      addr.imm = -gp_va_area;
      emitLea(g, &addr, R_ACC);
      addr.imm = -reg_save_area_ptr_off;
      emitMoveRA(g, R_ACC, &addr, dataSize);

      emitMoveCR(g, intRegParams * dataSize, R_ACC, T_U4);
      addr.imm = -gp_offset_off;
      emitMoveRA(g, R_ACC, &addr, sizeof(uint32_t));

      emitMoveCR(g, gp_va_area - fp_va_area + fpRegParams * dataSize, R_ACC, T_U4);
      addr.imm = -fp_offset_off;
      emitMoveRA(g, R_ACC, &addr, sizeof(uint32_t));

      addr.imm = stackParamOffset;
      emitLea(g, &addr, R_ACC);
      addr.imm = -overflow_arg_area_off;
      emitMoveRA(g, R_ACC, &addr, dataSize);
  }

  g->savedRegOffset = -baseOffset;

  int32_t frameSize = ALIGN_SIZE(baseOffset, 2 * sizeof(intptr_t));
  g->frameSize = frameSize;

  return frameSize;
}

static GeneratedFunction *generateFunction_x86_64(GenerationContext *ctx, AstFunctionDefinition *f) {
  HashMap *labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
  ctx->labelMap = labelMap;

  assert(f->body->statementKind == SK_BLOCK);

  GeneratedFunction *gen = allocateGenFunction(ctx);

  gen->symbol = f->declaration->symbol;
  gen->name = f->declaration->name;

  pushFrame(gen);

  size_t frameSize = allocateLocalSlots(gen, f);

  size_t delta = frameSize;

  if (frameSize)
    emitArithConst(gen, OP_SUB, R_ESP, delta, T_S8);

  Address addr = { R_EBP, R_BAD, 0, gen->allocaOffset, NULL, NULL };
  emitMoveRA(gen, R_ESP, &addr, sizeof(intptr_t));

  gen->stackOffset = 0;
  Boolean lastIsRet = generateBlock(gen, &f->body->block);
  assert(gen->stackOffset == 0);

  if (!lastIsRet) {
    TypeRef * returnType = f->declaration->returnType;
    size_t returnTypeSize = computeTypeSize(returnType);
    if (isCompositeType(returnType) && returnTypeSize > sizeof(intptr_t)) {
        Address addr = { R_EBP, R_BAD, 0, gen->returnStructAddressOffset, NULL, NULL };
        emitMoveAR(gen, &addr, R_EAX, sizeof(intptr_t));
    }

    popFrame(gen);
    emitReturn(gen);
  }

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  ctx->labelMap = NULL;
  releaseHashMap(labelMap);

  if (ctx->parserContext->config->asmDump) {
    fprintf(stdout, "<<< %s >>>\n", f->declaration->name);
    address b = gen->section->start + gen->sectionOffset;
    address e = gen->section->pc;
    disassemble(stdout, b, e - b);

    fprintf(stdout, "<<< bytes >>>\n");

    while (b != e) {
        fprintf(stdout, "%.2x ", *b++);
    }

    fprintf(stdout, "\n<<<>>>\n");
  }

  return gen;
}


void initArchCodegen_x86_64(ArchCodegen *cg) {
  cg->generateFunction = &generateFunction_x86_64;
  cg->generateVaribale = &generateVaribale_x86_64;
}
