
#include <assert.h>

#include <udis86.h>
#include <alloca.h>

#include "_elf.h"
#include "codegen.h"
#include "mem.h"
#include "parser.h"
#include "sema.h"

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

struct CaseLabel {
  int64_t caseConst;
  struct Label label;
};

typedef struct _GenerationContext {
  ParserContext *parserContext;

  GeneratedFile *file;

  Arena *codegenArena;

  Relocation *relocations;

  HashMap *labelMap;

  struct Label *continueLabel;
  struct Label *breakLabel;

  struct Label *defaultLabel;

  unsigned caseCount;
  struct CaseLabel *caseLabels;


  Section *bss;
  Section *rodata;
  Section *data;
  Section *rodataLocal;
  Section *dataLocal;
  Section *text;

} GenerationContext;

static GeneratedFile *allocateGenFile(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (GeneratedFile));
}


static GeneratedFunction *allocateGenFunction(GenerationContext *ctx) {
  GeneratedFunction *f = areanAllocate(ctx->codegenArena, sizeof (GeneratedFunction));
  f->returnStructAddressOffset = -1;
  f->arena = ctx->codegenArena;
  f->section = ctx->text;
  f->sectionOffset = (ctx->text->pc - ctx->text->start);
  return f;
}

static GeneratedVariable *allocateGenVarialbe(GenerationContext *ctx, AstValueDeclaration *d) {
  GeneratedVariable *v = areanAllocate(ctx->codegenArena, sizeof (GeneratedVariable));
  v->name = d->name;
  v->symbol = d->symbol;
  d->gen = v;
  return v;
}

static Instruction *allocateInstruction(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (Instruction));
}

static Operand *allocateOperand(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (Operand));
}

static struct Label *allocateLabel(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (struct Label));
}

static Relocation *allocateRelocation(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (Relocation));
}

static int symbolHashCode(intptr_t s) {
  return (int)(uintptr_t)s;
}

static int symbolEquals(intptr_t s1, intptr_t s2) {
  return (int)(s2 - s1);
}


void emitPushRegF(GeneratedFunction *f, enum Registers r, Boolean isD) {
  int size = sizeof(double);
  Address addr = { 0 };
  addr.base = R_ESP;
  addr.index = R_BAD;
  addr.imm = -size;

  f->stackOffset += size;

  emitMovfpRA(f, r, &addr, size);
  emitArithConst(f, OP_SUB, R_ESP, size, sizeof (intptr_t));
}

void emitPopRegF(GeneratedFunction *f, enum Registers r, Boolean isD) {
  int size = sizeof(double);
  Address addr = { 0 };
  addr.base = R_ESP;
  addr.index = R_BAD;
  addr.imm = 0;

  f->stackOffset -= size;

  emitMovfpAR(f, &addr, r, size);
  emitArithConst(f, OP_ADD, R_ESP, size, sizeof (intptr_t));
}

void emitByte(GeneratedFunction *f, uint8_t b) {
  emitSectionByte(f->section, b);
}

void emitWord(GeneratedFunction *f, uint16_t w) {
   emitByte(f, (uint8_t)w);

   if ((uint16_t)(uint8_t)w != w) {
       emitByte(f, (uint8_t)(w >> 8));
   }
}


void emitDouble(GeneratedFunction *f, uint32_t w) {
    emitByte(f, (uint8_t)(w));
    emitByte(f, (uint8_t)(w >> 8));
    emitByte(f, (uint8_t)(w >> 16));
    emitByte(f, (uint8_t)(w >> 24));
}

void emitDisp32(GeneratedFunction *f, uint32_t w) {
  if ((uint32_t)(uint16_t)w != w) {
      emitWord(f, (uint16_t) w);

      uint16_t high = (uint16_t)(w >> 16);
      emitByte(f, (uint8_t)high);
      emitByte(f, (uint8_t)(high >> 8));
  } else {
      if ((uint32_t)(uint8_t)w != w) {
          emitWord(f, w);
      } else {
          emitByte(f, (uint8_t)w);
          emitByte(f, 0);
      }
      emitByte(f, 0);
      emitByte(f, 0);
  }
}

void emitQuad(GeneratedFunction *f, uint64_t w) {
  emitDouble(f, (uint32_t) w);

  if ((uint64_t)(uint32_t)w != w) {
      emitDouble(f, (uint32_t)(w >> 32));
  }
}

void emitQuadOrDouble(GeneratedFunction *f, uint64_t w) {
  if ((uint64_t)(uint32_t)w == w) {
    emitDisp32(f, w);
  } else {
    emitDouble(f, (uint32_t) w);
    emitDouble(f, (uint32_t)(w >> 32));
  }
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

static void emitInc(GeneratedFunction *f, TypeRef *type) {

}

static void emitDec(GeneratedFunction *f, TypeRef *type) {

}

typedef union {
  uint8_t bytes[sizeof(float)];
  float f;
} FloatBytes;

typedef union {
  uint8_t bytes[sizeof(double)];
  double d;
} DoubleBytes;

static void emitFloatConst(GenerationContext *ctx, GeneratedFunction *f, AstConst *_const, size_t size, Address *addr) {
  assert(_const->op == CK_FLOAT_CONST);
  ptrdiff_t offset = ctx->rodata->pc - ctx->rodata->start;
  ptrdiff_t alligned = ALIGN_SIZE(offset, size);

  while (offset < alligned) {
      emitSectionByte(ctx->rodata, 0x00);
      ++offset;
  }

  Relocation *reloc = allocateRelocation(ctx);
  reloc->kind = RK_RIP;
  reloc->applySection = f->section;
  reloc->sectionData.dataSection = ctx->rodata;
  reloc->sectionData.dataSectionOffset = ctx->rodata->pc - ctx->rodata->start;
  reloc->next = f->section->reloc;
  f->section->reloc = reloc;


  if (size == 4) {
      FloatBytes fb; fb.f = (float)_const->f;
      emitSectionByte(ctx->rodata, fb.bytes[0]);
      emitSectionByte(ctx->rodata, fb.bytes[1]);
      emitSectionByte(ctx->rodata, fb.bytes[2]);
      emitSectionByte(ctx->rodata, fb.bytes[3]);
  } else if (size == 8) {
      DoubleBytes db; db.d = _const->f;
      emitSectionByte(ctx->rodata, db.bytes[0]);
      emitSectionByte(ctx->rodata, db.bytes[1]);
      emitSectionByte(ctx->rodata, db.bytes[2]);
      emitSectionByte(ctx->rodata, db.bytes[3]);
      emitSectionByte(ctx->rodata, db.bytes[4]);
      emitSectionByte(ctx->rodata, db.bytes[5]);
      emitSectionByte(ctx->rodata, db.bytes[6]);
      emitSectionByte(ctx->rodata, db.bytes[7]);
   } else {
      unreachable("long double consts are not supported yet");
   }

  addr->base = R_RIP;
  addr->index = R_BAD;
  addr->reloc = reloc;
  addr->scale = addr->imm = 0;
}

static int parseIfHex(char c) {
  if (!c) return -1;

  if ('0' <= c && c <= '9') return c - '0';
  if ('a' <= c && c <= 'f') return c - 'a';
  if ('A' <= c && c <= 'F') return c - 'A';

  return -1;

}

static void emitStringWithEscaping(Section *section, const char *str) {

  unsigned i = 0;

  Boolean isHex = FALSE;
  while (str[i]) {
      int p = 0;
      char c = str[i++];
      if (c == '\\') {
        char c2 = str[i++];
        char toEmit = -1;
        if (!c2) {
            emitSectionByte(section, c);
            --i;
            continue;
        }
        char c3, c4;
        switch (c2) {
          case '0':
            toEmit = 0;
            c3 = str[i++];
            if (c3 && '0' <= c3 && c3 <= '7') {
              toEmit = c3 - '0';
              c4 = str[i++];
              if (c4 && '0' <= c4 && c4 <= '7') {
                  toEmit = toEmit * 8 + (c4 - '0');
              } else {
                  --i;
              }
            } else {
                --i;
            }
            break;
          case 'a':
            toEmit = '\a';
            break;
          case 'b':
            toEmit = '\b';
            break;
          case 'f':
            toEmit = '\f';
            break;
          case 'n':
            toEmit = '\n';
            break;
          case 'r':
            toEmit = '\r';
            break;
          case 't':
            toEmit = '\t';
            break;
          case 'v':
            toEmit = '\v';
            break;
          case '\'':
            toEmit = '\'';
            break;
          case '\"':
            toEmit = '\"';
            break;
          case '?':
            toEmit = '\?';
            break;
          case 'x':
            toEmit = 0;
            c3 = str[i++];
            p = parseIfHex(c3);
            if (p >= 0) {
              toEmit = p;
              c4 = str[i++];
              p = parseIfHex(c4);
              if (c4 >= 0) {
                  toEmit = toEmit * 16 + p;
              } else {
                  --i;
              }
            } else {
                --i;
            }
            break;
          case '\\':
          default:
            toEmit = '\\';
            break;
        }
        emitSectionByte(section, toEmit);
      } else {
        emitSectionByte(section, c);
      }
  }
  emitSectionByte(section, '\0');
}

static void emitConst(GenerationContext *ctx, GeneratedFunction *f, AstConst *_const, size_t size) {
  // movq #const, %rax
  int64_t c = 0;
  switch (_const->op) {
  case CK_FLOAT_CONST: {
        Address addr = { 0 };

        emitFloatConst(ctx, f, _const, size, &addr);
        emitMovfpAR(f, &addr, R_FACC, size);

        break;
      }
  case CK_INT_CONST:
      c = _const->i;
      emitMoveCR(f, c, R_ACC, size);
      break;
  case CK_STRING_LITERAL: {
        const char *l = _const->l;
        Section *rodata = ctx->rodata;
        ptrdiff_t offset = rodata->pc - rodata->start;

        Relocation *reloc = allocateRelocation(ctx);
        reloc->applySection = f->section;
        reloc->kind = RK_RIP;
        reloc->sectionData.dataSection= rodata;
        reloc->sectionData.dataSectionOffset = rodata->pc - rodata->start;
        reloc->next = f->section->reloc;
        f->section->reloc = reloc;

        emitStringWithEscaping(rodata, l);

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

static TypeId typeToId(TypeRef *type) {
  switch (type->kind) {
  case TR_VALUE: {
        TypeId tid = type->descriptorDesc->typeId;
        if (tid == T_ENUM) tid = T_S4;
        return tid;
  }
  case TR_POINTED:
  case TR_FUNCTION:
  case TR_ARRAY: return T_U8;
  case TR_BITFIELD: return type->bitFieldDesc.storageType->descriptorDesc->typeId;
  default: unreachable("Unknown type ref"); return T_ERROR;
    }
}

static void emitLoad(GeneratedFunction *f, Address *from, enum Registers to, TypeId typeId) {
  switch (typeId) {
  case T_S1: emitMovxxAR(f, 0xBE, from, to); break;
  case T_S2: emitMovxxAR(f, 0xBF, from, to); break;
  case T_S4: emitMoveAR(f, from, to, sizeof(int32_t)); break;
  case T_S8: emitMoveAR(f, from, to, sizeof(int64_t)); break;
  case T_U1: emitMovxxAR(f, 0xB6, from, to); break;
  case T_U2: emitMovxxAR(f, 0xB6, from, to); break;
  case T_U4: emitMoveAR(f, from, to, sizeof(uint32_t)); break;
  case T_U8: emitMoveAR(f, from, to, sizeof(uint64_t)); break;
  case T_F4: emitMovfpAR(f, from, to, sizeof(float)); break;
  case T_F8: emitMovfpAR(f, from, to, sizeof(double)); break;
  case T_F10: unreachable("long double is not supported yet");
  default: unreachable("Unknown memory slot type");
  }
}

static void emitStore(GeneratedFunction *f, enum Registers from, Address *to, TypeId typeId) {
  switch (typeId) {
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
  case T_F10: unreachable("long double is not supported yet");
  default: unreachable("Unknown memory slot type");
  }
}

static void generateExpression(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression);
static enum JumpCondition generateCondition(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *cond, Boolean invertion);
static void translateAddress(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression, Address *addr);
static void storeBitField(GeneratedFunction *f, TypeRef *t, enum Registers from, Address *addr);

static void copyStructTo(GeneratedFunction *f, TypeRef *type, Address *src, Address *dst) {

  assert(isStructualType(type) || isUnionType(type));

  int32_t align = type->descriptorDesc->structInfo->align;
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


static size_t emitInitializerImpl(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, int32_t typeSize, Address *dst, AstInitializer *initializer) {
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

      generateExpression(ctx, f, scope, initializer->expression);

      if ((offset + slotSize) <= typeSize) {
        if (isRealType(slotType)) {
            Boolean isD = slotSize > sizeof(float);
            emitMovfpRA(f, R_FACC, &addr, slotSize);
        } else if (isStructualType(slotType)) {
            Address src = { R_ACC, R_BAD, 0, 0 };
            copyStructTo(f, expr->type, &src, &addr);
        } else if (slotType->kind == TR_BITFIELD) {
          storeBitField(f, slotType, R_ACC, &addr);
          // TODO:
        } else {
          emitMoveRA(f, R_ACC, &addr, slotSize);
        }
      }
      return slotSize;
    }
    break;
    case IK_LIST: {
        AstInitializerList *inits = initializer->initializerList;
        while (inits) {
            size_t tmp = emitInitializerImpl(ctx, f, scope, typeSize, dst, inits->initializer);

            emitted += tmp;
            inits = inits->next;
        }
    }
    return emitted;

  default: unreachable("Unknown init kind");

  }

  return 0;
}

static void emitLocalInitializer(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, TypeRef* type, int32_t frameOffset, AstInitializer *initializer) {
  Address addr = { R_EBP, R_BAD, 0, frameOffset };

  size_t typeSize = computeTypeSize(type);
  size_t emitted = emitInitializerImpl(ctx, f, scope, typeSize, &addr, initializer);

  if (isStructualType(type) && emitted < typeSize) {
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

static Symbol *extractSymbol(AstExpression *expr) {
  switch (expr->op) {
  case E_NAMEREF: return expr->nameRefExpr.s;
  case E_CAST: return extractSymbol(expr->castExpr.argument);
  case E_PAREN: return extractSymbol(expr->parened);
  case EU_REF: return extractSymbol(expr->unaryExpr.argument);
  default: return NULL;
  }
}

static void collectRelocAndAdent(AstExpression *expr, Relocation *reloc) {
  switch (expr->op) {
  case E_CONST: reloc->addend = expr->constExpr.i; return;
  case E_CAST: return collectRelocAndAdent(expr->castExpr.argument, reloc);
  case E_PAREN: return collectRelocAndAdent(expr->parened, reloc);
  case EU_REF: return collectRelocAndAdent(expr->unaryExpr.argument, reloc);
  case E_NAMEREF:
      reloc->symbolData.symbol = expr->nameRefExpr.s;
      reloc->symbolData.symbolName = expr->nameRefExpr.name;
      return;
//  case EU_MINUS: return FALSE;
  case EB_ADD:
      collectRelocAndAdent(expr->binaryExpr.left, reloc);
      collectRelocAndAdent(expr->binaryExpr.right, reloc);
      return;
  default: unreachable("unexpected expression in const initializer");

  }
}

static size_t fillReference(GenerationContext *ctx, Section *section, AstExpression *expr, size_t size) {
  Relocation *reloc = allocateRelocation(ctx);

  ptrdiff_t sectionOffset = section->pc - section->start;

  reloc->kind = RK_SYMBOL;
  reloc->applySection = section;
  reloc->applySectionOffset = sectionOffset;
  reloc->addend = 0;
  reloc->next = section->reloc;
  section->reloc = reloc;

  collectRelocAndAdent(expr, reloc);

  unsigned idx = 0;

  int32_t typeSize = computeTypeSize(expr->type);

  for (; idx < typeSize; ++idx) {
      emitSectionByte(section, 0x00);
  }

  return sizeof(intptr_t);
}

static size_t fillInitializer(GenerationContext *ctx, Section *section, AstInitializer *init, size_t size) {

  if (size <= 0) return 0;

  if (init->kind == IK_EXPRESSION) {
      AstExpression *expr = init->expression;
      AstConst *cexpr = eval(ctx->parserContext, expr);
      if (cexpr == NULL) {
          // probably it's a refernce to symbol
          return fillReference(ctx, section, expr, size);
      }
      TypeRef *constType = expr->type;
      size_t constSize = computeTypeSize(constType);
      switch (expr->constExpr.op) {
      case CK_INT_CONST: {
          uint64_t lconst = cexpr->i;
          emitSectionByte(section, (uint8_t)(lconst));
          if (constSize > 1) {
              emitSectionByte(section, (uint8_t)(lconst >> 8));
          }
          if (constSize > 2) {
              emitSectionByte(section, (uint8_t)(lconst >> 16));
              emitSectionByte(section, (uint8_t)(lconst >> 24));
          }
          if (constSize > 4) {
              emitSectionByte(section, (uint8_t)(lconst >> 32));
              emitSectionByte(section, (uint8_t)(lconst >> 40));
              emitSectionByte(section, (uint8_t)(lconst >> 48));
              emitSectionByte(section, (uint8_t)(lconst >> 56));
          }
          break;
        }
        case CK_FLOAT_CONST: {
            switch (constSize) {
              case sizeof(float): {
                  FloatBytes fb; fb.f = (float)cexpr->f;
                  emitSectionByte(section, (uint8_t)(fb.bytes[0]));
                  emitSectionByte(section, (uint8_t)(fb.bytes[1]));
                  emitSectionByte(section, (uint8_t)(fb.bytes[2]));
                  emitSectionByte(section, (uint8_t)(fb.bytes[3]));
                }
                break;
              case sizeof(double): {
                  DoubleBytes db; db.d = cexpr->f;
                  emitSectionByte(section, (uint8_t)(db.bytes[0]));
                  emitSectionByte(section, (uint8_t)(db.bytes[1]));
                  emitSectionByte(section, (uint8_t)(db.bytes[2]));
                  emitSectionByte(section, (uint8_t)(db.bytes[3]));
                  emitSectionByte(section, (uint8_t)(db.bytes[4]));
                  emitSectionByte(section, (uint8_t)(db.bytes[5]));
                  emitSectionByte(section, (uint8_t)(db.bytes[6]));
                  emitSectionByte(section, (uint8_t)(db.bytes[7]));
                }
                break;
              case sizeof(long double):
                unreachable("long double is not supported yet");
              default:
                unreachable("Unknown float const");
              }
            break;
        }
      case CK_STRING_LITERAL: {
        Section *rodata = ctx->rodata;
        ptrdiff_t literalSectionOffset = rodata->pc - rodata->start;
        const char *literal = cexpr->l;

        emitStringWithEscaping(rodata, literal);

        Relocation *reloc = allocateRelocation(ctx);

        ptrdiff_t sectionOffset = section->pc - section->start;

        reloc->kind = RK_REF;
        reloc->applySection = section;
        reloc->applySectionOffset = sectionOffset;
        reloc->sectionData.dataSection = rodata;
        reloc->addend = literalSectionOffset;
        reloc->sectionData.dataSectionOffset = 0;
        reloc->next = section->reloc;
        section->reloc = reloc;

        unsigned idx = 0;

        for (; idx < sizeof(intptr_t); ++idx) {
            emitSectionByte(section, 0x00);
        }

        break;
      }
    }

    return constSize;
  } else {
    assert(init->kind == IK_LIST);
    size_t result = 0;

    AstInitializerList *inits = init->initializerList;

    while (inits) {
        size_t thisResult = fillInitializer(ctx, section, inits->initializer, size);
        size -= thisResult;
        result += thisResult;
        inits = inits->next;
    }

    return result;
  }
}

static Boolean hasRelocationsExpr(AstExpression *expr) {
  switch (expr->op) {
  case E_CONST: return expr->constExpr.op == CK_STRING_LITERAL ? TRUE : FALSE;
  case E_CAST: return hasRelocationsExpr(expr->castExpr.argument);
  case E_PAREN: return hasRelocationsExpr(expr->parened);
  case EU_REF: return hasRelocationsExpr(expr->unaryExpr.argument);
  case E_NAMEREF: return TRUE;
  case EU_MINUS: return FALSE;
  case EB_ADD: return hasRelocationsExpr(expr->binaryExpr.left) || hasRelocationsExpr(expr->binaryExpr.right);
  default: unreachable("unexpected expression in const initializer");

  }
}

static Boolean hasRelocationsInit(AstInitializer *init) {
  if (init->kind == IK_EXPRESSION) {
      return hasRelocationsExpr(init->expression);
  } else {
      AstInitializerList *inits = init->initializerList;

      while (inits) {
          if (hasRelocationsInit(inits->initializer)) return TRUE;
          inits = inits->next;
      }
  }
}


static GeneratedVariable *generateVaribale(GenerationContext *ctx, AstValueDeclaration *d) {
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

  size_t objectSize = computeTypeSize(d->type);
  size_t filled = 0;
  ptrdiff_t offset = section->pc - section->start;

  if (d->initializer) {
      filled = fillInitializer(ctx, section, d->initializer, objectSize);
  }

  while (filled < objectSize) {
      emitSectionByte(section, 0x00);
      ++filled;
  }

  GeneratedVariable *v = allocateGenVarialbe(ctx, d);
  v->section = section;
  v->sectionOffset = offset;
  v->size = objectSize;
  return v;
}

static void generateBitExtend(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *extend) {
  unsigned w = extend->extendExpr.w;
  Boolean isU = extend->extendExpr.isUnsigned;
  TypeId id = typeToId(extend->type);

  generateExpression(ctx, f, scope, extend->extendExpr.argument);

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

static void generateCast(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstCastExpression *cast) {
    TypeRef *fromType = cast->argument->type;
    TypeRef *toType = cast->type;

    TypeId fromTypeId = typeToId(fromType);
    TypeId toTypeId = typeToId(toType);

    generateExpression(ctx, f, scope, cast->argument);

    if (toTypeId == T_S1) {
        if (fromTypeId >= T_F4) {
            if (fromTypeId == T_F4) {
              emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax,xmm0
            } else if (fromTypeId == T_F8) {
              emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            } else {
              unreachable("long double conversions are not implemented yet");
            }
        }
        emitMovxxRR(f, 0xBE, R_EAX, R_EAX); // movsx
    } else if (toTypeId == T_U1) {
        if (fromTypeId >= T_F4) {
            if (fromTypeId == T_F4) {
              emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax,xmm0
            } else if (fromTypeId == T_F8) {
              emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            } else {
              unreachable("long double conversions are not implemented yet");
            }
        }
        emitMovxxRR(f, 0xB6, R_EAX, R_EAX); // movzx
    } else if (toTypeId == T_S2) {
        if (fromTypeId >= T_F4) {
            if (fromTypeId == T_F4) {
              emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax,xmm0
            } else if (fromTypeId == T_F8) {
              emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            } else {
              unreachable("long double conversions are not implemented yet");
            }
        }
        emitConvertWDQ(f, 0x98, 4); // cwde
    } else if (toTypeId == T_U2) {
        if (fromTypeId >= T_F4) {
            if (fromTypeId == T_F4) {
              emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax,xmm0
            } else if (fromTypeId == T_F8) {
              emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            } else {
              unreachable("long double conversions are not implemented yet");
            }
        }
        emitMovxxRR(f, 0xB7, R_EAX, R_EAX); // movzx
    } else if (toTypeId == T_S4 || toTypeId == T_U4) {
        switch (fromTypeId) {
        case T_F4: emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); break;// cvttss2si eax,xmm0
        case T_F8: emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); break; // cvttsd2si eax,xmm0
        case T_F10: unreachable("long double conversions are not implemented yet");
        default: break;
        }
    } else if (toTypeId == T_S8 || toTypeId == T_U8) {
        if (fromTypeId < T_U4) {
            emitConvertWDQ(f, 0x98, 8); // cdqe
        } else if (fromTypeId >= T_F4) {
            if (toTypeId == T_S8) {
                if (fromTypeId == T_F4) {
                  emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, TRUE); // cvttss2si eax,xmm0
                } else if (fromTypeId == T_F8) {
                  emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, TRUE); // cvttsd2si eax,xmm0
                } else {
                  unreachable("long double conversions are not implemented yet");
                }
            } else {
                unreachable("FP to unsigned long long conversions are not implemented yet");
            }
        }
    } else if (toTypeId < T_F10) {
        if (fromTypeId < T_F4) {
          if (fromTypeId == T_U8) {
              unreachable("unsigned long long to FP conversions are not implemented yet");
          } else {
              emitConvertFP(f, toTypeId == T_F4 ? 0xF3 : 0xF2, 0x2A, R_ACC, R_FACC, fromTypeId == T_S8);
          }
        } else {
            if (fromTypeId == T_F4) {
                assert(toTypeId == T_F8);
                emitConvertFP(f, 0xF3, 0x5A, R_FACC, R_FACC, FALSE); // cvtss2sd xmm0,xmm0
            } else if (fromTypeId == T_F8) {
                assert(toTypeId == T_F4);
                emitConvertFP(f, 0xF2, 0x5A, R_FACC, R_FACC, FALSE); // cvtsd2ss xmm0,xmm0
            } else {
              unreachable("long double to FP conversions are not implemented yet");
            }
        }

    } else {
        unreachable("long double conversions are not implemented yet");
    }
}

static void generateBinary(GenerationContext *ctx, GeneratedFunction *f, AstExpression *binOp, Scope *scope) {
  assert(isBinOp(binOp->op));
  AstExpression *left = binOp->binaryExpr.left;
  assert(left);

  size_t opSize = computeTypeSize(binOp->type);
  Boolean isFP = isRealType(left->type);
  Boolean isD = isFP && opSize > 4;

  // TODO: optimize memory operations
  generateExpression(ctx, f, scope, left);

  AstExpression *right = binOp->binaryExpr.right;
  assert(right);
  assert(isFP == isRealType(right->type));

  enum Opcodes opcode = selectOpcode(binOp->op, binOp->type);

  if (right->op == E_CONST) {
      if (isFP) {
        Address addr = { 0 };
        emitFloatConst(ctx, f, &right->constExpr, opSize, &addr);
        emitArithAR(f, opcode, R_FACC, &addr, opSize);
      } else {
        uint64_t cnst = right->constExpr.i;
        emitArithConst(f, opcode, R_ACC, cnst, opSize);
      }
  } else {
    if (isFP) {
      emitPushRegF(f, R_FACC, isD);
    } else {
      emitPushReg(f, R_ACC); // save result
    }

    if (right->op == EU_DEREF) {
      Address addr = { 0 };
      translateAddress(ctx, f, scope, right->unaryExpr.argument, &addr);
      if (isFP) {
        emitPopRegF(f, R_FACC, isD);
      } else {
        emitPopReg(f, R_ACC); // save result
      }

      emitArithAR(f, opcode, isFP ? R_FACC : R_ACC, &addr, opSize);
    } else {
      generateExpression(ctx, f, scope, right);

      if (isFP) {
        emitMovfpRR(f, R_FACC, R_FTMP, isD);
        emitPopRegF(f, R_FACC, isD);
        emitArithRR(f, opcode, R_FACC, R_FTMP, opSize);
      } else {
        emitMoveRR(f, R_ACC, R_TMP, opSize);
        emitPopReg(f, R_ACC);
        emitArithRR(f, opcode, R_ACC, R_TMP, opSize);
      }
    }
  }
}

static void generateDiv(GenerationContext *ctx, GeneratedFunction *f, AstExpression *binOp, Scope *scope) {
  TypeRef *type = binOp->type;
  if (isRealType(type)) return generateBinary(ctx, f, binOp, scope);

  Boolean isMod = binOp->op == EB_MOD;
  size_t opSize = computeTypeSize(type);

  Boolean isU = isUnsignedType(type);
  AstExpression *left = binOp->binaryExpr.left;
  Boolean isLU = isUnsignedType(left->type);

  generateExpression(ctx, f, scope, left);

  emitPushReg(f, R_ACC);

  AstExpression *right = binOp->binaryExpr.right;
  Boolean isRU = isUnsignedType(right->type);

  enum Opcodes opcode;
  if (right->op == EU_DEREF) {
      Address addr = { 0 };
      translateAddress(ctx, f, scope, right->unaryExpr.argument, &addr);
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
      generateExpression(ctx, f, scope, right);
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

static void generateLogicalBinary(GenerationContext *ctx, GeneratedFunction *f, AstExpression *binOp, Scope *scope) {
  assert(binOp->op == EB_ANDAND || binOp->op == EB_OROR);

  enum JumpCondition cc = generateCondition(ctx, f, scope, binOp->binaryExpr.left, binOp->op == EB_ANDAND);

  struct Label elseB = { 0 }, endB = { 0 };

  emitCondJump(f, &elseB, cc, FALSE);

  cc = generateCondition(ctx, f, scope, binOp->binaryExpr.right, FALSE);

  emitSetccR(f, cc, R_ACC);
  emitMovxxRR(f, 0xB6, R_ACC, R_ACC);

  emitJumpTo(f, &endB);
  bindLabel(f, &elseB);
  emitMoveCR(f, binOp->op == EB_ANDAND ? 0 : 1, R_ACC, sizeof(int32_t));
  bindLabel(f, &endB);
}

static void loadBitField(GeneratedFunction *f, TypeRef *t, Address *addr, enum Registers to) {
  assert(t->kind == TR_BITFIELD);

  int s = t->bitFieldDesc.offset;
  int w = t->bitFieldDesc.width;

  TypeRef *storageType = t->bitFieldDesc.storageType;
  size_t size = computeTypeSize(storageType);
  size_t W = size * 8;

  emitLoad(f, addr, to, typeToId(storageType));

  uint64_t l = W - (w + s);
  uint64_t r = W - w;

  Boolean isU = isUnsignedType(storageType);

  emitArithConst(f, OP_SHL, to, l, size);
  emitArithConst(f, isU ? OP_SHR : OP_SAR, to, r, size);

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


  emitArithConst(f, OP_AND, from, ~(~0LLu << w), size);
  if (s != 0) {
      emitArithConst(f, OP_SHL, from, s, size);
  }

  uint64_t mask = ~(~(~0LLu << w) << s);
  emitArithConst(f, OP_AND, R_TMP, mask, size);

  emitArithRR(f, OP_OR, R_TMP, from, size);

  emitStore(f, R_TMP, addr, storageTypeId);

  if (s) {
      emitArithConst(f, OP_SHR, from, s, size);
  }
}

static void localVarAddress(GenerationContext *ctx, const Symbol *s, Address *addr) {
  addr->base = R_EBP;
  addr->index = R_BAD;
  addr->imm = s->variableDesc->gen->baseOffset;
}

static void translateAddress(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression, Address *addr) {

  if (expression->op == E_NAMEREF) {
    Symbol *s = expression->nameRefExpr.s;
    if (s->kind == ValueSymbol && s->variableDesc->flags.bits.isLocal) {
      localVarAddress(ctx, s, addr);
    } else {
      Relocation *reloc = allocateRelocation(ctx);
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
      generateExpression(ctx, f, scope, l);
      emitPushReg(f, R_ACC);
      generateExpression(ctx, f, scope, r);
      emitPopReg(f, R_EDI);
      addr->base = R_EDI;
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else if (r->op == EB_ADD) {
      // [base + (index * scale + imm)]
      generateExpression(ctx, f, scope, l);
      emitPushReg(f, R_ACC);
      generateExpression(ctx, f, scope, r);
      emitPopReg(f, R_EDI);
      addr->base = R_EDI;
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else {
        if (r->op == E_CONST) {
            // [expr + imm]
            generateExpression(ctx, f, scope, l);
            emitMoveRR(f, R_ACC, R_EDI, sizeof(intptr_t));
            addr->base = R_EDI;
            addr->index = R_BAD;
            addr->imm += r->constExpr.i;
        } else if (r->op == EB_MUL || l->op == EB_MUL) {
            // [expr + index * scale] || [index * scale + expr]
            AstExpression *idxs = r->op == EB_MUL ? r : l;
            AstExpression *expr = idxs == r ? l : r;
            generateExpression(ctx, f, scope, expr);
            emitPushReg(f, R_ACC);
            AstExpression *ml = idxs->binaryExpr.left;
            AstExpression *mr = idxs->binaryExpr.right;
            if (mr->op == E_CONST) {
                int64_t d = mr->constExpr.i;
                if (d == 0) {
                    addr->scale = 0;
                    addr->index = R_BAD;
                } else if (d == 1 || d == 2 || d == 4 || d == 8) {
                  generateExpression(ctx, f, scope, ml);
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
                  generateExpression(ctx, f, scope, idxs);
                  emitPopReg(f, R_EDI);
                  addr->base = R_EDI;
                  addr->scale = 0;
                  addr->index = R_ACC;
                }
            } else {
                generateExpression(ctx, f, scope, idxs);
                emitPopReg(f, R_EDI);
                addr->base = R_EDI;
                addr->scale = 0;
                addr->index = R_ACC;
            }
        } else {
            // [expr1 + expr2]
            generateExpression(ctx, f, scope, l);
            emitPushReg(f, R_ACC);
            generateExpression(ctx, f, scope, r);
            emitPopReg(f, R_EDI);
            addr->base = R_EDI;
            addr->index = R_ACC;
        }
    }

  } else {
    generateExpression(ctx, f, scope, expression);
    addr->base = R_ACC;
    addr->index = R_BAD;
  }
}

static void generateAssign(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  AstExpression *lvalue = expression->binaryExpr.left;
  AstExpression *rvalue = expression->binaryExpr.right;
  ExpressionType op = expression->op;

  generateExpression(ctx, f, scope, rvalue);

  TypeRef *lType = lvalue->type;
  TypeRef *rType = rvalue->type;
  Address addr = { 0 };

  AstExpression *addrExpr = lvalue;
  Boolean saved_acc = FALSE;
  TypeId lTypeId = typeToId(lType);
  TypeId rTypeId = typeToId(rType);

  Boolean isFP = lTypeId == T_F4 || lTypeId == T_F8;
  Boolean isD = lTypeId == T_F8;

  size_t typeSize = computeTypeSize(lType);

  size_t stackPending = 0;

  if (rvalue->op == E_CALL && isStructualType(rType)) {
      stackPending = ALIGN_SIZE(typeSize, 2 * sizeof(intptr_t));
      emitArithConst(f, OP_SUB, R_ESP, stackPending, sizeof(intptr_t));
  }


  if (!((addrExpr->op == E_NAMEREF || addrExpr->op == E_CONST) && op == EB_ASSIGN)) {
    if (isFP) {
      emitPushRegF(f, R_FACC, isD);
    } else {
      emitPushReg(f, R_ACC); // save result
    }
    saved_acc = TRUE;
  }

  if (addrExpr->op == EU_DEREF) {
    translateAddress(ctx, f, scope, addrExpr->unaryExpr.argument, &addr);
  } else {
    translateAddress(ctx, f, scope, addrExpr, &addr);
  }

  if (op == EB_ASSIGN) {
      // a = b

      if (lType->kind == TR_BITFIELD) {
          assert(stackPending == 0);
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
          if (isStructualType(lType) || isUnionType(lType)) {
            emitPopReg(f, R_TMP2); // load result
            if (stackPending) {
                emitArithConst(f, OP_ADD, R_ESP, stackPending, sizeof(intptr_t));
            }

            Address src = { 0 };
            src.base = R_TMP2;
            src.index = R_BAD;

            copyStructTo(f, lType, &src, &addr);
          } else {
            if (isFP) {
                emitPopRegF(f, R_FACC, isD);
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

                emitPopReg(f, resultReg);

                emitStore(f, isFP ? R_FACC : resultReg, &addr, rTypeId);

                if (resultReg != R_ACC) {
                    emitMoveRR(f, R_TMP2, resultReg, max(4, typeSize));
                }
            }
          }
      }
  } else {
    assert(stackPending == 0);
    if (lType->kind == TR_BITFIELD) {
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
            emitPopRegF(f, R_FTMP, isD);
            emitArithRR(f, selectAssignOpcode(op, lType), R_FACC, R_FTMP, typeSize);
            emitStore(f, R_FACC, &addr, rTypeId);
        } else {
            if (addr.reloc) {
                emitLea(f, &addr, R_EDI);
                addr.base = R_EDI; addr.reloc = NULL;
            }
            emitLoad(f, &addr, R_TMP, lTypeId);
            emitPopReg(f, R_TMP2);
            emitArithRR(f, selectAssignOpcode(op, lType), R_TMP, R_TMP2, typeSize);
            emitStore(f, R_TMP, &addr, rTypeId);
            emitMoveRR(f, R_TMP, R_ACC, typeSize);
        }
    }
  }
}


static void generateAssignDiv(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  assert(expression->op == EB_ASG_DIV || EB_ASG_MOD);

  TypeRef *type = expression->type;

  if (isRealType(type)) return generateAssign(ctx, f, scope, expression);

  AstExpression *lvalue = expression->binaryExpr.left;
  AstExpression *rvalue = expression->binaryExpr.right;

  generateExpression(ctx, f, scope, rvalue);

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

  if (rvalue->op == EU_DEREF) {
      translateAddress(ctx, f, scope, rvalue->unaryExpr.argument, &addr);
  } else {
      // TODO: probably it's illegal
      translateAddress(ctx, f, scope, rvalue, &addr);
  }

  enum Opcodes opcode;
  if (lType->kind == TR_BITFIELD) {
      TypeRef *storageType = lType->bitFieldDesc.storageType;

      // TODO: lea relocated addr
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

      // TODO: lea relocated addr
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


static Boolean generateAlloca(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  AstExpression *callee = expression->callExpr.callee;

  if (callee->op != E_NAMEREF) return FALSE;
  if (strcmp("alloca", callee->nameRefExpr.name)) return FALSE;

  const int32_t dataSize = sizeof(intptr_t);
  // alloca algo consts of three steps:
  // 1. move existing stack down on allocating delta
  // 2. set new rsp
  // 3. save new alloca stack border which is also a result of alloca

  // callee-saved regs: R_EBX, R_R12, R_R13, R_R14, R_R15

  AstExpressionList *args = expression->callExpr.arguments;
  assert(args->next == NULL);

  generateExpression(ctx, f, scope, args->expression);

  int32_t alignment = 2 * dataSize;

  // size is in R_ACC
  emitArithConst(f, OP_ADD, R_ACC, alignment - 1, dataSize);
  emitArithConst(f, OP_AND, R_ACC, ~(alignment - 1), dataSize);

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

  emitArithConst(f, OP_ADD, to, dataSize, dataSize);
  emitArithConst(f, OP_ADD, from, dataSize, dataSize);

  emitJumpTo(f, &head);

  bindLabel(f, &tail);

  emitArithRR(f, OP_SUB, R_ESP, delta, dataSize);
  emitNegR(f, delta, dataSize);
  emitArithRR(f, OP_ADD, delta, r_sab, dataSize);
  emitMoveRA(f, R_ACC, &sabAddress, dataSize);
}

static const enum Registers intArgumentRegs[] = { R_ARG_0, R_ARG_1, R_ARG_2, R_ARG_3, R_ARG_4, R_ARG_5 };
static const enum Registers fpArgumentRegs[] = { R_XMM0, R_XMM1, R_XMM2, R_XMM3, R_XMM4, R_XMM5, R_XMM6, R_XMM7 };

static void generateCall(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  assert(expression->op == E_CALL);

  if (generateAlloca(ctx, f, scope, expression)) return;

  AstExpression *callee = expression->callExpr.callee;
  AstExpressionList *args = expression->callExpr.arguments;
  TypeRef *type = expression->type;
  TypeRef *pcalleeType = callee->type;
  TypeRef *calleeType = pcalleeType->kind == TR_POINTED ? pcalleeType->pointedTo.toType : pcalleeType;
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
  unsigned stackArgSize = 0;

  if (isStructualType(returnType) && returnTypeSize > sizeof(intptr_t)) {
      stackArgSize = returnTypeSize;
      intRegArgs = firstIntRegArg = 1;
  }

  unsigned count = 0;

  for (tmp = args; tmp; tmp = tmp->next) {
      args = tmp;

      TypeRef *t = tmp->expression->type;

      unsigned argSize = max(4, computeTypeSize(t));

      ++count;

      if (isRealType(t)) {
        if (fpRegArgs < R_FP_PARAM_COUNT) {
          ++fpRegArgs;
          lastFpRegArg = count;
          continue;
        }
      } else if (!isStructualType(t) || argSize <= sizeof(intptr_t)) {
        if (intRegArgs < R_PARAM_COUNT) {
          ++intRegArgs;
          lastIntRegArg = count;
          continue;
        }
      }

      stackArgSize = ALIGN_SIZE(stackArgSize, argSize);

      offsets[count] = stackArgSize;

      stackArgSize += argSize;
  }

  unsigned alignedStackSize = ALIGN_SIZE(stackArgSize, 2 * sizeof(intptr_t));
  unsigned delta = alignedStackSize - stackArgSize;

  unsigned totalRegArg = intRegArgs + fpRegArgs;

  if (alignedStackSize)
    emitArithConst(f, OP_SUB, R_ESP, alignedStackSize, sizeof(intptr_t));

  if (isStructualType(returnType) && returnTypeSize > sizeof(intptr_t)) {
      offset = ALIGN_SIZE(returnTypeSize, sizeof(intptr_t));
      r_offsets[idx] = offset;
      r_types[idx] = returnType;
      ++idx;
  }

  int32_t stackBase = f->stackOffset;

  while (args) {
    AstExpression *arg = args->expression;
    TypeRef *argType = arg->type;

    unsigned typeSize = max(4, computeTypeSize(argType));
    int32_t rspOffset = offsets[count];

    Address dst = { R_ESP, R_BAD, 0, rspOffset, NULL, NULL };

    if (isStructualType(argType) && typeSize > sizeof(intptr_t)) {
      Address addr = { 0 };
      translateAddress(ctx, f, scope, arg->op == EU_DEREF ? arg->unaryExpr.argument : arg, &addr);
      dst.imm = rspOffset + (f->stackOffset - stackBase);
      copyStructTo(f, argType, &addr, &dst);
    } else {
      generateExpression(ctx, f, scope, arg);
      dst.imm = rspOffset + (f->stackOffset - stackBase);

      if (isRealType(argType)) {
        if (count <= lastFpRegArg) {
            r_offsets[totalRegArg - idx - 1] = rspOffset;
            r_types[totalRegArg - idx - 1] = argType;
            ++idx;
            emitPushRegF(f, R_FACC, TRUE);
        } else {
            emitMovfpRA(f, R_FACC, &dst, typeSize);
        }
      } else {
        if (count <= lastIntRegArg) {
            r_offsets[totalRegArg - idx - 1] = rspOffset;
            r_types[totalRegArg - idx - 1] = argType;
            ++idx;
            if (isStructualType(argType)) {
                Address addr = { R_ACC, R_BAD, 0, 0, NULL, NULL };
                emitLoad(f, &addr, R_ACC, typeSize);
            }
            emitPushReg(f, R_ACC);
        } else {
            emitMoveRA(f, R_ACC, &dst, typeSize);
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
      if (pcalleeType->kind == TR_FUNCTION && callee->op == EU_DEREF) {
        generateExpression(ctx, f, scope, callee->unaryExpr.argument);
      } else {
        generateExpression(ctx, f, scope, callee);
      }
      emitMoveRR(f, R_ACC, R_R10, sizeof(intptr_t));
  }

  unsigned ir = 0, fr = 0;

  for (i = 0; i < totalRegArg; ++i) {
      TypeRef *argType = r_types[i];
      size_t argSize = computeTypeSize(argType);
      if (isStructualType(argType) && argSize > sizeof(intptr_t)) {
          saddr.imm = r_offsets[i] + f->stackOffset;
          emitLea(f, &saddr, intArgumentRegs[ir++]);
      } else {
          if (isRealType(argType)) {
            enum Registers fpArg = fpArgumentRegs[fr++];
            emitPopRegF(f, fpArg, TRUE);
          } else {
            emitPopReg(f, intArgumentRegs[ir++]);
          }
      }
  }

  Boolean retFP = isRealType(returnType);

  if (fpRegArgs) {
    emitMoveCR(f, fpRegArgs, R_ACC, sizeof(int32_t));
  } else {
    emitArithRR(f, OP_XOR, R_ACC, R_ACC, sizeof(int64_t));
  }

  if (callee->op == E_NAMEREF) {
    Symbol *s = callee->nameRefExpr.s;
    Relocation *newReloc = allocateRelocation(ctx);
    newReloc->applySection = f->section;
    newReloc->symbolData.symbolName = s->name;
    newReloc->symbolData.symbol = s;
    newReloc->kind = RK_SYMBOL;
    newReloc->next = f->section->reloc;
    f->section->reloc = newReloc;

    emitCallLiteral(f, newReloc);
  } else {
    emitCall(f, R_R10);
  }

  if (isStructualType(returnType) && returnTypeSize <= sizeof(intptr_t)) {
      Address addr = { R_EBP, R_BAD, 0, f->smallStructSlotOffset, NULL, NULL };
      emitMoveRA(f, R_ACC, &addr, sizeof(intptr_t));
      emitLea(f, &addr, R_ACC);
  } else if (isIntegerType(returnType)) {
      TypeId tid = typeToId(returnType);
      switch (tid) {
      case T_S1: emitMovxxRR(f, 0xBE, R_ACC, R_ACC);
      case T_U1: emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
      case T_S2: emitMovxxRR(f, 0xBF, R_ACC, R_ACC);
      case T_U2: emitMovxxRR(f, 0xB7, R_ACC, R_ACC);
      }
  }

  if (alignedStackSize) {
    emitArithConst(f, OP_ADD, R_ESP, alignedStackSize, sizeof(intptr_t));
  }
}

static void generateVaArg(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  generateExpression(ctx, f, scope, expression->vaArg.va_list);

  TypeRef *vatype = expression->vaArg.argType;
  struct Label memLabl = { 0 }, doneLabl = { 0 };

  emitMoveRR(f, R_ACC, R_EDI, sizeof(intptr_t));

  TypeRef *valistType = expression->vaArg.va_list->type;
  Address valist_addr = { R_EDI, R_BAD, 0, 0, NULL, NULL };
  assert(is_va_list_Type(valistType));
  AstSUEDeclaration *vastruct = valistType->pointedTo.toType->descriptorDesc->structInfo;
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
      emitArithConst(f, OP_CMP, R_TMP, dataSize * (R_PARAM_COUNT + R_FP_PARAM_COUNT), sizeof(uint32_t));
      // if ( >= ) mem-load
      emitCondJump(f, &memLabl, JC_GE, TRUE);
      valist_addr.imm = memberOffset(vastruct, "reg_save_area");
      emitLoad(f, &valist_addr, R_ACC, T_U8);
      emitArithRR(f, OP_ADD, R_ACC, R_TMP, dataSize);
      emitArithConst(f, OP_ADD, R_TMP, dataSize, sizeof(uint32_t));
      valist_addr.imm = fp_offset_off;
      emitStore(f, R_TMP, &valist_addr, T_U4);
      emitJumpTo(f, &doneLabl);
  } else if (isScalarType(vatype)) {
      int32_t gp_offset_off = memberOffset(vastruct, "gp_offset");
      valist_addr.imm = gp_offset_off;
      // R_ACC = va_list->fp_offset
      emitLoad(f, &valist_addr, R_TMP, T_U4);

      // va_list->gp_offset >= R_PARAM_COUNT
      emitArithConst(f, OP_CMP, R_TMP, dataSize * R_PARAM_COUNT, sizeof(uint32_t));
      // if ( >= ) mem-load
      emitCondJump(f, &memLabl, JC_GE, TRUE);

      valist_addr.imm = memberOffset(vastruct, "reg_save_area");
      emitLoad(f, &valist_addr, R_ACC, T_U8);

      emitArithRR(f, OP_ADD, R_ACC, R_TMP, dataSize);
      emitArithConst(f, OP_ADD, R_TMP, dataSize, sizeof(uint32_t));
      valist_addr.imm = gp_offset_off;
      emitStore(f, R_TMP, &valist_addr, T_U4);
      emitJumpTo(f, &doneLabl);
  }

  bindLabel(f, &memLabl);

  int32_t ofa_area_off = memberOffset(vastruct, "overflow_arg_area");
  valist_addr.imm = ofa_area_off;
  emitLoad(f, &valist_addr, R_ACC, T_U8);
  int32_t align = typeAlignment(vatype);

  if (align > 8) {
    // (((len)+(align - 1)) & ~((align)-1))
    int32_t mask = ~(align - 1);
    emitArithConst(f, OP_ADD, R_ACC, align - 1, dataSize);
    emitArithConst(f, OP_AND, R_ACC, mask, dataSize);
  }

  int32_t argSize = max(8, computeTypeSize(vatype));

  emitMoveCR(f, ALIGN_SIZE(argSize, dataSize), R_TMP, dataSize);
  emitArithRR(f, OP_ADD, R_TMP, R_ACC, dataSize);
  emitStore(f, R_TMP, &valist_addr, T_U8);

  bindLabel(f, &doneLabl);
}


// result is in accamulator
static void generateExpression(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  Address addr = { 0 };
  size_t typeSize = computeTypeSize(expression->type);
  switch (expression->op) {
    case E_PAREN:
      generateExpression(ctx, f, scope, expression->parened);
      break;
    case E_CONST:
      emitConst(ctx, f, &expression->constExpr, typeSize);
      break;
    case E_VA_ARG:
      generateVaArg(ctx, f, scope, expression);
      break;
    case E_NAMEREF:
      translateAddress(ctx, f, scope, expression, &addr);
      emitLea(f, &addr, R_ACC);
      break;
    case E_CALL:
      generateCall(ctx, f, scope, expression);
      break;
    case E_TERNARY: {
        AstTernaryExpression *ternary = &expression->ternaryExpr;
        struct Label elseLabel = { 0 }, endLabel = { 0 };
        assert(ternary->condition);
        enum JumpCondition cc = generateCondition(ctx, f, scope, ternary->condition, TRUE);

        emitCondJump(f, &elseLabel, cc, FALSE);

        assert(ternary->ifTrue);
        generateExpression(ctx, f, scope, ternary->ifTrue);
        emitJumpTo(f, &endLabel);

        assert(ternary->ifFalse);
        bindLabel(f, &elseLabel);
        generateExpression(ctx, f, scope, ternary->ifFalse);

        bindLabel(f, &endLabel);
      }
      break;
    case E_BIT_EXTEND:
      generateBitExtend(ctx, f, scope, expression);
      break;
    case E_CAST:
      generateCast(ctx, f, scope, &expression->castExpr);
      break;
    case EB_ADD:
    case EB_SUB:
    case EB_LHS: /** << */
    case EB_RHS: /** >> */
    case EB_AND:
    case EB_OR:
    case EB_XOR:
    case EB_MUL:
      generateBinary(ctx, f, expression, scope);
      break;
    case EB_DIV:
    case EB_MOD:
      generateDiv(ctx, f, expression, scope);
      break;
    case EB_ANDAND:
    case EB_OROR:
      generateLogicalBinary(ctx, f, expression, scope);
      break;
    case EB_EQ:
    case EB_NE:
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE: {
        enum JumpCondition cc = generateCondition(ctx, f, scope, expression, FALSE);
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
      generateAssign(ctx, f, scope, expression);
      // TODO:
      break;
    case EB_ASG_DIV:
    case EB_ASG_MOD:
      generateAssignDiv(ctx, f, scope, expression);
      break;
    case EB_COMMA:
      generateExpression(ctx, f, scope, expression->binaryExpr.left);
      generateExpression(ctx, f, scope, expression->binaryExpr.right);
      break;
    case EU_REF:
      translateAddress(ctx, f, scope, expression->unaryExpr.argument, &addr);
      emitLea(f, &addr, R_ACC);
      break;
    case EU_DEREF:
      translateAddress(ctx, f, scope, expression->unaryExpr.argument, &addr);
      if (isStructualType(expression->type) || isUnionType(expression->type) || expression->type->kind == TR_ARRAY) {
        if (!(addr.base == R_ACC && addr.index == R_BAD && addr.imm == 0)) {
          emitLea(f, &addr, R_ACC);
        }
      } else {
        emitLoad(f, &addr, R_ACC, typeToId(expression->type));
      }
      break;
    case EU_PLUS:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      break;
    case EU_MINUS:
      // TODO: fp
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      if (isRealType(expression->type)) {
          switch (expression->type->descriptorDesc->typeId) {
           case T_F4:
              emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, FALSE);
              emitArithConst(f, OP_XOR, R_ACC, 1U << 31, typeSize);
              emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, FALSE);
              break;
           case T_F8:
              emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, TRUE);
              emitMoveCR(f, 1ULL << 63, R_TMP, typeSize);
              emitArithRR(f, OP_XOR, R_ACC, R_TMP, typeSize);
              emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, TRUE);
              break;
           case T_F10:
              unreachable("long double arith is not implemented yet");
           default:
              unreachable("unexpected FP type");
            }
      } else {
          emitNegR(f, R_ACC, typeSize);
      }
      break;
    case EU_TILDA:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      emitBitwiseNotR(f, R_ACC, computeTypeSize(expression->type));
      break;
    case EU_EXL:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      emitNot(f, R_ACC, computeTypeSize(expression->unaryExpr.argument->type));
      break;

    case E_LABEL_REF: {
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



static int generateStatement(GenerationContext *ctx, GeneratedFunction *f, AstStatement *stmt, Scope *scope, size_t frameOffset);
static int generateBlock(GenerationContext *ctx, GeneratedFunction *f, AstBlock *block, size_t frameOffset);


static size_t generateLabel(GenerationContext *ctx, GeneratedFunction *f, AstLabelStatement *label, Scope *scope, size_t frameOffset) {
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

  return generateStatement(ctx, f, label->body, scope, frameOffset);
}



static void emitReturn(GenerationContext *ctx, GeneratedFunction *f) {
  // movq $result, %rax
  // ret
  emitRet(f, 0);
}

static void moveTosToRet(GenerationContext *ctx, GeneratedFunction *f, size_t size) {

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
        visited += walkCaseLabels(body->ifStmt.thenBranch, caseLabels, idx);
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

static int generateSwitchStatement(GenerationContext *ctx, GeneratedFunction *f, AstSwitchStatement *stmt, Scope *scope, size_t frameOffset) {
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
  generateExpression(ctx, f, scope, condition);

  size_t condTypeSize = computeTypeSize(condition->type);

  int i;

  for (i = 0; i < visited; ++i) {
      int64_t caseConst = caseLabels[i].caseConst;
      struct Label *caseLabel = &caseLabels[i].label;
      emitArithConst(f, OP_CMP, R_ACC, caseConst, condTypeSize);
      emitCondJump(f, caseLabel, JC_EQ, FALSE);
  }

  if (stmt->hasDefault) {
      emitJumpTo(f, &defaultLabel);
  } else {
      emitJumpTo(f, &switchBreak);
  }

  generateStatement(ctx, f, stmt->body, scope, frameOffset);

  bindLabel(f, &switchBreak);

  ctx->caseCount = oldCaseCount;
  ctx->caseLabels = oldCaseLabels;
  ctx->defaultLabel = oldDefaultLabel;
  ctx->breakLabel = oldBreak;

  return 0;
}

static enum JumpCondition generateCondition(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *cond, Boolean invertion) {

  enum JumpCondition cc = JC_BAD;

  switch (cond->op) {
    case EB_GE: cc = invertion ? JC_NOT_GE : JC_GE; goto calc;
    case EB_LE: cc = invertion ? JC_NOT_LE : JC_LE; goto calc;
    case EB_LT: cc = invertion ? JC_NOT_L : JC_L;  goto calc;
    case EB_GT: cc = invertion ? JC_NOT_G : JC_G;  goto calc;
    case EB_NE: cc = invertion ? JC_EQ : JC_NE; goto calc;
    case EB_EQ: cc = invertion ? JC_NE : JC_EQ; goto calc;
    calc: {
      AstExpression *left = cond->binaryExpr.left;
      AstExpression *right = cond->binaryExpr.right;
      Boolean isFP = isRealType(left->type);
      size_t opSize = computeTypeSize(left->type);
      Boolean isD = isFP && opSize > 4;

      generateExpression(ctx, f, scope, left);

      TypeRef *lType = left->type;
      if (lType->kind == TR_BITFIELD) lType = lType->bitFieldDesc.storageType;

      Address addr = { 0 };
      if (isFP) {
          emitPushRegF(f, R_FACC, isD);
          if (cond->op == EB_EQ || cond->op == EB_NE) {

              enum JumpCondition setcc = cond->op == EB_EQ ? JC_NOT_PARITY : JC_PARITY;

              if (right->op == EU_DEREF) {
                  translateAddress(ctx, f, scope, right->unaryExpr.argument, &addr);
                  emitPopRegF(f, R_FTMP2, isD);
                  emitArithAR(f, OP_FUCMP, R_FTMP2, &addr, opSize);
                  emitSetccR(f, setcc, R_ACC);
                  emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
                  emitArithAR(f, OP_FUCMP, R_FTMP2, &addr, opSize);
              } else {
                  generateExpression(ctx, f, scope, right);
                  emitPopRegF(f, R_FTMP2, isD);
                  emitArithRR(f, OP_FUCMP, R_FTMP2, R_FACC, opSize);
                  emitSetccR(f, setcc, R_ACC);
                  emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
                  emitArithRR(f, OP_FUCMP, R_FTMP2, R_FACC, opSize);
              }

              struct Label l = { 0 };
              emitCondJump(f, &l, JC_EQ, TRUE);
              if (cond->op == EB_EQ) {
                  emitArithRR(f, OP_XOR, R_ACC, R_ACC, sizeof(int32_t));
              } else {
                  emitMoveCR(f, 1, R_ACC, sizeof(int32_t));
              }
              bindLabel(f, &l);

              emitTestRR(f, R_ACC, R_ACC, sizeof (int32_t));

              return invertion ? JC_ZERO : JC_NOT_ZERO;
          } else if (cond->op == EB_LT || cond->op == EB_LE) {
              if (right->op == EU_DEREF) {
                  translateAddress(ctx, f, scope, right->unaryExpr.argument, &addr);
                  emitPopReg(f, R_ACC);
                  emitArithAR(f, OP_FOCMP, R_ACC, &addr, opSize);
              } else {
                  generateExpression(ctx, f, scope, right);
                  emitPopReg(f, R_TMP);
                  emitArithRR(f, OP_FOCMP, R_TMP, R_ACC, opSize);
              }
          } else {
              generateExpression(ctx, f, scope, right);
              emitPopReg(f, R_FTMP);
              emitArithRR(f, OP_FOCMP, R_FACC, R_FTMP, opSize);
          }
      } else {
          if (right->op == E_CONST) {
            uint64_t cnst = right->constExpr.i;
            emitArithConst(f, OP_CMP, R_ACC, cnst, opSize);
          } else {
            emitPushReg(f, R_ACC);

            if (right->op == EU_DEREF) {
                Address addr = { 0 };
                translateAddress(ctx, f, scope, right->unaryExpr.argument, &addr);
                emitPopReg(f, R_ACC);
                emitArithAR(f, OP_CMP, R_ACC, &addr, opSize);
            } else {
                generateExpression(ctx, f, scope, right);
                emitPopReg(f, R_TMP);
                emitArithRR(f, OP_CMP, R_ACC, R_TMP, opSize);
            }
          }
      }

      return cc;
    }
    default:
      if (cond->op == EU_EXL) {
        generateExpression(ctx, f, scope, cond->unaryExpr.argument);
        emitTestRR(f, R_ACC, R_ACC, computeTypeSize(cond->type));
        return invertion ? JC_NOT_ZERO : JC_ZERO;
      } else {
        generateExpression(ctx, f, scope, cond);
        emitTestRR(f, R_ACC, R_ACC, computeTypeSize(cond->type));
        return invertion ? JC_ZERO : JC_NOT_ZERO;
      }
  }
}

static int generateIfStatement(GenerationContext *ctx, GeneratedFunction *f, AstIfStatement *stmt, Scope *scope, size_t frameOffset) {

  struct Label elseLabel = { 0 }, endLabel = { 0 };

  assert(stmt->condition);
  enum JumpCondition cc = generateCondition(ctx, f, scope, stmt->condition, TRUE);

  if (stmt->elseBranch) {
    emitCondJump(f, &elseLabel, cc, FALSE);
  } else {
    emitCondJump(f, &endLabel, cc, FALSE);
  }

  assert(stmt->thenBranch);
  size_t frameSize = generateStatement(ctx, f, stmt->thenBranch, scope, frameOffset);

  if (stmt->elseBranch) {
      emitJumpTo(f, &endLabel);
      bindLabel(f, &elseLabel);
      frameSize = max(frameSize, generateStatement(ctx, f, stmt->elseBranch, scope, frameOffset));
  }

  bindLabel(f, &endLabel);

  return frameSize;
}

static int generateLoopStatement(GenerationContext *ctx, GeneratedFunction *f, AstLoopStatement *stmt, Scope *scope, size_t frameOffset, Boolean doLoop) {

  struct Label loopHead = { 0 }, loopTail = { 0 }, continueLabel = { 0 };
  struct Label *oldBreak = ctx->breakLabel, *oldContinue = ctx->continueLabel;

  ctx->breakLabel = &loopTail;
  ctx->continueLabel = &continueLabel;

  bindLabel(f, &loopHead);

  assert(stmt->condition);

  if (!doLoop) {
      bindLabel(f, &continueLabel);
      enum JumpCondition cc = generateCondition(ctx, f, scope, stmt->condition, TRUE);
      emitCondJump(f, &loopTail, cc, FALSE);
  }

  assert(stmt->body);
  size_t frameSize = generateStatement(ctx, f, stmt->body, scope, frameOffset);

  if (doLoop) {
      bindLabel(f, &continueLabel);
      enum JumpCondition cc = generateCondition(ctx, f, scope, stmt->condition, FALSE);
      emitCondJump(f, &loopHead, cc, FALSE);
  } else {
      emitJumpTo(f, &loopHead);
  }

  bindLabel(f, &loopTail);

  ctx->breakLabel = oldBreak;
  ctx->continueLabel = oldContinue;

  return frameSize;
}

static int generateForStatement(GenerationContext *ctx, GeneratedFunction *f, AstForStatement *stmt, Scope *scope, size_t frameOffset) {
  if (stmt->initial) {
      generateExpression(ctx, f, scope, stmt->initial);
  }

  struct Label loopHead = { 0 }, loopTail = { 0 }, continueLabel = { 0 };

  struct Label *oldBreak = ctx->breakLabel, *oldContinue = ctx->continueLabel;

  ctx->breakLabel = &loopTail;
  ctx->continueLabel = stmt->modifier ? &continueLabel : &loopHead;

  bindLabel(f, &loopHead);

  if (stmt->condition) {
    enum JumpCondition cc = generateCondition(ctx, f, scope, stmt->condition, TRUE);
    emitCondJump(f, &loopTail, cc, FALSE);
  }

  assert(stmt->body);
  size_t frameSize = generateStatement(ctx, f, stmt->body, scope, frameOffset);

  if (stmt->modifier) {
    bindLabel(f, &continueLabel);
    generateExpression(ctx, f, scope, stmt->modifier);
  }

  emitJumpTo(f, &loopHead);
  bindLabel(f, &loopTail);

  ctx->breakLabel = oldBreak;
  ctx->continueLabel = oldContinue;

  return frameSize;
}

static int generateStatement(GenerationContext *ctx, GeneratedFunction *f, AstStatement *stmt, Scope *scope, size_t frameOffset) {

  switch (stmt->statementKind) {
  case SK_BLOCK:
      generateBlock(ctx, f, &stmt->block, frameOffset);
      break;
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
            if (v->initializer) {
                emitLocalInitializer(ctx, f, scope, v->type, v->gen->baseOffset, v->initializer);
            }
        } else {
          assert(v->flags.bits.isStatic);
          GeneratedVariable *gv = generateVaribale(ctx, v);
          v->gen = gv;
          gv->next = ctx->file->staticVariables;
          ctx->file->staticVariables = gv;
        }

        return typeSize;
      }
      break;
  }
  case SK_EMPTY: break;
  case SK_EXPR_STMT:
      generateExpression(ctx, f, scope, stmt->exprStmt.expression);
      break;
  case SK_LABEL:
      generateLabel(ctx, f, &stmt->labelStmt, scope, frameOffset);
      break;
  case SK_GOTO_L: {
      struct Label *l = (struct Label *)getFromHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label);
      if (l == NULL) {
          l = allocateLabel(ctx);
          putToHashMap(ctx->labelMap, (intptr_t)stmt->jumpStmt.label, (intptr_t)l);
      }
      emitJumpTo(f, l);
      break;
  }
  case SK_GOTO_P: {
        generateExpression(ctx, f, scope, stmt->jumpStmt.expression);
        emitJumpByReg(f, R_ACC);
        break;
  }
  case SK_RETURN: {
      AstExpression *retExpr = stmt->jumpStmt.expression;
      if (retExpr) {
          if (isStructualType(retExpr->type)) {
            Address src = { 0 };
            translateAddress(ctx, f, scope, retExpr->op == EU_DEREF ? retExpr->unaryExpr.argument : retExpr, &src);

            size_t retSize = computeTypeSize(retExpr->type);
            if (retSize > sizeof(intptr_t)) {
              Address addr = { R_EBP, R_BAD, 0, f->returnStructAddressOffset, NULL, NULL };
              emitMoveAR(f, &addr, R_EDI, sizeof(intptr_t));

              Address dst = { R_EDI, R_BAD, 0, 0, NULL, NULL };
              copyStructTo(f, retExpr->type, &src, &dst);
              emitMoveRR(f, R_EDI, R_ACC, sizeof(intptr_t));
            } else {
              emitLoad(f, &src, R_ACC, retSize);
            }
          } else {
            generateExpression(ctx, f, scope, retExpr);
          }
      }
      emitLeave(f);
      emitRet(f, 0);
      break;
      }
  case SK_BREAK:
        assert(ctx->breakLabel);
        emitJumpTo(f, ctx->breakLabel);
        break;
  case SK_CONTINUE:
        assert(ctx->continueLabel);
        emitJumpTo(f, ctx->continueLabel);
        break;
  case SK_IF:
      generateIfStatement(ctx, f, &stmt->ifStmt, scope, frameOffset);
      break;
  case SK_SWITCH:
      generateSwitchStatement(ctx, f, &stmt->switchStmt, scope, frameOffset);
      break;
  case SK_WHILE:
  case SK_DO_WHILE:
      generateLoopStatement(ctx, f, &stmt->loopStmt, scope, frameOffset, stmt->statementKind == SK_DO_WHILE);
      break;
  case SK_FOR:
      generateForStatement(ctx, f, &stmt->forStmt, scope, frameOffset);
      break;
  default:
      unreachable("Unreachable");
      break;
  }

  return 0;
}

static int generateBlock(GenerationContext *ctx, GeneratedFunction *f, AstBlock *block, size_t frameOffset) {

  AstStatementList *stmt = block->stmts;
  Scope *scope = block->scope;
  size_t innerScopeSize = 0;
  size_t initFrameOffset = frameOffset;

  while (stmt) {
    frameOffset += generateStatement(ctx, f, stmt->stmt, scope, frameOffset);
    stmt = stmt->next;
  }

  return 0;
}

static void pushFrame(GenerationContext *ctx, GeneratedFunction *f) {

  // pushq %rbp
  emitPushReg(f, R_EBP);
  // movq %rsp, %rbp
  emitMoveRR(f, R_ESP, R_EBP, sizeof(intptr_t));
}

static void popFrame(GenerationContext *ctx, GeneratedFunction *f) {
  // movq %rbp, %rsp
  // popq %rbp

  emitLeave(f);
}

static size_t allocateLocalSlots(GenerationContext *ctx, GeneratedFunction *g, AstFunctionDefinition *f) {
  AstValueDeclaration *param = f->declaration->parameters;
  AstValueDeclaration *local = f->locals;
  TypeRef *returnType = f->declaration->returnType;

  unsigned intRegParams = 0;
  unsigned fpRegParams = 0;

  int32_t baseOffset = 0; // from rbp;
  int32_t stackParamOffset = sizeof(intptr_t) + sizeof(intptr_t); // rbp itself + return pc

  baseOffset += sizeof(intptr_t);
  int32_t allocaOffset = g->allocaOffset = -baseOffset;

  int32_t smallStructOffset = 0;
  if (f->hasSmallStructs) {
      baseOffset += sizeof(intptr_t);
      smallStructOffset = g->smallStructSlotOffset = -baseOffset;
  }

  Address addr = { R_EBP, R_BAD, 0, 0, NULL, NULL };

  if (isStructualType(returnType) && computeTypeSize(returnType) > sizeof(intptr_t)) {
      baseOffset += sizeof(intptr_t);
      g->returnStructAddressOffset = addr.imm = -baseOffset;
      emitMoveRA(g, intArgumentRegs[intRegParams++], &addr, sizeof(intptr_t));
  }

  for (; param; param = param->next) {
    TypeRef *paramType = param->type;
    GeneratedVariable *gp = allocateGenVarialbe(ctx, param);
    size_t size = max(sizeof(int32_t), computeTypeSize(paramType));
    size_t align = min(size, sizeof(intptr_t));

    if (isStructualType(paramType) && size > sizeof(intptr_t)) {
        int32_t alignedOffset = ALIGN_SIZE(stackParamOffset, align);
        gp->baseOffset = alignedOffset;
        stackParamOffset = alignedOffset + size;
    } else if (isRealType(paramType)) {
        if (fpRegParams < R_FP_PARAM_COUNT) {
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
      GeneratedVariable *gp = allocateGenVarialbe(ctx, local);
      size_t size = computeTypeSize(localType);
      size_t align = typeAlignment(localType);

      baseOffset += size;
      baseOffset = (baseOffset + (align - 1)) & ~(align - 1);
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

      va_area->gen = allocateGenVarialbe(ctx, va_area);
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

      emitMoveCR(g, intRegParams * dataSize, R_ACC, sizeof(uint32_t));
      addr.imm = -gp_offset_off;
      emitMoveRA(g, R_ACC, &addr, sizeof(uint32_t));

      emitMoveCR(g, gp_va_area - fp_va_area + fpRegParams * dataSize, R_ACC, sizeof(uint32_t));
      addr.imm = -fp_offset_off;
      emitMoveRA(g, R_ACC, &addr, sizeof(uint32_t));

      addr.imm = stackParamOffset;
      emitLea(g, &addr, R_ACC);
      addr.imm = -overflow_arg_area_off;
      emitMoveRA(g, R_ACC, &addr, dataSize);
  }

  g->savedRegOffset = -baseOffset;

  if (f->hasSmallStructs) {
      addr.imm = smallStructOffset;
      emitArithRR(g, OP_XOR, R_ACC, R_ACC, sizeof(intptr_t));
      emitMoveRA(g, R_ACC, &addr, sizeof(intptr_t));
  }

  int32_t frameSize = ALIGN_SIZE(baseOffset, 2 * sizeof(intptr_t));
  g->frameSize = frameSize;

  return frameSize;
}

static GeneratedFunction *generateFunction(GenerationContext *ctx, AstFunctionDefinition *f) {
  HashMap *labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
  ctx->labelMap = labelMap;

  assert(f->body->statementKind == SK_BLOCK);

  GeneratedFunction *gen = allocateGenFunction(ctx);

  gen->symbol = f->declaration->symbol;
  gen->name = f->declaration->name;

  pushFrame(ctx, gen);

  size_t frameSize = allocateLocalSlots(ctx, gen, f);

  size_t delta = frameSize;

  if (frameSize)
    emitArithConst(gen, OP_SUB, R_ESP, delta, sizeof(intptr_t));

  Address addr = { R_EBP, R_BAD, 0, gen->allocaOffset, NULL, NULL };
  emitMoveRA(gen, R_ESP, &addr, sizeof(intptr_t));

  gen->stackOffset = 0;
  generateBlock(ctx, gen, &f->body->block, 0);
  assert(gen->stackOffset == 0);

  if (gen->section->pc[-1] != 0xC3) {
    TypeRef * returnType = f->declaration->returnType;
    size_t returnTypeSize = computeTypeSize(returnType);
    if (isStructualType(returnType) && returnTypeSize > sizeof(intptr_t)) {
        Address addr = { R_EBP, R_BAD, 0, gen->returnStructAddressOffset, NULL, NULL };
        emitMoveAR(gen, &addr, R_EAX, sizeof(intptr_t));
    }

    popFrame(ctx, gen);
    emitReturn(ctx, gen);
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

static void writeObjFile(const char *sourceFileName, const char *objDir, uint8_t *buffer, size_t bufferSize) {
  size_t len = strlen(sourceFileName);
  size_t dirLen = objDir ? strlen(objDir) : 0;
  size_t bufferLen = dirLen + 1 + len + 1;
  char *outputName = alloca(bufferLen);

  unsigned i = 0;
  unsigned j = 0;

  if (objDir) {
      strncpy(outputName, objDir, bufferLen);
      outputName[dirLen] = '/';
      i = dirLen + 1;
  } else {
      for (j = len - 1; j >= 0; --j) {
          if (sourceFileName[j] == '/') break;
      }
      ++j;
  }


  while (sourceFileName[j] != '.') {
    outputName[i++] = sourceFileName[j++];
  }

  outputName[i++] = '.';
  outputName[i++] = 'o';
  outputName[i++] = '\0';

  remove(outputName);
  FILE* output = fopen(outputName, "wb");
  if (output) {
    fwrite(buffer, bufferSize, 1, output);
    fclose(output);
  } else {
    fprintf(stderr, "cannot open output file %s\n", outputName);
  }
}

static void buildElfFile(GenerationContext *ctx, AstFile *astFile, GeneratedFile *genFile, ElfFile *elfFile) {

  size_t elfFileSize = 0;

  uint8_t *elfFileBytes = generateElfFile(elfFile, genFile, &elfFileSize);

  writeObjFile(astFile->fileName, ctx->parserContext->config->objDirName, elfFileBytes, elfFileSize);

  releaseHeap(elfFile->sections.asStruct.nullSection->start);
  releaseHeap(elfFile->sections.asStruct.text->start);
  releaseHeap(elfFile->sections.asStruct.reText->start);
  releaseHeap(elfFile->sections.asStruct.data->start);
  releaseHeap(elfFile->sections.asStruct.bss->start);
  releaseHeap(elfFile->sections.asStruct.rodata->start);
  releaseHeap(elfFile->sections.asStruct.rodataLocal->start);
  releaseHeap(elfFile->sections.asStruct.reRodataLocal->start);
  releaseHeap(elfFile->sections.asStruct.dataLocal->start);
  releaseHeap(elfFile->sections.asStruct.reDataLocal->start);
  releaseHeap(elfFile->sections.asStruct.symtab->start);
  releaseHeap(elfFile->sections.asStruct.strtab->start);
  releaseHeap(elfFile->sections.asStruct.shstrtab->start);

  releaseHeap(elfFileBytes);
}

GeneratedFile *generateCodeForFile(ParserContext *pctx, AstFile *astFile) {

  Section nullSection = { "", SHT_NULL, 0x00, 0 };
  Section text = { ".text", SHT_PROGBITS, SHF_EXECINSTR | SHF_ALLOC, 1 }, reText = { ".rela.text", SHT_RELA, SHF_INFO_LINK, 8 };
  Section data = { ".data", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 16 };
  Section bss = { ".bss", SHT_NOBITS, SHF_WRITE | SHF_ALLOC, 32 };
  Section rodata = { ".rodata", SHT_PROGBITS, SHF_ALLOC, 16 };
  Section dataLocal = { ".data.rel.local", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 8 }, reDataLocal = { ".rela.data.rel.local", SHT_RELA, SHF_INFO_LINK, 8 };
  Section roDataLocal = { "data.rel.ro.local", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 8 }, reRoDataLocal = { ".rela.data.rel.ro.local", SHT_RELA, SHF_INFO_LINK, 8 };
  Section symtab = { ".symtab", SHT_SYMTAB, 0x00, 8 };
  Section strtab = { ".strtab", SHT_STRTAB, 0x00, 1 };
  Section shstrtab = { ".shstrtab", SHT_STRTAB, 0x00, 1 };

  ElfFile elfFile = { 0 };
  elfFile.sections.asStruct.nullSection = &nullSection;
  elfFile.sections.asStruct.text = &text;
  elfFile.sections.asStruct.reText = &reText; reText.relocatedSection = &text;
  elfFile.sections.asStruct.data = &data;
  elfFile.sections.asStruct.bss = &bss;
  elfFile.sections.asStruct.rodata = &rodata;
  elfFile.sections.asStruct.rodataLocal = &roDataLocal;
  elfFile.sections.asStruct.reRodataLocal = &reRoDataLocal; reRoDataLocal.relocatedSection = &roDataLocal;
  elfFile.sections.asStruct.dataLocal = &dataLocal;
  elfFile.sections.asStruct.reDataLocal = &reDataLocal; reDataLocal.relocatedSection = &dataLocal;
  elfFile.sections.asStruct.symtab = &symtab;
  elfFile.sections.asStruct.strtab = &strtab;
  elfFile.sections.asStruct.shstrtab = &shstrtab;

  GenerationContext ctx = { pctx, NULL, pctx->memory.codegenArena };
  GeneratedFile *file = allocateGenFile(&ctx);
  ctx.file = file;
  file->name = astFile->fileName;

  ctx.text = &text;
  ctx.bss = &bss;
  ctx.rodata = &rodata;
  ctx.data = &data;
  ctx.dataLocal = &dataLocal;
  ctx.rodataLocal = &roDataLocal;

  AstTranslationUnit *unit = astFile->units;

  while (unit) {
    if (unit->kind == TU_FUNCTION_DEFINITION) {
        GeneratedFunction *f = generateFunction(&ctx, unit->definition);
        unit->definition->declaration->gen = f;
        unit->definition->declaration->symbol->function->gen = f;

        if (unit->definition->declaration->flags.bits.isStatic) {
            f->next = file->staticFunctions;
            file->staticFunctions= f;
        } else {
            f->next = file->functions;
            file->functions= f;
        }
    } else {
        assert(unit->kind == TU_DECLARATION);
        AstDeclaration *d = unit->declaration;
        if (d->kind == DK_VAR) {
          GeneratedVariable *v = generateVaribale(&ctx, d->variableDeclaration);
          if (v) {
            d->variableDeclaration->gen = v;

            if (d->variableDeclaration->flags.bits.isStatic) {
              v->next = file->staticVariables;
              file->staticVariables = v;
            } else {
              v->next = file->variables;
              file->variables = v;
            }
          }
        }
    }
    unit = unit->next;
  }

  buildElfFile(&ctx, astFile, file, &elfFile);

  return file;
}
