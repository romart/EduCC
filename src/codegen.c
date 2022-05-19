
#include <assert.h>

#include <udis86.h>

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

  HashMap *localSymMap;
  HashMap *labelMap;


  struct Label *returnLabel;
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
  f->returnStructOffset = -1;
  f->arena = ctx->codegenArena;
  f->section = ctx->text;
  f->sectionOffset = (ctx->text->pc - ctx->text->start);
  return f;
}

static GeneratedVariable *allocateGenVarialbe(GenerationContext *ctx) {
  GeneratedVariable *v = areanAllocate(ctx->codegenArena, sizeof (GeneratedVariable));
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

  emitMovfpRA(f, r, &addr, TRUE);
  emitArithConst(f, OP_L_SUB, R_ESP, size, size);
}

void emitPopRegF(GeneratedFunction *f, enum Registers r, Boolean isD) {
  int size = sizeof(double);
  Address addr = { 0 };
  addr.base = R_ESP;
  addr.index = R_BAD;
  addr.imm = 0;

  emitMovfpAR(f, &addr, r, TRUE);
  emitArithConst(f, OP_L_ADD, R_ESP, size, size);
}

void emitByte(GeneratedFunction *f, uint8_t b) {
  emitSectionByte(f->section, b);
}

void emitWord(GeneratedFunction *f, u_int16_t w) {
   emitByte(f, (u_int8_t)w);

   if ((u_int16_t)(u_int8_t)w != w) {
       emitByte(f, (u_int8_t)(w >> 8));
   }
}


void emitDouble(GeneratedFunction *f, u_int32_t w) {
    emitByte(f, (uint8_t)(w));
    emitByte(f, (uint8_t)(w >> 8));
    emitByte(f, (uint8_t)(w >> 16));
    emitByte(f, (uint8_t)(w >> 24));
}

void emitDisp32(GeneratedFunction *f, u_int32_t w) {
  if ((u_int32_t)(u_int16_t)w != w) {
      emitWord(f, (u_int16_t) w);

      u_int16_t high = (u_int16_t)(w >> 16);
      emitByte(f, (u_int8_t)high);
      emitByte(f, (u_int8_t)(high >> 8));
  } else {
      if ((u_int32_t)(u_int8_t)w != w) {
          emitWord(f, w);
      } else {
          emitByte(f, (u_int8_t)w);
          emitByte(f, 0);
      }
      emitByte(f, 0);
      emitByte(f, 0);
  }
}

void emitQuad(GeneratedFunction *f, u_int64_t w) {
  emitDouble(f, (u_int32_t) w);

  if ((u_int64_t)(u_int32_t)w != w) {
      emitDouble(f, (u_int32_t)(w >> 32));
  }
}

void emitQuadOrDouble(GeneratedFunction *f, u_int64_t w) {
  if ((u_int64_t)(u_int32_t)w == w) {
    emitDisp32(f, w);
  } else {
    emitDouble(f, (u_int32_t) w);
    emitDouble(f, (u_int32_t)(w >> 32));
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
      return selectByType(type, OP_L_ADD, OP_F_ADD, OP_D_ADD, OP_I_ADD, OP_L_ADD);
    case EB_SUB:
      return selectByType(type, OP_L_SUB, OP_F_SUB, OP_D_SUB, OP_I_SUB, OP_L_SUB);
    case EB_MUL:
      return selectByType(type, OP_L_MUL, OP_F_MUL, OP_D_MUL, OP_I_MUL, OP_L_MUL);
    case EB_MOD:
      return selectByType(type, OP_L_MOD,  OP_F_MOD, OP_D_MOD, OP_I_MOD, OP_L_MOD);
    case EB_DIV:
      return selectByType(type, OP_L_DIV, OP_F_DIV, OP_D_DIV, OP_I_DIV, OP_L_DIV);
    case EB_LHS:
      assert(isIntegerType(type));
      id = type->descriptorDesc->typeId;
      return id == T_S8 || id == T_U8 ? OP_L_SHL : OP_I_SHL;
    case EB_RHS:
      assert(isIntegerType(type));
      id = type->descriptorDesc->typeId;
      switch (id) {
        case T_U8: return OP_L_SAR;
        case T_U4:
        case T_U2:
        case T_U1: return OP_I_SAR;
        case T_S8: return OP_L_SHR;
        default:  return OP_I_SHR;
      }
    case EB_AND:
      assert(isIntegerType(type));
      id = type->descriptorDesc->typeId;
      return id == T_S8 || id == T_U8 ? OP_L_AND : OP_I_AND;
    case EB_OR:
      assert(isIntegerType(type));
      id = type->descriptorDesc->typeId;
      return id == T_S8 || id == T_U8 ? OP_L_OR : OP_I_OR;
    case EB_XOR:
      assert(isIntegerType(type));
      id = type->descriptorDesc->typeId;
      return id == T_S8 || id == T_U8 ? OP_L_XOR : OP_I_XOR;

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
          case 'e':
            toEmit = '\e';
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
        emitMovfpAR(f, &addr, R_FACC, size > 4);

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

static void generateExpression(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression);
static enum JumpCondition generateCondition(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *cond, Boolean invertion);
static void translateAddress(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression, Address *addr);


static void copyStructTo(GeneratedFunction *f, TypeRef *type, Address *src, Address *dst) {

  unsigned size = computeTypeSize(type);

  unsigned copied = 0;

  while (copied < size) {
      emitMoveAR(f, src, R_TMP, sizeof(intptr_t));
      emitMoveRA(f, R_TMP, dst, sizeof(intptr_t));

      src->imm += sizeof(intptr_t);
      dst->imm += sizeof(intptr_t);
      copied += sizeof(intptr_t);
  }
}


static size_t emitInitializerImpl(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, int32_t typeSize, Address *dst, AstInitializer *initializer) {
  size_t emitted = 0;

  switch (initializer->kind) {
  case IK_EXPRESSION: {
      AstExpression *expr = initializer->expression;
      size_t exprSize = computeTypeSize(expr->type);

      generateExpression(ctx, f, scope, initializer->expression);

      if (typeSize > 0) {
        if (isRealType(expr->type)) {
            Boolean isD = expr->type->descriptorDesc->typeId != T_F4;
            emitMovfpRA(f, R_FACC, dst, isD);
        } else  if (isStructualType(expr->type)) {
            Address src = { R_ACC, R_BAD, 0, 0 };
            copyStructTo(f, expr->type, &src, dst);
        } else {
          emitMoveRA(f, R_ACC, dst, typeSize);
        }
      }
      return exprSize;
    }
    break;
    case IK_LIST: {
        AstInitializerList *inits = initializer->initializerList;
        while (inits) {
            size_t tmp = emitInitializerImpl(ctx, f, scope, typeSize, dst, inits->initializer);

            typeSize -= tmp;
            emitted += tmp;
            dst->imm += tmp;
            inits = inits->next;
        }
    }
    return emitted;

  default: unreachable("Unknown init kind");

  }

  return 0;
}

static void emitLocalInitializer(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, int32_t typeSize, int32_t frameOffset, AstInitializer *initializer) {
  Address addr = { R_EBP, R_BAD, 0, frameOffset };

  size_t emitted = emitInitializerImpl(ctx, f, scope, typeSize, &addr, initializer);

  if (emitted < typeSize) {
    emitArithRR(f, OP_L_XOR, R_ACC, R_ACC);
    while (emitted < typeSize) {
      emitMoveRA(f, R_ACC, &addr, sizeof(intptr_t));
      addr.imm += sizeof(intptr_t);
      emitted += sizeof(intptr_t);
    }
  }
}

static size_t fillInitializer(GenerationContext *ctx, Section *section, AstInitializer *init, size_t size) {

  if (size <= 0) return 0;

  if (init->kind == IK_EXPRESSION) {
      AstExpression *expr = init->expression;
      AstConst *cexpr = eval(ctx->parserContext, expr);
      assert(cexpr);
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

  GeneratedVariable *v = allocateGenVarialbe(ctx);
  v->name = d->name;
  v->section = section;
  v->sectionOffset = offset;
  v->size = objectSize;
  v->symbol = d->symbol;
}

static TypeId approximateTypeId(TypeRef *type) {
  if (type->kind == TR_POINTED) {
      return T_U8;
  } else {
      assert(type->kind == TR_VALUE);
      return type->descriptorDesc->typeId;
  }
}

static void generateCast(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstCastExpression *cast) {
    TypeRef *fromType = cast->argument->type;
    TypeRef *toType = cast->type;

    TypeId fromTypeId = approximateTypeId(fromType);
    TypeId toTypeId = approximateTypeId(toType);

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
        emitConvertWDQ(f, 32); // cwde
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
        if (fromTypeId >= T_F4) {
            if (fromTypeId == T_F4) {
              emitConvertFP(f, 0xF3, 0x2C, R_FACC, R_ACC, FALSE); // cvttss2si eax,xmm0
            } else if (fromTypeId == T_F8) {
              emitConvertFP(f, 0xF2, 0x2C, R_FACC, R_ACC, FALSE); // cvttsd2si eax,xmm0
            } else {
              unreachable("long double conversions are not implemented yet");
            }
        }
    } else if (toTypeId == T_S8 || toTypeId == T_U8) {
        if (fromTypeId < T_U4) {
            emitConvertWDQ(f, 64); // cdqe
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

    if (right->op == E_NAMEREF || right->op == EU_DEREF) {
      Address addr = { 0 };
      translateAddress(ctx, f, scope, right->op == EU_DEREF ? right->unaryExpr.argument : right, &addr);
      if (isFP) {
        emitPopRegF(f, R_FACC, isD);
      } else {
        emitPopReg(f, R_ACC); // save result
      }

      emitArithAR(f, opcode, isFP ? R_FACC : R_ACC, &addr, max(4, computeTypeSize(binOp->type)));
    } else {
      generateExpression(ctx, f, scope, right);

      if (isFP) {
        emitMovfpRR(f, R_FACC, R_FTMP, isD);
        emitPopRegF(f, R_FACC, isD);
        emitArithRR(f, opcode, R_FACC, R_FTMP);
      } else {
        emitMoveRR(f, R_ACC, R_TMP, opSize);
        emitPopReg(f, R_ACC);
        emitArithRR(f, opcode, R_ACC, R_TMP);
      }
    }
  }
}

static void generateLogicalBinary(GenerationContext *ctx, GeneratedFunction *f, AstExpression *binOp, Scope *scope) {
  assert(binOp->op == EB_ANDAND || binOp->op == EB_OROR);

  enum JumpCondition cc = generateCondition(ctx, f, scope, binOp->binaryExpr.left, binOp->op == EB_ANDAND);

  struct Label elseB = { 0 }, endB = { 0 };

  emitCondJump(f, &elseB, cc);

  cc = generateCondition(ctx, f, scope, binOp->binaryExpr.right, FALSE);

  switch (binOp->binaryExpr.right->op) {
    case EB_GE:
    case EB_LE:
    case EB_LT:
    case EB_GT:
    case EB_NE:
    case EB_EQ:
      emitSetccR(f, cc, R_ACC);
      emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
    default:
      break;
  }

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
  int size = computeTypeSize(storageType);

  emitMoveAR(f, addr, to, size);

  emitShr(f, to, s);

  u_int64_t mask = ~(~0LLu << w);

  emitArithConst(f, size > 4 ? OP_L_AND : OP_I_AND, to, mask, size);
}

static void storeBitField(GeneratedFunction *f, TypeRef *t, enum Registers from, Address *addr) {
  assert(t->kind == TR_BITFIELD);

  int s = t->bitFieldDesc.offset;
  int w = t->bitFieldDesc.width;


  TypeRef *storageType = t->bitFieldDesc.storageType;
  int size = computeTypeSize(storageType);

  emitMoveAR(f, addr, R_TMP, size);

  u_int64_t mask = ~(~0LLu << w) << s;

  emitArithConst(f, size > 4 ? OP_L_AND : OP_I_AND, R_TMP, mask, size);

  emitShl(f, from, s);

  emitArithRR(f, size > 4 ? OP_L_OR : OP_I_OR, R_TMP, from);

  emitMoveAR(f, addr, R_TMP, size);

  emitShr(f, from, s);
}

static void localVarAddress(GenerationContext *ctx, const Symbol *s, Address *addr) {
  int64_t offset = (int64_t)getFromHashMap(ctx->localSymMap, (intptr_t)s);
  addr->base = R_EBP;
  addr->index = R_BAD;
  addr->imm = offset;
}

static void loadValueTo(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, const Symbol *s, enum Registers result, Boolean isFP) {
  Address addr = { 0 };
  localVarAddress(ctx, s, &addr);
  size_t typeSize = computeTypeSize(s->variableDesc->type);
  if (isFP) {
    emitMovfpAR(f, &addr, result, typeSize > 4);
  } else {
    emitMoveAR(f, &addr, result, typeSize);
  }
}

static void loadValue(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, const Symbol *s) {
  Boolean isFP = isRealType(s->variableDesc->type);
  loadValueTo(ctx, f, scope, s, isFP ? R_FACC : R_ACC, isFP);
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
      emitMoveRR(f, R_ACC, R_EBX, sizeof(intptr_t));
      addr->base = R_EBX;
      generateExpression(ctx, f, scope, r);
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else if (r->op == EB_ADD) {
      // [base + (index * scale + imm)]
      generateExpression(ctx, f, scope, l);
      emitMoveRR(f, R_ACC, R_EBX, sizeof(intptr_t));
      addr->base = R_EBX;
      generateExpression(ctx, f, scope, r);
      emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
      addr->index = R_ECX;
      addr->scale = 0;
    } else {
        if (r->op == E_CONST) {
            // [expr + imm]
            generateExpression(ctx, f, scope, l);
            emitMoveRR(f, R_ACC, R_EBX, sizeof(intptr_t));
            addr->base = R_EBX;
            addr->index = R_BAD;
            addr->imm += r->constExpr.i;
        } else if (r->op == EB_MUL || l->op == EB_MUL) {
            // [expr + index * scale] || [index * scale + expr]
            AstExpression *idxs = r->op == EB_MUL ? r : l;
            AstExpression *expr = idxs == r ? l : r;
            generateExpression(ctx, f, scope, expr);
            emitMoveRR(f, R_ACC, R_EBX, sizeof(intptr_t));
            addr->base = R_EBX;
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
                  emitMoveRR(f, R_ACC, R_ECX, sizeof(intptr_t));
                  addr->index = R_ECX;
                } else {
                  generateExpression(ctx, f, scope, idxs);
                  addr->scale = 0;
                  addr->index = R_ACC;
                }
            } else {
                generateExpression(ctx, f, scope, idxs);
                addr->scale = 0;
                addr->index = R_ACC;
            }
        } else {
            // [expr1 + expr2]
            generateExpression(ctx, f, scope, l);
            emitMoveRR(f, R_ACC, R_EBX, sizeof(intptr_t));
            addr->base = R_EBX;
            generateExpression(ctx, f, scope, r);
            addr->base = R_ACC;
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
  Address addr = { 0 };

  AstExpression *addrExpr = lType->kind == TR_BITFIELD ? lvalue->fieldExpr.recevier : lvalue;
  Boolean saved_acc = FALSE;
  Boolean isFP = isRealType(lType);
  size_t typeSize = computeTypeSize(lType);
  Boolean isD = isFP && typeSize > 4;

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
          assert(lvalue->op == EF_ARROW || lvalue->op == EF_DOT);
          AstStructDeclarator *decl = lvalue->fieldExpr.member;

          addr.imm += decl->offset;
          emitLea(f, &addr, R_EBX);

          addr.base = R_EBX;
          addr.index = R_BAD;
          addr.imm = 0;

          if (saved_acc) {
              emitPopReg(f, R_ACC);
          }
          storeBitField(f, lType, R_ACC, &addr);
      } else {
          if (saved_acc) {
            if (isFP) {
              emitPopRegF(f, R_FACC, isD);
            } else {
              emitPopReg(f, R_ACC); // load result
            }
          }

          if (isFP) {
            emitMovfpRA(f, R_FACC, &addr, isD);
          } else {
            if (isStructualType(lType)) {
                Address src = { 0 };
                src.base = R_ACC;
                src.index = R_BAD;
                copyStructTo(f, lType, &src, &addr);
            } else {
                emitMoveRA(f, R_ACC, &addr, typeSize);
            }
          }
      }
  } else {
    if (lType->kind == TR_BITFIELD) {
        assert(lvalue->op == EF_ARROW || lvalue->op == EF_DOT);
        AstStructDeclarator *decl = lvalue->fieldExpr.member;

        addr.imm += decl->offset;
        emitLea(f, &addr, R_EBX);

        addr.base = R_EBX;
        addr.index = R_BAD;
        addr.imm = 0;

        loadBitField(f, lType, &addr, R_ACC);
        emitPopReg(f, R_TMP);

        emitArithRR(f, selectAssignOpcode(op, lType), R_ACC, R_TMP);

        storeBitField(f, lType, R_ACC, &addr);

    } else {
        Address addr2 = { 0 };
        addr2.base = R_EBX;
        addr2.index = R_BAD;
        if (isFP)
          emitMovfpAR(f, &addr, R_FACC, isD);
        else
          emitMoveAR(f, &addr, R_ACC, typeSize);
        if (saved_acc) {
            if (isFP) {
              emitPopRegF(f, R_FTMP, isD);
            } else {
              emitPopReg(f, R_TMP); // load result
            }
        }
        emitArithRR(f, selectAssignOpcode(op, lType), isFP ? R_FACC : R_ACC, isFP ? R_FTMP : R_TMP);
        if (isFP) {
          emitMovfpRA(f, R_FACC, &addr, isD);
        } else {
          emitMoveRA(f, R_ACC, &addr, typeSize);
        }
    }
  }
}

static enum Opcodes selectIncDecOpcode(ExpressionType astOp, TypeRef *type) {
  size_t size = computeTypeSize(type);
  if (astOp == EU_POST_DEC || astOp == EU_PRE_DEC) {
      return size > 4 ? OP_L_SUB : OP_L_SUB;
  } else {
      assert(astOp == EU_POST_INC || astOp == EU_PRE_INC);
      return size > 4 ? OP_L_ADD : OP_L_ADD;
  }
}


static void generatePostIncDec(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  // i++
  // ==>
  // i -> r_acc
  // r_tmp -> r_acc +/- 1
  // r_tmp -> i

  TypeRef *type = expression->type;

  Address addr = { 0 };
  size_t typeSize = computeTypeSize(type);
  int64_t disp = type->kind == TR_POINTED ? computeTypeSize(type->pointedTo) : 1;
  enum Opcodes opcode = selectIncDecOpcode(expression->op, type);

  if (type->kind == TR_BITFIELD) {
    AstExpression *lvalue = expression->unaryExpr.argument;

    translateAddress(ctx, f, scope, lvalue->fieldExpr.recevier, &addr);

    assert(lvalue->op == EF_ARROW || lvalue->op == EF_DOT);
    AstStructDeclarator *decl = lvalue->fieldExpr.member;

    addr.imm += decl->offset;
    emitLea(f, &addr, R_EBX);

    addr.base = R_EBX;
    addr.index = R_BAD;
    addr.imm = 0;

    loadBitField(f, type, &addr, R_ACC);
    emitPushReg(f, R_ACC);
    emitArithConst(f, opcode, R_ACC, disp, typeSize);
    storeBitField(f, type, R_ACC, &addr);

    emitPopReg(f, R_ACC);
  } else {
    translateAddress(ctx, f, scope, expression->unaryExpr.argument, &addr);

    emitMoveAR(f, &addr, R_ACC, typeSize);
    emitMoveRR(f, R_ACC, R_TMP, typeSize);
    emitArithConst(f, opcode, R_TMP, disp, typeSize);
    emitMoveRA(f, R_TMP, &addr, typeSize);
  }

}

static const enum Registers intArgumentRegs[] = { R_ARG_0, R_ARG_1, R_ARG_2, R_ARG_3, R_ARG_4, R_ARG_5 };
static const enum Registers fpArgumentRegs[] = { R_XMM0, R_XMM1, R_XMM2, R_XMM3, R_XMM4, R_XMM5, R_XMM6, R_XMM7 };

static void generateCall(GenerationContext *ctx, GeneratedFunction *f, Scope *scope, AstExpression *expression) {
  assert(expression->op == E_CALL);

  AstExpression *callee = expression->callExpr.callee;
  AstExpressionList *args = expression->callExpr.arguments;
  TypeRef *type = expression->type;
  TypeRef *pcalleeType = callee->type;
  assert(pcalleeType->kind == TR_POINTED);
  TypeRef *calleeType = pcalleeType->pointedTo;
  assert(calleeType->kind == TR_FUNCTION);
  TypeRef *returnType = calleeType->functionTypeDesc.returnType;

  unsigned offset = 0; // call frame start
  int r_offsets[R_PARAM_COUNT + R_FP_PARAM_COUNT] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  TypeRef *r_types[R_PARAM_COUNT + R_FP_PARAM_COUNT] = { NULL };


  unsigned frameOffset = f->frameOffset + f->frameSize; //ALIGN_SIZE(f->frameOffset + f->localsSize + f->argsSize, sizeof(intptr_t));
  assert(ALIGN_SIZE(frameOffset, 16) == frameOffset);

  unsigned idx = 0;

  unsigned returnTypeSize = 0;

  unsigned firstIntRegArg = 0;
  unsigned lastIntRegArg = 0;

  unsigned lastFpRegArg = 0;

  unsigned intRegArgs = 0;
  unsigned fpRegArgs = 0;

  if (isStructualType(returnType)) {
      returnTypeSize = computeTypeSize(returnType);
      offset = returnTypeSize;
      r_offsets[idx] = offset;
      r_types[idx] = returnType;
      intRegArgs = firstIntRegArg = 1;
  }

  AstExpressionList *tmp = args;

  unsigned count = 0;
  unsigned stackArgSize = returnTypeSize;

  while (tmp) {
      args = tmp;

      TypeRef *t = tmp->expression->type;

      unsigned argSize = max(4, computeTypeSize(t));

      ++count;

      if (isRealType(t) && fpRegArgs < R_FP_PARAM_COUNT) {
        ++fpRegArgs;
        lastFpRegArg = count;
      } else if (!isStructualType(t) && intRegArgs < R_PARAM_COUNT) {
        ++intRegArgs;
        lastIntRegArg = count;
      } else {
        stackArgSize += argSize;
      }

      tmp = tmp->next;

  }

  unsigned alignedStackSize = ALIGN_SIZE(stackArgSize, 2 * sizeof(intptr_t));
  unsigned delta = alignedStackSize - stackArgSize;

  unsigned totalRegArg = intRegArgs + fpRegArgs;

  if (alignedStackSize)
    emitArithConst(f, OP_L_SUB, R_ESP, alignedStackSize, sizeof(intptr_t));

  while (args) {
    AstExpression *arg = args->expression;
    TypeRef *argType = arg->type;

    unsigned typeSize = max(4, computeTypeSize(argType));

    offset += typeSize;

    unsigned rbpOffset = -(frameOffset + offset + delta);

    Address dst = { 0 };

    dst.base = R_EBP;
    dst.index = R_BAD;
    dst.imm = rbpOffset;

    if (isStructualType(argType)) {
      Address addr = { 0 };
      translateAddress(ctx, f, scope, arg, &addr);
      copyStructTo(f, argType, &addr, &dst);
    } else {
      generateExpression(ctx, f, scope, arg);

      if (isRealType(argType)) {
        if (count <= lastFpRegArg) {
            r_offsets[totalRegArg - idx - 1] = offset;
            r_types[totalRegArg - idx - 1] = argType;
            ++idx;
            emitPushRegF(f, R_FACC, TRUE);
        } else {
            emitMovfpRA(f, R_FACC, &dst, typeSize > sizeof(float));
        }
      } else {
        if (count <= lastIntRegArg) {
            r_offsets[totalRegArg - idx - 1] = offset;
            r_types[totalRegArg - idx - 1] = argType;
            ++idx;
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

  Address saddr = { 0 };
  saddr.base = R_EBP;
  saddr.index = R_BAD;

  if (callee->op != E_NAMEREF) {
      generateExpression(ctx, f, scope, callee);
      emitMoveRR(f, R_ACC, R_TMP, sizeof(intptr_t));
  }

  unsigned ir = 0, fr = 0;

  for (i = 0; i < idx; ++i) {
      TypeRef *argType = r_types[i];
      size_t argSize = computeTypeSize(argType);
      if (isStructualType(argType)) {
          unsigned argOffset = -(frameOffset + r_offsets[i] + delta);
          saddr.imm = argOffset;
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
    emitArithRR(f, OP_L_XOR, R_ACC, R_ACC);
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
    emitCall(f, R_TMP);
  }

  if (alignedStackSize) {
    emitArithConst(f, OP_L_ADD, R_ESP, alignedStackSize, sizeof(intptr_t));
  }
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
    case E_NAMEREF: {
        const Symbol *s = expression->nameRefExpr.s;
        if (s->kind == ValueSymbol) {
          AstValueDeclaration *v = s->variableDesc;
          if (v->flags.bits.isLocal) {
            loadValue(ctx, f, scope, s);
            break;
          }
        }

        translateAddress(ctx, f, scope, expression, &addr);
        emitMoveAR(f, &addr, R_ACC, typeSize);
      }
      break;
    case E_CALL:
      generateCall(ctx, f, scope, expression);
      break;
    case E_TERNARY: {
        AstTernaryExpression *ternary = &expression->ternaryExpr;
        struct Label elseLabel = { 0 }, endLabel = { 0 };
        assert(ternary->condition);
        enum JumpCondition cc = generateCondition(ctx, f, scope, ternary->condition, TRUE);

        emitCondJump(f, &elseLabel, cc);

        assert(ternary->ifTrue);
        generateExpression(ctx, f, scope, ternary->ifTrue);
        emitJumpTo(f, &endLabel);

        assert(ternary->ifFalse);
        bindLabel(f, &elseLabel);
        generateExpression(ctx, f, scope, ternary->ifFalse);

        bindLabel(f, &endLabel);
      }
      break;
    case E_CAST:
      generateCast(ctx, f, scope, &expression->castExpr);
      break;
    case EB_ADD:
    case EB_SUB:
    case EB_MUL:
    case EB_DIV:
    case EB_MOD:
    case EB_LHS: /** << */
    case EB_RHS: /** >> */
    case EB_AND:
    case EB_OR:
    case EB_XOR:
      generateBinary(ctx, f, expression, scope);
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
    case EB_ASG_DIV:
    case EB_ASG_MOD:
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
      emitMoveAR(f, &addr, R_ACC, typeSize);
      break;
    case EU_PLUS:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      break;
    case EU_MINUS:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      // TODO: fp
      if (isRealType(expression->type)) {
          switch (expression->type->descriptorDesc->typeId) {
           case T_F4:
              emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, FALSE);
              emitArithConst(f, OP_I_XOR, R_ACC, 1U << 31, typeSize);
              emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, FALSE);
              break;
           case T_F8:
              emitMovdq(f, 0x66, 0x0F, 0x7E, R_ACC, R_FACC, TRUE);
              emitMoveCR(f, 1ULL << 63, R_TMP, typeSize);
              emitArithRR(f, OP_I_XOR, R_ACC, R_TMP);
              emitMovdq(f, 0x66, 0x0F, 0x6E, R_ACC, R_FACC, TRUE);
              break;
           case T_F10:
              unreachable("long double arith is not implemented yet");
           default:
              unreachable("unexpected FP type");
            }
      } else {
        emitMoveRR(f, R_ACC, R_TMP, typeSize);
        emitZeroReg(f, R_ACC);
        emitArithRR(f, selectOpcode(EB_SUB, expression->type), R_ACC, R_TMP);
      }
      break;
    case EU_TILDA:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      emitBitwiseNot(f, R_ACC);
      break;
    case EU_EXL:
      generateExpression(ctx, f, scope, expression->unaryExpr.argument);
      emitNot(f, R_ACC, computeTypeSize(expression->unaryExpr.argument->type));
      break;
    case EU_POST_INC:
    case EU_POST_DEC:
      generatePostIncDec(ctx, f, scope, expression);
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
    case EF_DOT:
//      generateDotOperator(ctx, f, scope, &expression->fieldExpr);
//      break;
    case EF_ARROW:
      // TODO: // has to be lowered
//      break;
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

static void pushCalleeSaveRegisters(GeneratedFunction *f) {
  emitPushReg(f, R_R15);
  emitPushReg(f, R_R14);
  emitPushReg(f, R_R13);
  emitPushReg(f, R_R12);
  emitPushReg(f, R_EBX);
}

static void pushFrame(GenerationContext *ctx, GeneratedFunction *f, unsigned paramCount) {

  // pushq %rbp
  emitPushReg(f, R_EBP);
  // movq %rsp, %rbp
  emitMoveRR(f, R_ESP, R_EBP, sizeof(intptr_t));
  pushCalleeSaveRegisters(f);
}

static void popCalleSaveRegisters(GeneratedFunction *f) {
  emitPopReg(f, R_EBX);
  emitPopReg(f, R_R12);
  emitPopReg(f, R_R13);
  emitPopReg(f, R_R14);
  emitPopReg(f, R_R15);
}

static void popFrame(GenerationContext *ctx, GeneratedFunction *f) {
  // movq %rbp, %rsp
  // popq %rbp

  if (f->frameSize)
    emitArithConst(f, OP_L_ADD, R_ESP, f->frameSize, sizeof(intptr_t));
  popCalleSaveRegisters(f);
  emitPopReg(f, R_EBP);
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
//  TypeId condTypeId = stmt->condition->type->descriptorDesc->typeId;

  const enum Opcodes cmpOpcode = condTypeSize == 8 ? OP_L_CMP : OP_I_CMP;

  int i;

  for (i = 0; i < visited; ++i) {
      int64_t caseConst = caseLabels[i].caseConst;
      struct Label *caseLabel = &caseLabels[i].label;
      emitArithConst(f, cmpOpcode, R_ACC, caseConst, condTypeSize);
      emitCondJump(f, caseLabel, JC_EQ);
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

      enum Opcodes opcode = selectByType(left->type, OP_L_CMP, OP_F_CMP, OP_D_CMP, OP_I_CMP, OP_L_CMP);

      if (right->op == E_CONST) {
          uint64_t cnst = right->constExpr.i;
          emitArithConst(f, opcode, isFP ? R_FACC : R_ACC, cnst, opSize);
      } else {
          if (isFP)
            emitPushRegF(f, R_FACC, isD);
          else
            emitPushReg(f, R_ACC);

          if (right->op == E_NAMEREF || right->op == EU_DEREF) {
              Address addr = { 0 };
              translateAddress(ctx, f, scope, right->op == EU_DEREF ? right->unaryExpr.argument : right, &addr);
              if (isFP)
                emitPopRegF(f, R_FACC, isD);
              else
                emitPopReg(f, R_ACC);

              emitArithAR(f, opcode, isFP ? R_FACC : R_ACC, &addr, opSize);
          } else {
              generateExpression(ctx, f, scope, cond->binaryExpr.right);

              if (isFP)
                emitPopRegF(f, R_FTMP, isD);
              else
                emitPopReg(f, R_TMP);

              emitArithRR(f, opcode, isFP ? R_FTMP : R_TMP, isFP ? R_FACC : R_ACC);
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
//  generateExpression(ctx, f, scope, stmt->condition);

  if (stmt->elseBranch) {
    emitCondJump(f, &elseLabel, cc);
  } else {
    emitCondJump(f, &endLabel, cc);
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
      emitCondJump(f, &loopTail, cc);
  }

  assert(stmt->body);
  size_t frameSize = generateStatement(ctx, f, stmt->body, scope, frameOffset);

  if (doLoop) {
      bindLabel(f, &continueLabel);
      enum JumpCondition cc = generateCondition(ctx, f, scope, stmt->condition, FALSE);
      emitCondJump(f, &loopHead, cc);
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
    emitCondJump(f, &loopTail, cc);
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

        if (v->flags.bits.isLocal) {
            unsigned localOffset = frameOffset + typeSize;
            putToHashMap(ctx->localSymMap, (intptr_t)s, (intptr_t)(-localOffset));
            if (v->initializer) {
                emitLocalInitializer(ctx, f, scope, typeSize, -localOffset, v->initializer);
            }
        } else {
          assert(v->flags.bits.isStatic);
          GeneratedVariable *gv = generateVaribale(ctx, v);
          gv->next = ctx->file->variables;
          ctx->file->variables = gv;
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
          generateExpression(ctx, f, scope, retExpr);
          if (isRealType(retExpr->type)) {
              // make sure result is in xmm0
          } else if (isStructualType(retExpr->type)) {
              Address addr = { 0 };
              addr.base = R_EBP;
              addr.index = R_BAD;
              addr.imm = f->returnStructOffset;
              emitMoveAR(f, &addr, R_EDI, sizeof(intptr_t));
              Address src = { 0 }, dst = { 0 };
              src.index = dst.index = R_BAD;
              src.base = R_ACC;
              dst.base = R_EDI;

              copyStructTo(f, retExpr->type, &src, &dst);
          }
      }
      emitJumpTo(f, ctx->returnLabel);
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

static size_t computeLocalSizeForStatement(AstStatement *stmt);

static size_t computeLocalSizeForBlock(AstBlock *block) {
  AstStatementList *stmt = block->stmts;
  size_t result = 0;
  size_t currentScopeSize = 0;
  while (stmt) {
    size_t stmtSize = computeLocalSizeForStatement(stmt->stmt);
    if (stmt->stmt->statementKind == SK_DECLARATION) {
        currentScopeSize += stmtSize;
        result = currentScopeSize;
    } else {
        size_t scopeSize = currentScopeSize + stmtSize;
        result = max(scopeSize, result);
    }
    stmt = stmt->next;
  }

  return result;
}

static size_t computeLocalSizeForStatement(AstStatement *stmt) {
  size_t resulkt = 0;
  switch (stmt->statementKind) {
  case SK_BLOCK:
      return computeLocalSizeForBlock(&stmt->block);
  case SK_DECLARATION: {
      AstDeclaration *d = stmt->declStmt.declaration;
      if (d->kind == DK_VAR) {
        AstValueDeclaration *v = d->variableDeclaration;
        return computeTypeSize(v->type);
      }
  }
  case SK_EMPTY:
  case SK_EXPR_STMT:
  case SK_GOTO_L:
  case SK_GOTO_P:
  case SK_RETURN:
  case SK_BREAK:
  case SK_CONTINUE:
      break;
  case SK_LABEL:
      return computeLocalSizeForStatement(stmt->labelStmt.body);
  case SK_IF:
      return max(computeLocalSizeForStatement(stmt->ifStmt.thenBranch), stmt->ifStmt.elseBranch ? computeLocalSizeForStatement(stmt->ifStmt.elseBranch) : -1);
  case SK_SWITCH:
      return computeLocalSizeForStatement(stmt->switchStmt.body);
  case SK_WHILE:
  case SK_DO_WHILE:
      return computeLocalSizeForStatement(stmt->loopStmt.body);
  case SK_FOR:
      return computeLocalSizeForStatement(stmt->forStmt.body);
  default:
      unreachable("Unreachable");
      break;
  }

  return 0;
}

static size_t computeLocalsSize(AstFunctionDefinition *f) {
  return computeLocalSizeForStatement(f->body);
}

static GeneratedFunction *generateFunction(GenerationContext *ctx, AstFunctionDefinition *f) {
  HashMap *localSymbolMap = createHashMap(DEFAULT_MAP_CAPACITY, &symbolHashCode, &symbolEquals);

  size_t localsSize = computeLocalsSize(f);

  ctx->localSymMap = localSymbolMap;
  HashMap *labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
  ctx->labelMap = labelMap;

  assert(f->body->statementKind == SK_BLOCK);

  struct Label returnLabel = { 0 };

  ctx->returnLabel = &returnLabel;

  GeneratedFunction *gen = allocateGenFunction(ctx);

  gen->symbol = f->declaration->symbol;
  gen->name = f->declaration->name;

  TypeRef * returnType = f->declaration->returnType;
  AstValueDeclaration *p = f->declaration->parameters;
  unsigned intRegParams = 0;
  unsigned fpRegParams = 0;

  int stackParamAreaOffset = sizeof(intptr_t) + sizeof(intptr_t);
  int calleeSavedRegsArea = 5 * sizeof(intptr_t);
  int regsParamAreaOffset = calleeSavedRegsArea + localsSize;
  int regOffset = regsParamAreaOffset;
  int stackOffset = stackParamAreaOffset;

  gen->frameOffset = calleeSavedRegsArea;

  // TODO: variadic functions
  pushFrame(ctx, gen, f->declaration->parameterCount);

  Address addr = { 0 };
  addr.base = R_EBP;
  addr.index = R_BAD;

  if (isStructualType(returnType)) {
    regOffset += sizeof(intptr_t);
    addr.imm = gen->returnStructOffset = - regOffset;
    emitMoveRA(gen, intArgumentRegs[intRegParams], &addr, sizeof(intptr_t));
    ++intRegParams;
  }

  while (p) {
      assert(p->kind == VD_PARAMETER);
      const Symbol *s = (const Symbol *)getFromHashMap(f->scope->symbols, (intptr_t)p->name);
      TypeRef *type = p->type;
      assert(s);

      size_t typeSize = max(4, computeTypeSize(p->type));

      if (isRealType(type)) {
        if (fpRegParams >= R_FP_PARAM_COUNT) {
            stackOffset = ALIGN_SIZE(stackOffset, typeSize);
            putToHashMap(localSymbolMap, (intptr_t)s, (intptr_t)stackOffset);

            stackOffset += typeSize;
        } else {
            regOffset += typeSize;
            regOffset = ALIGN_SIZE(regOffset, typeSize);
            putToHashMap(localSymbolMap, (intptr_t)s, (intptr_t)(-regOffset));

            addr.imm = -regOffset;

            emitMovfpRA(gen, fpArgumentRegs[fpRegParams], &addr, typeSize > sizeof(float));

            ++fpRegParams;
        }
      } else {
        if (isStructualType(type) || intRegParams >= R_PARAM_COUNT) {
          stackOffset = ALIGN_SIZE(stackOffset, typeSize);
          putToHashMap(localSymbolMap, (intptr_t)s, (intptr_t)stackOffset);
          stackOffset += typeSize;
        } else {
          regOffset += typeSize;
          regOffset = ALIGN_SIZE(regOffset, typeSize);
          putToHashMap(localSymbolMap, (intptr_t)s, (intptr_t)(-regOffset));
          addr.imm = -regOffset;

          emitMoveRA(gen, intArgumentRegs[intRegParams], &addr, typeSize);
          ++intRegParams;
        }
      }

      p = p->next;
  }

  size_t alignedSize = ALIGN_SIZE(regOffset, 2 * sizeof(intptr_t));

  gen->frameSize = alignedSize - calleeSavedRegsArea;

  if (gen->frameSize)
    emitArithConst(gen, OP_L_SUB, R_ESP, gen->frameSize, sizeof(intptr_t));

  gen->argsSize = regOffset - regsParamAreaOffset;
  gen->localsSize = localsSize;

  generateBlock(ctx, gen, &f->body->block, 0);

  bindLabel(gen, &returnLabel);
  if (isStructualType(returnType)) {
      addr.base = R_EBP;
      addr.imm = gen->returnStructOffset;
      emitMoveAR(gen, &addr, R_EAX, sizeof(intptr_t));
  }
  popFrame(ctx, gen);
  emitReturn(ctx, gen);

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  ctx->labelMap = ctx->localSymMap = NULL;
  releaseHashMap(labelMap);
  releaseHashMap(localSymbolMap);

  if (ctx->parserContext->config->verbose) {
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

  ctx->returnLabel = NULL;


  return gen;
}

static writeObjFile(const char *sourceFileName, const char *objDir, uint8_t *buffer, size_t bufferSize) {
  size_t len = strlen(sourceFileName);
  size_t dirLen = objDir ? strlen(objDir) : 0;
  size_t bufferLen = dirLen + 1 + len + 1;
  char *outputName = alloca(bufferLen);

  unsigned i = 0;

  if (objDir) {
      strncpy(outputName, objDir, bufferLen);
      outputName[dirLen] = '/';
      i = dirLen + 1;
  }

  unsigned j = 0;

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