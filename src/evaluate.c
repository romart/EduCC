
#include <stddef.h>
#include "common.h"
#include "parser.h"
#include "tree.h"
#include "sema.h"

static Boolean derefIntConst(AstConst *astConst, int64_const_t *result) {
  if (astConst->op == CK_INT_CONST) {
      *result = astConst->i;
      return TRUE;
  }

  return FALSE;
}

static Boolean derefFloatConst(AstConst *astConst, float80_const_t *result) {
  if (astConst->op == CK_FLOAT_CONST) {
      *result = astConst->f;
      return TRUE;
  }
  return FALSE;
}

// int -> int

static int64_const_t ee_i_u_minus(int64_const_t i) {
  return -i;
}

static int64_const_t ee_i_u_tilda(int64_const_t i) {
  return ~i;
}

static int64_const_t ee_i_u_exl(int64_const_t i) {
  return !i;
}

static int64_const_t ee_i_u_inc(int64_const_t i) {
  return i + 1;
}

static int64_const_t ee_i_u_dec(int64_const_t i) {
  return i - 1;
}

// float -> float

static float80_const_t ee_f_u_minus(float80_const_t f) {
  return -f;
}

//static float80_const_t ee_f_u_tilda(float80_const_t f) {
//  return ~f;
//}

static float80_const_t ee_f_u_exl(float80_const_t f) {
  return !f;
}

static float80_const_t ee_f_u_inc(float80_const_t f) {
  return f + 1.0f;
}

static float80_const_t ee_f_u_dec(float80_const_t f) {
  return f - 1.0f;
}

// int, int -> int

static int64_const_t ee_i_b_plus_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l + (sint64_const_t)r;
}

static int64_const_t ee_i_b_minus_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l - (sint64_const_t)r;
}

static int64_const_t ee_i_b_mul_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l * (sint64_const_t)r;
}

static int64_const_t ee_i_b_div_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l / (sint64_const_t)r;
}

static int64_const_t ee_i_b_mod_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l % (sint64_const_t)r;
}

static int64_const_t ee_i_b_lhs_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l << (sint64_const_t)r;
}

static int64_const_t ee_i_b_rhs_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l >> (sint64_const_t)r;
}

static int64_const_t ee_i_b_and_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l & (sint64_const_t)r;
}

static int64_const_t ee_i_b_or_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l | (sint64_const_t)r;
}

static int64_const_t ee_i_b_xor_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l ^ (sint64_const_t)r;
}

// unsigned, unsigned -> unsigned

static int64_const_t ee_i_b_plus_u(int64_const_t l, int64_const_t r) {
  return l + r;
}

static int64_const_t ee_i_b_minus_u(int64_const_t l, int64_const_t r) {
  return l - r;
}

static int64_const_t ee_i_b_mul_u(int64_const_t l, int64_const_t r) {
  return l * r;
}

static int64_const_t ee_i_b_div_u(int64_const_t l, int64_const_t r) {
  return l / r;
}

static int64_const_t ee_i_b_mod_u(int64_const_t l, int64_const_t r) {
  return l % r;
}

static int64_const_t ee_i_b_lhs_u(int64_const_t l, int64_const_t r) {
  return l << r;
}

static int64_const_t ee_i_b_rhs_u(int64_const_t l, int64_const_t r) {
  return l >> r;
}

static int64_const_t ee_i_b_and_u(int64_const_t l, int64_const_t r) {
  return l & r;
}

static int64_const_t ee_i_b_or_u(int64_const_t l, int64_const_t r) {
  return l | r;
}

static int64_const_t ee_i_b_xor_u(int64_const_t l, int64_const_t r) {
  return l ^ r;
}


static int64_const_t ee_i_b_andand(int64_const_t l, int64_const_t r) {
  return l && r;
}

static int64_const_t ee_i_b_oror(int64_const_t l, int64_const_t r) {
  return l || r;
}

static int64_const_t ee_i_b_eq(int64_const_t l, int64_const_t r) {
  return l == r;
}

static int64_const_t ee_i_b_ne(int64_const_t l, int64_const_t r) {
  int rs = l != r;
  return rs;
}

static int64_const_t ee_i_b_lt_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l < (sint64_const_t)r;
}

static int64_const_t ee_i_b_le_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l <= (sint64_const_t)r;
}

static int64_const_t ee_i_b_gt_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l > (sint64_const_t)r;
}

static int64_const_t ee_i_b_ge_s(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l >= (sint64_const_t)r;
}

static int64_const_t ee_i_b_lt_u(int64_const_t l, int64_const_t r) {
  return (sint64_const_t)l < (sint64_const_t)r;
}

static int64_const_t ee_i_b_le_u(int64_const_t l, int64_const_t r) {
  return l <= r;
}

static int64_const_t ee_i_b_gt_u(int64_const_t l, int64_const_t r) {
  return l > r;
}

static int64_const_t ee_i_b_ge_u(int64_const_t l, int64_const_t r) {
  return l >= r;
}

// float, float -> float

static float80_const_t ee_f_b_plus(float80_const_t l, float80_const_t r) {
  return l + r;
}

static float80_const_t ee_f_b_minus(float80_const_t l, float80_const_t r) {
  return l - r;
}

static float80_const_t ee_f_b_mul(float80_const_t l, float80_const_t r) {
  return l * r;
}

static float80_const_t ee_f_b_div(float80_const_t l, float80_const_t r) {
  return l / r;
}

static int64_const_t ee_f_b_andand(float80_const_t l, float80_const_t r) {
  return l && r;
}

static int64_const_t ee_f_b_oror(float80_const_t l, float80_const_t r) {
  return l || r;
}

static int64_const_t ee_f_b_eq(float80_const_t l, float80_const_t r) {
  return l == r;
}

static int64_const_t ee_f_b_ne(float80_const_t l, float80_const_t r) {
  return l != r;
}

static int64_const_t ee_f_b_lt(float80_const_t l, float80_const_t r) {
  return l < r;
}

static int64_const_t ee_f_b_le(float80_const_t l, float80_const_t r) {
  return l <= r;
}

static int64_const_t ee_f_b_gt(float80_const_t l, float80_const_t r) {
  return l > r;
}

static int64_const_t ee_f_b_ge(float80_const_t l, float80_const_t r) {
  return l >= r;
}

static int no_checks(int64_const_t l, int64_const_t r) {
  return TRUE;
}

static int div_check(int64_const_t l, int64_const_t r) {
  return r != 0;
}

typedef float80_const_t (*float_unary_evaluate)(float80_const_t);
typedef int64_const_t (*int_unary_evaluate)(int64_const_t);

typedef float80_const_t (*float_binary_evaluate)(float80_const_t, float80_const_t);
typedef int64_const_t (*float2int_binary_evaluate)(float80_const_t, float80_const_t);
typedef int64_const_t (*int_binary_evaluate)(int64_const_t, int64_const_t);

typedef int (*evaluateChecker)(int64_const_t, int64_const_t);

static Coordinates noCoords = { NULL, NULL };

static AstConst *evaluateUnaryConst(ParserContext *ctx, AstConst *expr, int_unary_evaluate eInt, float_unary_evaluate eFloat) {
  if (expr->op == CK_FLOAT_CONST) {
      if (eFloat == NULL) return NULL; // cannot evaluate
      float80_const_t v = eFloat(expr->f);
      return &createAstConst(ctx, &noCoords, expr->op, &v)->constExpr;
  } else {
      int64_const_t v = 0;
      derefIntConst(expr, &v);
      v = eInt(v);
      return &createAstConst(ctx, &noCoords, expr->op, &v)->constExpr;
  }
}

static AstConst *evaluateBinaryCondConst(ParserContext *ctx, AstConst *left, AstConst *right, evaluateChecker checker, int_binary_evaluate eInt, float2int_binary_evaluate eFloat) {
  if (left->op == CK_FLOAT_CONST || right->op == CK_FLOAT_CONST) {
      if (eFloat == NULL) return NULL; // cannot evaluate
      float80_const_t lv, rv;
      sint64_const_t iv = 0;
      if (left->op == CK_FLOAT_CONST && right->op == CK_FLOAT_CONST) {
          lv = left->f;
          rv = right->f;
      } else if (left->op != CK_FLOAT_CONST) {
          derefIntConst(left, (int64_const_t *)&iv);
          lv = (float80_const_t)iv;
          rv = right->f;
      } else {
          derefIntConst(right, (int64_const_t *)&iv);
          lv = left->f;
          rv = (float80_const_t)iv;
      }
      int64_const_t v = eFloat(lv, rv);
      return &createAstConst(ctx, &noCoords, CK_INT_CONST, &v)->constExpr;
  } else {
      int64_const_t lv = 0, rv = 0;
      derefIntConst(left, &lv);
      derefIntConst(right, &rv);
      if (checker(lv, rv)) {
        int64_const_t v = eInt(lv, rv);
        return &createAstConst(ctx, &noCoords, CK_INT_CONST, &v)->constExpr;
      }
      return NULL;
  }
}

static AstConst *evaluateBinaryConst(ParserContext *ctx, AstConst *left, AstConst *right, evaluateChecker checker, int_binary_evaluate eInt, float_binary_evaluate eFloat) {
  if (left->op == CK_FLOAT_CONST || right->op == CK_FLOAT_CONST) {
      if (eFloat == NULL) return NULL; // cannot evaluate
      float80_const_t lv, rv;
      sint64_const_t iv = 0;
      if (left->op == CK_FLOAT_CONST && right->op == CK_FLOAT_CONST) {
          lv = left->f;
          rv = right->f;
      } else if (left->op != CK_FLOAT_CONST) {
          derefIntConst(left, (int64_const_t *)&iv);
          lv = (float80_const_t)iv;
          rv = right->f;
      } else {
          derefIntConst(right, (int64_const_t *)&iv);
          lv = left->f;
          rv = (float80_const_t)iv;
      }
      float80_const_t v = eFloat(lv, rv);
      return &createAstConst(ctx, &noCoords, CK_FLOAT_CONST, &v)->constExpr;
  } else {
      int64_const_t lv = 0, rv = 0;
      derefIntConst(left, &lv);
      derefIntConst(right, &rv);
      if (checker(lv, rv)) {
        int64_const_t v = eInt(lv, rv);
        return &createAstConst(ctx, &noCoords, CK_INT_CONST, &v)->constExpr;
      }
      return NULL;
  }
}

static AstConst* evalCast(ParserContext *ctx, TypeRef *toType, AstConst *arg) {
  if (toType->kind == TR_POINTED) {
      // TODO: think about const address representation
      arg->i = (intptr_t)arg->i;
      return arg;
  }

  if (toType->kind == TR_VALUE) {
     TypeDesc *desc = toType->descriptorDesc;
     switch (desc->typeId) {
       case T_S1:
         arg->i = arg->op == CK_FLOAT_CONST ? (int8_t)arg->f : (int8_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_S2:
         arg->i = arg->op == CK_FLOAT_CONST ? (int16_t)arg->f : (int16_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_S4:
         arg->i = arg->op == CK_FLOAT_CONST ? (int32_t)arg->f : (int32_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_S8:
         arg->i = arg->op == CK_FLOAT_CONST ? (int64_t)arg->f : (int64_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_U1:
         arg->i = arg->op == CK_FLOAT_CONST ? (uint8_t)arg->f : (uint8_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_U2:
         arg->i = arg->op == CK_FLOAT_CONST ? (uint16_t)arg->f : (uint16_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_U4:
         arg->i = arg->op == CK_FLOAT_CONST ? (uint32_t)arg->f : (uint32_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_U8:
         // TODO: support ul <-> fp conversions
         arg->i = arg->op == CK_FLOAT_CONST ? (int64_t)arg->f : (uint64_t)arg->i;
         arg->op = CK_INT_CONST;
         break;
       case T_F4:
         arg->f = arg->op == CK_FLOAT_CONST ? (float)arg->f : (float)((int64_t)arg->i);
         arg->op = CK_FLOAT_CONST;
         break;
       case T_F8:
         arg->f = arg->op == CK_FLOAT_CONST ? (double)arg->f : (double)((int64_t)arg->i);
         arg->op = CK_FLOAT_CONST;
         break;
       case T_F10:
         arg->f = arg->op == CK_FLOAT_CONST ? (long double)arg->f : (long double)((int64_t)arg->i);
         arg->op = CK_FLOAT_CONST;
         break;
       default:
         return NULL;
     }
  }

  return arg;
}

AstConst* eval(ParserContext *ctx, AstExpression* expression) {

  if (isErrorType(expression->type)) return NULL; // cannot evaluate error expression

  int64_const_t ic;
  float80_const_t fc;
  float f = 4.2f;
  float f2;
  int ii;
  ExpressionType op = expression->op;
  int_unary_evaluate un_i_eval = NULL;
  float_unary_evaluate un_f_eval = NULL;
  int_binary_evaluate bin_i_eval = NULL;
  float_binary_evaluate bin_f_eval = NULL;
  float2int_binary_evaluate bin_f2i_eval = NULL;
  evaluateChecker chk_eval = no_checks;

  Boolean isU = isUnsignedType(expression->type);
  Boolean isDiv = FALSE;

  switch (op) {
    case E_CONST: return &expression->constExpr;
    case EB_COMMA:
      return eval(ctx, expression->binaryExpr.right);
    case EB_ADD: bin_i_eval = isU ? ee_i_b_plus_u : ee_i_b_plus_s; bin_f_eval = ee_f_b_plus; goto binary;
    case EB_SUB: bin_i_eval = isU ? ee_i_b_minus_u : ee_i_b_minus_s; bin_f_eval = ee_f_b_minus; goto binary;
    case EB_MUL: bin_i_eval = isU ? ee_i_b_mul_u : ee_i_b_mul_s; bin_f_eval = ee_f_b_mul; goto binary;
    case EB_DIV: bin_i_eval = isU ? ee_i_b_div_u : ee_i_b_div_s; bin_f_eval = ee_f_b_div; chk_eval = div_check; isDiv = TRUE; goto binary;
    case EB_MOD: bin_i_eval = isU ? ee_i_b_mod_u : ee_i_b_mod_s; chk_eval = div_check; isDiv = TRUE; goto binary; // only int
    case EB_LHS: bin_i_eval = isU ? ee_i_b_lhs_u : ee_i_b_lhs_s; goto binary; // only int
    case EB_RHS: bin_i_eval = isU ? ee_i_b_rhs_u : ee_i_b_rhs_s; goto binary; // only int
    case EB_AND: bin_i_eval = isU ? ee_i_b_and_u : ee_i_b_and_s; goto binary; // only int
    case EB_OR:  bin_i_eval = isU ? ee_i_b_or_u : ee_i_b_or_s;  goto binary;// only int
    case EB_XOR: bin_i_eval = isU ? ee_i_b_xor_u : ee_i_b_xor_s; goto binary; // only int
    binary: {
      AstConst *left = eval(ctx, expression->binaryExpr.left);
      if (left == NULL) return NULL;
      AstConst *right = eval(ctx, expression->binaryExpr.right);
      if (right == NULL) return NULL;
      if (isDiv && right->op == CK_INT_CONST && right->i == 0) {
          // Do not evaluate division by zero
          AstExpression *oldLeft = expression->binaryExpr.left;
          expression->binaryExpr.left = createAstConst2(ctx, &oldLeft->coordinates, oldLeft->type, left);
          AstExpression *oldRight = expression->binaryExpr.right;
          expression->binaryExpr.right = createAstConst2(ctx, &oldRight->coordinates, oldRight->type, right);
          return NULL;
      }
      return evaluateBinaryConst(ctx, left, right, chk_eval, bin_i_eval, bin_f_eval);
    }
    case EB_ANDAND: bin_i_eval = ee_i_b_andand; bin_f2i_eval = ee_f_b_andand; goto cond;
    case EB_OROR:   bin_i_eval = ee_i_b_oror; bin_f2i_eval = ee_f_b_oror; goto cond;
    case EB_EQ: bin_i_eval = ee_i_b_eq; bin_f2i_eval = ee_f_b_eq; goto cond;
    case EB_NE: bin_i_eval = ee_i_b_ne; bin_f2i_eval = ee_f_b_ne; goto cond;
    case EB_LT: bin_i_eval = isU ? ee_i_b_lt_u : ee_i_b_lt_s; bin_f2i_eval = ee_f_b_lt; goto cond;
    case EB_LE: bin_i_eval = isU ? ee_i_b_le_u : ee_i_b_le_s; bin_f2i_eval = ee_f_b_le; goto cond;
    case EB_GT: bin_i_eval = isU ? ee_i_b_gt_u : ee_i_b_gt_s; bin_f2i_eval = ee_f_b_gt; goto cond;
    case EB_GE: bin_i_eval = isU ? ee_i_b_ge_u : ee_i_b_ge_s; bin_f2i_eval = ee_f_b_ge; goto cond;
    cond: {
        AstConst *left = eval(ctx, expression->binaryExpr.left);
        if (left == NULL) return NULL;
        AstConst *right = eval(ctx, expression->binaryExpr.right);
        if (right == NULL) return NULL;
        return evaluateBinaryCondConst(ctx, left, right, chk_eval, bin_i_eval, bin_f2i_eval);
    }
    case E_TERNARY: {
        AstConst *cond = eval(ctx, expression->ternaryExpr.condition);
        if (cond) {
            int cond_v;
            switch (cond->op) {
              case CK_INT_CONST: cond_v = (int)cond->i; break;
              case CK_FLOAT_CONST: cond_v = (int)cond->f; break;
              case CK_STRING_LITERAL: cond_v = TRUE; break;
              default: unreachable("Const evaluation error"); return NULL;
            }
            return eval(ctx, cond_v ? expression->ternaryExpr.ifTrue : expression->ternaryExpr.ifFalse);
        }
        return NULL;
    }
    case E_PAREN:
      return eval(ctx, expression->parened);
    case E_CAST: {
      AstConst *arg = eval(ctx, expression->castExpr.argument);
      if (arg == NULL) return NULL;
      return evalCast(ctx, expression->castExpr.type, arg);
    }
    case EU_POST_INC:
    case EU_POST_DEC:
    case EU_PLUS:
    case EU_PRE_INC: un_i_eval = ee_i_u_inc; un_f_eval = ee_f_u_inc; goto unary;
    case EU_PRE_DEC: un_i_eval = ee_i_u_dec; un_f_eval = ee_f_u_dec; goto unary;
    case EU_MINUS:   un_i_eval = ee_i_u_minus; un_f_eval = ee_f_u_minus; goto unary;
    case EU_TILDA:   un_i_eval = ee_i_u_tilda; goto unary;// only for int
    case EU_EXL:     un_i_eval = ee_i_u_exl; un_f_eval = ee_f_u_exl; goto unary;
    unary: {
      AstConst *expr = eval(ctx, expression->unaryExpr.argument);
      if (expr) {
        if (op == EU_POST_INC || op == EU_POST_DEC || op == EU_PLUS)
          return expr;
        else {
          return evaluateUnaryConst(ctx, expr, un_i_eval, un_f_eval);
        }
      }
      return NULL;
    }
    case EB_A_ACC:
      return NULL; // cannot evaluate array access
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
      return NULL; // cannot evaluate assignemt
    case EF_DOT:
      return NULL;
    case EF_ARROW: {
      AstConst *c = eval(ctx, expression->fieldExpr.recevier);
      if (c) {
          c->i += expression->fieldExpr.member->offset;
      }
      return c;
    }
    case E_CALL:
    case E_NAMEREF:
      return NULL; // same for call foo() and nameref
    case E_LABEL_REF:
      return NULL; // no idea how to evaluate this
    case EU_REF:
      return eval(ctx, expression->unaryExpr.argument);
    case EU_DEREF:
      return NULL; // ref/deref is not supported yet;
    case E_ERROR:
    default:
      return NULL; // error cannot be evaluated
  }
}
