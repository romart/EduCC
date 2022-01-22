
#include "common.h"
#include "parser.h"
#include "tree.h"
#include "sema.h"

static Boolean derefIntConst(AstConst *astConst, int64_const_t *result) {
  if (astConst->op == CK_INT_CONST) {
      *result = astConst->i;
      return TRUE;
  }

  if (astConst->op == CK_SIZEOF) {
      *result = astConst->t->size;
      return TRUE;
  }

  return FALSE;
}

static Boolean derefFloatConst(AstConst *astConst, float64_const_t *result) {
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

static float64_const_t ee_f_u_minus(float64_const_t f) {
  return -f;
}

//static float64_const_t ee_f_u_tilda(float64_const_t f) {
//  return ~f;
//}

static float64_const_t ee_f_u_exl(float64_const_t f) {
  return !f;
}

static float64_const_t ee_f_u_inc(float64_const_t f) {
  return f + 1.0f;
}

static float64_const_t ee_f_u_dec(float64_const_t f) {
  return f - 1.0f;
}

// int, int -> int

static int64_const_t ee_i_b_plus(int64_const_t l, int64_const_t r) {
  return l + r;
}

static int64_const_t ee_i_b_minus(int64_const_t l, int64_const_t r) {
  return l - r;
}

static int64_const_t ee_i_b_mul(int64_const_t l, int64_const_t r) {
  return l * r;
}

static int64_const_t ee_i_b_div(int64_const_t l, int64_const_t r) {
  // TODO: handle r == 0
  return l / r;
}

static int64_const_t ee_i_b_mod(int64_const_t l, int64_const_t r) {
  // TODO: handle r == 0
  return l % r;
}

static int64_const_t ee_i_b_lhs(int64_const_t l, int64_const_t r) {
  return l << r;
}

static int64_const_t ee_i_b_rhs(int64_const_t l, int64_const_t r) {
  return l >> r;
}

static int64_const_t ee_i_b_and(int64_const_t l, int64_const_t r) {
  return l & r;
}

static int64_const_t ee_i_b_or(int64_const_t l, int64_const_t r) {
  return l | r;
}

static int64_const_t ee_i_b_xor(int64_const_t l, int64_const_t r) {
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
  return l != r;
}

static int64_const_t ee_i_b_lt(int64_const_t l, int64_const_t r) {
  return l < r;
}

static int64_const_t ee_i_b_le(int64_const_t l, int64_const_t r) {
  return l <= r;
}

static int64_const_t ee_i_b_gt(int64_const_t l, int64_const_t r) {
  return l > r;
}

static int64_const_t ee_i_b_ge(int64_const_t l, int64_const_t r) {
  return l >= r;
}

// float, float -> float

static float64_const_t ee_f_b_plus(float64_const_t l, float64_const_t r) {
  return l + r;
}

static float64_const_t ee_f_b_minus(float64_const_t l, float64_const_t r) {
  return l - r;
}

static float64_const_t ee_f_b_mul(float64_const_t l, float64_const_t r) {
  return l * r;
}

static float64_const_t ee_f_b_div(float64_const_t l, float64_const_t r) {
  return l / r;
}

static float64_const_t ee_f_b_andand(float64_const_t l, float64_const_t r) {
  return l && r;
}

static float64_const_t ee_f_b_oror(float64_const_t l, float64_const_t r) {
  return l || r;
}

static float64_const_t ee_f_b_eq(float64_const_t l, float64_const_t r) {
  return l == r;
}

static float64_const_t ee_f_b_ne(float64_const_t l, float64_const_t r) {
  return l != r;
}

static float64_const_t ee_f_b_lt(float64_const_t l, float64_const_t r) {
  return l < r;
}

static float64_const_t ee_f_b_le(float64_const_t l, float64_const_t r) {
  return l <= r;
}

static float64_const_t ee_f_b_gt(float64_const_t l, float64_const_t r) {
  return l > r;
}

static float64_const_t ee_f_b_ge(float64_const_t l, float64_const_t r) {
  return l >= r;
}

static int no_checks(int64_const_t l, int64_const_t r) {
  return TRUE;
}

static int div_check(int64_const_t l, int64_const_t r) {
  return r != 0;
}

typedef float64_const_t (*float_unary_evaluate)(float64_const_t);
typedef int64_const_t (*int_unary_evaluate)(int64_const_t);

typedef float64_const_t (*float_binary_evaluate)(float64_const_t, float64_const_t);
typedef int64_const_t (*int_binary_evaluate)(int64_const_t, int64_const_t);

typedef int (*evaluateChecker)(int64_const_t, int64_const_t);

static AstConst *evaluateUnaryConst(ParserContext *ctx, AstConst *expr, int_unary_evaluate eInt, float_unary_evaluate eFloat) {
  if (expr->op == CK_FLOAT_CONST) {
      if (eFloat == NULL) return NULL; // cannot evaluate
      float64_const_t v = eFloat(expr->f);
      return &createAstConst(ctx, -1, -1, expr->op, &v)->constExpr;
  } else {
      int64_const_t v = 0;
      derefIntConst(expr, &v);
      v = eInt(v);
      return &createAstConst(ctx, -1, -1, expr->op, &v)->constExpr;
  }
}

static AstConst *evaluateBinaryConst(ParserContext *ctx, AstConst *left, AstConst *right, evaluateChecker checker, int_binary_evaluate eInt, float_binary_evaluate eFloat) {
  if (left->op == CK_FLOAT_CONST || right->op == CK_FLOAT_CONST) {
      if (eFloat == NULL) return NULL; // cannot evaluate
      float64_const_t lv, rv;
      sint64_const_t iv = 0;
      if (left->op == CK_FLOAT_CONST && right->op == CK_FLOAT_CONST) {
          lv = left->f;
          rv = right->f;
      } else if (left->op != CK_FLOAT_CONST) {
          derefIntConst(left, (int64_const_t *)&iv);
          lv = (float64_const_t)iv;
          rv = right->f;
      } else {
          derefIntConst(right, (int64_const_t *)&iv);
          lv = left->f;
          rv = (float64_const_t)iv;
      }
      float64_const_t v = eFloat(lv, rv);
      return &createAstConst(ctx, -1, -1, CK_FLOAT_CONST, &v)->constExpr;
  } else {
      int64_const_t lv = 0, rv = 0;
      derefIntConst(left, &lv);
      derefIntConst(right, &rv);
      if (checker(lv, rv)) {
        int64_const_t v = eInt(lv, rv);
        return &createAstConst(ctx, -1, -1, CK_INT_CONST, &v)->constExpr;
      }
      return NULL;
  }
}

AstConst* eval(ParserContext *ctx, AstExpression* expression) {
  int64_const_t ic;
  float64_const_t fc;
  float f = 4.2f;
  float f2;
  int ii;
  ExpressionType op = expression->op;


  int_unary_evaluate un_i_eval = NULL;
  float_unary_evaluate un_f_eval = NULL;
  int_binary_evaluate bin_i_eval = NULL;
  float_binary_evaluate bin_f_eval = NULL;
  evaluateChecker chk_eval = no_checks;

  switch (op) {
    case E_CONST: return &expression->constExpr;
    case EB_COMMA:
      return eval(ctx, expression->binaryExpr.right);
    case EB_ADD: bin_i_eval = ee_i_b_plus; bin_f_eval = ee_f_b_plus; goto binary;
    case EB_SUB: bin_i_eval = ee_i_b_minus; bin_f_eval = ee_f_b_minus; goto binary;
    case EB_MUL: bin_i_eval = ee_i_b_mul; bin_f_eval = ee_f_b_mul; goto binary;
    case EB_DIV: bin_i_eval = ee_i_b_div; bin_f_eval = ee_f_b_div; chk_eval = div_check; goto binary;
    case EB_MOD: bin_i_eval = ee_i_b_mod; chk_eval = div_check; goto binary; // only int
    case EB_LHS: bin_i_eval = ee_i_b_lhs; goto binary; // only int
    case EB_RHS: bin_i_eval = ee_i_b_rhs; goto binary; // only int
    case EB_AND: bin_i_eval = ee_i_b_and; goto binary; // only int
    case EB_OR:  bin_i_eval = ee_i_b_or;  goto binary;// only int
    case EB_XOR: bin_i_eval = ee_i_b_xor; goto binary; // only int
    case EB_ANDAND: bin_i_eval = ee_i_b_andand; bin_f_eval = ee_f_b_andand; goto binary;
    case EB_OROR:   bin_i_eval = ee_i_b_oror; bin_f_eval = ee_f_b_oror; goto binary;
    case EB_EQ: bin_i_eval = ee_i_b_eq; bin_f_eval = ee_f_b_eq; goto binary;
    case EB_NE: bin_i_eval = ee_i_b_ne; bin_f_eval = ee_f_b_ne; goto binary;
    case EB_LT: bin_i_eval = ee_i_b_lt; bin_f_eval = ee_f_b_lt; goto binary;
    case EB_LE: bin_i_eval = ee_i_b_le; bin_f_eval = ee_f_b_le; goto binary;
    case EB_GT: bin_i_eval = ee_i_b_gt; bin_f_eval = ee_f_b_gt; goto binary;
    case EB_GE: bin_i_eval = ee_i_b_ge; bin_f_eval = ee_f_b_ge; goto binary;
    binary: {
      AstConst *left = eval(ctx, expression->binaryExpr.left);
      if (left == NULL) return NULL;
      AstConst *right = eval(ctx, expression->binaryExpr.right);
      if (right == NULL) return NULL;
      return evaluateBinaryConst(ctx, left, right, chk_eval, bin_i_eval, bin_f_eval);
    }
    case E_TERNARY: {
        AstConst *cond = eval(ctx, expression->ternaryExpr.condition);
        if (cond) {
            int cond_v;
            switch (cond->op) {
              case CK_INT_CONST: cond_v = (int)cond->i; break;
              case CK_FLOAT_CONST: cond_v = (int)cond->f; break;
              case CK_STRING_LITERAL: cond_v = TRUE; break;
              case CK_SIZEOF: cond_v = (int)cond->t->size; break;
              default: unreachable("Const evaluation error"); return NULL;
            }
            return eval(ctx, cond_v ? expression->ternaryExpr.ifTrue : expression->ternaryExpr.ifFalse);
        }
        return NULL;
    }
    case E_CAST:
      // TODO: not supported yet
      return NULL;
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
    case EU_SIZEOF:
      ic = computeTypeSize(ctx, expression->unaryExpr.argument->type);
      return &createAstConst(ctx, -1, -1, CK_INT_CONST, &ic)->constExpr;
    case EB_ASSIGN:
    case EB_RIGHT_ASSIGN:
    case EB_LEFT_ASSIGN:
    case EB_ADD_ASSIGN:
    case EB_SUB_ASSIGN:
    case EB_MUL_ASSIGN:
    case EB_DIV_ASSIGN:
    case EB_MOD_ASSIGN:
    case EB_AND_ASSIGN:
    case EB_XOR_ASSIGN:
    case EB_OR_ASSIGN:
      return NULL; // cannot evaluate assignemt
    case EF_DOT:
    case EF_ARROW:
      return NULL; // field expressions are not evaluatable either
    case E_CALL:
    case E_NAMEREF:
      return NULL; // same for call foo() and nameref
    case EU_DEREF:
    case EU_REF:
      return NULL; // ref/deref is not supported yet;
    case E_ERROR:
      return NULL; // error cannot be evaluated
  }
}
