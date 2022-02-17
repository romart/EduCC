
#include <assert.h>

#include "common.h"
#include "parser.h"
#include "sema.h"

static AstExpression *transformExpression(ParserContext *ctx, AstExpression *expr);

static TypeRef *voidPtrType(ParserContext *ctx) {
  SpecifierFlags f = { 0 };

  return makePointedType(ctx, f, makePrimitiveType(ctx, T_VOID, 0));
}

static AstExpression *cannonizeArrayAccess(ParserContext *ctx, AstExpression *expr) {
  assert(expr->op == EB_A_ACC);
  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);
  AstExpression *base = isPointerLikeType(left->type) ? left : right;
  AstExpression *index = base == left ? right : left;
  TypeRef *arrayType = base->type;
  assert(isPointerLikeType(arrayType));
  TypeRef *pointerType = arrayType->kind == TR_ARRAY ? makePointedType(ctx, arrayType->flags, arrayType->arrayTypeDesc.elementType) : arrayType;
  TypeRef *elementType = pointerType->pointedTo;

  size_t elementSize = computeTypeSize(elementType);

  TypeRef *indexType = makePrimitiveType(ctx, T_U8, 0);

  // TODO: probably void* isn't the best choose, think about introducing an address type for that purpose
  base->type = voidPtrType(ctx); // we do some address arith here, first normalize pointer

  AstExpression *elemSizeConst = createAstConst(ctx, &expr->coordinates, CK_INT_CONST, &elementSize);
  elemSizeConst->type = indexType;
  AstExpression *indexValue = createBinaryExpression(ctx, EB_MUL, indexType, index, elemSizeConst);
  AstConst *evaluated = eval(ctx, indexValue);
  if (evaluated) {
      indexValue = createAstConst2(ctx, &indexValue->coordinates, indexValue->type, evaluated);
  }
  AstExpression *summed = createBinaryExpression(ctx, EB_ADD, pointerType, base, indexValue);
  AstExpression *derefered = createUnaryExpression(ctx, &expr->coordinates, EU_DEREF, summed);
  derefered->type = elementType;

  return derefered;
}

static AstExpression *cannonizeBinaryExpression(ParserContext *ctx, AstExpression *expr, Boolean isComute) {
  if (!isBinary(expr->op)) return expr;

  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;

  isComute = isCommute(expr->op);

  // 1 op x -> x op 1
  if (isComute && left->op == E_CONST) {
      AstExpression *t = left;
      left = right;
      right = t;
      expr->binaryExpr.left = left;
      expr->binaryExpr.right = right;
  }

  // (x op 1) op 2 -> x op (1 op 2)

  if (right->op == E_CONST) {
      if (left->op == expr->op) {
        AstExpression *x = left->binaryExpr.left;
        AstExpression *c1 = left->binaryExpr.right;
        AstExpression *c2 = right;
        if (c1->op == E_CONST) {
            expr->binaryExpr.left = x;
            left->binaryExpr.left = c1;
            left->binaryExpr.right = c2;
            AstConst *evaluated = eval(ctx, left);
            assert(evaluated);
            expr->binaryExpr.right = createAstConst2(ctx, &right->coordinates, right->type, evaluated);
            left = expr->binaryExpr.left;
            right = expr->binaryExpr.right;
        }
      }
  }


  // (x op 1) op y -> (x op y) op 1

  if (isComute && left->op == expr->op) {
      AstExpression *l_left = left->binaryExpr.left;
      AstExpression *l_right = left->binaryExpr.right;
      if (l_right->op == E_CONST) {
          left->binaryExpr.left = l_left;
          left->binaryExpr.right = right;
          left->coordinates.endOffset = right->coordinates.endOffset;
          expr->binaryExpr.right = l_right;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
      }
  }

  if (isAdditiveOp(expr->op)) {
      if (right->op == E_CONST) {
          if (right->constExpr.i == 0) {
              return left;
          }
      }
  }

  return expr;
}

static AstExpression *cannonizeSubExpression(ParserContext *ctx, AstExpression *expr);

static AstExpression *cannonizeAddExpression(ParserContext *ctx, AstExpression *expr) {
  if (expr->op != EB_ADD) return expr;

  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;

  // x + ptr -> ptr + x
  if (isPointerLikeType(right->type)) {
    expr->binaryExpr.left = right;
    expr->binaryExpr.right = left;
    left = expr->binaryExpr.left;
    right = expr->binaryExpr.right;
  }

  // (ptr + x) + y -> ptr + (x + y)
  if (left->op == expr->op) {
    AstExpression *l_left = left->binaryExpr.left;
    AstExpression *l_right = left->binaryExpr.right;
    if (isPointerLikeType(l_left->type)) {
        left->binaryExpr.left = l_right;
        left->binaryExpr.right = right;
        left->coordinates.startOffset = l_right->coordinates.startOffset;
        left->coordinates.endOffset = right->coordinates.endOffset;
        expr->binaryExpr.left = l_left;
        expr->binaryExpr.right = left;
        left = expr->binaryExpr.left;
        right = expr->binaryExpr.right;
    }
  }

  // (ptr - x) + y -> ptr - x + y -> ptr - (x - y)
  if (left->op == EB_SUB) {
    AstExpression *l_left = left->binaryExpr.left;
    AstExpression *l_right = left->binaryExpr.right;
    if (isPointerLikeType(l_left->type)) {
        expr->op = EB_SUB;
        left->binaryExpr.left = l_right;
        left->binaryExpr.right = right;
        left->type = right->type;
        left->op = EB_SUB;
        left->coordinates.startOffset = l_right->coordinates.startOffset;
        left->coordinates.endOffset = right->coordinates.endOffset;
        expr->binaryExpr.left = l_left;
        expr->binaryExpr.right = left;
        left = expr->binaryExpr.left;
        right = expr->binaryExpr.right;
    }
  }

  // x + (ptr + y) -> ptr + (x + y)
  if (expr->op == EB_ADD && right->op == expr->op) {
      AstExpression *r_left = right->binaryExpr.left;
      AstExpression *r_right = right->binaryExpr.right;
      if (isPointerLikeType(r_left->type)) {
          right->binaryExpr.left = left;
          right->type = left->type;
          right->coordinates.startOffset = left->coordinates.startOffset;
          expr->binaryExpr.left = r_left;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
      }
  }

  // x + (ptr - y) -> x + ptr - y -> ptr + (x - y)
  if (expr->op == EB_ADD && right->op == EB_SUB) {
      AstExpression *r_left = right->binaryExpr.left;
      AstExpression *r_right = right->binaryExpr.right;
      if (isPointerLikeType(r_left->type)) {
          TypeRef *l_type = left->type;
          right->binaryExpr.left = left;
          right->op = EB_SUB;
          right->type = l_type;
          right->coordinates.startOffset = left->coordinates.startOffset;
          expr->binaryExpr.left = r_left;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
      }
  }

  if (expr->op == EB_SUB)
    expr = cannonizeSubExpression(ctx, expr);

  return cannonizeBinaryExpression(ctx, expr, TRUE);
}

static AstExpression *cannonizeSubExpression(ParserContext *ctx, AstExpression *expr) {
  if (expr->op != EB_SUB) return expr;

  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;

  // x - -y -> x + y
  if (right->op == EU_MINUS) {
      expr->op = EB_ADD;
      expr->binaryExpr.right = right->unaryExpr.argument;
      return cannonizeAddExpression(ctx, expr);
  }

  // (ptr + x) - y -> ptr + (x - y)
  if (left->op == EB_ADD) {
    AstExpression *l_left = left->binaryExpr.left;
    AstExpression *l_right = left->binaryExpr.right;
    if (isPointerLikeType(l_left->type)) {
        left->binaryExpr.left = l_right;
        left->binaryExpr.right = right;
        left->op = EB_SUB;
        left->type = right->type;
        left->coordinates.startOffset = l_right->coordinates.startOffset;
        left->coordinates.endOffset = right->coordinates.endOffset;

        expr->binaryExpr.left = l_left;
        expr->binaryExpr.right = left;
        expr->op = EB_ADD;

        left = expr->binaryExpr.left;
        right = expr->binaryExpr.right;
    }
  }

  // (ptr - x) - y -> ptr - (x + y)
  if (expr->op == EB_SUB && left->op == EB_SUB) {
    AstExpression *l_left = left->binaryExpr.left;
    AstExpression *l_right = left->binaryExpr.right;
    if (isPointerLikeType(l_left->type)) {
        left->binaryExpr.left = l_right;
        left->binaryExpr.right = right;
        left->op = EB_ADD;
        left->type = right->type;

        left->coordinates.startOffset = l_right->coordinates.startOffset;
        left->coordinates.endOffset = right->coordinates.endOffset;

        expr->binaryExpr.left = l_left;
        expr->binaryExpr.right = left;

        left = expr->binaryExpr.left;
        right = expr->binaryExpr.right;
    }
  }

  if (expr->op == EB_ADD)
    expr = cannonizeAddExpression(ctx, expr);

  return cannonizeBinaryExpression(ctx, expr, TRUE);
}



static AstExpression *cannonizeFieldExpression(ParserContext *ctx, AstExpression *receiver, AstStructDeclarator *member, AstExpression *orig, Boolean deBit) {
  AstExpression *offset = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &member->offset);
  offset->type = makePrimitiveType(ctx, T_U8, 0);

  // normalize pointer
  receiver->type = voidPtrType(ctx);

  SpecifierFlags f = { 0 };
  AstExpression *offsetedPtr = createBinaryExpression(ctx, EB_ADD, receiver->type, receiver, offset);
  TypeRef *memberType = member->typeRef;
  TypeRef *ptrType = NULL;

  if (memberType->kind == TR_BITFIELD) {
      ptrType = makePointedType(ctx, f, memberType->bitFieldDesc.storageType);
  } else {
      ptrType = makePointedType(ctx, f, memberType);
  }

  offsetedPtr->type = ptrType;
  offsetedPtr = cannonizeAddExpression(ctx, offsetedPtr);

  AstExpression *derefered = createUnaryExpression(ctx, &orig->coordinates, EU_DEREF, offsetedPtr);
  derefered->type = ptrType->pointedTo;

  if (memberType->kind == TR_BITFIELD && deBit) {
      // de-bit
      TypeRef *storageType = memberType->bitFieldDesc.storageType;
      u_int64_t w = memberType->bitFieldDesc.width;
      u_int64_t mask = ~(~0LLu << w);
      u_int64_t s = memberType->bitFieldDesc.offset;

      // (x >> s) & mask

      AstExpression *shiftConst = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &s);
      shiftConst->type = makePrimitiveType(ctx, T_S4, 0);

      AstExpression *shifted = createBinaryExpression(ctx, EB_RHS, storageType, derefered, shiftConst);

      AstExpression *maskConst = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &mask);
      maskConst->type = storageType;

      return createBinaryExpression(ctx, EB_ANDAND, storageType, shifted, maskConst);
  }

  return derefered;
}

static AstExpression *cannonizeArrowExpression(ParserContext *ctx, AstExpression *expr, Boolean deBit) {
  assert(expr->op == EF_ARROW);
  AstExpression *receiver = transformExpression(ctx, expr->fieldExpr.recevier);
  return cannonizeFieldExpression(ctx, receiver, expr->fieldExpr.member, expr, deBit);
}

static AstExpression *cannonizeDotExpression(ParserContext *ctx, AstExpression *expr, Boolean deBit) {
  assert(expr->op == EF_DOT);

  AstExpression *receiver = transformExpression(ctx, expr->fieldExpr.recevier);

  if (receiver->op == E_CALL) {
      // keep it as is and desugar later in codegen
      expr->fieldExpr.recevier = receiver;
      return expr;
  }

  AstExpression *pReceiver = NULL;

  if (receiver->op == EU_DEREF) {
      pReceiver = receiver->unaryExpr.argument;
  } else {
      pReceiver = createUnaryExpression(ctx, &receiver->coordinates, EU_REF, receiver);
      pReceiver->type = makePointedType(ctx, receiver->type->flags, receiver->type);
  }


  return cannonizeFieldExpression(ctx, pReceiver, expr->fieldExpr.member, expr, deBit);

}

static ExpressionType swapOp(ExpressionType op) {
  switch (op) {
  case EB_LE: return EB_GE; // (x <= y) <==> (y >= x)
  case EB_LT: return EB_GE; // (x < y) <==>  (y > x)
  case EB_GE: return EB_GE; // (x >= y) <==> (y <= x)
  case EB_GT: return EB_GE; // (x > y) <==> (y < x)
  default: return op;
  }
}

static AstExpression *cannonizeRelativeExpression(ParserContext *ctx, AstExpression *expr) {
  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  // 1 op x -> x ^op 1
  if (left->op == E_CONST) {
      AstExpression *t = left;
      left = right;
      right = t;
      expr->binaryExpr.left = left;
      expr->binaryExpr.right = right;
      expr->op = swapOp(expr->op);
  }

  return expr;
}

static AstExpression *cannonizeAssignmentExpression(ParserContext *ctx, AstExpression *expr) {
  AstExpression *lhs = expr->binaryExpr.left;
  AstExpression *rhs = expr->binaryExpr.right;

  if (lhs->type->kind == TR_BITFIELD) {
      // we will properly de-sugar in codegen
      lhs->fieldExpr.recevier = transformExpression(ctx, lhs->fieldExpr.recevier);
  } else {
      lhs = transformExpression(ctx, lhs);
  }

  rhs = transformExpression(ctx, rhs);

  expr->binaryExpr.left = lhs;
  expr->binaryExpr.right = rhs;

  return expr;
}

static AstExpression *transformExpression(ParserContext *ctx, AstExpression *expr) {

  AstConst *evaluated = eval(ctx, expr);
  if (evaluated) {
      return createAstConst2(ctx, &expr->coordinates, expr->type, evaluated);
  }

  ExpressionType op2 = E_ERROR;
  AstExpressionList *args = NULL;

  switch (expr->op) {
    case E_TERNARY:
      expr->ternaryExpr.condition = transformExpression(ctx, expr->ternaryExpr.condition);
      expr->ternaryExpr.ifTrue = transformExpression(ctx, expr->ternaryExpr.ifTrue);
      expr->ternaryExpr.ifFalse = transformExpression(ctx, expr->ternaryExpr.ifFalse);
      break;
    case E_CAST:
      if (typesEquals(expr->castExpr.type, expr->castExpr.argument->type))
        return expr->castExpr.argument;
      break;
    case E_PAREN:
      return transformExpression(ctx, expr->parened);
    case E_CALL:
      // TODO:
      expr->callExpr.callee = transformExpression(ctx, expr->callExpr.callee);
      args = expr->callExpr.arguments;
      while (args) {
          args->expression = transformExpression(ctx, args->expression);
          args = args->next;
      }
      break;

    case EU_PRE_INC:
    case EU_POST_INC:
    case EU_PRE_DEC:
    case EU_POST_DEC:
      expr->unaryExpr.argument = transformExpression(ctx, expr->unaryExpr.argument);
      break;
    case EU_DEREF: op2 = EU_REF; goto dr;
    case EU_REF:   op2 = EU_DEREF;
    dr: {
        AstExpression *arg = transformExpression(ctx, expr->unaryExpr.argument);
        if (arg->op == op2) {
            // &*x -> x, *&x -> x
            return arg->unaryExpr.argument;
        } else expr->unaryExpr.argument = arg;
    }
    break;


    case EU_PLUS:
      return transformExpression(ctx, expr->unaryExpr.argument);
    case EU_MINUS:
    case EU_TILDA:
    case EU_EXL:
      expr->unaryExpr.argument = transformExpression(ctx, expr->unaryExpr.argument);
      if (expr->op != EU_EXL) { // !!f contains implicit coversion
          if (expr->unaryExpr.argument->op == expr->op) {
              // ~~x -> x; --x -> x
              return expr->unaryExpr.argument;
          }
      }
      break;
    case EB_ADD:
      return cannonizeAddExpression(ctx, expr);
    case EB_MUL:
    case EB_AND:
    case EB_OR:
    case EB_XOR:
    case EB_ANDAND:
    case EB_OROR:
    case EB_EQ:
    case EB_NE:
      return cannonizeBinaryExpression(ctx, expr, TRUE);
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE:
      return cannonizeRelativeExpression(ctx, expr);
    case EB_A_ACC:
      return cannonizeArrayAccess(ctx, expr);
    case EB_SUB:
      return cannonizeSubExpression(ctx, expr);
    case EB_MOD:
    case EB_DIV:
      return cannonizeBinaryExpression(ctx, expr, FALSE);
    case EB_LHS:
    case EB_RHS:
    case EB_COMMA:
      expr->binaryExpr.left = transformExpression(ctx, expr->binaryExpr.left);
      expr->binaryExpr.right = transformExpression(ctx, expr->binaryExpr.right);
      break;
    case EF_DOT:
      // o.a.b.c
      return cannonizeDotExpression(ctx, expr, TRUE);
    case EF_ARROW:
      // p->a->b->c
      return cannonizeArrowExpression(ctx, expr, TRUE);
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
      return cannonizeAssignmentExpression(ctx, expr);
    case E_LABEL_REF:
    case E_NAMEREF:
    case E_CONST:
      break;
    case E_ERROR:
    default:
      unreachable("ERROR EXPRESSION in not allowed in correct checked code");
  }
  return expr;
}

static AstInitializer *transformInitializer(ParserContext *ctx, AstInitializer *init) {
  if (init->kind == IK_EXPRESSION) {
      init->expression = transformExpression(ctx, init->expression);
  } else {
      AstInitializerList *inits = init->initializerList;
      while (inits) {
          inits->initializer = transformInitializer(ctx, inits->initializer);
          inits = inits->next;
      }
  }

  return init;
}

static AstStatement *transformStatement(ParserContext *ctx, AstStatement *stmt) {

  switch (stmt->statementKind) {
    case SK_BLOCK: {
        AstStatementList *stmts = stmt->block.stmts;
        while (stmts) {
            stmts->stmt = transformStatement(ctx, stmts->stmt);
            stmts = stmts->next;
        }
        break;
    }
    case SK_EXPR_STMT:
      stmt->exprStmt.expression = transformExpression(ctx, stmt->exprStmt.expression);
      break;
    case SK_LABEL:
      stmt->labelStmt.body = transformStatement(ctx, stmt->labelStmt.body);
      break;
    case SK_DECLARATION: {
        AstDeclaration *decl = stmt->declStmt.declaration;
        if (decl->kind == DK_VAR && decl->variableDeclaration->initializer) {
            decl->variableDeclaration->initializer = transformInitializer(ctx, decl->variableDeclaration->initializer);
        }
        break;
    }
    case SK_IF:
      stmt->ifStmt.condition = transformExpression(ctx, stmt->ifStmt.condition);
      stmt->ifStmt.thenBranch = transformStatement(ctx, stmt->ifStmt.thenBranch);
      if (stmt->ifStmt.elseBranch)
        stmt->ifStmt.elseBranch = transformStatement(ctx, stmt->ifStmt.elseBranch);
      break;
    case SK_SWITCH:
      stmt->switchStmt.condition = transformExpression(ctx, stmt->switchStmt.condition);
      stmt->switchStmt.body = transformStatement(ctx, stmt->switchStmt.body);
      break;
    case SK_WHILE:
      stmt->loopStmt.condition = transformExpression(ctx, stmt->loopStmt.condition);
      stmt->loopStmt.body = transformStatement(ctx, stmt->loopStmt.body);
      break;
    case SK_DO_WHILE:
      stmt->loopStmt.body = transformStatement(ctx, stmt->loopStmt.body);
      stmt->loopStmt.condition = transformExpression(ctx, stmt->loopStmt.condition);
      break;
    case SK_FOR:
      if (stmt->forStmt.initial)
        stmt->forStmt.initial = transformExpression(ctx, stmt->forStmt.initial);
      if (stmt->forStmt.condition)
        stmt->forStmt.condition = transformExpression(ctx, stmt->forStmt.condition);
      if (stmt->forStmt.modifier)
        stmt->forStmt.modifier = transformExpression(ctx, stmt->forStmt.modifier);
      stmt->forStmt.body = transformStatement(ctx, stmt->forStmt.body);
      break;

    case SK_GOTO_P:
      stmt->jumpStmt.expression = transformExpression(ctx, stmt->jumpStmt.expression);
      break;
    case SK_RETURN:
      if (stmt->jumpStmt.expression) {
          stmt->jumpStmt.expression = transformExpression(ctx, stmt->jumpStmt.expression);
      }
    case SK_EMPTY:
    case SK_GOTO_L:
    case SK_CONTINUE:
    case SK_BREAK:
      break;
    case SK_ERROR:
      unreachable("SK_ERROR should not exists in correct checked code");
  }

  return stmt;
}

void cannonizeAstFile(ParserContext *ctx, AstFile *file) {


  AstTranslationUnit *unit = file->units;

  while (unit) {
      if (unit->kind == TU_FUNCTION_DEFINITION) {
          AstFunctionDefinition *def = unit->definition;
          transformStatement(ctx, def->body);
      }
      unit = unit->next;
  }

}
