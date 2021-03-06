
#include <assert.h>

#include "common.h"
#include "parser.h"
#include "sema.h"

static AstExpression *transformExpression(ParserContext *ctx, AstExpression *expr);
static AstStatement *transformStatement(ParserContext *ctx, AstStatement *stmt);
static AstInitializer *transformInitializer(ParserContext *ctx, AstInitializer *init);

static TypeRef *voidPtrType(ParserContext *ctx) {
  return makePointedType(ctx, 0U, makePrimitiveType(ctx, T_VOID, 0));
}

static AstExpression *cannonizeArrayAccess(ParserContext *ctx, AstExpression *expr) {
  assert(expr->op == EB_A_ACC);

  AstExpression *left = expr->binaryExpr.left;
  AstExpression *right = expr->binaryExpr.right;

  AstExpression *base = isPointerLikeType(left->type) ? left : right;
  AstExpression *index = base == left ? right : left;

  TypeRef *arrayType = base->type;
  assert(isPointerLikeType(arrayType));
  TypeRef *pointerType = NULL;

  if (arrayType->kind == TR_ARRAY) {
      pointerType = makePointedType(ctx, arrayType->flags.storage, arrayType->arrayTypeDesc.elementType);
  } else if (arrayType->kind == TR_VLA) {
      pointerType = makePointedType(ctx, arrayType->flags.storage, arrayType->vlaDescriptor.elementType);
  } else {
      pointerType = arrayType;
  }

  TypeRef *elementType = pointerType->pointed;
  int32_t indexOrigSize = computeTypeSize(index->type);
  TypeRef *indexType = makePrimitiveType(ctx, isUnsignedType(index->type) ? T_U8 : T_S8, 0);

  Boolean isFlat = (base->type->kind == TR_ARRAY /*|| base->type->kind == TR_VLA*/) && base->op != E_CONST;

  base = transformExpression(ctx, base);
  index = transformExpression(ctx, index);

  if (isFlat) {
      // TODO: revise
      assert(base->op == EU_DEREF);
      base = base->unaryExpr.argument;
  }

  // TODO: probably void* isn't the best choose, think about introducing an address type for that purpose
  base->type = voidPtrType(ctx); // we do some address arith here, first normalize pointer

  AstExpression *indexValue = NULL;

  if (elementType->kind == TR_VLA) {
      AstExpression *vlaSize = computeVLASize(ctx, &expr->coordinates, elementType);
      indexValue = createBinaryExpression(ctx, EB_MUL, indexType, vlaSize, index);
      indexValue = transformExpression(ctx, indexValue);
  } else {
      size_t elementSize = computeTypeSize(elementType);
      AstExpression *elemSizeConst = createAstConst(ctx, &expr->coordinates, CK_INT_CONST, &elementSize, 0);
      elemSizeConst->type = indexType;

      if (indexOrigSize < 8) {
          index = createCastExpression(ctx, &index->coordinates, indexType, index);
      }

      indexValue = createBinaryExpression(ctx, EB_MUL, indexType, index, elemSizeConst);
      AstConst *evaluated = eval(ctx, indexValue);
      if (evaluated) {
          indexValue = createAstConst2(ctx, &indexValue->coordinates, indexType, evaluated);
      }
  }

  AstExpression *summed = createBinaryExpression(ctx, EB_ADD, pointerType, base, indexValue);
  AstExpression *transformed = transformExpression(ctx, summed);
  if (elementType->kind != TR_VLA) {
    AstExpression *derefered = createUnaryExpression(ctx, &expr->coordinates, EU_DEREF, transformed);
    derefered->type = elementType;
    return derefered;
  } else {
    return transformed;
  }
}

static AstExpression *cannonizeBinaryExpression(ParserContext *ctx, AstExpression *expr) {
  if (!isBinary(expr->op)) return expr;

  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;

  Boolean isReal = isRealType(expr->type);
  if (isReal) return expr;

  Boolean isComute = isCommute(expr->op);

  // 1 op x -> x op 1
  if (isComute && left->op == E_CONST && left->constExpr.op != CK_STRING_LITERAL) {
      AstExpression *t = left;
      left = right;
      right = t;
      expr->binaryExpr.left = left;
      expr->binaryExpr.right = right;
  }

  if (right->op == E_CONST) {
      // (x op 1) op 2 -> x op (1 op2 2)
      if (left->op == expr->op) {
        AstExpression *x = left->binaryExpr.left;
        AstExpression *c1 = left->binaryExpr.right;
        AstExpression *c2 = right;
        if (c1->op == E_CONST) {

            if (expr->op == EB_MOD) {
              expr->binaryExpr.left = x;
              // (x % 2) % 3 -> x % min(2, 3)
              if (c2->constExpr.i < c1->constExpr.i) {
                  expr->binaryExpr.right = c2;
              } else {
                  expr->binaryExpr.right = c1;
              }
            } else {
                expr->binaryExpr.left = x;
                left->binaryExpr.left = c1;
                left->binaryExpr.right = c2;
                // (x + 2) + 3 -> x + (2 + 3)
                // (x * 2) * 3 -> x * (2 * 3)
                ExpressionType newOp = left->op;
                // (x - 2) - 3 -> x - (2 + 3)
                if (left->op == EB_SUB) newOp = EB_ADD;
                // (x / 2) / 3 -> x / (2 * 3)
                if (left->op == EB_DIV) newOp = EB_MUL;

                left->op = newOp;
                AstConst *evaluated = eval(ctx, left);
                assert(evaluated);
                expr->binaryExpr.right = createAstConst2(ctx, &right->coordinates, right->type, evaluated);
            }
            left = expr->binaryExpr.left;
            right = expr->binaryExpr.right;
        }
      }

      // (x + 1) - 2 -> x + 1 - 2 -> x + (1 - 2)
      if (left->op == EB_ADD && expr->op == EB_SUB) {
          AstExpression *x = left->binaryExpr.left;
          AstExpression *c1 = left->binaryExpr.right;
          AstExpression *c2 = right;
          if (c1->op == E_CONST && c1->constExpr.op != CK_STRING_LITERAL) {
              expr->binaryExpr.left = x;
              left->binaryExpr.left = c1;
              left->binaryExpr.right = c2;

              expr->op = EB_ADD;
              left->op = EB_SUB;

              AstConst *evaluated = eval(ctx, left);
              assert(evaluated);
              expr->binaryExpr.right = createAstConst2(ctx, &right->coordinates, right->type, evaluated);

              left = expr->binaryExpr.left;
              right = expr->binaryExpr.right;
          }
      }

      // (x - 5) + 7 -> x - 5 + 7 -> x - (5 - 7)
      if (left->op == EB_SUB && expr->op == EB_ADD) {
          AstExpression *x = left->binaryExpr.left;
          AstExpression *c1 = left->binaryExpr.right;
          AstExpression *c2 = right;
          if (c1->op == E_CONST && c1->constExpr.op != CK_STRING_LITERAL) {
              expr->binaryExpr.left = x;
              left->binaryExpr.left = c1;
              left->binaryExpr.right = c2;

              expr->op = EB_SUB;

              AstConst *evaluated = eval(ctx, left);
              assert(evaluated);
              expr->binaryExpr.right = createAstConst2(ctx, &right->coordinates, right->type, evaluated);

              left = expr->binaryExpr.left;
              right = expr->binaryExpr.right;
          }
      }

  }


  if (!isReal) {

    // (x op 1) op y -> (x op y) op 1
    if (isAdditiveOp(left->op) && left->op == expr->op) {
        AstExpression *l_left = left->binaryExpr.left;
        AstExpression *l_right = left->binaryExpr.right;
        if (l_right->op == E_CONST && l_right->constExpr.op != CK_STRING_LITERAL) {
            left->binaryExpr.left = l_left;
            left->binaryExpr.right = right;
            left->coordinates.right = right->coordinates.right;
            expr->binaryExpr.right = l_right;
            left = expr->binaryExpr.left;
            right = expr->binaryExpr.right;
        }
    }

    // (x + 1) - y -> (x - y) + 1
    if (expr->op == EB_SUB && left->op == EB_ADD) {
        AstExpression *l_left = left->binaryExpr.left;
        AstExpression *l_right = left->binaryExpr.right;
        if (l_right->op == E_CONST && l_right->constExpr.op != CK_STRING_LITERAL) {
          left->binaryExpr.right = right;
          left->op = EB_SUB;
          expr->op = EB_ADD;
          expr->binaryExpr.right = l_right;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
        }
    }

    // (x - 1) + y -> (x + y) - 1
    if (expr->op == EB_ADD && left->op == EB_SUB) {
        AstExpression *l_left = left->binaryExpr.left;
        AstExpression *l_right = left->binaryExpr.right;
        if (l_right->op == E_CONST && l_right->constExpr.op != CK_STRING_LITERAL) {
          left->binaryExpr.right = right;
          left->op = EB_ADD;
          expr->op = EB_SUB;
          expr->binaryExpr.right = l_right;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
        }
    }
  }

  if (isAdditiveOp(expr->op)) {
      if (right->op == E_CONST) {
          if (right->constExpr.i == 0) {
              return left;
          }
      }
      if (left->op == E_CONST) {
          if (left->constExpr.i == 0) {
            if (expr->op == EB_SUB) {
                // 0 - x -> -x
                AstExpression *t = createUnaryExpression(ctx, &expr->coordinates, EU_MINUS, right);
                t->type = right->type;
                return t;
            } else {
                return right;
            }
          }
      }
  }

  if (isMultiplicative(expr->op)) {
      if (left->op == E_CONST) {
          if (left->constExpr.op == CK_INT_CONST) {
              // 1 op x -> x
              if (left->constExpr.i == 1) {
                  return right;
              }
          }

          if (left->constExpr.op == CK_FLOAT_CONST) {
              // 1.0f op x -> x
              if (left->constExpr.f == 1.0f) {
                  return right;
              }
          }
      }

      if (right->op == E_CONST) {
          if (right->constExpr.op == CK_INT_CONST) {
              // x op 1 -> x
              if (right->constExpr.i == 1) {
                  return left;
              }
          }

          if (right->constExpr.op == CK_FLOAT_CONST) {
              // x op 1.f -> x
              if (right->constExpr.f == 1.0f) {
                  return left;
              }
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
        left->type = right->type;
        left->coordinates.left = l_right->coordinates.left;
        left->coordinates.right = right->coordinates.right;
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
        left->coordinates.left = l_right->coordinates.left;
        left->coordinates.right = right->coordinates.right;
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
          right->coordinates.left = left->coordinates.left;
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
          right->coordinates.left = left->coordinates.left;
          expr->binaryExpr.left = r_left;
          left = expr->binaryExpr.left;
          right = expr->binaryExpr.right;
      }
  }

  if (expr->op == EB_SUB)
    expr = cannonizeSubExpression(ctx, expr);

  return cannonizeBinaryExpression(ctx, expr);
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
        left->coordinates.left = l_right->coordinates.left;
        left->coordinates.right = right->coordinates.right;

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

        left->coordinates.left = l_right->coordinates.left;
        left->coordinates.right = right->coordinates.right;

        expr->binaryExpr.left = l_left;
        expr->binaryExpr.right = left;

        left = expr->binaryExpr.left;
        right = expr->binaryExpr.right;
    }
  }

  if (expr->op == EB_ADD)
    expr = cannonizeAddExpression(ctx, expr);

  return cannonizeBinaryExpression(ctx, expr);
}

static AstExpression *cannonizeFieldExpression(ParserContext *ctx, AstExpression *receiver, StructualMember *member, AstExpression *orig, Boolean deBit) {
  int64_t memberOffset = effectiveMemberOffset(member);
  AstExpression *offset = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &memberOffset, 0);
  offset->type = makePrimitiveType(ctx, T_U8, 0);

  // normalize pointer
  receiver->type = voidPtrType(ctx);

  AstExpression *offsetedPtr = createBinaryExpression(ctx, EB_ADD, receiver->type, receiver, offset);
  TypeRef *memberType = member->type;
  TypeRef *ptrType = NULL;

  if (memberType->kind == TR_BITFIELD) {
      ptrType = makePointedType(ctx, 0U, memberType->bitFieldDesc.storageType);
  } else {
      ptrType = makePointedType(ctx, 0U, memberType);
  }

  offsetedPtr->type = ptrType;
  offsetedPtr = cannonizeAddExpression(ctx, offsetedPtr);

  AstExpression *derefered = createUnaryExpression(ctx, &orig->coordinates, EU_DEREF, offsetedPtr);
  derefered->type = ptrType->pointed;

  if (memberType->kind == TR_BITFIELD && deBit) {
      // de-bit
      TypeRef *storageType = memberType->bitFieldDesc.storageType;
      uint64_t w = memberType->bitFieldDesc.width;
      uint64_t mask = ~(~0LLu << w);
      uint64_t s = memberType->bitFieldDesc.offset;

      Boolean isU = isUnsignedType(storageType);

      AstExpression *extracted = NULL;
      size_t storageSize = computeTypeSize(storageType);

      size_t W = storageSize * 8;

      uint64_t l = W - (w + s);

      AstExpression *lshiftConst = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &l, 0);
      lshiftConst->type = storageType;

      AstExpression *lShifted = createBinaryExpression(ctx, EB_LHS, storageType, derefered, lshiftConst);

      uint64_t r = W - w;

      AstExpression *rshiftConst = createAstConst(ctx, &orig->coordinates, CK_INT_CONST, &r, 0);
      rshiftConst->type = storageType;

      AstExpression *result = createBinaryExpression(ctx, EB_RHS, storageType, lShifted, rshiftConst);

      if (storageSize < 4) {
          result = createBitExtendExpression(ctx, makePrimitiveType(ctx, isU ? T_U4 : T_S4, 0), w, isU, result);
      }
      return result;
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

  AstExpression *pReceiver = NULL;

  if (receiver->op == EU_DEREF) {
      pReceiver = receiver->unaryExpr.argument;
  } else if (receiver->op == E_CALL) {
      pReceiver = receiver;
      pReceiver->type = makePointedType(ctx, receiver->type->flags.storage, receiver->type);
  } else {
      pReceiver = createUnaryExpression(ctx, &receiver->coordinates, EU_REF, receiver);
      pReceiver->type = makePointedType(ctx, receiver->type->flags.storage, receiver->type);
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
  } else {
      expr->binaryExpr.left = left;
      expr->binaryExpr.right = right;
  }

  return expr;
}

static AstExpression *cannonizeAssignmentExpression(ParserContext *ctx, AstExpression *expr) {
  AstExpression *lhs = expr->binaryExpr.left;
  AstExpression *rhs = expr->binaryExpr.right;

  if (lhs->type->kind == TR_BITFIELD) {
      // we will properly de-sugar in codegen
      TypeRef *bfType = lhs->type;
      assert(lhs->op == EF_DOT || lhs->op == EF_ARROW);
      lhs = lhs->op == EF_DOT ? cannonizeDotExpression(ctx, lhs, FALSE) : cannonizeArrowExpression(ctx, lhs, FALSE);
      lhs->type = bfType;
  } else {
      lhs = transformExpression(ctx, lhs);
  }

  rhs = transformExpression(ctx, rhs);

  expr->binaryExpr.left = lhs;
  expr->binaryExpr.right = rhs;

  return expr;
}

static AstExpression *cannonizeShiftExpression(ParserContext *ctx, AstExpression *expr) {

  assert(isShiftOp(expr->op));

  AstExpression *left = transformExpression(ctx, expr->binaryExpr.left);
  AstExpression *right = transformExpression(ctx, expr->binaryExpr.right);

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;


  // (x op y) op z -> x op (y + z)
  if (left->op == expr->op) {
    AstExpression *l_left = left->binaryExpr.left;
    AstExpression *r_right = left->binaryExpr.right;

    expr->binaryExpr.left = l_left;

    left->binaryExpr.left = r_right;
    left->binaryExpr.right = right;
    left->coordinates.left = r_right->coordinates.left;
    left->coordinates.right = right->coordinates.right;
    left->op = EB_ADD;
    left->type = right->type;

    left = cannonizeAddExpression(ctx, left);

    AstConst *evaluated = eval(ctx, left);

    if (evaluated) {
        left = createAstConst2(ctx, &left->coordinates, left->type, evaluated);
    }

    expr->binaryExpr.right = left;
  }

  return expr;
}

static AstExpression *cannonizePostIncDec(ParserContext *ctx, AstExpression *expr) {
  TypeRef *type = expr->type;

  int64_t delta = isPointerLikeType(type) ? computeTypeSize(type->pointed) : 1;

  // x++ -> (x += delta) - delta
  // x-- -> (x -= delta) + delta

  AstExpression *argument = expr->unaryExpr.argument;

  if (type->kind == TR_BITFIELD) {
      assert(argument->op == EF_DOT || argument->op == EF_ARROW);
      argument = argument->op == EF_DOT ? cannonizeDotExpression(ctx, argument, FALSE) : cannonizeArrowExpression(ctx, argument, FALSE);
  } else {
      argument = transformExpression(ctx, argument);
  }

  ExpressionType asg_op = expr->op == EU_POST_DEC ? EB_ASG_SUB : EB_ASG_ADD;
  ExpressionType snd_op = expr->op == EU_POST_DEC ? EB_ADD : EB_SUB;


  AstExpression *deltaConst = NULL;

  TypeId tid = typeToId(type);

  if (tid < T_F4) {
    deltaConst = createAstConst(ctx, &expr->coordinates, CK_INT_CONST, &delta, 0);
  } else {
    long double d = delta;
    deltaConst = createAstConst(ctx, &expr->coordinates, CK_FLOAT_CONST, &d, 0);
  }

  deltaConst->type = isPointerLikeType(type) ? makePrimitiveType(ctx, tid, 0) : type;
  argument->type = type;

  AstExpression *asg = createBinaryExpression(ctx, asg_op, type, argument, deltaConst);

  if (type->kind == TR_BITFIELD) {
      type = type->bitFieldDesc.storageType;
  }

  return createBinaryExpression(ctx, snd_op, type, asg, deltaConst);
}

static AstExpression *transformExpression(ParserContext *ctx, AstExpression *expr) {

  AstConst *evaluated = eval(ctx, expr);
  if (evaluated) {
      return createAstConst2(ctx, &expr->coordinates, expr->type, evaluated);
  }

  ExpressionType op2 = E_ERROR;
  AstExpressionList *args = NULL;
  AstExpression *tmp = NULL;

  switch (expr->op) {
    case E_TERNARY:
      expr->ternaryExpr.condition = transformExpression(ctx, expr->ternaryExpr.condition);
      expr->ternaryExpr.ifTrue = transformExpression(ctx, expr->ternaryExpr.ifTrue);
      expr->ternaryExpr.ifFalse = transformExpression(ctx, expr->ternaryExpr.ifFalse);
      break;
    case E_CAST:
      tmp = transformExpression(ctx, expr->castExpr.argument);
      if (typesEquals(expr->castExpr.type, expr->castExpr.argument->type) || isVoidType(expr->castExpr.type))
        return tmp;
      else
        expr->castExpr.argument = tmp;
      break;
    case E_PAREN:
      return transformExpression(ctx, expr->parened);
    case E_BLOCK:
      expr->block = transformStatement(ctx, expr->block);
      break;
    case E_COMPOUND:
      expr->compound = transformInitializer(ctx, expr->compound);
      break;
    case E_CALL:
      // TODO:
      expr->callExpr.callee = transformExpression(ctx, expr->callExpr.callee);
      args = expr->callExpr.arguments;
      while (args) {
          args->expression = transformExpression(ctx, args->expression);
          args = args->next;
      }
      break;

    case EU_POST_INC:
    case EU_POST_DEC:
      return cannonizePostIncDec(ctx, expr);
    case EU_PRE_INC:
    case EU_PRE_DEC:
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
    case EB_MOD:
    case EB_DIV:
      return cannonizeBinaryExpression(ctx, expr);
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE:
      return cannonizeRelativeExpression(ctx, expr);
    case EB_A_ACC:
      return cannonizeArrayAccess(ctx, expr);
    case EB_SUB:
      return cannonizeSubExpression(ctx, expr);
    case EB_LHS:
    case EB_RHS:
      return cannonizeShiftExpression(ctx, expr);
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
    case E_VA_ARG:
      expr->vaArg.va_list = transformExpression(ctx, expr->vaArg.va_list);
      return expr;
    case E_LABEL_REF:
    case E_NAMEREF:
    case E_BIT_EXTEND:
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
      if (stmt->forStmt.initial) {
        AstStatementList *stmts = stmt->forStmt.initial;
        for (; stmts; stmts = stmts->next) {
          stmts->stmt = transformStatement(ctx, stmts->stmt);
        }
      }
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
      } else {
          AstDeclaration *d = unit->declaration;
          if (d->kind == DK_VAR) {
              AstValueDeclaration *v = d->variableDeclaration;
              if (v->initializer) {
                  v->initializer = transformInitializer(ctx, v->initializer);
              }
          }
      }
      unit = unit->next;
  }

}
