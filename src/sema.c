
#include <assert.h>

#include "sema.h"
#include "treeDump.h"
#include "parser.h"


static TypeEqualityKind structualTypesEquality(TypeDesc *d1, TypeDesc *d2, TypeId kind) {
  if (d1->typeId == kind && d2->typeId == kind) {
      if (d1->typeDefinition == d2->typeDefinition) {
        return TEK_EQUAL;
      } else {
        return TEK_NOT_EQUAL;
      }
  }

  if (d1->typeId == kind || d2->typeId == kind) {
      return TEK_NOT_EQUAL;
  }

  return TEK_UNKNOWN;
}

static TypeEqualityKind primitiveTypesEquality(TypeId t1, TypeId t2) {
  if (t1 == T_VOID && t2 == T_VOID) return TEK_EQUAL;

  if (t1 == T_VOID || t2 == T_VOID) return TEK_NOT_EQUAL;

  if (t1 == t2) return TEK_EQUAL;

  return TEK_NOT_EQUAL;
}

static Boolean isIntegerTypeId(TypeId tid) {
  return T_S1 <= tid && tid <= T_U8;
}

static TypeEqualityKind typeDescriprorEquals(TypeDesc *d1, TypeDesc *d2) {

  if (d1->typeId == T_ERROR || d2->typeId == T_ERROR) return TEK_NOT_EQUAL;

  TypeEqualityKind structCheck = structualTypesEquality(d1, d2, T_STRUCT);
  if (structCheck != TEK_UNKNOWN) return structCheck;
  TypeEqualityKind unionCheck = structualTypesEquality(d1, d2, T_UNION);
  if (unionCheck != TEK_UNKNOWN) return unionCheck;

  if (d1->typeId == T_ENUM && d2->typeId == T_ENUM) {
    if (d1->typeDefinition == d2->typeDefinition) {
        return TEK_EQUAL;
    } else {
        return TEK_NOT_EQUAL;
    }
  }

  if (d1->typeId == T_ENUM && isIntegerTypeId(d2->typeId) || d2->typeId == T_ENUM && isIntegerTypeId(d1->typeId)) {
      return TEK_EQUAL;
  }

  if (d1->typeId == d2->typeId) return TEK_EQUAL;

  return TEK_NOT_EQUAL;
}

static TypeEqualityKind valueTypeEquality(TypeRef *t1, TypeRef *t2) {

  if (t1->kind == TR_BITFIELD) t1 = t1->bitFieldDesc.storageType;
  if (t2->kind == TR_BITFIELD) t2 = t2->bitFieldDesc.storageType;


  if (t1->kind == TR_VALUE && t2->kind == TR_VALUE) {
    TypeEqualityKind equality = typeDescriprorEquals(t1->descriptorDesc, t2->descriptorDesc);
    if (equality == TEK_EQUAL) {
      if (t1->flags.storage == t2->flags.storage) {
        return TEK_EQUAL;
      } else {
        // 'int' vs 'const int'
        return TEK_ALMOST_EQUAL;
      }
    }
    return equality;
  }

  if (t1->kind == TR_VALUE || t2->kind == TR_VALUE) {
      // 'VALUE type' vs 'non-value type'
      return TEK_NOT_EQUAL;
  }

  return TEK_UNKNOWN;
}

TypeEqualityKind typeEquality(TypeRef *t1, TypeRef *t2) {
  TypeEqualityKind equality = valueTypeEquality(t1, t2);

  if (equality != TEK_UNKNOWN) return equality;

  if (t1->kind == TR_POINTED && t2->kind == TR_POINTED) {
      equality = typeEquality(t1->pointedTo.toType, t2->pointedTo.toType);
      if (t1->flags.bits.isConst == t2->flags.bits.isConst) {
        if (equality == TEK_EQUAL) {
            return TEK_EQUAL;
          } else {
            // 'int *' is not equal to 'int *const'
            return equality;
          }
      }
  }

  if (t1->kind == TR_POINTED && t2->kind == TR_ARRAY || t2->kind == TR_POINTED && t1->kind == TR_ARRAY) {
      TypeRef *arrayType = t1->kind == TR_ARRAY ? t1 : t2;
      TypeRef *pointerType = t1 == arrayType ? t2 : t1;

      TypeRef *elementType = arrayType->arrayTypeDesc.elementType;
      TypeRef *pointedType = pointerType->pointedTo.toType;

      equality = typeEquality(elementType, pointedType);
      assert(equality != TEK_UNKNOWN);
      if (equality <= TEK_ALMOST_EQUAL) {
        if (pointedType->flags.bits.isConst) {
            return TEK_ALMOST_EQUAL;
        } else {
            return TEK_NOT_EXATCLY_EQUAL;
        }
      }

      return equality;
  }


  if (t1->kind == TR_POINTED || t2->kind == TR_POINTED) {
    return TEK_NOT_EQUAL;
  }

  if (t1->kind == TR_ARRAY && t2->kind == TR_ARRAY) {
      if (t1->arrayTypeDesc.size != t2->arrayTypeDesc.size) {
          return TEK_NOT_EQUAL;
      }
      return typeEquality(t1->arrayTypeDesc.elementType, t2->arrayTypeDesc.elementType);
  }


  if (t1->kind == TR_ARRAY || t2->kind == TR_ARRAY) {
    return TEK_NOT_EQUAL;
  }

  if (t1->kind == TR_BITFIELD) {
      if (t2->kind == TR_BITFIELD) return typeEquality(t1->bitFieldDesc.storageType, t2->bitFieldDesc.storageType);
      return typeEquality(t1->bitFieldDesc.storageType, t2);
  }

  assert(t1->kind == TR_FUNCTION);
  assert(t2->kind == TR_FUNCTION);


  FunctionTypeDescriptor *fd1 = &t1->functionTypeDesc;
  FunctionTypeDescriptor *fd2 = &t2->functionTypeDesc;

  if (fd1->isVariadic != fd2->isVariadic) return TEK_EQUAL;

  equality = typeEquality(fd1->returnType, fd2->returnType);

  if (equality != TEK_EQUAL) return equality;

  TypeList *p1 = fd1->parameters;
  TypeList *p2 = fd2->parameters;

  for (;;) {
      if (p1 && p2) {
        equality = typeEquality(p1->type, p2->type);
        if (equality != TEK_EQUAL) return equality;
        p1 = p1->next;
        p2 = p2->next;
      } else if (p1 || p2) {
        return TEK_NOT_EQUAL;
      } else {
        return TEK_EQUAL;
      }
  }

  unreachable("infinite loop");
}

TypeRef *commonPrimitiveType(ParserContext *ctx, TypeRef *a, TypeRef *b) {

  // TODO: should const qualifier be taken into account?

  if (a->kind == TR_BITFIELD) a = a->bitFieldDesc.storageType;
  if (b->kind == TR_BITFIELD) b = b->bitFieldDesc.storageType;

  assert(a->kind == TR_VALUE);
  assert(b->kind == TR_VALUE);

  TypeId aId = a->descriptorDesc->typeId;
  TypeId bId = b->descriptorDesc->typeId;

  if (aId == T_ENUM) { aId = T_S4; a = makePrimitiveType(ctx, T_S4, 0); }
  if (bId == T_ENUM) { bId = T_S4; b = makePrimitiveType(ctx, T_S4, 0); }

  assert(T_VOID < aId && aId < T_BUILT_IN_TYPES);
  assert(T_VOID < bId && bId < T_BUILT_IN_TYPES);

  if (aId == T_F10) return a;
  if (bId == T_F10) return b;

  if (aId == T_F8) return a;
  if (bId == T_F8) return b;

  if (aId == T_F4) return a;
  if (bId == T_F4) return b;

  if (aId > T_S8 && bId > T_S8) { // both unsigned
      return aId > bId ? a : b;
  }

  if (aId <= T_S8 && bId <= T_S8) { // both signed
      return aId > bId ? a : b;
  }

  TypeId sId = aId < bId ? aId : bId;
  TypeRef *sT = aId == sId ? a : b;

  TypeId uId = aId == sId ? bId : aId;
  TypeRef *uT = aId == sId ? b : a;

  int sOff = sId - T_S1;
  int uOff = uId - T_U1;

  return sOff> uOff  ? sT : uT;
}

TypeCastabilityKind typeCastability(TypeRef *to, TypeRef *from) {
  return TCK_UNKNOWN;
}


Boolean typesEquals(TypeRef *t1, TypeRef *t2) {
  TypeEqualityKind equality = typeEquality(t1, t2);
  return equality == TEK_EQUAL || equality == TEK_ALMOST_EQUAL ? TRUE : FALSE;
}

Boolean isErrorType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_ERROR)
    return TRUE;
  return FALSE;
}

static Boolean isEnumType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_ENUM)
    return TRUE;
  return FALSE;
}

Boolean isStructualType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_STRUCT)
    return TRUE;
  return FALSE;
}

Boolean isUnionType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_UNION)
    return TRUE;
  return FALSE;
}

Boolean isCompositeType(TypeRef *type) {
  if (type->kind == TR_VALUE && (type->descriptorDesc->typeId == T_UNION || type->descriptorDesc->typeId == T_STRUCT))
    return TRUE;
  return FALSE;
}

Boolean isVoidType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_VOID)
    return TRUE;
  return FALSE;
}

Boolean isIntegerType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
    TypeId typeId = type->descriptorDesc->typeId;
    if (T_VOID < typeId && typeId < T_F4 || typeId == T_ENUM)
      return TRUE;
  }
  return FALSE;
}

Boolean isUnsignedType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
    TypeId typeId = type->descriptorDesc->typeId;
    if (T_U1 <= typeId && typeId <= T_U8)
      return TRUE;
  }
  return FALSE;
}

static Boolean isPrimitiveType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
    TypeId typeId = type->descriptorDesc->typeId;
    if (T_VOID < typeId && typeId < T_BUILT_IN_TYPES || typeId == T_ENUM)
      return TRUE;
  }
  return FALSE;
}

Boolean isPointerLikeType(TypeRef *type) {
  return type->kind == TR_POINTED || type->kind == TR_ARRAY;
}

Boolean isArrayish(TypeRef *type) {
  return type->kind == TR_ARRAY || type->kind == TR_POINTED && type->pointedTo.arrayType != NULL;
}

Boolean isScalarType(TypeRef *type) {
  return type->kind == TR_POINTED || isPrimitiveType(type);
}

Boolean isRealType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      TypeId tid = type->descriptorDesc->typeId;
      return T_F4 <= tid && tid <= T_F10 ? TRUE : FALSE;
  }
  return FALSE;
}

Boolean isCharType(TypeRef *type) {
  return type->kind == TR_VALUE && type->descriptorDesc->typeId == T_S1 || type->descriptorDesc->typeId == T_U1;
}

Boolean is_va_list_Type(TypeRef *type) {
  if (type->kind != TR_POINTED) return FALSE;
  TypeRef *pointed = type->pointedTo.toType;
  if (pointed->kind != TR_VALUE) return FALSE;

  if (pointed->descriptorDesc->typeId != T_STRUCT) return FALSE;

  TypeDefiniton *decl = pointed->descriptorDesc->typeDefinition;

  return strcmp("__va_elem", decl->name) == 0;
}

Boolean isIncompleteType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      TypeId tid = type->descriptorDesc->typeId;
      if (tid == T_VOID) return TRUE;
      if (tid == T_STRUCT || tid == T_UNION) {
          return type->descriptorDesc->size == UNKNOWN_SIZE ? TRUE : FALSE;
      }
  }
  if (type->kind == TR_ARRAY) {
      return type->arrayTypeDesc.size == UNKNOWN_SIZE ? TRUE : FALSE;
  }
  return FALSE;
}

int32_t typeAlignment(TypeRef *type) {
  TypeRef *effectiveType;
  int32_t align;
  switch (type->kind) {
    case TR_POINTED:
    case TR_FUNCTION:
      return sizeof(intptr_t);
    case TR_ARRAY:
      return typeAlignment(type->arrayTypeDesc.elementType);
    case TR_BITFIELD: effectiveType = type->bitFieldDesc.storageType; goto value_type;
    case TR_VALUE: effectiveType = type;
      value_type:
      switch (effectiveType->descriptorDesc->typeId) {
      case T_S1:
      case T_U1:
          return sizeof(uint8_t);
      case T_S2:
      case T_U2:
          return sizeof(uint16_t);
      case T_U4:
      case T_S4:
      case T_F4:
      case T_ENUM:
          return sizeof(uint32_t);
      case T_S8:
      case T_U8:
      case T_F8:
          return sizeof(uint64_t);
      case T_F10:
          return sizeof(long double);
      case T_STRUCT:
      case T_UNION:
          return effectiveType->descriptorDesc->typeDefinition->align;
      case T_VOID:
      case T_ERROR:
          return -1;
      default:
          unreachable("Unknown type ID");
      }
      break;

    default: unreachable("Unexpected type kind");
  }

  return 1;
}

int32_t memberOffset(TypeDefiniton *declaration, const char *memberName) {
  StructualMember *member = findStructualMember(declaration, memberName);

  if (member) return member->offset;

  return -1;
}

TypeRef *computeArrayAccessExpressionType(ParserContext *ctx, Coordinates *coords, TypeRef *l, TypeRef *r) {
  if (isErrorType(l)) return l;
  if (isErrorType(r)) return r;

  TypeRef *arrayType = isPointerLikeType(l) ? l : r;
  TypeRef *indexType = arrayType == l ? r : l;

  if (!isIntegerType(indexType)) {
      reportDiagnostic(ctx, DIAG_ARRAY_SUBSCRIPT_NOT_INT, coords);
      return makeErrorRef(ctx);
  }

  if (arrayType->kind == TR_POINTED) {
      return arrayType->pointedTo.toType;
  }

  if (arrayType->kind == TR_ARRAY) {
      return arrayType->arrayTypeDesc.elementType;
  }

  reportDiagnostic(ctx, DIAG_SUBSCRIPTED_NOT_A_POINTER, coords);

  return makeErrorRef(ctx);
}


TypeRef *computeFunctionReturnType(ParserContext *ctx, Coordinates *coords, TypeRef *_calleeType) {
    if (isErrorType(_calleeType)) return _calleeType;

    TypeRef *calleType = _calleeType;
    if (_calleeType->kind == TR_POINTED) {
        calleType = _calleeType->pointedTo.toType;
    }

    if (calleType->kind != TR_FUNCTION) {
        reportDiagnostic(ctx, DIAG_INVOKE_NOT_FUNCTIONAL, coords, _calleeType);
        return makeErrorRef(ctx);
    }

    TypeRef *returnType = calleType->functionTypeDesc.returnType;

    if (isStructualType(returnType) && computeTypeSize(returnType) <= sizeof(intptr_t)) {
        ctx->stateFlags.hasSmallStructs = 1;
    }

    return returnType;
}

StructualMember *findStructualMember(TypeDefiniton *definition, const char *name) {
  StructualMember *member = definition->members;

  while (member) {
      if (strcmp(member->name, name) == 0) {
          break;
      }
      member = member->next;
  }

  return member;
}

StructualMember *computeMember(ParserContext *ctx, Coordinates *coords, TypeRef *_receiverType, const char *memberName, ExpressionType op) {
  assert(op == EF_DOT || op == EF_ARROW);
  if (isErrorType(_receiverType) || memberName == NULL) return NULL;

  TypeRef *receiverType = _receiverType;

  if (op == EF_ARROW) {
    if (_receiverType->kind == TR_POINTED) {
        receiverType = _receiverType->pointedTo.toType;
    } else if (_receiverType->kind == TR_ARRAY) {
        receiverType = _receiverType->arrayTypeDesc.elementType;
    } else {
        reportDiagnostic(ctx, DIAG_MEMBER_REF_NOT_A_POINTER, coords, _receiverType);
        return NULL;
    }
  }

  if (!isCompositeType(receiverType)) {
      reportDiagnostic(ctx, DIAG_MEMBER_REF_NOT_A_STRUCTUAL, coords, receiverType);
      return NULL;
  }

  TypeDefiniton *declaration = receiverType->descriptorDesc->typeDefinition;

  StructualMember *member = findStructualMember(declaration, memberName);

  if (member == NULL) {
      reportDiagnostic(ctx, DIAG_NO_MEMBER_NAME, coords, memberName, receiverType);
  }

  return member;
}

TypeRef *computeTernaryType(ParserContext *ctx, Coordinates *coords, TypeRef* cond, TypeRef* ifTrue, TypeRef *ifFalse, ExpressionType op) {
  if (isErrorType(cond)) return cond;
  if (isErrorType(ifTrue)) return ifTrue;
  if (isErrorType(ifFalse)) return ifFalse;

  if (isStructualType(ifTrue) || isStructualType(ifFalse)) {
      if (typesEquals(ifTrue, ifFalse)) {
          return ifTrue;
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  TypeRefKind tKind = ifTrue->kind;
  TypeRefKind fKind = ifFalse->kind;

  if (isPrimitiveType(ifTrue) && isPrimitiveType(ifFalse)) {
      return commonPrimitiveType(ctx, ifTrue, ifFalse);
  }

  if (isPointerLikeType(ifTrue)) {
      TypeRef *pointedT = ifTrue->kind == TR_POINTED ? ifTrue->pointedTo.toType : ifTrue->arrayTypeDesc.elementType;
      if (isPointerLikeType(ifFalse)) {
          TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointedTo.toType : ifFalse->arrayTypeDesc.elementType;
          if (!typesEquals(pointedT, pointedF)) { // TODO: could fail with array vs ponter
              if (!isVoidType(pointedT) && !isVoidType(pointedF)) {
                reportDiagnostic(ctx, DIAG_POINTER_TYPE_MISMATCH, coords, ifTrue, ifFalse);
              }
          }
          return makePointedType(ctx, ifTrue->flags.storage, pointedT);
      } else if (isIntegerType(ifFalse)) {
          reportDiagnostic(ctx, DIAG_POINTER_INT_MISMATCH_IN_COND, coords, ifTrue, ifFalse);
          return makePointedType(ctx, ifTrue->flags.storage, pointedT);
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  if (isPointerLikeType(ifFalse)) {
      TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointedTo.toType : ifFalse->arrayTypeDesc.elementType;
      if (isIntegerType(ifTrue)) {
          reportDiagnostic(ctx, DIAG_POINTER_INT_MISMATCH_IN_COND, coords, ifTrue, ifFalse);
          return makePointedType(ctx, ifFalse->flags.storage, pointedF);
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  return makeErrorRef(ctx); // Unknown situation
}

TypeRef *elementType(TypeRef *type) {
  if (type->kind == TR_POINTED) return type->pointedTo.toType;
  if (type->kind == TR_ARRAY) return type->arrayTypeDesc.elementType;
  return NULL;
}

static Boolean isConstZero(AstExpression *expr) {
  expr = deparen(expr);
  return expr->op == E_CONST && expr->constExpr.i == 0;
}

TypeRef *computeBinaryType(ParserContext *ctx, Coordinates *coords, AstExpression* leftExpr, AstExpression *rightExpr, ExpressionType op) {

  TypeRef *left = leftExpr->type;
  TypeRef *right = rightExpr->type;

  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  if (isStructualType(left) || isStructualType(right)) {
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
      return makeErrorRef(ctx);
  }

  if (op == EB_EQ || op == EB_NE) {
      if (isPointerLikeType(left) || isPointerLikeType(right)) {
          if (isIntegerType(left) || isIntegerType(right)) {
              if (!isConstZero(leftExpr) && !isConstZero(rightExpr)) {
                reportDiagnostic(ctx, DIAG_INT_PTR_COMPARISON, coords, left, right);
              }
          } else if (!isPointerLikeType(left) || !isPointerLikeType(right)) {
              reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
              return makeErrorRef(ctx);
          }
      }
  }

  if (EB_LT <= op && op <= EB_GE) {
      if (isPointerLikeType(left) || isPointerLikeType(right)) {
          if (isIntegerType(left) || isIntegerType(right)) {
              reportDiagnostic(ctx, DIAG_ORDEDER_INT_PTR_COMPARISON, coords, left, right);
          } else if (!isPointerLikeType(left) || !isPointerLikeType(right)) {
              reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
              return makeErrorRef(ctx);
          }
      }
  }

  if (EB_ANDAND <= op && op <= EB_GE) {
      return makePrimitiveType(ctx, T_S4, 0);
  }

  if (EB_LHS <= op && op <= EB_OR) {
      if ((isIntegerType(left) || left->kind == TR_BITFIELD) && (isIntegerType(right) || right->kind == TR_BITFIELD)) {
          return commonPrimitiveType(ctx, left, right);
      }
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
      return makeErrorRef(ctx);
  }


  if (EB_MUL <= op && op <= EB_DIV) {
    if (isPrimitiveType(left) && isPrimitiveType(right)) {
        return commonPrimitiveType(ctx, left, right);
    }
    reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
    return makeErrorRef(ctx);
  }

  if (op == EB_MOD) {
      if (isIntegerType(left) && isIntegerType(right)) {
          return commonPrimitiveType(ctx, left, right);
      }
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
      return makeErrorRef(ctx);
  }


  if (op == EB_ADD) {
      if (isPointerLikeType(left) && isPointerLikeType(right)) {
          reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
          return makeErrorRef(ctx);
      }
  }

  if (isPointerLikeType(left) && isPointerLikeType(right)) {
      assert(op == EB_SUB);

      TypeRef *savedL = left;
      TypeRef *savedR = right;

      TypeRef *l = left;
      TypeRef *r = right;

      while (l && r) {
          left = l;
          right = r;
          l = elementType(l);
          r = elementType(r);
      }

      if (l == r && typesEquals(left, right)) {
          TypeRef *elemType = elementType(savedL);
          int size = computeTypeSize(elemType);

          if (size != UNKNOWN_SIZE) {
            return makePrimitiveType(ctx, T_S8, 0);
          } else if (!isVoidType(elemType) && size == 0) {
            reportDiagnostic(ctx, DIAG_PTR_ARITH_EMPTY_TYPE, coords);
          } else {
            reportDiagnostic(ctx, DIAG_PTR_ARITH_INCOMPLETE_TYPE, coords, elemType);
          }
      } else {
        reportDiagnostic(ctx, DIAG_INCOMPATIBLE_PTR_DIFF, coords, savedL, savedR);
      }
      return makeErrorRef(ctx);
  }

  if (isPointerLikeType(left)) {
      TypeRef *elemType = elementType(left);
      int elemSize = computeTypeSize(elemType);
      if (elemSize != UNKNOWN_SIZE && isIntegerType(right)) {
        TypeRef *pointedL = elementType(left);
        return makePointedType(ctx, left->flags.storage, pointedL);
      }

      if (elemSize == UNKNOWN_SIZE) {
        reportDiagnostic(ctx, DIAG_PTR_ARITH_INCOMPLETE_TYPE, coords, elemType);
      } else if (!isVoidType(elemType) && elemSize == 0) {
        reportDiagnostic(ctx, DIAG_PTR_ARITH_EMPTY_TYPE, coords);
      } else {
        reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
      }

      return makeErrorRef(ctx);
  }

  if (isPointerLikeType(right)) {
      TypeRef *elemType = elementType(right);
      int elemSize = computeTypeSize(elemType);
      if (elemSize != UNKNOWN_SIZE && isIntegerType(left)) {
        if (op == EB_ADD) {
          TypeRef *pointedR = elementType(right);
          return makePointedType(ctx, right->flags.storage, pointedR);
        }
      }

      if (elemSize == UNKNOWN_SIZE) {
        reportDiagnostic(ctx, DIAG_PTR_ARITH_INCOMPLETE_TYPE, coords, elemType);
      } else if (!isVoidType(elemType) && elemSize == 0) {
        reportDiagnostic(ctx, DIAG_PTR_ARITH_EMPTY_TYPE, coords);
      } else {
        reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, coords, left, right);
      }

      return makeErrorRef(ctx);
  }

  return commonPrimitiveType(ctx, left, right);
}


TypeRef *computeIncDecType(ParserContext *ctx, Coordinates *coords, TypeRef *argumentType, Boolean isDec) {
  if (isErrorType(argumentType)) return argumentType;

  if (argumentType->kind == TR_FUNCTION || argumentType->kind == TR_ARRAY || argumentType->flags.bits.isConst || isStructualType(argumentType)) {
      enum DiagnosticId diag = isDec ? DIAG_CANNOT_DECREMENT : DIAG_CANNOT_INCREMENT;
      reportDiagnostic(ctx, diag, coords, argumentType);
      return makeErrorRef(ctx);
  }

  if (argumentType->kind == TR_POINTED) {
      TypeRef *ptrType = argumentType->pointedTo.toType;
      int typeSize = computeTypeSize(ptrType);
      if (typeSize == UNKNOWN_SIZE) {
          reportDiagnostic(ctx, DIAG_PTR_ARITH_INCOMPLETE_TYPE, coords, argumentType);
          return makeErrorRef(ctx);
      } else if (!isVoidType(ptrType) && typeSize == 0) {
          reportDiagnostic(ctx, DIAG_PTR_ARITH_EMPTY_TYPE, coords);
          return makeErrorRef(ctx);
      }
  }

  return argumentType;
}

static TypeRef *computeTypeForDerefOperator(ParserContext *ctx, Coordinates *coords, TypeRef *argumentType) {
  if (argumentType->kind == TR_POINTED) {
      return argumentType->pointedTo.toType;
  }

  if (argumentType->kind == TR_ARRAY) {
      return argumentType->arrayTypeDesc.elementType;
  }

  reportDiagnostic(ctx, DIAG_INDERECTION_POINTER_OP, coords, argumentType);

  return makeErrorRef(ctx);
}

TypeRef *computeTypeForUnaryOperator(ParserContext *ctx, Coordinates *coords, TypeRef *argumentType, ExpressionType op) {
  if (isErrorType(argumentType)) return argumentType;

  switch (op) {
    case EU_REF:   // &a
      return makePointedType(ctx, 0U, argumentType);
    case EU_DEREF: // *a
      return computeTypeForDerefOperator(ctx, coords, argumentType);
    case EU_EXL:   // !a
      if (isScalarType(argumentType) || argumentType->kind == TR_BITFIELD) {
           return argumentType;
      } else {
          reportDiagnostic(ctx, DIAG_INVALID_UNARY_ARGUMENT, coords, argumentType);
          return makeErrorRef(ctx);
      }
    case EU_PLUS:  // +a
    case EU_MINUS: // -a
      if (isPrimitiveType(argumentType) || argumentType->kind == TR_BITFIELD) {
          return argumentType;
      } else {
          reportDiagnostic(ctx, DIAG_INVALID_UNARY_ARGUMENT, coords, argumentType);
          return makeErrorRef(ctx);
      }
    case EU_TILDA: // ~a
      if (isIntegerType(argumentType) || argumentType->kind == TR_BITFIELD) {
          return argumentType;
      } else {
          reportDiagnostic(ctx, DIAG_INVALID_UNARY_ARGUMENT, coords, argumentType);
          return makeErrorRef(ctx);
      }
    default:
      unreachable("Unexpected Unary operator type");
      return NULL;
    }

  return argumentType;
}

Boolean isAssignableTypes(ParserContext *ctx, Coordinates *coords, TypeRef *to, TypeRef *from, AstExpression *fromExpr, Boolean init) {
  if (isErrorType(to)) return TRUE;
  if (isErrorType(from)) return TRUE;

  if (!init && to->flags.bits.isConst) {
      reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, to);
      return FALSE;
  }

  if (to->kind == TR_ARRAY) {
      reportDiagnostic(ctx, DIAG_ARRAY_TYPE_IS_NOT_ASSIGNABLE, coords, to);
      return FALSE;
  }

  if (isStructualType(to) || isStructualType(from)) {
      if (typesEquals(to, from)) {
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, coords, to, from);
          return FALSE;
      }
  }

  if (to->kind == TR_POINTED) {
      TypeRef *pLeft = to->pointedTo.toType;
      if (isPointerLikeType(from)) {
          TypeRef *pointed = from->kind == TR_POINTED ? from->pointedTo.toType : from->arrayTypeDesc.elementType;
          if (isVoidType(pLeft) || isVoidType(pointed)) {
              return TRUE;
          } else if (typesEquals(pLeft, pointed)) {
              return TRUE;
          } else {
              reportDiagnostic(ctx, DIAG_ASSIGN_INCOMPATIBLE_POINTERS, coords, to, from);
              return TRUE;
          }
      } else if (isIntegerType(from)) {
          if (!isConstZero(fromExpr)) {
              reportDiagnostic(ctx, DIAG_ASSIGN_INT_TO_POINTER, coords, to, from);
          }
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, coords, to, from);
          return FALSE;
      }
  }


  if (isPointerLikeType(from)) {
      if (isIntegerType(to)) {
          if (!isConstZero(fromExpr)) {
            reportDiagnostic(ctx, DIAG_ASSIGN_INT_TO_POINTER, coords, to, from);
          }
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, coords, to, from);
          return FALSE;
      }
  }

  return TRUE;
}

Boolean checkTypeAssignable(ParserContext *ctx, Coordinates *coords, TypeRef *type, Boolean report) {
  if (isErrorType(type)) return TRUE;

  if (type->flags.bits.isConst) {
      if(report) {
        reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, type);
      }
      return FALSE;
  }

  if (type->kind == TR_ARRAY) {
      if (report) {
        reportDiagnostic(ctx, DIAG_ARRAY_TYPE_IS_NOT_ASSIGNABLE, coords, type);
      }
      return FALSE;
  }

  return TRUE;
}


Boolean checkRefArgument(ParserContext *ctx, Coordinates *coords, AstExpression *arg, Boolean report) {

  arg = deparen(arg);

  switch (arg->op) {
  case E_NAMEREF:
  case EF_ARROW:
  case EF_DOT:
  case EB_A_ACC:
  case EU_DEREF:
      return TRUE;
  default:
      if (report) {
          reportDiagnostic(ctx, DIAG_CANNOT_TAKE_ADDRESS_OF_RVALUE, coords, arg->type);
      }
      return FALSE;
  }
}

Boolean checkExpressionIsAssignable(ParserContext *ctx, Coordinates *coords, AstExpression *expr, Boolean report) {

  expr = deparen(expr);

  switch (expr->op) {
  case EF_ARROW:
  case EF_DOT:
  case EB_A_ACC:
  case EU_DEREF:
      if (expr->type->flags.bits.isConst) {
          if (report) {
            reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, expr->type);
          }
          return FALSE;
      }
      return TRUE;
//  case EU_DEREF:
//      if (expr->type->pointedTo->flags.bits.isConst) {
//          reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, expr->type);
//          return FALSE;
//      }
//      return TRUE;
  case E_NAMEREF:
  default:
      reportDiagnostic(ctx, DIAG_EXPRESSION_IS_NOT_ASSIGNABLE, coords);
      return FALSE;
  }
}

static Boolean checkUnionIsCastableToType(ParserContext *ctx, Coordinates *coords, TypeRef *from, TypeDefiniton *definition, Boolean report) {
  assert(definition->kind == TDK_UNION);

  StructualMember *member = definition->members;

  while (member) {
    if (typeEquality(member->type, from) == TEK_EQUAL) {
        return TRUE;
    }
    member = member->next;
  }

  if (report) {
    reportDiagnostic(ctx, DIAG_CAST_TO_UNION_NOT_PRESENT, coords, from);
  }

  return FALSE;
}

Boolean checkTypeIsCastable(ParserContext *ctx, Coordinates *coords, TypeRef *to, TypeRef *from, Boolean report) {
  if (isErrorType(from)) return TRUE;
  if (isErrorType(to)) return TRUE;

  if (to->kind == TR_ARRAY) {
      if (report) {
        reportDiagnostic(ctx, DIAG_NON_CASTABLE_TYPE, coords, to);
      }
      return FALSE;
  }

  if (to->kind == TR_VALUE) {
      TypeDesc *desc = to->descriptorDesc;
      if (desc->typeId == T_STRUCT) {
          if (from->kind == TR_VALUE && desc->typeDefinition == from->descriptorDesc->typeDefinition) {
              return TRUE;
          }
          if (report) {
            reportDiagnostic(ctx, DIAG_NON_CASTABLE_TYPE, coords, to);
          }
          return FALSE;
      }
      if (desc->typeId == T_UNION) {
          if (from->kind == TR_VALUE && desc->typeDefinition == from->descriptorDesc->typeDefinition) {
              return TRUE;
          }
          return checkUnionIsCastableToType(ctx, coords, from, desc->typeDefinition, report);
      }
  }

  if (isPointerLikeType(from) && isRealType(to)) {
      if (report) {
        reportDiagnostic(ctx, DIAG_POINTER_CANNOT_BE_CAST, coords, to);
      }
      return FALSE;
  }

  if (isRealType(from) && isPointerLikeType(to)) {
      if (report) {
        reportDiagnostic(ctx, DIAG_CANNOT_BE_CAST_TO_POINTER, coords, from);
      }
      return FALSE;
  }


  if (isStructualType(from)) {
      if (report) {
        reportDiagnostic(ctx, DIAG_NON_CASTABLE_OPERAND, coords, from);
      }
      return FALSE;
  }

  return TRUE;
}

static ExpressionType assignOpTokenToOp(ExpressionType asg_op) {
    switch (asg_op) {
      case EB_ASG_MUL: return EB_MUL;
      case EB_ASG_DIV: return EB_DIV;
      case EB_ASG_MOD: return EB_MOD;
      case EB_ASG_ADD: return EB_ADD;
      case EB_ASG_SUB: return EB_SUB;
      case EB_ASG_SHL: return EB_LHS;
      case EB_ASG_SHR: return EB_RHS;
      case EB_ASG_AND: return EB_AND;
      case EB_ASG_XOR: return EB_XOR;
      case EB_ASG_OR: return EB_OR;
      default: unreachable("Unepxected token");
    }

    unreachable("Unepxected token");
    return (ExpressionType)-1;
}

TypeRef *computeAssignmentTypes(ParserContext *ctx, Coordinates *coords, ExpressionType op, AstExpression *leftExpr, AstExpression *rightExpr) {
  TypeRef *left = leftExpr->type;
  TypeRef *right = rightExpr->type;

  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  TypeRef *rhsType = right;

  if (op != EB_ASSIGN) {
      ExpressionType op2 = assignOpTokenToOp(op);
      rhsType = computeBinaryType(ctx, coords, leftExpr, rightExpr, op2);
  }

  if (isAssignableTypes(ctx, coords, left, rhsType, rightExpr, FALSE)) {
      return left;
  }

  return makeErrorRef(ctx);

}

TypeRef *computeFunctionType(ParserContext *ctx, Coordinates *coords, AstFunctionDeclaration *declaration) {
  TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));

  result->kind = TR_FUNCTION;
  result->functionTypeDesc.returnType = declaration->returnType;
  result->functionTypeDesc.isVariadic = declaration->isVariadic;

  AstValueDeclaration *param = declaration->parameters;
  TypeList *head = NULL, *tail = NULL;
  while (param) {
      TypeList *next = (TypeList *)areanAllocate(ctx->memory.typeArena, sizeof(TypeList));
      next->type = param->type;
      if (tail) {
          tail->next = next;
      } else {
          head = next;
      }
      tail = next;
      param = param->next;
  }

  result->functionTypeDesc.parameters = head;

  return result;
}

static Boolean isStaticSymbol(Symbol *s) {
  if (s->kind == FunctionSymbol || s->kind == ValueSymbol && !s->variableDesc->flags.bits.isLocal) {
    // TODO: probably it's not the best solution
    return TRUE;
  }
  return FALSE;
}

static Boolean isCompileTimeConstant2(AstExpression *expr, Boolean allowRefs) {
  switch (expr->op) {
    case EU_PRE_INC:
    case EU_POST_INC:
    case EU_PRE_DEC:
    case EU_POST_DEC:
    case EU_PLUS:      /** +a */
    case EU_MINUS:     /** -a */
    case EU_TILDA:     /** ~a */
    case EU_EXL:       /** !a */
      return isCompileTimeConstant2(expr->unaryExpr.argument, FALSE);
    case E_TERNARY:
      return isCompileTimeConstant2(expr->ternaryExpr.condition, FALSE)
          && isCompileTimeConstant2(expr->ternaryExpr.ifTrue, FALSE)
          && isCompileTimeConstant2(expr->ternaryExpr.ifFalse, FALSE);
    case E_CONST:
      return TRUE;
    case E_CAST:
      return isCompileTimeConstant2(expr->castExpr.argument, allowRefs);
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
    case EB_ANDAND:
    case EB_OROR:
    case EB_EQ:
    case EB_NE:
    case EB_LT:
    case EB_LE:
    case EB_GT:
    case EB_GE:
    case EB_COMMA:
      return isCompileTimeConstant2(expr->binaryExpr.left, FALSE) && isCompileTimeConstant2(expr->binaryExpr.right, FALSE);
    case E_PAREN:
      return isCompileTimeConstant2(expr->parened, allowRefs);
    case EU_REF:
      return isCompileTimeConstant2(expr->unaryExpr.argument, TRUE);
    case EB_A_ACC:
      if (allowRefs) {
          AstExpression *l = expr->binaryExpr.left;
          AstExpression *r = expr->binaryExpr.right;
          AstExpression *arr = isPointerLikeType(l->type) ? l : r;
          AstExpression *idx = arr == l ? r : l;
          return isCompileTimeConstant2(arr, TRUE) && isCompileTimeConstant2(idx, FALSE);
      }
      return FALSE;
    case E_NAMEREF: {
      if (allowRefs) {
        Symbol *s = expr->nameRefExpr.s;
        return isStaticSymbol(s);
      }
    }
    case EF_ARROW:
      if (allowRefs) {
          return isCompileTimeConstant2(expr->fieldExpr.recevier, TRUE);
      }
    default:
      return FALSE;
  }
}

static Boolean isCompileTimeConstant(AstExpression *expr) {
  if (expr->op == E_NAMEREF) {
    return isStaticSymbol(expr->nameRefExpr.s);
  }
  return isCompileTimeConstant2(expr, FALSE);
}

static AstExpression *parenIfNeeded(ParserContext *ctx, AstExpression *expr) {
  switch (expr->op) {
    case E_CONST:
    case E_NAMEREF:
    case E_CALL:
    case E_PAREN:
    case E_ERROR:
    case EB_COMMA:
    case EB_A_ACC:
      return expr;
    default:
      return createParenExpression(ctx, &expr->coordinates, expr);
  }
}

enum ParsedLoc {
  PL_OPEN,
  PL_INNER,
  PL_CLOSE,
  PL_SEPARATOR
};

typedef struct _ParsedInitializer {
  Coordinates coords;
  AstExpression *expression;
  int32_t level;
  enum ParsedLoc loc;
  struct _ParsedInitializer *next;
} ParsedInitializer;

static AstInitializer *finalizeInitializerInternal(ParserContext *ctx, TypeRef *valueType, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel);

static AstInitializer *fillInitializer(ParserContext *ctx, Coordinates *coords, TypeRef *type, int32_t offset) {

  if (isScalarType(type) || type->kind == TR_BITFIELD) {
      AstInitializer *new = createAstInitializer(ctx, coords, IK_EXPRESSION);
      new->offset = offset;
      new->slotType = type;
      uint64_t c = 0;
      if (type->kind == TR_POINTED) {
        new->expression = createAstConst(ctx, coords, CK_INT_CONST, &c);
      } else if (isRealType(type)) {
        long double cc = 0.0;
        new->expression = createAstConst(ctx, coords, CK_FLOAT_CONST, &cc);
      } else {
        new->expression = createAstConst(ctx, coords, CK_INT_CONST, &c);
      }
      new->expression->type = type;
      return new;
  }

  AstInitializer *new = createAstInitializer(ctx, coords, IK_LIST);
  new->offset = offset;
  new->slotType = type;

  AstInitializerList head = { 0 };
  AstInitializerList *current = &head;
  int32_t elementCount = 0;

  if (type->kind == TR_ARRAY) {
      int32_t arraySize = type->arrayTypeDesc.size;
      TypeRef *elementType = type->arrayTypeDesc.elementType;
      int32_t elementSize = computeTypeSize(elementType);
      int32_t innerOffset = offset;

      int i;
      for (i = 0; i < arraySize; ++i) {
          AstInitializerList *newNode = createAstInitializerList(ctx);
          newNode->initializer = fillInitializer(ctx, coords, elementType, offset);

          current = current->next = newNode;
          innerOffset += elementSize;
          ++elementCount;
      }
  } else if (isStructualType(type)) {
      TypeDefiniton *definition = type->descriptorDesc->typeDefinition;
      assert(definition);

      StructualMember *member = definition->members;

      for (; member; member = member->next) {
          AstInitializerList *newNode = createAstInitializerList(ctx);

          newNode->initializer = fillInitializer(ctx, coords, member->type, offset + member->offset);

          current = current->next = newNode;
          ++elementCount;
      }
  } else if (isUnionType(type)) {
      TypeDefiniton *definition = type->descriptorDesc->typeDefinition;
      assert(definition);

      StructualMember *member = definition->members;
      AstInitializerList *newNode = createAstInitializerList(ctx);
      newNode->initializer = fillInitializer(ctx, coords, member->type, offset + 0);
      current = current->next = newNode;
  }

  new->initializerList = head.next;
  new->numOfInitializers = elementCount;

  return new;
}

static AstInitializer *finalizeStructInitializer(ParserContext *ctx, TypeRef *type, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel) {
  assert(isStructualType(type));
  TypeDefiniton *definition = type->descriptorDesc->typeDefinition;
  assert(definition->kind == TDK_STRUCT);

  AstExpression *expr = initializer->expression;

  if (expr) {
      if (isTopLevel && !isCompileTimeConstant(expr)) {
        reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coords);
        AstInitializer *new = createAstInitializer(ctx, &initializer->coords, IK_EXPRESSION);
        new->expression = createErrorExpression(ctx, &expr->coordinates);
        new->slotType = makeErrorRef(ctx);
        new->offset = -1;

        *next = initializer->next;
        return new;
      }

      if (typesEquals(type, expr->type)) {
          AstInitializer *new = createAstInitializer(ctx, &initializer->coords, IK_EXPRESSION);
          new->expression = expr;
          new->offset = offset;
          new->slotType = type;

          *next = initializer->next;
          return new;
      }
  }

  Coordinates coords = initializer->coords;

  StructualMember *member = definition->members;
  AstInitializerList head = { 0 };
  AstInitializerList *newInit = &head;

  int32_t level = initializer->level;

  unsigned memberCount = 0;

  while (member && initializer) {
      if (initializer->level < level) {
          break;
      }

      ++memberCount;

      TypeRef *memberType = member->type;
      int32_t memberOffset = offset + member->offset;

      AstInitializerList *newNode = createAstInitializerList(ctx);

      newNode->initializer = finalizeInitializerInternal(ctx, memberType, initializer, &initializer, memberOffset, isTopLevel);
      memberOffset += computeTypeSize(memberType);
      newInit = newInit->next = newNode;
      member = member->next;
  }

  if (initializer && initializer->level >= level) {
      while (initializer && initializer->level >= level)  {
          if (initializer->expression) {
            reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates);
          }
          coords.right = initializer->coords.right;
          initializer = initializer->next;
      }
  }


  for (; member; member = member->next) {
      AstInitializerList *newNode = createAstInitializerList(ctx);

      newNode->initializer = fillInitializer(ctx, &coords, member->type, offset + member->offset);

      newInit = newInit->next = newNode;
      ++memberCount;
  }

  AstInitializer *new = createAstInitializer(ctx, &coords, IK_LIST);

  new->initializerList = head.next;
  new->numOfInitializers = memberCount;
  new->offset = offset;
  new->slotType = type;

  *next = initializer;

  return new;
}


static AstInitializerList *createSymbolInitNode(ParserContext *ctx, Coordinates *_coords, TypeRef *type, int64_t c, int32_t offset) {

//  int64_t c = s[i];
  AstExpression *expr = createAstConst(ctx, _coords, CK_INT_CONST, &c);
  expr->type = type;

  AstInitializer *init = createAstInitializer(ctx, _coords, IK_EXPRESSION);
  init->expression = expr;
  init->slotType = type;
  init->offset = offset;

  AstInitializerList *node = createAstInitializerList(ctx);
  node->initializer = init;

  return node;
}

static AstInitializer *stringLiteralToInitializer(ParserContext *ctx, Coordinates *coords, const char *s, int32_t offset) {
  AstInitializerList head = { 0 }, *current = &head;

  unsigned i = 0;

  TypeRef *charType = makePrimitiveType(ctx, T_S1, 0);

  // TODO: probably worth to rethink coords?
  for (; s[i]; ++i) {
    current = current->next = createSymbolInitNode(ctx, coords, charType, s[i], offset + i);
  }

  current = current->next = createSymbolInitNode(ctx, coords, charType, 0, offset + i);

  AstInitializer *listInit = createAstInitializer(ctx, coords, IK_LIST);
  listInit->initializerList = head.next;
  listInit->numOfInitializers = i + 1;

  return listInit;
}

static AstInitializer *finalizeArrayInitializer(ParserContext *ctx, TypeRef *type, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel) {

  TypeRef *elementType = type->arrayTypeDesc.elementType;
  AstExpression *expr = initializer->expression;
  Coordinates coords = initializer->coords;

  if (expr) {
      if (isCharType(elementType)) {
          if (expr->op == E_CONST && expr->constExpr.op == CK_STRING_LITERAL) {
              assert(expr->type->kind == TR_ARRAY);
              type->arrayTypeDesc.size = expr->type->arrayTypeDesc.size;
              AstInitializer *init = stringLiteralToInitializer(ctx, &expr->coordinates, expr->constExpr.l, offset);
              init->slotType = type;
              init->offset = offset;
              *next = initializer->next;
              return init;
          } else {
              reportDiagnostic(ctx, DIAG_ARRAY_INIT_LIST_OR_LITERAL, &initializer->coords);
              AstInitializer *new = createAstInitializer(ctx, &coords, IK_EXPRESSION);
              new->expression = createErrorExpression(ctx, &expr->coordinates);
              new->slotType = makeErrorRef(ctx);
              new->offset = -1;
              *next = initializer->next;
              return new;
          }
      } else {
          reportDiagnostic(ctx, DIAG_ARRAY_INIT_LIST, &initializer->coords);
          AstInitializer *new = createAstInitializer(ctx, &coords, IK_EXPRESSION);
          new->expression = createErrorExpression(ctx, &expr->coordinates);
          new->slotType = makeErrorRef(ctx);
          new->offset = -1;
          *next = initializer->next;
          return new;
      }
  }

  assert(expr == NULL);

  AstInitializerList head = { 0 };
  AstInitializerList *newInit = &head;

  int32_t arraySize = type->arrayTypeDesc.size;
  int32_t currentOffset = offset;
  int32_t elementSize = computeTypeSize(elementType);
  int32_t level = initializer->level;
  int32_t elementsCount = 0;

  while (initializer) {

      if (initializer->level < level) {
          break;
      }

      if (initializer->expression == NULL) {
          coords.right = initializer->coords.right;
          initializer = initializer->next;
          continue;
      }

      if (elementsCount >= arraySize) {
          while (initializer && initializer->level >= level)  {
              if (initializer->expression) {
                reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates);
              }
              coords.right = initializer->coords.right;
              initializer = initializer->next;
          }
          break;
      }

      AstInitializer *newInitializer = finalizeInitializerInternal(ctx, elementType, initializer, &initializer, currentOffset, isTopLevel);
      AstInitializerList *newNode = createAstInitializerList(ctx);

      ++elementsCount;

      currentOffset += elementSize;
      newNode->initializer = newInitializer;
      newInit = newInit->next = newNode;

  }

  for (; elementsCount < arraySize; ++elementsCount) {
      AstInitializerList *newNode = createAstInitializerList(ctx);
      newNode->initializer = fillInitializer(ctx, &coords, elementType, currentOffset);
      newInit = newInit->next = newNode;
      currentOffset += elementSize;
  }

  AstInitializer *result = createAstInitializer(ctx, &coords, IK_LIST);
  result->numOfInitializers = elementsCount;
  result->initializerList = head.next;
  result->offset = offset;
  result->slotType = type;

  *next = initializer;

  return result;
}

static AstInitializer *finalizeUnionInitializer(ParserContext *ctx, TypeRef *type, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel) {
  assert(isUnionType(type));

  TypeDefiniton *definition = type->descriptorDesc->typeDefinition;
  AstExpression *expr = initializer->expression;

  if (expr) {
      if (isTopLevel && !isCompileTimeConstant(expr)) {
        reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coords);
        AstInitializer *new = createAstInitializer(ctx, &initializer->coords, IK_EXPRESSION);
        new->expression = createErrorExpression(ctx, &expr->coordinates);
        new->slotType = makeErrorRef(ctx);
        new->offset = -1;

        *next = initializer->next;
        return new;
      }

      if (typesEquals(type, expr->type)) {
          AstInitializer *new = createAstInitializer(ctx, &initializer->coords, IK_EXPRESSION);
          new->expression = expr;
          new->offset = offset;
          new->slotType = type;

          *next = initializer->next;
          return new;
      }
  }

  StructualMember *member = definition->members;
  Coordinates *coords = &initializer->coords;
  TypeRef *memberType = member->type;
  int32_t memberOffset = offset + member->offset;

  AstInitializerList *newNode = createAstInitializerList(ctx);

  newNode->initializer = finalizeInitializerInternal(ctx, memberType, initializer, &initializer, memberOffset, isTopLevel);

  AstInitializer *new = createAstInitializer(ctx, coords, IK_LIST);

  new->initializerList = newNode;
  new->numOfInitializers = 1;
  new->offset = offset;
  new->slotType = type;

  *next = initializer;

  return new;
}

static AstInitializer *finalizeScalarInitializer(ParserContext *ctx, TypeRef *type, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel) {

  assert(isScalarType(type) || type->kind == TR_BITFIELD);

  AstExpression *expr = initializer->expression;
  AstInitializer *new = createAstInitializer(ctx, &initializer->coords, IK_EXPRESSION);

  if (expr == NULL) {
      if (initializer->next == NULL || initializer->next->expression == NULL && initializer->next->level == initializer->level) {
          // empty case int x = {};

          Coordinates coords = initializer->coords;
          if (initializer->next)
          coords.right = initializer->next->coords.right;

          AstExpression *constNull = NULL;
          if (isRealType(type)) {
            long double v = 0.0;
            constNull = createAstConst(ctx, &coords, CK_FLOAT_CONST, &v);
          } else {
            uint64_t v = 0;
            constNull = createAstConst(ctx, &coords, CK_INT_CONST, &v);
          }

          constNull->type = type;
          new->expression = constNull;
          new->slotType = type;
          new->offset = offset;
          return new;
      } else {
        return finalizeScalarInitializer(ctx, type, initializer->next, next, offset, isTopLevel);
      }
  }

  TypeRef *exprType = expr->type;

  if (isAssignableTypes(ctx, &expr->coordinates, type, exprType, expr, TRUE)) {
      if (isTopLevel && !isCompileTimeConstant(expr)) {
          reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coords);
          new->expression = createErrorExpression(ctx, &expr->coordinates);
          new->slotType = makeErrorRef(ctx);
          new->offset = -1;
      } else {
        if (!typesEquals(type, exprType)) {
            expr = createCastExpression(ctx, &expr->coordinates, type, expr);
        }

        new->expression = expr;
        new->slotType = type;
        new->offset = offset;
      }
  } else {
      new->expression = createErrorExpression(ctx, &expr->coordinates);
      new->slotType = makeErrorRef(ctx);
      new->offset = -1;
  }

  *next = initializer->next;
  return new;
}

static int32_t countElementsIn(ParsedInitializer *parsed) {
  int32_t levelElems = 0;
  int32_t levelBrakets = 0;
  int32_t level = parsed->level;

  Boolean skipping = FALSE;

  for (; parsed && parsed->level >= level; parsed = parsed->next) {
      if (parsed->level == level && parsed->expression) {
          ++levelElems;
      } else if (parsed->level == level + 1 && parsed->loc == PL_OPEN) {
          ++levelElems;
      }
  }

  return levelElems;
}

static AstInitializer *finalizeInitializerInternal(ParserContext *ctx, TypeRef *valueType, ParsedInitializer *initializer, ParsedInitializer **next, int32_t offset, Boolean isTopLevel) {
  Coordinates *coords = &initializer->coords;
  if (isErrorType(valueType)) {
      AstInitializer *new = createAstInitializer(ctx, coords, IK_EXPRESSION);
      new->expression = createErrorExpression(ctx, coords);
      new->numOfInitializers = -1;
      new->slotType = valueType;
      new->offset = -1;
      *next = initializer->next;
      return new;
  }

  int32_t typeSize = computeTypeSize(valueType);

  if (valueType->kind == TR_ARRAY && valueType->arrayTypeDesc.size == UNKNOWN_SIZE) {
      // TODO: probably worth to support flexible arrays
      AstExpression *expr = initializer->expression;
      if (expr == NULL) {
          valueType->arrayTypeDesc.size = countElementsIn(initializer);
      }
  }

  if (isStructualType(valueType)) {
      return finalizeStructInitializer(ctx, valueType, initializer, next, offset, isTopLevel);
  }

  if (valueType->kind == TR_ARRAY) {
      return finalizeArrayInitializer(ctx, valueType, initializer, next, offset, isTopLevel);
  }

  if (isUnionType(valueType)) {
      return finalizeUnionInitializer(ctx, valueType, initializer, next, offset, isTopLevel);
  }


  return finalizeScalarInitializer(ctx, valueType, initializer, next, offset, isTopLevel);
}

static ParsedInitializer *allocParsedInitializer(ParserContext *ctx, Coordinates *coords, AstExpression *expr, int32_t level, enum ParsedLoc loc)  {
  ParsedInitializer *p = areanAllocate(ctx->memory.astArena, sizeof(ParsedInitializer));

  p->coords = *coords;
  p->expression = expr;
  p->level = level;
  p->loc = loc;

  return p;
}

static ParsedInitializer *flatInitializer(ParserContext *ctx, AstInitializer *init, int32_t level, ParsedInitializer **next) {

  if (init->kind == IK_EXPRESSION) {
      return *next = allocParsedInitializer(ctx, &init->coordinates, init->expression, level, PL_INNER);
  } else {
      ParsedInitializer head = { 0 };
      ParsedInitializer *current = &head;
      Coordinates coords = init->coordinates;

      coords.left = coords.right = init->coordinates.left;

      current = current->next = allocParsedInitializer(ctx, &coords, NULL, level + 1, PL_OPEN);

      AstInitializerList *inits = init->initializerList;

      for (;inits; inits = inits->next) {
          ParsedInitializer *next = NULL;
          current->next = flatInitializer(ctx, inits->initializer, level + 1, &next);
          current = next;
      }

      coords.left = coords.right = init->coordinates.right;
      current = current->next = allocParsedInitializer(ctx, &coords, NULL, level + 1, PL_CLOSE);

      *next = current->next = allocParsedInitializer(ctx, &coords, NULL, level, PL_SEPARATOR);
      return head.next;
  }
}

AstInitializer *finalizeInitializer(ParserContext *ctx, TypeRef *valueType, AstInitializer *init, Boolean isTopLevel) {
  ParsedInitializer *dummy;
  // TODO: parse tokens into ParsedInitializer initially
  ParsedInitializer *parsed = flatInitializer(ctx, init, 0, &dummy);

  return finalizeInitializerInternal(ctx, valueType, parsed, &dummy, 0, isTopLevel);
}

AstExpression *transformTernaryExpression(ParserContext *ctx, AstExpression *expr) {
  assert(expr->op == E_TERNARY);

  if (isErrorType(expr->type)) return expr;

  AstExpression *ifTrue = expr->ternaryExpr.ifTrue;
  AstExpression *ifFalse = expr->ternaryExpr.ifFalse;

  if (!typeEquality(expr->type, ifTrue->type)) {
      expr->ternaryExpr.ifTrue = createCastExpression(ctx, &ifTrue->coordinates, expr->type, parenIfNeeded(ctx, ifTrue));
  }

  if (!typeEquality(expr->type, ifFalse->type)) {
      expr->ternaryExpr.ifFalse = createCastExpression(ctx, &ifFalse->coordinates, expr->type, parenIfNeeded(ctx, ifFalse));
  }

  return expr;
}

AstExpression *transformBinaryExpression(ParserContext *ctx, AstExpression *expr) {
  assert(EB_ADD <= expr->op && expr->op <= EB_GE);

  if (isErrorType(expr->type)) return expr;

  AstExpression *left = expr->binaryExpr.left;
  AstExpression *right = expr->binaryExpr.right;

  if (isPointerLikeType(left->type) || isPointerLikeType(right->type)) {

      if (isPointerLikeType(expr->type)) {
          TypeRef *elemType = elementType(left->type);
          AstExpression *base = left;
          AstExpression *offset = right;
          if (elemType == NULL) {
              elemType = elementType(right->type);
              base = right;
              offset = left;
          }

          int64_t elemSize = isVoidType(elemType) ? 1 : computeTypeSize(elemType);

          if (elemSize != 1) {
            AstExpression *sizeConst = createAstConst(ctx, &offset->coordinates, CK_INT_CONST, &elemSize);
            sizeConst->type = makePrimitiveType(ctx, T_S8, 0);
            offset = createBinaryExpression(ctx, EB_MUL, sizeConst->type, offset, sizeConst);
          }

          if (base == left) {
              expr->binaryExpr.left = base;
              expr->binaryExpr.right = offset;
          } else {
              expr->binaryExpr.left = offset;
              expr->binaryExpr.right = base;
          }

      } else {
          if (expr->op == EB_SUB) {
            TypeRef *elemType = elementType(left->type);
            int64_t elemSize = isVoidType(elemType) ? 1 : computeTypeSize(elemType);
            if (elemSize != 1) {
              AstExpression *astConst = createAstConst(ctx, &expr->coordinates, CK_INT_CONST, &elemSize);
              astConst->type = expr->type;
              return createBinaryExpression(ctx, EB_DIV, expr->type, expr, astConst);
            }
          }
      }
      return expr;
  }

  assert(left->type->kind == TR_VALUE || left->type->kind == TR_BITFIELD);
  assert(right->type->kind == TR_VALUE || right->type->kind == TR_BITFIELD);

  TypeRef *commonType = commonPrimitiveType(ctx, left->type, right->type);

  if (!typesEquals(commonType, left->type)) {
      left = createCastExpression(ctx, &left->coordinates, commonType, parenIfNeeded(ctx, left));
  }

  if (!typesEquals(commonType, right->type)) {
      right = createCastExpression(ctx, &right->coordinates, commonType, parenIfNeeded(ctx, right));
  }

  expr->binaryExpr.left = left;
  expr->binaryExpr.right = right;

  return expr;
}

AstExpression *transformAssignExpression(ParserContext *ctx, AstExpression *expr) {
  assert(isAssignmentOp(expr->op));

  if (isErrorType(expr->type)) return expr;

  AstExpression *lvalue = expr->binaryExpr.left;
  AstExpression *rvalue = expr->binaryExpr.right;

  if (!typesEquals(lvalue->type, rvalue->type)) {
      TypeRef *castTo = lvalue->type;

      if (castTo->kind == TR_BITFIELD)
        castTo = castTo->bitFieldDesc.storageType;

      expr->binaryExpr.right = createCastExpression(ctx, &rvalue->coordinates, castTo, parenIfNeeded(ctx, rvalue));
  }

  return expr;
}

void verifyStatementLevelExpression(ParserContext *ctx, AstExpression *expr) {
  expr = deparen(expr);
  TypeRef *type = expr->type;
  if (isErrorType(type) || isVoidType(type)) return;

  if (expr->op == E_CALL || isAssignmentOp(expr->op)) return;

  if (expr->op == EU_POST_INC || expr->op == EU_POST_DEC || expr->op == EU_PRE_INC || expr->op == EU_PRE_DEC) return;

  if (expr->op == EB_COMMA) {
      verifyStatementLevelExpression(ctx, expr->binaryExpr.left);
      verifyStatementLevelExpression(ctx, expr->binaryExpr.right);
  } else {
      reportDiagnostic(ctx, DIAG_UNUSED_EXPR_RES, &expr->coordinates);
  }
}

// true if everything is OK
Boolean verifyValueType(ParserContext *ctx, Coordinates *coords, TypeRef *valueType) {
  if (isErrorType(valueType)) return TRUE;

  if (valueType->kind == TR_VALUE) {
      TypeDesc *desc = valueType->descriptorDesc;
      if (desc->typeId == T_VOID || desc->size == UNKNOWN_SIZE) {
          reportDiagnostic(ctx, DIAG_VAR_INCOMPLETE_TYPE, coords, desc);
          return FALSE;
      }
  }

  return TRUE;
}

void verifyAndTransformCallAruments(ParserContext *ctx, Coordinates *coords, TypeRef *functionType, AstExpressionList *aruments) {
  if (isErrorType(functionType)) return;

  if (functionType->kind == TR_POINTED)
    functionType = functionType->pointedTo.toType;

  assert(functionType->kind == TR_FUNCTION);

  AstExpressionList *argument = aruments;
  TypeList *param = functionType->functionTypeDesc.parameters;

  while (argument && param) {
      AstExpression *arg = argument->expression;
      TypeRef *aType = arg->type;
      TypeRef *pType = param->type;
      if (isAssignableTypes(ctx, &arg->coordinates, pType, aType, arg, FALSE)) {
          if (!typesEquals(pType, aType)) {
              argument->expression = createCastExpression(ctx, coords, pType, parenIfNeeded(ctx, arg));
          }
          argument = argument->next;
          param = param->next;
      } else {
          break;
      }
  }

  if (argument == NULL && param != NULL) { // fewer
      reportDiagnostic(ctx, DIAG_TOO_FEW_ARGS, coords);
  }

  if (argument != NULL && param == NULL) {
      if (!functionType->functionTypeDesc.isVariadic) {
          reportDiagnostic(ctx, DIAG_TOO_MANY_ARGS, coords);
      } else {
          /*
           *  6.5.2.2.7 The ellipsis notation in a function prototype declarator causes argument type
           *  conversion to stop after the last declared parameter. The default argument promotions are
           *  performed on trailing arguments.
           */
          while(argument) {
              AstExpression *arg = argument->expression;
              TypeRef *argType = arg->type;
              int size = computeTypeSize(argType);

              if (isRealType(argType) && size == 4) {
                TypeRef *doubleType = makePrimitiveType(ctx, T_F8, 0);
                argument->expression = createCastExpression(ctx, &arg->coordinates, doubleType, arg);
              } else if (isScalarType(argType) && size < 8) {
                assert(argType->kind == TR_VALUE);
                TypeId typeId = argType->descriptorDesc->typeId;
                TypeId toTypeId = typeId < T_S8 ? T_S8 : T_U8;
                TypeRef *toType = makePrimitiveType(ctx, toTypeId, 0);
                argument->expression = createCastExpression(ctx, &arg->coordinates, toType, arg);
              }
              argument = argument->next;
          }
      }
  }
}

static Boolean checkInSet(int v, unsigned caseLimit, int *caseSet) {
  unsigned i;
  for (i = 0; i < caseLimit; ++i) {
      if (caseSet[i] == v) return TRUE;
  }

  return FALSE;
}

static void verifySwtichCasesRec(ParserContext *ctx, AstStatement *stmt, unsigned caseCount, unsigned *caseIndex, int *caseSet, Boolean *hasDefault) {
  switch (stmt->statementKind) {
    case SK_BLOCK: {
        AstStatementList *node = stmt->block.stmts;
        while (node) {
            verifySwtichCasesRec(ctx, node->stmt, caseCount, caseIndex, caseSet, hasDefault);
            node = node->next;
        }
        break;
    }

    case SK_LABEL: {
        AstLabelStatement *label = &stmt->labelStmt;
        switch (label->kind) {
        case LK_LABEL:
            verifySwtichCasesRec(ctx, label->body, caseCount, caseIndex, caseSet, hasDefault);
            break;
        case LK_CASE:
            if (checkInSet(label->caseConst, *caseIndex, caseSet)) {
                reportDiagnostic(ctx, DIAG_DUPLICATE_CASE_VALUE, &stmt->coordinates, label->caseConst);
            } else {
                assert(*caseIndex < caseCount);
                caseSet[*caseIndex] = label->caseConst;
                *caseIndex += 1;
            }
            verifySwtichCasesRec(ctx, label->body, caseCount, caseIndex, caseSet, hasDefault);
            break;
        case LK_DEFAULT:
            if (*hasDefault) {
                reportDiagnostic(ctx, DIAG_MULTIPLE_DEFAULT_LABELS, &stmt->coordinates);
            }
            (*hasDefault) = TRUE;
            verifySwtichCasesRec(ctx, label->body, caseCount, caseIndex, caseSet, hasDefault);

            break;
        }
        break;
    }

    case SK_IF:
        verifySwtichCasesRec(ctx, stmt->ifStmt.thenBranch, caseCount, caseIndex, caseSet, hasDefault);
        if (stmt->ifStmt.elseBranch) {
            verifySwtichCasesRec(ctx, stmt->ifStmt.elseBranch, caseCount, caseIndex, caseSet, hasDefault);
        }
        break;
    case SK_WHILE:
        verifySwtichCasesRec(ctx, stmt->loopStmt.body, caseCount, caseIndex, caseSet, hasDefault);
        break;
    case SK_DO_WHILE:
        verifySwtichCasesRec(ctx, stmt->loopStmt.body, caseCount, caseIndex, caseSet, hasDefault);
        break;
    case SK_FOR:
        verifySwtichCasesRec(ctx, stmt->forStmt.body, caseCount, caseIndex, caseSet, hasDefault);
        break;
    case SK_SWITCH: // stop
    case SK_RETURN:
    case SK_DECLARATION:
    case SK_EXPR_STMT:
    case SK_EMPTY:
    case SK_ERROR:
    case SK_BREAK:
    case SK_CONTINUE:
    case SK_GOTO_L:
    case SK_GOTO_P:
      break;
  }
}

void verifyGotoExpression(ParserContext *ctx, AstExpression *expr) {
  TypeRef *type = expr->type;
  if (isErrorType(type)) return;

  // TODO: introduce special type for such pointers
  if (type->kind == TR_POINTED && isVoidType(type->pointedTo.toType)) {
      return ;
  }

  reportDiagnostic(ctx, DIAG_ILL_INDIRECT_GOTO_OPERAND, &expr->coordinates);
}

void verifySwitchCases(ParserContext *ctx, AstStatement *switchBody, unsigned caseCount) {
  int *caseSet = heapAllocate(sizeof (int) * caseCount);
  unsigned caseIndex = 0;
  Boolean hasDefault = FALSE;

  verifySwtichCasesRec(ctx, switchBody, caseCount, &caseIndex, caseSet, &hasDefault);

  releaseHeap(caseSet);
}

int stringHashCode(intptr_t v) {
    const char *s = (const char *)v;
    assert(s != NULL && "hashMap key is NULL");
    int result = 0;

    int i = 0;

    while (s[i]) {
        result *= 31;
        result += s[i++];
    }

    return result;
}

int stringCmp(intptr_t v1, intptr_t v2) {
    const char *s1 = (const char *)v1;
    const char *s2 = (const char *)v2;

    return strcmp(s1, s2);
}

Scope *newScope(ParserContext *ctx, Scope *parent) {
  Scope *result = (Scope *)areanAllocate(ctx->memory.typeArena, sizeof (Scope));
  result->parent = parent;
  result->symbols = createHashMap(DEFAULT_MAP_CAPACITY, stringHashCode, stringCmp);
  result->next = ctx->scopeList;
  ctx->scopeList = result;
  return result;
}

static Symbol *findSymbolInScope(Scope *scope, const char *name) {
  return (Symbol *)getFromHashMap(scope->symbols, (intptr_t)name);
}

Symbol* findSymbol(ParserContext *ctx, const char *name) {
    if (name) {
      Scope* s = ctx->currentScope;
      while (s != NULL) {
          Symbol *sb = (Symbol *)getFromHashMap(s->symbols, (intptr_t)name);
          if (sb != NULL) return sb;
          s = s->parent;
      }
    }

    return NULL;
}


int isTypeName(ParserContext *ctx, const char* name, struct _Scope* scope) {
    Symbol *s = findSymbol(ctx, name);
    return s && s->kind == TypedefSymbol;
}

Symbol *newSymbol(ParserContext *ctx, SymbolKind kind, const char *name) {
  Symbol *s = (Symbol *)areanAllocate(ctx->memory.typeArena, sizeof(Symbol));
  s->kind = kind;
  s->name = name;
  return s;
}

Symbol *declareSymbol(ParserContext *ctx, SymbolKind kind, const char *name) {
    Symbol *s = newSymbol(ctx, kind, name);
    Scope *scope = ctx->currentScope;
    putToHashMap(scope->symbols, (intptr_t)name, (intptr_t)s);

    return s;
}

Symbol* findOrDeclareSymbol(ParserContext* ctx, SymbolKind kind, const char* name) {
    Symbol *existed = findSymbol(ctx, name);
    if (existed) return existed;
    return declareSymbol(ctx, kind, name);
}

static int functionsEqual(AstFunctionDeclaration *f1, AstFunctionDeclaration *f2) {
  return TRUE;
}


typedef int (*symbolProcessor)(ParserContext *, Symbol *, void *);

static Symbol *declareGenericSymbol(ParserContext *ctx, SymbolKind kind, const char *name, void *value, symbolProcessor existed, symbolProcessor new) {

  if (name == NULL) return NULL;

  Symbol *s = findSymbolInScope(ctx->currentScope, name);
  if (s) {
      if (s->kind == kind) {
          existed(ctx, s, value);
      } else {
          // TODO: fix location
          Coordinates coords = { ctx->token, ctx->token };
          reportDiagnostic(ctx, DIAG_SYMBOL_REDEFINITION, &coords, name);
      }
  } else {
    s = declareSymbol(ctx, kind, name);
    new(ctx, s, value);
  }

  return s;
}

static int existedTypeDefProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == TypedefSymbol);
  TypeRef *oldType = s->typeref;
  TypeRef *newType = (TypeRef *)value;
  Coordinates coords = { ctx->token, ctx->token };
  if (typesEquals(oldType, newType)) {
      // TODO: Fix location
      reportDiagnostic(ctx, DIAG_TYPEDEF_REDEFINITION_C11, &coords, s->name);
  } else {
      reportDiagnostic(ctx, DIAG_TYPEDEF_REDEFINITION_TYPES, &coords, oldType, newType);
  }
}

static int newTypeDefProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == TypedefSymbol);
  s->typeref = (TypeRef *)value;
}

Symbol *declareTypeDef(ParserContext *ctx, const char *name, TypeRef *type) {
  return declareGenericSymbol(ctx, TypedefSymbol, name, type, existedTypeDefProcessor, newTypeDefProcessor);
}

static int existedFunctionProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == FunctionSymbol);
  AstFunctionDeclaration *oldDeclaration = s->function;
  AstFunctionDeclaration *newDeclaration = (AstFunctionDeclaration *)value;
  if (functionsEqual(oldDeclaration, newDeclaration)) {
      // TODO: link them into list
  } else {
      Coordinates coords = { ctx->token, ctx->token };
      reportDiagnostic(ctx, DIAG_FUN_CONFLICTING_TYPES, &coords, oldDeclaration->name);
  }
}

static int newFunctionProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == FunctionSymbol);
  s->function = (AstFunctionDeclaration *)value;
}

Symbol *declareFunctionSymbol(ParserContext *ctx, const char *name, AstFunctionDeclaration *declaration) {
  return declareGenericSymbol(ctx, FunctionSymbol, name, declaration, existedFunctionProcessor, newFunctionProcessor);
}

static int existedValueProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == ValueSymbol);

  AstValueDeclaration *oldValue = s->variableDesc;
  AstValueDeclaration *newValue = (AstValueDeclaration *)value;

  TypeRef *oldType = oldValue->type;
  TypeRef *newType = newValue->type;

  if (typesEquals(oldType, newType)) {
      if (oldValue->flags.bits.isExternal || newValue->flags.bits.isExternal) {
          // TODO: link declarations to list

      } else {
          reportDiagnostic(ctx, DIAG_MEMBER_REDEFINITION, &newValue->coordinates, s->name, newType, oldType);
      }
  } else {
      Coordinates coords = { ctx->token, ctx->token };
      reportDiagnostic(ctx, DIAG_VALUE_REDEFINITION_TYPES, &newValue->coordinates, s->name, newType, oldType);
  }
}

static int newValueProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == ValueSymbol);
  s->variableDesc = (AstValueDeclaration *)value;
}

Symbol *declareValueSymbol(ParserContext *ctx, const char *name, AstValueDeclaration *declaration) {
  return declareGenericSymbol(ctx, ValueSymbol, name, declaration, existedValueProcessor, newValueProcessor);
}

Symbol *declareTypeSymbol(ParserContext *ctx, SymbolKind symbolKind, TypeId typeId, const char *symbolName, TypeDefiniton *definition) {
  Symbol *s = findSymbolInScope(ctx->currentScope, symbolName); // TODO: allow local struct redeclaration

  const char *name = definition->name;

  TypeDesc *typeDescriptor;
  if (!s) {
      s = declareSymbol(ctx, symbolKind, symbolName);
      int typeSize = UNKNOWN_SIZE;
      if (definition->members) {
        typeSize = computeTypeDefinitionSize(ctx, definition);
      }
      typeDescriptor = s->typeDescriptor = createTypeDescriptor(ctx, typeId, name, typeSize);
      typeDescriptor->typeDefinition = definition;
  } else {
      if (s->kind != symbolKind) {
          reportDiagnostic(ctx, DIAG_USE_WITH_DIFFERENT_TAG, &definition->coordinates, name);
          // TODO: also point to already defined one
      } else {
          typeDescriptor = s->typeDescriptor;
          TypeDefiniton *existedDeclaration = typeDescriptor->typeDefinition;
          if (definition->members) {
            if (existedDeclaration->members) {
              reportDiagnostic(ctx, DIAG_MEMBER_REDEFINITION, &definition->coordinates, name);
              // TODO: also point to already defined one
            } else {
              typeDescriptor->typeDefinition = definition;
              typeDescriptor->size = computeTypeDefinitionSize(ctx, definition);
            }
          }
      }
  }
  return s;
}

Symbol *declareEnumConstantSymbol(ParserContext *ctx, EnumConstant *enumerator) {
  Symbol *s = findSymbolInScope(ctx->currentScope, enumerator->name);
  if (s) {
      enum DiagnosticId diag = s->kind == EnumConstSymbol ? DIAG_ENUMERATOR_REDEFINITION : DIAG_MEMBER_REDEFINITION;
      reportDiagnostic(ctx, diag, &enumerator->coordinates, enumerator->name);
      return NULL; // or 's'?
  }

  s = declareSymbol(ctx, EnumConstSymbol, enumerator->name);
  s->enumerator = enumerator;
  return s;
}

// Types

static TypeDesc errorTypeImpl = { T_ERROR, UNKNOWN_SIZE,  "<error>", NULL };

TypeDesc *errorTypeDescriptor = &errorTypeImpl;

TypeDesc builtInTypeDescriptors[] = {
    { T_VOID, 0, "void", NULL },

    { T_S1, 1, "signed char", NULL },
    { T_S2, 2, "signed short", NULL },
    { T_S4, 4, "signed int", NULL },
    { T_S8, 8, "signed long", NULL },

    { T_U1, 1, "unsigned char", NULL },
    { T_U2, 2, "unsigned short", NULL },
    { T_U4, 4, "unsigned int", NULL },
    { T_U8, 8, "unsigned long", NULL },

    { T_F4, 4, "float", NULL },
    { T_F8, 8, "double", NULL },
    { T_F10, 16, "long double", NULL }
};

int typeIdSize(TypeId id) {
  assert(T_VOID <= id && id < T_BUILT_IN_TYPES);

  return builtInTypeDescriptors[id].size;
}

TypeId typeToId(TypeRef *type) {
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


int computeTypeSize(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      return type->descriptorDesc->size;
  }

  if (type->kind == TR_POINTED) {
      if (type->pointedTo.arrayType != NULL) {
          int32_t arraySize = computeTypeSize(type->pointedTo.arrayType);
          if (arraySize != UNKNOWN_SIZE) return arraySize;
      }
      return POINTER_TYPE_SIZE;
  }

  if (type->kind == TR_BITFIELD) {
      return computeTypeSize(type->bitFieldDesc.storageType);
  }

  if (type->kind == TR_ARRAY) {
      ArrayTypeDescriptor *atype = &type->arrayTypeDesc;
      if (atype->size != UNKNOWN_SIZE) {
        return atype->size * computeTypeSize(atype->elementType);
      }
      return UNKNOWN_SIZE;
  }

  return POINTER_TYPE_SIZE;
}

static int32_t computeStructualTypeSize(ParserContext *ctx, StructualMember *members, int32_t align, Boolean isUnion) {
  int result = 0;

  for (; members; members = members->next) {
    TypeRef *type = members->type;

    int memberTypeSize = computeTypeSize(type);
    if (memberTypeSize < 0) {
        reportDiagnostic(ctx, DIAG_FIELD_INCOMPLETE_TYPE, &members->coordinates, members->name, type);
        return UNKNOWN_SIZE;
    }
    result = isUnion ? max(result, memberTypeSize) : max(result, members->offset + memberTypeSize);
  }

  return ALIGN_SIZE(result, align);
}


int32_t computeTypeDefinitionSize(ParserContext *ctx, TypeDefiniton *definition) {
  if (!definition->isDefined) {
      // report diagnostic
      return UNKNOWN_SIZE;
  }

  if (definition->kind == TDK_ENUM) {
      return builtInTypeDescriptors[T_S4].size;
  }

  return computeStructualTypeSize(ctx, definition->members, definition->align, definition->kind == TDK_UNION);
}

TypeRef *makePrimitiveType(ParserContext *ctx, TypeId id, unsigned flags) {
  assert(T_VOID <= id && id < T_BUILT_IN_TYPES);

  TypeDesc *desc = &builtInTypeDescriptors[id];

  return makeBasicType(ctx, desc, flags);
}

TypeRef *makeBasicType(ParserContext *ctx, TypeDesc *descriptor, unsigned flags) {
  TypeRef *ref = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));

  ref->kind = TR_VALUE;
  ref->flags.storage = flags;
  ref->descriptorDesc = descriptor;

  return ref;
}

TypeRef* makePointedType(ParserContext *ctx, unsigned flags, TypeRef *pointedTo) {
    TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
    result->kind = TR_POINTED;
    result->flags.storage = flags;
    result->pointedTo.toType = pointedTo;
    return result;
}

TypeRef *makeArrayType(ParserContext *ctx, int size, TypeRef *elementType) {
    TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
    result->kind = TR_ARRAY;
    result->arrayTypeDesc.size = size;
    result->arrayTypeDesc.elementType = elementType;
    return result;
}

TypeRef *makeFunctionType(ParserContext *ctx, TypeRef *returnType, FunctionParams *params) {
    TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
    result->kind = TR_FUNCTION;
    result->functionTypeDesc.isVariadic = params->isVariadic;
    result->functionTypeDesc.returnType = returnType;

    AstValueDeclaration *parameter = params->parameters;

    TypeList head = { 0 };
    TypeList *cur = &head;

    SpecifierFlags flags = { 0 };
    while (parameter) {
      cur = cur->next = (TypeList*)areanAllocate(ctx->memory.typeArena, sizeof (TypeList));
      cur->type = parameter->type;
      parameter = parameter->next;
    }
    result->functionTypeDesc.parameters = head.next;

    return result;
}

void verifyFunctionReturnType(ParserContext *ctx, Declarator *declarator, TypeRef *returnType) {
  TypeRefKind returnRefKind = returnType->kind;

  if (returnRefKind == TR_FUNCTION || returnRefKind == TR_ARRAY) {
      enum DiagnosticId diag = returnRefKind == TR_FUNCTION ? DIAG_FUNCTION_RETURN_FUNCTION_TYPE : DIAG_FUNCTION_RETURN_ARRAY_TYPE;
      reportDiagnostic(ctx, diag, &declarator->coordinates, returnType);
  }
}

static void verifyFunctionType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_FUNCTION);

  verifyFunctionReturnType(ctx, declarator, type->functionTypeDesc.returnType);
}

static void verifyArrayType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_ARRAY);
  TypeRef *elementType = type->arrayTypeDesc.elementType;

  if (elementType->kind == TR_FUNCTION) {
      reportDiagnostic(ctx, DIAG_ARRAY_OF_FUNCTIONS_ILLEGAL, &declarator->coordinates, type);
  }
}

TypeRef *makeTypeRef(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator) {

    TypeRef *type = specifiers->basicType;

    DeclaratorPart *part = declarator->declaratorParts;

    while (part) {
        switch (part->kind) {
        case DPK_POINTER:
            type = makePointedType(ctx, part->flags.storage, type);
            break;
        case DPK_ARRAY:
            type = makeArrayType(ctx, part->arraySize, type);
            verifyArrayType(ctx, declarator, type);
            break;
        case DPK_FUNCTION:
            type = makeFunctionType(ctx, type, &part->parameters);
            verifyFunctionType(ctx, declarator, type);
            break;
        case DPK_NONE:
        default:
            unreachable("UNKNOWN Declarator Part");
        }
        part = part->next;
    }

    return type;
}

TypeRef *makeBitFieldType(ParserContext *ctx, TypeRef *storage, unsigned offset, unsigned width) {
  TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));

  result->kind = TR_BITFIELD;
  result->bitFieldDesc.storageType = storage;
  result->bitFieldDesc.offset = offset;
  result->bitFieldDesc.width = width;

  return result;
}

TypeRef *makeErrorRef(ParserContext *ctx) {
  return makeBasicType(ctx, errorTypeDescriptor, 0);
}
