
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
      equality = typeEquality(t1->pointed, t2->pointed);
      if (t1->flags.bits.isConst == t2->flags.bits.isConst) {
        if (equality == TEK_EQUAL) {
            return TEK_EQUAL;
          } else {
            // 'int *' is not equal to 'int *const'
            return equality;
          }
      }
  }


  if (t1->kind == TR_VLA && t2->kind == TR_VLA) {
      if (t1->vlaDescriptor.sizeSymbol == t2->vlaDescriptor.sizeSymbol) return TEK_EQUAL;

      return typeEquality(t1->vlaDescriptor.elementType, t2->vlaDescriptor.elementType);
  }

  if (t1->kind == TR_POINTED && t2->kind == TR_ARRAY || t2->kind == TR_POINTED && t1->kind == TR_ARRAY) {
      TypeRef *arrayType = t1->kind == TR_ARRAY ? t1 : t2;
      TypeRef *pointerType = t1 == arrayType ? t2 : t1;

      TypeRef *elementType = arrayType->arrayTypeDesc.elementType;
      TypeRef *pointedType = pointerType->pointed;

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

  return sOff > uOff  ? sT : uT;
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

Boolean isFlatType(TypeRef *type) {
  return type->kind == TR_ARRAY || isCompositeType(type);
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
  return type->kind == TR_POINTED || type->kind == TR_ARRAY || type->kind == TR_VLA;
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
  if (type->kind != TR_ARRAY) return FALSE;

  if (type->arrayTypeDesc.size != 1) return FALSE;

  TypeRef * elementType = type->arrayTypeDesc.elementType;

  if (elementType->kind != TR_VALUE) return FALSE;

  if (elementType->descriptorDesc->typeId != T_STRUCT) return FALSE;

  TypeDefiniton *decl = elementType->descriptorDesc->typeDefinition;

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
    case TR_VLA:
    case TR_ARRAY:
      return typeAlignment(type->arrayTypeDesc.elementType);
    case TR_BITFIELD: effectiveType = type->bitFieldDesc.storageType; goto value_type;
    case TR_VALUE: effectiveType = type;
      value_type:
      switch (effectiveType->descriptorDesc->typeId) {
      case T_BOOL:
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

  if (!member) return -1;

  return effectiveMemberOffset(member);
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
      return arrayType->pointed;
  }

  if (arrayType->kind == TR_ARRAY) {
      return arrayType->arrayTypeDesc.elementType;
  }

  if (arrayType->kind == TR_VLA) {
      return arrayType->vlaDescriptor.elementType;
  }

  reportDiagnostic(ctx, DIAG_SUBSCRIPTED_NOT_A_POINTER, coords);

  return makeErrorRef(ctx);
}


TypeRef *computeFunctionReturnType(ParserContext *ctx, Coordinates *coords, TypeRef *_calleeType) {
    if (isErrorType(_calleeType)) return _calleeType;

    TypeRef *calleType = _calleeType;
    if (_calleeType->kind == TR_POINTED) {
        calleType = _calleeType->pointed;
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

int32_t effectiveMemberOffset(StructualMember *member) {

  int32_t result = 0;

  for (; member; member = member->parent) {
      result += member->offset;
  }

  return result;
}

StructualMember *findStructualMember(TypeDefiniton *definition, const char *name) {
  StructualMember *member = definition->members;

  while (member) {
      if (strcmp(member->name, name) == 0) {
          break;
      }
      if (member->name[0] == '$') {
          assert(isCompositeType(member->type));
          StructualMember *t = findStructualMember(member->type->descriptorDesc->typeDefinition, name);
          if (t) return t;
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
        receiverType = _receiverType->pointed;
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


static StructualMember *nextMember(StructualMember *m) {
  if (m->name && m->name[0] == '$') {
      assert(isCompositeType(m->type));
      TypeDefiniton *def = m->type->descriptorDesc->typeDefinition;
      if (def->members) return def->members;
  }
  if (m->next) return m->next;
  if (m->parent) return m->parent->next;
  return NULL;
}

static StructualMember *findAnotherMember(StructualMember *this) {
  StructualMember *another;

  for (another = nextMember(this); another; another = nextMember(another)) {
      if (another->name == NULL) continue;
      if (strcmp(this->name, another->name) == 0) {
          return another;
      }
  }

  return NULL;
}

void verifyStructualMembers(ParserContext *ctx, StructualMember *members) {
  StructualMember *m = members;

  for (; m; m = nextMember(m)) {
      if (m->name == NULL) continue;
      if (m->name[0] == '$') continue;
      StructualMember *copy = findAnotherMember(m);
      if (copy) {
          reportDiagnostic(ctx, DIAG_DUPLICATE_MEMBER, &copy->coordinates, copy->name);
      }
  }
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
      TypeRef *pointedT = ifTrue->kind == TR_POINTED ? ifTrue->pointed : ifTrue->arrayTypeDesc.elementType;
      if (isPointerLikeType(ifFalse)) {
          TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointed : ifFalse->arrayTypeDesc.elementType;
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
      TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointed : ifFalse->arrayTypeDesc.elementType;
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
  if (type->kind == TR_POINTED) return type->pointed;
  if (type->kind == TR_ARRAY) return type->arrayTypeDesc.elementType;
  if (type->kind == TR_VLA) return type->vlaDescriptor.elementType;
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

  if (isVoidType(left) || isVoidType(right)) {
    Coordinates *coords = isVoidType(left) ? &leftExpr->coordinates : &rightExpr->coordinates;
    reportDiagnostic(ctx, DIAG_VOID_NOT_IGNORED, coords);
    return makeErrorRef(ctx);
  }

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

      // TODO: vla with non-vla
      /**
       *
       *     int *pvla[x]; -- VLA of pointers
       *     int (*vlap)[x]; -- pointer to VLA
       *     long d1 = pvla - vlap;
       *     long d2 = vlap - pvla;
       *
       */

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
      TypeRef *ptrType = argumentType->pointed;
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
  if (argumentType->kind == TR_FUNCTION) {
      return argumentType;
  }

  if (argumentType->kind == TR_POINTED) {
      return argumentType->pointed;
  }

  if (argumentType->kind == TR_ARRAY) {
      return argumentType->arrayTypeDesc.elementType;
  }

  if (argumentType->kind == TR_VLA) {
          return argumentType->vlaDescriptor.elementType;
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
      if (argumentType->kind == TR_ARRAY || isScalarType(argumentType) || argumentType->kind == TR_BITFIELD) {
          return makePrimitiveType(ctx, T_S4, 0);
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

static Boolean isCompatibleType(ParserContext *ctx, Coordinates *coords, TypeRef *t1, TypeRef *t2, enum DiagnosticId *diag, Boolean isZeroConst) {

  if (isErrorType(t1)) return FALSE;
  if (isErrorType(t2)) return FALSE;

  if (isStructualType(t1) || isStructualType(t2)) {
      if (typesEquals(t1, t2)) {
          return TRUE;
      } else {
          *diag = DIAG_FROM_INCOMPATIBLE_TYPE;
          return FALSE;
      }
  }

  if (t1->kind == TR_POINTED) {
      TypeRef *pLeft = t1->pointed;
      if (isPointerLikeType(t2)) {
          TypeRef *pointed = t2->kind == TR_POINTED ? t2->pointed : t2->arrayTypeDesc.elementType;
          if (isVoidType(pLeft) || isVoidType(pointed)) {
              return TRUE;
          } else if (typesEquals(pLeft, pointed)) {
              return TRUE;
          } else {
              *diag = DIAG_INCOMPATIBLE_POINTERS;
              return TRUE;
          }
      } else if (isIntegerType(t2)) {
          if (!isZeroConst) {
              *diag = DIAG_INT_TO_POINTER;
          }
          return TRUE;
      } else {
          *diag = DIAG_FROM_INCOMPATIBLE_TYPE;
          return FALSE;
      }
  }


  if (isPointerLikeType(t2)) {
      if (isIntegerType(t1)) {
          if (!isZeroConst) {
              *diag = DIAG_INT_TO_POINTER;
          }
          return TRUE;
      } else {
          *diag = DIAG_FROM_INCOMPATIBLE_TYPE;
          return FALSE;
      }
  }

  return TRUE;
}

static Boolean checkFunctionPassingType(ParserContext *ctx, Coordinates *coords, TypeRef *paramType, AstExpression *arg, const char *s1, const char *s2) {
  // TODO: this code is far from perfect
  TypeRef *argType = arg->type;
  if (isErrorType(paramType)) return TRUE;
  if (isErrorType(argType)) return TRUE;

  if (paramType->kind == TR_ARRAY) {
      TypeRef *paramElementType = paramType->arrayTypeDesc.elementType;
      TypeRef *argElementType = NULL;
      if (argType->kind == TR_POINTED) {
          argElementType = argType->pointed;
      } else if (argType->kind == TR_ARRAY) {
          // todo check msize
          argElementType = argType->arrayTypeDesc.elementType;
      }
      if (argElementType) {
        TypeEqualityKind equality = typeEquality(argElementType, paramElementType);
        if (equality < TEK_NOT_EXATCLY_EQUAL) return TRUE;
        if (equality < TEK_NOT_EQUAL) {
            reportDiagnostic(ctx, DIAG_INCOMPATIBLE_POINTERS, coords, s1, paramType, s2, argType);
            return TRUE;
        }
        reportDiagnostic(ctx, DIAG_FROM_INCOMPATIBLE_TYPE, coords, s1, paramType, s2, argType);
        return FALSE;
      }
  }

  enum DiagnosticId diag = -1;

  Boolean result = isCompatibleType(ctx, coords, paramType, argType, &diag, isConstZero(arg));

  if (diag != -1) {
      reportDiagnostic(ctx, diag, coords, s1, paramType, s2, argType);
  }

  return result;
}

static Boolean checkArgumentType(ParserContext *ctx, Coordinates *coords, TypeRef *paramType, AstExpression *arg) {
  return checkFunctionPassingType(ctx, coords, paramType, arg, "passing to", "from");
}

Boolean checkReturnType(ParserContext *ctx, Coordinates *coords, TypeRef *returnType, AstExpression *expr) {
  return checkFunctionPassingType(ctx, coords, returnType, expr, "returning", "from a function with result of");
}

static Boolean isAssignableTypes(ParserContext *ctx, Coordinates *coords, TypeRef *to, TypeRef *from, AstExpression *fromExpr) {
  if (isErrorType(to)) return TRUE;
  if (isErrorType(from)) return TRUE;

  if (to->flags.bits.isConst) {
      reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, to);
      return FALSE;
  }

  if (to->kind == TR_ARRAY) {
      reportDiagnostic(ctx, DIAG_ARRAY_TYPE_IS_NOT_ASSIGNABLE, coords, to);
      return FALSE;
  }

  enum DiagnosticId diag = -1;

  Boolean result = isCompatibleType(ctx, coords, to, from, &diag, isConstZero(fromExpr));

  if (diag != -1) {
      reportDiagnostic(ctx, diag, coords, "assigning to", to, "from", from);
  }

  return result;
}

Boolean checkRefArgument(ParserContext *ctx, Coordinates *coords, AstExpression *arg, Boolean report) {

  arg = deparen(arg);

  switch (arg->op) {
  case E_NAMEREF:
  case EF_ARROW:
  case EF_DOT:
  case EB_A_ACC:
  case EU_DEREF:
  case E_COMPOUND:
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

  if (isAssignableTypes(ctx, coords, left, rhsType, rightExpr)) {
      return left;
  }

  return makeErrorRef(ctx);

}

static Boolean isCompileTimeConstant(AstExpression *expr);


static Boolean isCompilteTimeInitializer(AstInitializer *init) {
  if (init->kind == IK_EXPRESSION) return isCompileTimeConstant(init->expression);

  AstInitializerList *n= init->initializerList;
  for (; n; n = n->next) {
      if (!isCompilteTimeInitializer(n->initializer)) return FALSE;
  }

  return TRUE;
}

static Boolean isCompileTimeStatement(AstStatement *stmt) {
  switch (stmt->statementKind) {
    case SK_EXPR_STMT: return isCompileTimeConstant(stmt->exprStmt.expression);
    case SK_BLOCK: {
        AstStatementList *n = stmt->block.stmts;
        for (; n; n = n->next) {
            if (!isCompileTimeStatement(n->stmt)) return FALSE;
        }
        return TRUE;
    }
    default: return FALSE;
    }
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
    case E_COMPOUND: return isCompilteTimeInitializer(expr->compound);
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
    case E_BLOCK:
      return isCompileTimeStatement(expr->block);
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
    case EU_DEREF:
      return isCompileTimeConstant2(expr->unaryExpr.argument, allowRefs);
    case E_NAMEREF: {
      Symbol *s = expr->nameRefExpr.s;
      if (s->kind == FunctionSymbol) return TRUE;
      if (allowRefs) {
        return isStaticSymbol(s);
      } else if (s->kind == ValueSymbol) {
        return s->variableDesc->type->kind == TR_ARRAY;
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

static AstInitializer *typeInitializer(ParserContext *ctx, TypeRef *valueType, int32_t offset, unsigned flexible);
static ParsedInitializer *finalizeInitializerInternal(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel);

static void fillInitializer(ParserContext *ctx, AstInitializer *semaInit) {

  if (semaInit->state != IS_CLEAR) return;

  TypeRef *type = semaInit->slotType;
  Coordinates coords = semaInit->coordinates;

  if (isScalarType(type) || type->kind == TR_BITFIELD) {
      assert(semaInit->kind == IK_EXPRESSION);
      uint64_t c = 0;
      if (type->kind == TR_POINTED) {
        semaInit->expression = createAstConst(ctx, &coords, CK_INT_CONST, &c, 0);
      } else if (isRealType(type)) {
        long double cc = 0.0;
        semaInit->expression = createAstConst(ctx, &coords, CK_FLOAT_CONST, &cc, 0);
      } else {
        semaInit->expression = createAstConst(ctx, &coords, CK_INT_CONST, &c, 0);
      }
      semaInit->expression->type = type;
      semaInit->state = IS_FILLED;
      return ;
  }

  if (type->kind == TR_ARRAY && semaInit->isFlexible) {
      type->arrayTypeDesc.size = 0;
      semaInit->state = IS_FILLED;
      return;
  }


  AstInitializerList *initList = semaInit->initializerList;
  assert(initList);

  for (; initList; initList = initList->next) {
      AstInitializer *init = initList->initializer;
      init->coordinates = coords;
      fillInitializer(ctx, init);
  }

  semaInit->state = IS_FILLED;
}

static AstInitializerList *findStructDesignatorWithFilling(ParserContext *ctx, AstInitializer *semaInit, AstInitializerList *current, ParsedInitializer *parsed) {
  AstInitializerList *inits = semaInit->initializerList;

  // struct SX { int a, b, c; };

  if (parsed->kind != DK_STRUCT) {
      // struct SX sx = { [0] = 1 };
      reportDiagnostic(ctx, DIAG_ARRAY_DESIGNATOR_IN_STRUCT, &parsed->coords, semaInit->slotType);
      return NULL;
  }

  const char *name = parsed->designator.identifier;

  if (name == NULL) return NULL;

  for (; inits; inits = inits->next) {
      AstInitializer *current = inits->initializer;
      assert(current->designation == DK_STRUCT);
      if (strcmp(name, current->designator.member) == 0) {
          break;
      }
  }

  if (inits == NULL) {
      // struct SX sx = { .d = 10 };
      reportDiagnostic(ctx, DIAG_NO_DESIGNATED_MEMBER, &parsed->coords, name, semaInit->slotType);
  } else {
      AstInitializerList *start = current ? current : semaInit->initializerList;
      AstInitializer *designated = inits->initializer;
      if (designated->offset > start->initializer->offset) {
          for (current = start; current != inits; current = current->next) {
              fillInitializer(ctx, current->initializer);
          }
      }
  }

  return inits;
}

static ParsedInitializer *skipIncorrectDesignation(ParsedInitializer *parsed) {
  while (parsed->loc != PL_SEPARATOR && parsed->loc != PL_CLOSE) {
      parsed = parsed->next;
  }

  return parsed;
}

static ParsedInitializer *finalizeStructInitializer(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel) {
  TypeRef *type = semaInit->slotType;
  assert(isStructualType(type));

  AstExpression *expr = initializer->expression;

  if (expr) {
      semaInit->coordinates = initializer->coords;

      if (isTopLevel && !isCompileTimeConstant(expr)) {
        reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coords);
        semaInit->kind = IK_EXPRESSION;
        semaInit->expression = createErrorExpression(ctx, &expr->coordinates);
        semaInit->state = IS_INIT;
        return initializer->next;
      }

      if (typesEquals(type, expr->type)) {
          if (semaInit->state == IS_INIT) {
              reportDiagnostic(ctx, DIAG_INITIALIZER_OVERRIDE, &expr->coordinates);
          }
          semaInit->kind = IK_EXPRESSION;
          semaInit->expression = expr;
          semaInit->state = IS_INIT;
          return initializer->next;
      }
  }

  Coordinates coords = initializer->coords;

  AstInitializerList *initList = semaInit->initializerList;

  if (initializer->loc == PL_DESIGNATOR) {
      AstInitializerList *designated = findStructDesignatorWithFilling(ctx, semaInit, semaInit->initializerList, initializer);
      if (designated) {
          for (;initList != designated; initList = initList->next) {
              fillInitializer(ctx, initList->initializer);
          }
          initializer = finalizeInitializerInternal(ctx, initializer->next, designated->initializer, isTopLevel);
          initList = designated->next;
      } else {
          initializer = skipIncorrectDesignation(initializer);
      }
  }

  int32_t level = initializer->level;

  Boolean embraced = initializer->loc == PL_OPEN;
  if (embraced) initializer = initializer->next;


  while (initializer) {
      if (initializer->loc == PL_SEPARATOR)  {
          initializer = initializer->next;
          continue;
      }
      if (initializer->level == level && initializer->loc == PL_CLOSE) {
          coords = initializer->coords;
          break;
      }

      if (initializer->loc == PL_DESIGNATOR) {
          if (embraced) {
              AstInitializerList *designated = findStructDesignatorWithFilling(ctx, semaInit, initList, initializer);
              if (designated) {
                  initList = designated;
                  initializer = initializer->next;
              } else  {
                  initializer = skipIncorrectDesignation(initializer);
                  continue;
              }
          } else {
              coords = initializer->coords;
              break;
          }
      }

      if (initList) {
        AstInitializer *init = initList->initializer;
        coords = init->coordinates = initializer->coords;
        initializer = finalizeInitializerInternal(ctx, initializer, init, isTopLevel);
        initList = initList->next;
      } else {
        if (!embraced) {
            coords = initializer->coords;
            break;
        }
        if (initializer->expression) {
          reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates, "struct");
        }
        initializer = initializer->next;
      }
  }


  for (; initList; initList = initList->next) {
      AstInitializer *init = initList->initializer;
      init->coordinates = coords;
      fillInitializer(ctx, init);
  }

  semaInit->coordinates.right = coords.right;

  if (embraced && initializer->level == level && initializer->loc == PL_CLOSE) {
    initializer = initializer->next;
  }

  semaInit->state = IS_INIT;

  return initializer;
}


static AstInitializer *createSymbolInitNode(ParserContext *ctx, Coordinates *_coords, TypeRef *type, int64_t c, int32_t offset) {

  AstExpression *expr = createAstConst(ctx, _coords, CK_INT_CONST, &c, 0);
  expr->type = type;

  AstInitializer *init = createAstInitializer(ctx, _coords, IK_EXPRESSION);
  init->expression = expr;
  init->slotType = type;
  init->offset = offset;

   return init;
}

static void stringLiteralToInitializer(ParserContext *ctx, AstInitializer *semaInit, Coordinates *coords, const char *s, size_t length) {

  TypeRef *charType = makePrimitiveType(ctx, T_S1, 0);
  TypeRef *arrayType = semaInit->slotType;
  int32_t arraySize = arrayType->arrayTypeDesc.size;
  int32_t offset = semaInit->offset;

  assert(semaInit->kind == IK_LIST);

  semaInit->coordinates = *coords;

  unsigned i = 0;
  if (arraySize < 0) {
    AstInitializerList head = { 0 }, *current = &head;

    for (; i < length; ++i) {
      current = current->next = createAstInitializerList(ctx);
      current->initializer = createSymbolInitNode(ctx, coords, charType, s[i], offset + i);
    }

    arrayType->arrayTypeDesc.size = length;
    semaInit->initializerList = head.next;
  } else {
    AstInitializerList *current = semaInit->initializerList;

    for (i = 0; i < arraySize && current; ++i, current = current->next) {
      current->initializer = createSymbolInitNode(ctx, coords, charType, s[i], offset + i);
    }

    if (arraySize < length) {
      if (arraySize + 1 != length || s[arraySize]) {
        reportDiagnostic(ctx, DIAG_STRING_INIT_TOO_LONG, coords);
      }
    }

    for (; current; current = current->next) {
      current->initializer = createSymbolInitNode(ctx, coords, charType, 0, offset + i);
    }
  }
}

static AstInitializerList *findIncompleteArrayDesignatorWithFilling(ParserContext *ctx, AstInitializer *semaInit, AstInitializerList *start, ParsedInitializer *parsed, int32_t offset) {
  if (parsed->kind != DK_ARRAY) {
      // int a[] = { .x = 10 };
      reportDiagnostic(ctx, DIAG_STRUCT_DESIGNATOR_IN_ARRAY, &parsed->coords, semaInit->slotType);
      return NULL;
  }

  int32_t index = parsed->designator.index;

  if (index < 0) {
      // some error happened in parser and already reported, just skip it
      return NULL;
  }

  TypeRef *arrayType = semaInit->slotType;
  int32_t arraySize = arrayType->arrayTypeDesc.size;

//  int a[] = { [1] = 10, [2] = 20, [1] = 30 };
  int32_t newArraySize = max(arraySize, index + 1);
  arrayType->arrayTypeDesc.size = newArraySize;
  TypeRef *elementType = arrayType->arrayTypeDesc.elementType;

  int32_t align = typeAlignment(elementType);
  int32_t elementSize = computeTypeSize(elementType);

  int32_t idx = 0;

  AstInitializerList *n = start;
  AstInitializerList head = { 0 }, *current = &head;

  for (; n; n = n->next, ++idx) {
      if (n->initializer->designator.index == index) return n;
      offset = ALIGN_SIZE(offset, align);
      current = current->next = n;
      offset += elementSize;
  }

  for (; idx < index; ++idx) {
      offset = ALIGN_SIZE(offset, align);
      AstInitializer *init = typeInitializer(ctx, elementType, offset, FALSE);
      init->designation = DK_ARRAY;
      init->designator.index = idx;
      init->coordinates = parsed->coords;
      fillInitializer(ctx, init);
      current = current->next = createAstInitializerList(ctx);
      current->initializer = init;
      offset += elementSize;
  }

  current = current->next = createAstInitializerList(ctx);
  AstInitializer *init = typeInitializer(ctx, elementType, offset, FALSE);
  init->designation = DK_ARRAY;
  init->designator.index = index;
  current->initializer = init;

  semaInit->initializerList = head.next;

  return current;
}


static ParsedInitializer *initializeIncompleteArray(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, AstInitializerList *start, int32_t stopLevel, Boolean embraced, Boolean isTopLevel) {
  assert(semaInit->initializerList == NULL);

  TypeRef *arrayType = semaInit->slotType;
  TypeRef *elementType = arrayType->arrayTypeDesc.elementType;
  int32_t arraySize = arrayType->arrayTypeDesc.size;

  AstInitializerList head = { 0 };
  AstInitializerList *current = NULL;
  AstInitializerList *prev = &head;

  int32_t elementSize = computeTypeSize(elementType);
  int32_t align = typeAlignment(elementType);
  int32_t offset = semaInit->offset;
  int32_t index = 0;

  if (start) {
      assert(semaInit->initializerList);
      current = start;
      head.next = semaInit->initializerList;
      for (prev = semaInit->initializerList; prev->next != current; prev = prev->next);
  }

  Coordinates coords = initializer->coords;

  while (initializer) {
      arraySize = max(arraySize, index);
      if (initializer->loc == PL_SEPARATOR)  {
          initializer = initializer->next;
          continue;
      }

      if (initializer->level == stopLevel && initializer->loc == PL_CLOSE) {
          coords = initializer->coords;
          break;
      }

      offset = ALIGN_SIZE(offset, align);

      AstInitializer *init = NULL;
      if (initializer->loc == PL_DESIGNATOR) {
          if (embraced) {
              AstInitializerList *designated = findIncompleteArrayDesignatorWithFilling(ctx, semaInit, semaInit->initializerList, initializer, offset);
              if (designated) {
                  current = designated;
                  init = designated->initializer;
                  initializer = initializer->next;
              } else {
                  initializer = skipIncorrectDesignation(initializer);
                  continue;
              }
          } else {
              coords = initializer->coords;
              break;
          }
      } else if (prev->next == NULL) {
          current = prev->next = createAstInitializerList(ctx);
          init = typeInitializer(ctx, elementType, offset, FALSE);
          init->designation = DK_ARRAY;
          init->designator.index = index;
          current->initializer = init;
      } else {
          init = current->initializer;
      }


      init->coordinates = coords;

      initializer = finalizeInitializerInternal(ctx, initializer, init, isTopLevel);

      index = init->designator.index + 1;
      offset = init->offset + elementSize;
      prev = current;
      current = current->next;
      if (semaInit->initializerList == NULL) semaInit->initializerList = head.next;
  }

  semaInit->state = IS_INIT;
  arrayType->arrayTypeDesc.size = arraySize;

  return initializer;
}

static AstInitializerList *findCompleteArrayDesignatorWithFilling(ParserContext *ctx, AstInitializer *semaInit, AstInitializerList *current, ParsedInitializer *parsed) {
  if (parsed->kind != DK_ARRAY) {
      // int a[] = { .x = 10 };
      reportDiagnostic(ctx, DIAG_STRUCT_DESIGNATOR_IN_ARRAY, &parsed->coords, semaInit->slotType);
      return NULL;
  }

  int32_t index = parsed->designator.index;

  if (index < 0) {
      // some error happened in parser and already reported, just skip it
      return NULL;
  }

  TypeRef *arrayType = semaInit->slotType;
  int32_t arraySize = arrayType->arrayTypeDesc.size;

  if (index < arraySize) {

      if (current == NULL) current = semaInit->initializerList;

      for (; current->initializer->designator.index < index; current = current->next) {
        fillInitializer(ctx, current->initializer);
      }
      if (current->initializer->designator.index == index) return current;
      AstInitializerList *i = semaInit->initializerList;

      for (; i->initializer->designator.index != index; i = i->next);

      return i;
  } else {
    // int c[10] = { [20] = 42 };
    reportDiagnostic(ctx, DIAG_ARRAY_DESIGNATOR_INDEX_EXCEED, &parsed->coords, index, arraySize);
    return NULL;
  }
}

static ParsedInitializer *initializeCompleteArray(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, AstInitializerList *start, int32_t stopLevel, Boolean embraced, Boolean isTopLevel) {
  AstInitializerList *initList = start;

  semaInit->coordinates.left = initializer->coords.left;
  Coordinates coords = initializer->coords;

  while (initializer) {
      if (initializer->loc == PL_SEPARATOR)  {
          initializer = initializer->next;
          continue;
      }
      if (initializer->level == stopLevel && initializer->loc == PL_CLOSE) {
          coords = initializer->coords;
          break;
      }

      if (initializer->loc == PL_DESIGNATOR) {
          if (embraced) {
              AstInitializerList *designated = findCompleteArrayDesignatorWithFilling(ctx, semaInit, initList, initializer);
              if (designated) {
                  initList = designated;
                  initializer = initializer->next;
              } else {
                  initializer = skipIncorrectDesignation(initializer);
                  continue;
              }
          } else {
              coords = initializer->coords;
              break;
          }
      }

      if (initList) {
        initializer = finalizeInitializerInternal(ctx, initializer, initList->initializer, isTopLevel);
        initList = initList->next;
      } else {
        if (!embraced) {
            coords = initializer->coords;
            break;
        }
        if (initializer->expression) {
            reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates, "array");
        }
        initializer = initializer->next;
      }
  }


  for (; initList; initList = initList->next) {
      AstInitializer *init = initList->initializer;
      init->coordinates = coords;
      fillInitializer(ctx, init);
  }

  return initializer;
}

static ParsedInitializer *finalizeArrayInitializer(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel) {

  TypeRef *type = semaInit->slotType;

  TypeRef *elementType = type->arrayTypeDesc.elementType;

  Coordinates coords = initializer->coords;

  if (semaInit->isFlexible && !isTopLevel) {
    reportDiagnostic(ctx, DIAG_FLEXIBLE_MEMBER_INIT, &coords);
    semaInit->kind = IK_EXPRESSION;
    semaInit->expression = createErrorExpression(ctx, &coords);
    semaInit->slotType = makeErrorRef(ctx);
    return skipIncorrectDesignation(initializer);
  }

  if (isCharType(elementType)) {
      Boolean enbraced = FALSE;
      ParsedInitializer *saved = initializer;

      if (initializer->loc == PL_OPEN) {
          enbraced = TRUE;
          initializer = initializer->next;
      }

      AstExpression *expr = initializer->expression;
      if (expr) {
          if (expr->op == E_CONST && expr->constExpr.op == CK_STRING_LITERAL) {

              if (semaInit->state == IS_INIT) {
                  reportDiagnostic(ctx, DIAG_INITIALIZER_OVERRIDE, &expr->coordinates);
              }

              stringLiteralToInitializer(ctx, semaInit, &expr->coordinates, expr->constExpr.l.s, expr->constExpr.l.length);
              semaInit->state = IS_INIT;

              if (enbraced) {
                  for (initializer = initializer->next; initializer->loc != PL_CLOSE; initializer = initializer->next) {
                      if (initializer->expression) {
                          reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates, "char array");
                      }
                  }
              }

              return initializer->next;
          }
      }

      initializer = saved;
  }

  AstInitializerList *start = semaInit->initializerList;

  if (initializer->loc == PL_DESIGNATOR) {
      AstInitializerList *designated = semaInit->isIncomplete
          ? findIncompleteArrayDesignatorWithFilling(ctx, semaInit, semaInit->initializerList, initializer, 0)
          : findCompleteArrayDesignatorWithFilling(ctx, semaInit, semaInit->initializerList, initializer);
      if (designated) {
          initializer = finalizeInitializerInternal(ctx, initializer->next, designated->initializer, isTopLevel);
          start = designated->next;
      } else {
          initializer = skipIncorrectDesignation(initializer);
      }
  }

  int32_t level = initializer->level;
  Boolean enbraced = initializer->loc == PL_OPEN;

  if (enbraced) initializer = initializer->next;

  if (semaInit->isIncomplete) {
      initializer = initializeIncompleteArray(ctx, initializer, semaInit, start, level, enbraced, isTopLevel);
  } else {
      initializer = initializeCompleteArray(ctx, initializer, semaInit, start, level, enbraced, isTopLevel);
  }

  semaInit->coordinates.right = initializer->coords.right;

  if (enbraced && initializer->level == level && initializer->loc == PL_CLOSE) {
      initializer = initializer->next;
  }

  semaInit->state = IS_INIT;

  return initializer;

}

static ParsedInitializer *finalizeUnionInitializer(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel) {
  TypeRef *type = semaInit->slotType;

  assert(isUnionType(type));

  AstExpression *expr = initializer->expression;

  if (expr) {
      if (isTopLevel && !isCompileTimeConstant(expr)) {
        reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coords);
        semaInit->coordinates = initializer->coords;
        semaInit->expression = createErrorExpression(ctx, &expr->coordinates);
        semaInit->state = IS_INIT;

        return initializer->next;
      }

      if (typesEquals(type, expr->type)) {
          semaInit->coordinates = initializer->coords;
          semaInit->kind = IK_EXPRESSION;
          semaInit->expression = expr;
          semaInit->state = IS_INIT;

          return initializer->next;
      }
  }

  AstInitializerList *list = semaInit->initializerList;

  int32_t level = initializer->level;
  Boolean embraced = initializer->loc == PL_OPEN;

  if (embraced) {
    initializer = initializer->next;
  }

  if (list) {
      for (; list; list = list->next) {
          AstInitializer *init = list->initializer;
          init->coordinates = initializer->coords;
          fillInitializer(ctx, init);
      }

      list = semaInit->initializerList;

      if (initializer->loc == PL_DESIGNATOR) {
          AstInitializerList *designated = findStructDesignatorWithFilling(ctx, semaInit, list, initializer);
          if (designated) {
              initializer = finalizeInitializerInternal(ctx, initializer->next, designated->initializer, isTopLevel);
              list = designated;
          } else {
              initializer = skipIncorrectDesignation(initializer);
          }
      } else {
        initializer = finalizeInitializerInternal(ctx, initializer, list->initializer, isTopLevel);
      }
  }

  if (initializer) {
      if (initializer->level >= level && initializer->loc != PL_CLOSE) {
        while (initializer) {
          ParsedInitializer *prev = initializer;

          if (initializer->expression) {
            reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates, "union");
          }

          if (initializer->level == level && initializer->loc == PL_CLOSE) break;
          initializer = initializer->next;
        }
      }
  }

  if (embraced) {
      initializer = initializer->next;
  }

  semaInit->state = IS_INIT;

  return initializer;
}

static ParsedInitializer *finalizeScalarInitializer(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel) {

  TypeRef *type = semaInit->slotType;
  Coordinates *coords = &initializer->coords;

  assert(isScalarType(type) || type->kind == TR_BITFIELD);

  int32_t level = initializer->level;
  if (initializer->loc == PL_OPEN) {
      initializer = finalizeScalarInitializer(ctx, initializer->next, semaInit, isTopLevel);
      if (initializer->level >= level || initializer->loc != PL_CLOSE)  {
          while (initializer) {
            ParsedInitializer *prev = initializer;

            if (initializer->expression) {
              reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &initializer->expression->coordinates, "scalar");
            }

            if (prev->level == level && prev->loc == PL_CLOSE) {
                initializer = initializer->next;
                break;
            }
            initializer = initializer->next;
          }
      }
      return initializer;
  }

  if (initializer->loc == PL_CLOSE) {
    reportDiagnostic(ctx, DIAG_SCALAR_INIT_EMPTY, coords);
    semaInit->coordinates = *coords;
    semaInit->expression = createErrorExpression(ctx, coords);
    return initializer;
  }

  if (initializer->expression == NULL) {
      reportDiagnostic(ctx, DIAG_NOT_SCALAR_INITIALZIER, &initializer->coords);
      semaInit->expression = createErrorExpression(ctx, &initializer->coords);
      semaInit->coordinates = initializer->coords;
      semaInit->state = IS_INIT;
      return skipIncorrectDesignation(initializer);
  }

  AstExpression *expr = initializer->expression;
  TypeRef *exprType = expr->type;

  enum DiagnosticId diag = -1;
  Boolean comaptible = isCompatibleType(ctx, &expr->coordinates, type, exprType, &diag, isConstZero(expr));
  if (diag != -1) {
      reportDiagnostic(ctx, diag, &expr->coordinates, "initializing", type, "with an expression of", exprType);
  }

  if (comaptible) {
      if (isTopLevel && !isCompileTimeConstant(expr)) {
          reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, coords);
          semaInit->expression = createErrorExpression(ctx, &expr->coordinates);
      } else {
        if (!typesEquals(type, exprType)) {
            expr = createCastExpression(ctx, &expr->coordinates, type, expr);
        }
        if (semaInit->state == IS_INIT) {
            reportDiagnostic(ctx, DIAG_INITIALIZER_OVERRIDE, &expr->coordinates);
        }
        semaInit->expression = expr;
      }
  } else {
      semaInit->expression = createErrorExpression(ctx, &expr->coordinates);
  }

  semaInit->coordinates = *coords;
  semaInit->state = IS_INIT;

  return initializer->next;
}

static ParsedInitializer *finalizeInitializerInternal(ParserContext *ctx, ParsedInitializer *initializer, AstInitializer *semaInit, Boolean isTopLevel) {
  Coordinates *coords = &initializer->coords;
  TypeRef *valueType = semaInit->slotType;

  if (isErrorType(valueType) || valueType->kind == TR_VLA) {
      if (valueType->kind == TR_VLA) {
          reportDiagnostic(ctx, DIAG_VM_OBJECT_MAY_NOT_BE_INITIALIZED, &initializer->coords);
      }
      semaInit->coordinates = initializer->coords;
      semaInit->expression = createErrorExpression(ctx, coords);
      semaInit->state = IS_INIT;
      return skipIncorrectDesignation(initializer->next);
  }

  if (isStructualType(valueType)) {
      return finalizeStructInitializer(ctx, initializer, semaInit, isTopLevel);
  }

  if (valueType->kind == TR_ARRAY) {
      return finalizeArrayInitializer(ctx, initializer, semaInit, isTopLevel);
  }

  if (isUnionType(valueType)) {
      return finalizeUnionInitializer(ctx, initializer, semaInit, isTopLevel);
  }

  return finalizeScalarInitializer(ctx, initializer, semaInit, isTopLevel);
}

AstInitializer *typeInitializer(ParserContext *ctx, TypeRef *valueType, int32_t offset, unsigned flexible) {
  if (isScalarType(valueType) || valueType->kind == TR_BITFIELD) {
      AstInitializer *init = createEmptyInitializer(ctx);
      init->designation = DK_NONE;
      init->kind = IK_EXPRESSION;
      init->slotType = valueType;
      init->offset = offset;
      return init;
  }

  if (valueType->kind == TR_ARRAY) {
      TypeRef *elementType = valueType->arrayTypeDesc.elementType;
      int32_t currentOffset = offset;
      int32_t elementAlign = typeAlignment(elementType);
      int32_t size = valueType->arrayTypeDesc.size;
      int32_t elementSize = computeTypeSize(elementType);

      if (flexible) {
          valueType = makeArrayType(ctx, UNKNOWN_SIZE, elementType);
          size = UNKNOWN_SIZE;
      }

      AstInitializerList head = { 0 }, *current = &head;
      int32_t i = 0;

      for (; i < size; ++i) {
          currentOffset = ALIGN_SIZE(currentOffset, elementAlign);
          AstInitializer *elementInit = typeInitializer(ctx, elementType, currentOffset, FALSE);
          elementInit->designation = DK_ARRAY;
          elementInit->designator.index = i;
          current = current->next = createAstInitializerList(ctx);
          current->initializer = elementInit;
          currentOffset += elementSize;
      }

      AstInitializer *init = createEmptyInitializer(ctx);
      init->kind = IK_LIST;
      init->designation = DK_NONE;
      init->slotType = valueType;
      init->offset = offset;
      init->isIncomplete = size < 0;
      init->isFlexible = flexible;
      init->initializerList = head.next;
      return init;
  }

  if (isCompositeType(valueType)) {
      TypeDefiniton *definition = valueType->descriptorDesc->typeDefinition;
      assert(definition->kind == TDK_STRUCT || definition->kind == TDK_UNION);

      StructualMember *member = definition->members;
      AstInitializerList head = { 0 }, *current = &head;

      for (; member; member = member->next) {
          int32_t memberOffset = offset + member->offset;
          AstInitializer *memberInit = typeInitializer(ctx, member->type, memberOffset, flexible && member->isFlexible && member->next == NULL);
          memberInit->designation = DK_STRUCT;
          if (member->name[0] == '$') {
            assert(memberInit->kind == IK_LIST);
            AstInitializerList *inits = memberInit->initializerList;
            for (; inits; inits = inits->next) {
                AstInitializer *init = inits->initializer;
                current = current->next = inits;
            }
          } else {
            memberInit->designator.member = member->name;
            current = current->next = createAstInitializerList(ctx);
            current->initializer = memberInit;
          }
      }

      AstInitializer *init = createEmptyInitializer(ctx);
      init->kind = IK_LIST;
      init->designation = DK_NONE;
      init->slotType = valueType;
      init->offset = offset;
      init->isFlexible = flexible;
      init->initializerList = head.next;

      return init;
  }

  if (valueType->kind == TR_VLA) {
      AstInitializer *init = createEmptyInitializer(ctx);
      init->kind = IK_LIST;
      init->slotType = valueType;
      return init;
  }

  if (valueType->kind == TR_FUNCTION) {
      unreachable("I am not sure if and how functional type could be initialized");
  }

  unreachable("Unknown type to be initialized");
}

static Boolean isFlexible(TypeRef *type) {
  if (type->kind != TR_VALUE) return FALSE;

  TypeDefiniton *def = type->descriptorDesc->typeDefinition;
  if (def == NULL) return FALSE;

  return def->isFlexible;
}

AstInitializer *finalizeInitializer(ParserContext *ctx, TypeRef *valueType, ParsedInitializer *parsed, Boolean isTopLevel) {
  AstExpression *expr = parsed->expression;

  if (isErrorType(valueType) || valueType->kind == TR_VLA) {
      if (valueType->kind == TR_VLA) {
          reportDiagnostic(ctx, DIAG_VM_OBJECT_MAY_NOT_BE_INITIALIZED, &parsed->coords);
      }
      AstInitializer *init = createAstInitializer(ctx, &parsed->coords, IK_EXPRESSION);
      init->offset = -1;
      init->expression = createErrorExpression(ctx, &parsed->coords);
      init->slotType = makeErrorRef(ctx);
      return init;
  }

  if (valueType->kind == TR_ARRAY && expr) {
      TypeRef *elementType = valueType->arrayTypeDesc.elementType;
      enum DiagnosticId diag = -1;

      if (isCharType(elementType)) {
          if (expr->op == E_CONST && expr->constExpr.op == CK_STRING_LITERAL) {
              // OK, will handle it further
          } else {
              diag = DIAG_ARRAY_INIT_LIST_OR_LITERAL;
          }
      } else {
          diag = DIAG_ARRAY_INIT_LIST;
      }

      if (diag != -1) {
          reportDiagnostic(ctx, diag, &parsed->coords);
          AstInitializer *new = createAstInitializer(ctx, &expr->coordinates, IK_EXPRESSION);
          new->expression = createErrorExpression(ctx, &expr->coordinates);
          new->slotType = makeErrorRef(ctx);
          new->offset = -1;
          return new;
      }
  }

  AstInitializer *semanthicInit = typeInitializer(ctx, valueType, 0, isFlexible(valueType));

  ParsedInitializer *final = finalizeInitializerInternal(ctx, parsed, semanthicInit, isTopLevel);
  assert(final == NULL);

  return semanthicInit;
}

AstExpression *transformCondition(ParserContext *ctx, AstExpression *expr) {
  if (expr == NULL) return expr;
  if (!isRealType(expr->type)) return expr;

  switch (expr->op) {
    case EB_EQ:
    case EB_NE:
    case EB_LE:
    case EB_LT:
    case EB_GE:
    case EB_GT:
      return expr;
    default: break;
  }

  long double v = 0.0L;
  AstExpression *nullConst = createAstConst(ctx, &expr->coordinates, CK_FLOAT_CONST, &v, sizeof v);
  nullConst->type = expr->type;

  TypeRef *intType = makePrimitiveType(ctx, T_S4, 0);
  return createBinaryExpression(ctx, EB_NE, intType, expr, nullConst);
}

AstExpression *transformTernaryExpression(ParserContext *ctx, AstExpression *expr) {
  assert(expr->op == E_TERNARY);

  if (isErrorType(expr->type)) return expr;

  expr->ternaryExpr.condition = transformCondition(ctx, expr->ternaryExpr.condition);

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
          AstExpression *elementSize = NULL;
          if (elemType->kind == TR_VLA) {
              elementSize = computeVLASize(ctx, &expr->coordinates, elemType);
          } else {
            int64_t elemSize = isVoidType(elemType) ? 1 : computeTypeSize(elemType);
            if (elemSize != 1) {
              elementSize = createAstConst(ctx, &offset->coordinates, CK_INT_CONST, &elemSize, 0);
            }
          }

          if (elementSize) {
              elementSize->type = makePrimitiveType(ctx, T_S8, 0);
              offset = createBinaryExpression(ctx, EB_MUL, elementSize->type, offset, elementSize);
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
            AstExpression *elementSize = NULL;

            if (elemType->kind == TR_VLA) {
                elementSize = computeVLASize(ctx, &expr->coordinates, elemType);
            } else {
              int64_t elemSize = isVoidType(elemType) ? 1 : computeTypeSize(elemType);
              if (elemSize != 1) {
                elementSize = createAstConst(ctx, &expr->coordinates, CK_INT_CONST, &elemSize, 0);
              }
            }

            if (elementSize) {
                elementSize->type = expr->type;
                return createBinaryExpression(ctx, EB_DIV, expr->type, expr, elementSize);
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

  if (expr->op == EB_ASG_ADD || expr->op == EB_ASG_SUB) {
      if (lvalue->type->kind == TR_POINTED && isIntegerType(rvalue->type)) {
          // ptr += x | ptr -= x
          TypeRef *ptrType = lvalue->type;
          AstExpression *elementSize = NULL;
          if (ptrType->kind == TR_VLA) {
              elementSize = computeVLASize(ctx, &rvalue->coordinates, ptrType);
          } else {
              int64_t typeSize = isVoidType(ptrType) ? 1 : computeTypeSize(ptrType->pointed);
              assert(typeSize != UNKNOWN_SIZE);
              elementSize = createAstConst(ctx, &rvalue->coordinates, CK_INT_CONST, &typeSize, 0);

          }
          elementSize->type = makePrimitiveType(ctx, T_S8, 0);
          expr->binaryExpr.right = createBinaryExpression(ctx, EB_MUL, elementSize->type, rvalue, elementSize);
      }
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
    functionType = functionType->pointed;

  assert(functionType->kind == TR_FUNCTION);

  AstExpressionList *argument = aruments;
  TypeList *param = functionType->functionTypeDesc.parameters;

  while (argument && param) {
      AstExpression *arg = argument->expression;
      TypeRef *aType = arg->type;
      TypeRef *pType = param->type;
      if (checkArgumentType(ctx, &arg->coordinates, pType, arg)) {
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
  if (type->kind == TR_POINTED && isVoidType(type->pointed)) {
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
      if (oldValue->flags.bits.isLocal && oldValue->flags.bits.isLocal) {
          reportDiagnostic(ctx, DIAG_MEMBER_REDEFINITION, &newValue->coordinates, s->name, newType, oldType);
      } else {
          // TODO: link declarations to list
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
      if (definition->isDefined) {
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
          if (definition->isDefined) {
            if (existedDeclaration->isDefined) {
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

    { T_BOOL, 1, "_Bool", NULL },

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
 case TR_VLA:
 case TR_ARRAY: return T_U8;
 case TR_BITFIELD: return type->bitFieldDesc.storageType->descriptorDesc->typeId;
 default: unreachable("Unknown type ref"); return T_ERROR;
   }
}

static AstExpression *copyExpression(ParserContext *ctx, AstExpression *expression) {
  return expression;
}

AstExpression *computeVLASize(ParserContext *ctx, Coordinates *coords, TypeRef *type) {
  AstExpression *sizeExpression = NULL;
  TypeRef *sizeType = makePrimitiveType(ctx, T_U8, 0);
  if (type->kind == TR_VLA) {
      AstExpression *elementSize = computeVLASize(ctx, coords, type->vlaDescriptor.elementType);
      Symbol *sizeSymbol = type->vlaDescriptor.sizeSymbol;
      AstExpression *numOfElements = NULL;
      if (sizeSymbol) {
        AstExpression *numOfElementsRef = createNameRef(ctx, coords, sizeSymbol->name, sizeSymbol);
        numOfElementsRef->type = makePointedType(ctx, 0, sizeType);
        numOfElements = createUnaryExpression(ctx, coords, EU_DEREF, numOfElementsRef);
        numOfElements->type = sizeType;
      } else if (type->vlaDescriptor.sizeExpression) {
        // it means we have unmaterialized VLA such as sizeof(int[x][y]) or int (*)[n][m]
        // seems like it's safe to use original expression directly otherwise it's UB
        numOfElements = type->vlaDescriptor.sizeExpression;
      } else {
        numOfElements = createErrorExpression(ctx, coords);
      }
      sizeExpression = createBinaryExpression(ctx, EB_MUL, sizeType, numOfElements, elementSize);
  } else {
      int64_t elementSize = computeTypeSize(type);
      sizeExpression = createAstConst(ctx, coords, CK_INT_CONST, &elementSize, 8);
      sizeExpression->type = sizeType;
  }
  return sizeExpression;
}


int computeTypeSize(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      return type->descriptorDesc->size;
  }

  if (type->kind == TR_POINTED) {
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
    result->pointed = pointedTo;
    return result;
}

TypeRef *makeArrayType(ParserContext *ctx, int size, TypeRef *elementType) {
    TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
    result->kind = TR_ARRAY;
    result->arrayTypeDesc.size = size;
    result->arrayTypeDesc.elementType = elementType;
    return result;
}

TypeRef *makeVLAType(ParserContext *ctx, AstExpression *sizeExpression, TypeRef *elementType) {
  TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
  result->kind = TR_VLA;
  result->vlaDescriptor.elementType = elementType;
  result->vlaDescriptor.sizeExpression = sizeExpression;
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

  if (returnRefKind == TR_FUNCTION || returnRefKind == TR_ARRAY || returnRefKind == TR_VLA) {
      enum DiagnosticId diag = returnRefKind == TR_FUNCTION ? DIAG_FUNCTION_RETURN_FUNCTION_TYPE : DIAG_FUNCTION_RETURN_ARRAY_TYPE;
      reportDiagnostic(ctx, diag, &declarator->coordinates, returnType);
  }
}

static void verifyFunctionType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_FUNCTION);

  verifyFunctionReturnType(ctx, declarator, type->functionTypeDesc.returnType);
}

static void verifyArrayType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_ARRAY || type->kind == TR_VLA);
  TypeRef *elementType = type->kind == TR_ARRAY ? type->arrayTypeDesc.elementType : type->vlaDescriptor.elementType;

  if (elementType->kind == TR_FUNCTION) {
      reportDiagnostic(ctx, DIAG_ARRAY_OF_FUNCTIONS_ILLEGAL, &declarator->coordinates, type);
  }
}

static TypeRef *makeArrayTypeFromDeclarator(ParserContext *ctx, TypeRef *elementType, DeclaratorPart *part, const char *id, DeclaratorScope scope) {
  assert(part->kind == DPK_ARRAY);

  AstExpression *sizeExpession = part->arrayDeclarator.sizeExpression;

  if (part->arrayDeclarator.isStar) {
      assert(sizeExpession == NULL);

      if (scope != DS_PARAMETERS) {
          reportDiagnostic(ctx, DIAG_ARRAY_STAR_OUTSIDE_PROTOTYPE, &part->coordinates);
      }

      return makeVLAType(ctx, NULL, elementType);
  }

  if (sizeExpession == NULL) {
      return makeArrayType(ctx, -1, elementType);
  }

  AstConst *e = eval(ctx, sizeExpession);
  TypeRef *result = NULL;
  if (e) {
      int32_t size = e->i;
      if (size < 0) {
          if (id) {
            reportDiagnostic(ctx, DIAG_DECLARED_ARRAY_NEGATIVE_SIZE, &sizeExpession->coordinates, id);
          } else {
            reportDiagnostic(ctx, DIAG_ARRAY_NEGATIVE_SIZE, &sizeExpession->coordinates);
          }
          size = UNKNOWN_SIZE;
      }

      if (elementType->kind != TR_VLA) {
        result = makeArrayType(ctx, size, elementType);
        result->arrayTypeDesc.isStatic = part->arrayDeclarator.isStatic;
      } else {
        AstExpression *evaluated = createAstConst2(ctx, &sizeExpession->coordinates, sizeExpession->type, e);
        result = makeVLAType(ctx, evaluated, elementType);
      }
  } else {
      result = makeVLAType(ctx, sizeExpession, elementType);
  }

  if (scope != DS_PARAMETERS) {
      if (part->arrayDeclarator.isStatic) reportDiagnostic(ctx, DIAG_ARRAY_MODIFIER_NOT_IN_PROTOTYPE, &part->coordinates, "static");
      if (part->arrayDeclarator.isConst) reportDiagnostic(ctx, DIAG_ARRAY_MODIFIER_NOT_IN_PROTOTYPE, &part->coordinates, "const");
      if (part->arrayDeclarator.isRestrict) reportDiagnostic(ctx, DIAG_ARRAY_MODIFIER_NOT_IN_PROTOTYPE, &part->coordinates, "restrict");
      if (part->arrayDeclarator.isVolatile) reportDiagnostic(ctx, DIAG_ARRAY_MODIFIER_NOT_IN_PROTOTYPE, &part->coordinates, "volatile");
  }

  result->flags.bits.isConst = part->arrayDeclarator.isConst;
  result->flags.bits.isRestrict = part->arrayDeclarator.isRestrict;
  result->flags.bits.isVolatile = part->arrayDeclarator.isVolatile;

  return result;
}

TypeRef *makeTypeRef(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator, DeclaratorScope scope) {

    TypeRef *type = specifiers->basicType;

    DeclaratorPart *part = declarator->declaratorParts;

    while (part) {
        switch (part->kind) {
        case DPK_POINTER:
            type = makePointedType(ctx, part->flags.storage, type);
            break;
        case DPK_ARRAY:
            type = makeArrayTypeFromDeclarator(ctx, type, part, declarator->identificator, scope);
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
