
#include <assert.h>

#include "sema.h"
#include "treeDump.h"
#include "parser.h"


static TypeEqualityKind structualTypesEquality(TypeDesc *d1, TypeDesc *d2, TypeId kind) {
  if (d1->typeId == kind && d2->typeId == kind) {
      if (d1->structInfo == d2->structInfo) {
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

static TypeEqualityKind typeDescriprorEquals(TypeDesc *d1, TypeDesc *d2) {

  if (d1->typeId == T_ERROR || d2->typeId == T_ERROR) return TEK_NOT_EQUAL;

  TypeEqualityKind structCheck = structualTypesEquality(d1, d2, T_STRUCT);
  if (structCheck != TEK_UNKNOWN) return structCheck;
  TypeEqualityKind unionCheck = structualTypesEquality(d1, d2, T_UNION);
  if (unionCheck != TEK_UNKNOWN) return unionCheck;

  if (d1->typeId == T_ENUM && d2->typeId == T_ENUM) {
    if (d1->structInfo == d2->structInfo) {
        return TEK_EQUAL;
    } else {
        return TEK_NOT_EQUAL;
    }
  }

  if (d1->typeId == d2->typeId) return TEK_EQUAL;

  return TEK_NOT_EQUAL;
}

static TypeEqualityKind valueTypeEquality(TypeRef *t1, TypeRef *t2) {
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
      equality = typeEquality(t1->pointedTo, t2->pointedTo);
      if (t1->flags.bits.isConst == t2->flags.bits.isConst) {
        if (equality == TEK_EQUAL) {
            return TEK_EQUAL;
          } else {
            // 'int *' is not equal to 'int *const'
            return equality;
          }
      }
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

  return uOff > sOff ? uT : sT;
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

static Boolean isStructualType(TypeRef *type) {
  if (type->kind == TR_VALUE && type->descriptorDesc->typeId == T_STRUCT || type->descriptorDesc->typeId == T_STRUCT)
    return TRUE;
  return FALSE;
}

static Boolean isVoidType(TypeRef *type) {
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

static Boolean isPrimitiveType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
    TypeId typeId = type->descriptorDesc->typeId;
    if (T_VOID < typeId && typeId < T_BUILT_IN_TYPES || typeId == T_ENUM)
      return TRUE;
  }
  return FALSE;
}

static Boolean isPointerLikeType(TypeRef *type) {
  return type->kind == TR_POINTED || type->kind == TR_ARRAY;
}

static Boolean isScalarType(TypeRef *type) {
  return type->kind == TR_POINTED || isPrimitiveType(type);
}

static Boolean isRealType(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      TypeId tid = type->descriptorDesc->typeId;
      return T_F4 <= tid && tid <= T_F10 ? TRUE : FALSE;
  }
  return FALSE;
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

TypeRef *computeArrayAccessExpressionType(ParserContext *ctx, int so, int eo, TypeRef *arrayType, TypeRef *indexType) {
  if (isErrorType(arrayType)) return arrayType;
  if (isErrorType(indexType)) return indexType;

  Coordinates coords = { so, eo };

  if (!isIntegerType(indexType)) {
      reportDiagnostic(ctx, DIAG_ARRAY_SUBSCRIPT_NOT_INT, &coords);
      return makeErrorRef(ctx);
  }

  if (arrayType->kind == TR_POINTED) {
      return arrayType->pointedTo;
  }

  if (arrayType->kind == TR_ARRAY) {
      return arrayType->arrayTypeDesc.elementType;
  }

  reportDiagnostic(ctx, DIAG_SUBSCRIPTED_NOT_A_POINTER, &coords);

  return makeErrorRef(ctx);
}


TypeRef *computeFunctionReturnType(ParserContext *ctx, int so, int eo, TypeRef *_calleeType) {
    if (isErrorType(_calleeType)) return _calleeType;

    TypeRef *calleType = _calleeType;
    if (_calleeType->kind == TR_POINTED) {
        calleType = _calleeType->pointedTo;
    }

    if (calleType->kind != TR_FUNCTION) {
        Coordinates coords = { so, eo };
        reportDiagnostic(ctx, DIAG_INVOKE_NOT_FUNCTIONAL, &coords, _calleeType);
        return makeErrorRef(ctx);
    }

    return calleType->functionTypeDesc.returnType;
}



TypeRef *computeMemberAccessType(ParserContext *ctx, int so, int eo, TypeRef *_receiverType, const char *memberName, ExpressionType op) {
  assert(op == EF_DOT || op == EF_ARROW);
  if (isErrorType(_receiverType)) return _receiverType;

  TypeRef *receiverType = _receiverType;

  Coordinates coords = { so, eo };

  if (op == EF_ARROW) {
    if (_receiverType->kind == TR_POINTED) {
        receiverType = _receiverType->pointedTo;
    } else if (_receiverType->kind == TR_ARRAY) {
        receiverType = _receiverType->arrayTypeDesc.elementType;
    } else {
        reportDiagnostic(ctx, DIAG_MEMBER_REF_NOT_A_POINTER, &coords, _receiverType);
        return makeErrorRef(ctx);
    }
  }


  if (!isStructualType(receiverType)) {
      reportDiagnostic(ctx, DIAG_MEMBER_REF_NOT_A_STRUCTUAL, &coords, receiverType);
      return makeErrorRef(ctx);
  }

  AstSUEDeclaration *declaration = receiverType->descriptorDesc->structInfo;

  AstStructDeclarator *memberDeclarator = NULL;

  AstStructMember *member = declaration->members;

  while (member) {
      if (member->kind == SM_DECLARATOR) {
        AstStructDeclarator *declarator = member->declarator;
        if (strcmp(declarator->name, memberName) == 0) {
            memberDeclarator = declarator;
            break;
        }
      }
      member = member->next;
  }


  if (memberDeclarator == NULL) {
      reportDiagnostic(ctx, DIAG_NO_MEMBER_NAME, &coords, memberName, receiverType);
      return makeErrorRef(ctx);
  }


  return memberDeclarator->typeRef;
}

TypeRef *computeTernaryType(ParserContext *ctx, int so, int eo, TypeRef* cond, TypeRef* ifTrue, TypeRef *ifFalse, ExpressionType op) {
  if (isErrorType(cond)) return cond;
  if (isErrorType(ifTrue)) return ifTrue;
  if (isErrorType(ifFalse)) return ifFalse;

  Coordinates coords = { so, eo };

  if (isStructualType(ifTrue) || isStructualType(ifFalse)) {
      if (typesEquals(ifTrue, ifFalse)) {
          return ifTrue;
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, &coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  TypeRefKind tKind = ifTrue->kind;
  TypeRefKind fKind = ifFalse->kind;

  if (isPrimitiveType(ifTrue) && isPrimitiveType(ifFalse)) {
      return commonPrimitiveType(ctx, ifTrue, ifFalse);
  }

  if (isPointerLikeType(ifTrue)) {
      TypeRef *pointedT = ifTrue->kind == TR_POINTED ? ifTrue->pointedTo : ifTrue->arrayTypeDesc.elementType;
      if (isPointerLikeType(ifFalse)) {
          TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointedTo : ifFalse->arrayTypeDesc.elementType;
          if (!typesEquals(pointedT, pointedF)) { // TODO: could fail with array vs ponter
              reportDiagnostic(ctx, DIAG_POINTER_TYPE_MISMATCH, &coords, ifTrue, ifFalse);
          }
          return makePointedType(ctx, ifTrue->flags, pointedT);
      } else if (isIntegerType(ifFalse)) {
          reportDiagnostic(ctx, DIAG_POINTER_INT_MISMATCH_IN_COND, &coords, ifTrue, ifFalse);
          return makePointedType(ctx, ifTrue->flags, pointedT);
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, &coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  if (isPointerLikeType(ifFalse)) {
      TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointedTo : ifFalse->arrayTypeDesc.elementType;
      if (isIntegerType(ifTrue)) {
          reportDiagnostic(ctx, DIAG_POINTER_INT_MISMATCH_IN_COND, &coords, ifTrue, ifFalse);
          return makePointedType(ctx, ifFalse->flags, pointedF);
      } else {
          reportDiagnostic(ctx, DIAG_INCOMPATIBLE_OPERANDS, &coords, ifTrue, ifFalse);
          return makeErrorRef(ctx);
      }
  }

  return makeErrorRef(ctx); // Unknown situation
}

TypeRef *computeBinaryType(ParserContext *ctx, int so, int eo, TypeRef* left, TypeRef *right, ExpressionType op) {
  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  Coordinates coords = { so, eo };

  if (isStructualType(left) || isStructualType(right)) {
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
      return makeErrorRef(ctx);
  }

  if (EB_ANDAND <= op && op <= EB_GE) {
      return makePrimitiveType(ctx, T_S4, 0);
  }

  if (EB_LHS <= op && op <= EB_XOR) {
      if (isIntegerType(left) && isIntegerType(right)) {
          return commonPrimitiveType(ctx, left, right);
      }
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
      return makeErrorRef(ctx);
  }


  if (EB_MUL <= op && op <= EB_MOD) {
    if (isPrimitiveType(left) && isPrimitiveType(right)) {
        return commonPrimitiveType(ctx, left, right);
    }
    reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
    return makeErrorRef(ctx);
  }


  if (op == EB_ADD) {
      if (isPointerLikeType(left) && isPointerLikeType(right)) {
          reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
          return makeErrorRef(ctx);
      }
  }

  if (isPointerLikeType(left) && isPointerLikeType(right)) {
      assert(op == EB_SUB);
      return makePrimitiveType(ctx, T_S8, 0);
  }

  if (isPointerLikeType(left)) {
      if (isIntegerType(right)) {
        TypeRef *pointedL = left->kind == TR_POINTED ? left->pointedTo : left->arrayTypeDesc.elementType;
        return makePointedType(ctx, left->flags, pointedL);
      }
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
      return makeErrorRef(ctx);
  }

  if (isPointerLikeType(right)) {
      if (isIntegerType(left)) {
        TypeRef *pointedR = right->kind == TR_POINTED ? right->pointedTo : right->arrayTypeDesc.elementType;
        return makePointedType(ctx, right->flags, pointedR);
      }
      reportDiagnostic(ctx, DIAG_INVALID_BINARY_OPS, &coords, left, right);
      return makeErrorRef(ctx);
  }

  return commonPrimitiveType(ctx, left, right);
}


TypeRef *computeIncDecType(ParserContext *ctx, int so, int eo, TypeRef *argumentType, ExpressionType op) {
  assert(op == EU_PRE_INC || op == EU_PRE_DEC || op == EU_POST_INC || op == EU_POST_DEC);

  if (isErrorType(argumentType)) return argumentType;

  if (argumentType->kind == TR_FUNCTION || argumentType->kind == TR_ARRAY || isStructualType(argumentType)) {
      enum DiagnosticId diag = op == EU_POST_DEC || op == EU_PRE_DEC ? DIAG_CANNOT_DECREMENT : DIAG_CANNOT_INCREMENT;
      Coordinates coords = { so, eo };
      reportDiagnostic(ctx, diag, &coords, argumentType);
      return makeErrorRef(ctx);
  }

  return argumentType;
}

static TypeRef *computeTypeForDerefOperator(ParserContext *ctx, int so, int eo, TypeRef *argumentType) {
  if (argumentType->kind == TR_POINTED) {
      return argumentType->pointedTo;
  }

  if (argumentType->kind == TR_ARRAY) {
      return argumentType->arrayTypeDesc.elementType;
  }

  Coordinates coords = { so, eo };
  reportDiagnostic(ctx, DIAG_INDERECTION_POINTER_OP, &coords, argumentType);

  return makeErrorRef(ctx);
}

TypeRef *computeTypeForUnaryOperator(ParserContext *ctx, int so, int eo, TypeRef *argumentType, ExpressionType op) {
  if (isErrorType(argumentType)) return argumentType;

  SpecifierFlags flags = { 0 };

  switch (op) {
    case EU_REF:   // &a
      return makePointedType(ctx, flags, argumentType);
    case EU_DEREF: // *a
      return computeTypeForDerefOperator(ctx, so, eo, argumentType);
    case EU_PLUS:  // +a
    case EU_MINUS: // -a
    case EU_EXL:   // !a
    case EU_TILDA: // ~a
      if (op == EU_TILDA && isIntegerType(argumentType)) {
          return argumentType;
      } else if (op != EU_TILDA && isPrimitiveType(argumentType)) {
          return argumentType;
      } else {
          Coordinates coords = { so, eo };
          reportDiagnostic(ctx, DIAG_INVALID_UNARY_ARGUMENT, &coords, argumentType);
          return makeErrorRef(ctx);
      }
    default:
      unreachable("Unexpected Unary operator type");
      return NULL;
    }

  return argumentType;
}

Boolean isAssignableTypes(ParserContext *ctx, int so, int eo, TypeRef *to, TypeRef *from, Boolean init) {
  if (isErrorType(to)) return TRUE;
  if (isErrorType(from)) return TRUE;

  Coordinates coords = { so, eo };

  if (!init && to->flags.bits.isConst) {
      reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, &coords, to);
      return FALSE;
  }

  if (to->kind == TR_ARRAY) {
      reportDiagnostic(ctx, DIAG_ARRAY_TYPE_IS_NOT_ASSIGNABLE, &coords, to);
      return FALSE;
  }

  if (isStructualType(to) || isStructualType(from)) {
      if (typesEquals(to, from)) {
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, &coords, to, from);
          return FALSE;
      }
  }

  if (to->kind == TR_POINTED) {
      TypeRef *pLeft = to->pointedTo;
      if (isPointerLikeType(from)) {
          TypeRef *pointed = from->kind == TR_POINTED ? from->pointedTo : from->arrayTypeDesc.elementType;
          if (isVoidType(pLeft) || isVoidType(pointed)) {
              return TRUE;
          } else if (typesEquals(pLeft, pointed)) {
              return TRUE;
          } else {
              reportDiagnostic(ctx, DIAG_ASSIGN_INCOMPATIBLE_POINTERS, &coords, to, from);
              return TRUE;
          }
      } else if (isIntegerType(from)) {
          reportDiagnostic(ctx, DIAG_ASSIGN_INT_TO_POINTER, &coords, to, from);
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, &coords, to, from);
          return FALSE;
      }
  }


  if (isPointerLikeType(from)) {
      if (isIntegerType(to)) {
          reportDiagnostic(ctx, DIAG_ASSIGN_INT_TO_POINTER, &coords, to, from);
          return TRUE;
      } else {
          reportDiagnostic(ctx, DIAG_ASSIGN_FROM_INCOMPATIBLE_TYPE, &coords, to, from);
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

  switch (expr->op) {
  case E_NAMEREF:
  case EF_ARROW:
  case EF_DOT:
  case EB_A_ACC:
      if (expr->type->flags.bits.isConst) {
          if (report) {
            reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, expr->type);
          }
          return FALSE;
      }
      return TRUE;
  case EU_DEREF:
      if (expr->type->pointedTo->flags.bits.isConst) {
          reportDiagnostic(ctx, DIAG_ASSIGN_IN_CONST, coords, expr->type);
          return FALSE;
      }
      return TRUE;
  default:
      reportDiagnostic(ctx, DIAG_EXPRESSION_IS_NOT_ASSIGNABLE, coords);
      return FALSE;
  }
}

static Boolean checkUnionIsCastableToType(ParserContext *ctx, Coordinates *coords, TypeRef *from, AstSUEDeclaration *unionDecl, Boolean report) {
  assert(unionDecl->kind == DK_UNION);

  AstStructMember *member = unionDecl->members;

  while (member) {
      if (member->kind == SM_DECLARATOR) {
        AstStructDeclarator *declarator = member->declarator;
        if (typeEquality(declarator->typeRef, from) == TEK_EQUAL) {
            return TRUE;
        }
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
          if (from->kind == TR_VALUE && desc->structInfo == from->descriptorDesc->structInfo) {
              return TRUE;
          }
          if (report) {
            reportDiagnostic(ctx, DIAG_NON_CASTABLE_TYPE, coords, to);
          }
          return FALSE;
      }
      if (desc->typeId == T_UNION) {
          if (from->kind == TR_VALUE && desc->structInfo == from->descriptorDesc->structInfo) {
              return TRUE;
          }
          return checkUnionIsCastableToType(ctx, coords, from, desc->structInfo, report);
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

TypeRef *computeAssignmentTypes(ParserContext *ctx, int so, int eo, TypeRef *left, TypeRef *right) {
  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  if (isAssignableTypes(ctx, so, eo, left, right, FALSE)) {
    return left;
  }

  return makeErrorRef(ctx);
}

TypeRef *computeFunctionType(ParserContext *ctx, int so, int eo, AstFunctionDeclaration *declaration) {
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

static void unwindInitializer(ParserContext *ctx, AstInitializer *transformed, TypeRef *type, AstInitializerList **head, AstInitializerList **tail, int *count) {

  AstInitializerList *tmp = NULL;
  if (transformed) {
    if (transformed->kind == IK_EXPRESSION) {
        tmp = createAstInitializerList(ctx);
        tmp->initializer = transformed;
        ++(*count);
    } else {
        assert(transformed->kind == IK_LIST);
        if (isScalarType(type)) {
          tmp = transformed->initializerList;
          *count += transformed->numOfInitializers;
        } else {
          tmp = createAstInitializerList(ctx);
          tmp->initializer = transformed;
          ++(*count);
        }
    }
  }

  if (*tail) {
      (*tail)->next = tmp;
  } else {
      *head = tmp;
  }

  while (tmp) {
      *tail = tmp;
      tmp = tmp->next;
  }
}


static Boolean isCompileTimeConstant(AstExpression *expr) {
  switch (expr->op) {
    case EU_PRE_INC:
    case EU_POST_INC:
    case EU_PRE_DEC:
    case EU_POST_DEC:
    case EU_PLUS:      /** +a */
    case EU_MINUS:     /** -a */
    case EU_TILDA:     /** ~a */
    case EU_EXL:       /** !a */
      return isCompileTimeConstant(expr->unaryExpr.argument);
    case E_TERNARY:
      return isCompileTimeConstant(expr->ternaryExpr.condition)
          && isCompileTimeConstant(expr->ternaryExpr.ifTrue)
          && isCompileTimeConstant(expr->ternaryExpr.ifFalse);
    case E_CONST:
      return TRUE;
    case E_CAST:
      return isCompileTimeConstant(expr->castExpr.argument);
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
      return isCompileTimeConstant(expr->binaryExpr.left) && isCompileTimeConstant(expr->binaryExpr.right);
    case EB_COMMA:
      return isCompileTimeConstant(expr->binaryExpr.right);
    default:
      return FALSE;
  }
}


static AstInitializer *finalizeArrayInitializer(ParserContext *ctx, TypeRef *elementType, AstInitializer *initializer, unsigned arraySize, unsigned *counter, Boolean isTopLevel) {
  int so = initializer->coordinates.startOffset;
  int eo = initializer->coordinates.endOffset;

  if (initializer->kind == IK_EXPRESSION) {
      AstExpression *expr = initializer->expression;
      if (expr) {
        if (isTopLevel && !isCompileTimeConstant(expr)) {
            reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coordinates);
        }
        if (!(isStructualType(elementType) && isIntegerType(expr->type))) { // array initializer like `struct S as[] = { 0 }` is totally acceptable
          isAssignableTypes(ctx, so, eo, elementType, expr->type, TRUE);
        }
      } else {
        initializer->expression = createErrorExpression(ctx, so, eo);
      }
      (*counter)++;
      return initializer;
  } else {
      assert(initializer->kind == IK_LIST);
      AstInitializerList *inner = initializer->initializerList;
      AstInitializerList *head = NULL, *tail = NULL;
      if (elementType->kind == TR_POINTED || isPrimitiveType(elementType)) {
        if (inner) {
          int count = 0;
          while (inner) {
              if (*counter >= arraySize) {
                reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &inner->initializer->coordinates);
              }

              AstInitializer *transformed = finalizeArrayInitializer(ctx, elementType, inner->initializer, arraySize, counter, isTopLevel);
              unwindInitializer(ctx, transformed, elementType, &head, &tail, &count);

              inner = inner->next;
          }
          AstInitializer *result = createAstInitializer(ctx, so, eo, IK_LIST);
          result->initializerList = head;
          result->numOfInitializers = count;
          return result;
        } else {
          reportDiagnostic(ctx, DIAG_SCALAR_INIT_EMPTY, &initializer->coordinates);
          AstInitializer *result = createAstInitializer(ctx, so, eo, IK_EXPRESSION);
          result->expression = createErrorExpression(ctx, so, eo);
          return result;
        }
    } else {
      AstInitializer *result = finalizeInitializer(ctx, elementType, initializer, isTopLevel);
      (*counter)++;
      return result;
    }
  }

  return NULL;
}

static AstInitializer *finalizeStructMember(ParserContext *ctx, AstStructMember **pmember, AstInitializer *initializer, Boolean isTopLevel) {
  int so = initializer->coordinates.startOffset;
  int eo = initializer->coordinates.endOffset;
  AstStructMember *member  = *pmember;
  assert(member->kind == SM_DECLARATOR);
  TypeRef *memberType = member->declarator->typeRef;
  if (initializer->kind == IK_EXPRESSION) {
      AstExpression *expr = initializer->expression;
      if (expr) {
        if (isTopLevel && !isCompileTimeConstant(expr)) {
            reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coordinates);
        }
        isAssignableTypes(ctx, so, eo, memberType, expr->type, TRUE);
      } else {
        initializer->expression = createErrorExpression(ctx, so, eo);
      }
      *pmember = member->next;
      return initializer;
  } else {
      assert(initializer->kind == IK_LIST);
      AstInitializerList *inner = initializer->initializerList;
      AstInitializerList *head = NULL, *tail = NULL;
      if (memberType->kind == TR_POINTED || isPrimitiveType(memberType)) {
          AstInitializer *result = NULL;
          if (inner) {
            int count = 0;
            while (inner && member) {
                if (member->kind != SM_DECLARATOR) {
                    member = member->next;
                    continue;
                }
                TypeRef *memberType = member->declarator->typeRef;
                AstInitializer *transformed = finalizeStructMember(ctx, &member, inner->initializer, isTopLevel);
                unwindInitializer(ctx, transformed, memberType, &head, &tail, &count);

                inner = inner->next;
            }

            if (inner != NULL) {
                reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &inner->initializer->coordinates);
            }

            result = createAstInitializer(ctx, so, eo, IK_LIST);
            result->initializerList = head;
            result->numOfInitializers = count;
          } else {
            reportDiagnostic(ctx, DIAG_SCALAR_INIT_EMPTY, &initializer->coordinates);
            result = createAstInitializer(ctx, so, eo, IK_EXPRESSION);
            result->expression = createErrorExpression(ctx, so, eo);
          }
          *pmember = member;
          return result;
      } else {
          AstInitializer *result = finalizeInitializer(ctx, memberType, initializer, isTopLevel);
          *pmember = member->next;
          return result;
      }
  }
  return NULL;
}

AstInitializer *finalizeInitializer(ParserContext *ctx, TypeRef *valueType, AstInitializer *initializer, Boolean isTopLevel) {
  int so = initializer->coordinates.startOffset;
  int eo = initializer->coordinates.endOffset;
  if (isErrorType(valueType)) {
      initializer->kind = IK_EXPRESSION;
      initializer->expression = createErrorExpression(ctx, so, eo);
      initializer->numOfInitializers = -1;
      return initializer;
  }

  if (valueType->kind == TR_ARRAY && valueType->arrayTypeDesc.size == UNKNOWN_SIZE) {
      if (initializer->kind == IK_LIST) {
          assert(initializer->numOfInitializers >= 0);
          valueType->arrayTypeDesc.size = initializer->numOfInitializers;
      } else {
          reportDiagnostic(ctx, DIAG_INVALID_INITIALIZER, &initializer->coordinates);
          return initializer;
      }
  }

  if (initializer->kind == IK_EXPRESSION) {
      AstExpression *expr = initializer->expression;
      if (expr) {
        if (isTopLevel && !isCompileTimeConstant(expr)) {
            reportDiagnostic(ctx, DIAG_INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, &initializer->coordinates);
        }
        isAssignableTypes(ctx, so, eo, valueType, expr->type, TRUE);
      }
      return initializer;
  } else {
      TypeRefKind kind = valueType->kind;
      AstInitializerList *inner = initializer->initializerList;
      if (isScalarType(valueType)) {
            if (inner) {
              AstInitializer * result = finalizeInitializer(ctx, valueType, inner->initializer, isTopLevel);
              AstInitializerList *next = inner->next;
              if (next) {
                  reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &next->initializer->coordinates);
              }
              return result;
            } else {
                reportDiagnostic(ctx, DIAG_SCALAR_INIT_EMPTY, &initializer->coordinates);
                return initializer;
            }
      } else if (isStructualType(valueType)) {
          AstSUEDeclaration *declaration = valueType->descriptorDesc->structInfo;
          AstStructMember *member = declaration->members;

          AstInitializerList *head = NULL, *tail = NULL;

          int count = 0;

          while (inner && member) {
              TypeRef *type = NULL;

              if (member->kind == SM_DECLARATION) {
                  member = member->next;
                  continue;
              }
              assert(member->kind == SM_DECLARATOR);
              type = member->declarator->typeRef;

              AstInitializer *transformed = finalizeStructMember(ctx, &member, inner->initializer, isTopLevel);

              unwindInitializer(ctx, transformed, type, &head, &tail, &count);
              inner = inner->next;
          }

          AstInitializer *result = createAstInitializer(ctx, so, eo, IK_LIST);
          result->numOfInitializers = count;
          result->initializerList = head;
          return result;
      } else if (valueType->kind == TR_ARRAY) {
          TypeRef *elementType = valueType->arrayTypeDesc.elementType;
          unsigned arrayCounter = 0, arraySize = valueType->arrayTypeDesc.size;
          AstInitializerList *head = NULL, *tail = NULL;
          int count = 0;

          while (inner) {
              if (arrayCounter >= arraySize) {
                  reportDiagnostic(ctx, DIAG_W_EXCESS_ELEMENTS_INIT, &inner->initializer->coordinates);
              }

              AstInitializer *transformed = finalizeArrayInitializer(ctx, elementType, inner->initializer, arraySize, &arrayCounter, isTopLevel);

              unwindInitializer(ctx, transformed, elementType, &head, &tail, &count);
              inner = inner->next;
          }
          AstInitializer *result = createAstInitializer(ctx, so, eo, IK_LIST);
          result->numOfInitializers = count;
          result->initializerList = head;
          return result;
      } else {
          // ft fx = { 01.f, 5, { 0, 0.1f, "ccc" } , 0 };
          reportDiagnostic(ctx, DIAG_ILLEGAL_INIT_ONLY_VARS, &initializer->coordinates);
      }
  }
  return NULL;
}

// true if everything is OK
Boolean verifyValueType(ParserContext *ctx, int so, int eo, TypeRef *valueType) {
  if (isErrorType(valueType)) return TRUE;

  Coordinates coords = { so, eo };

  if (valueType->kind == TR_VALUE) {
      TypeDesc *desc = valueType->descriptorDesc;
      if (desc->typeId == T_VOID || desc->size == UNKNOWN_SIZE) {
          reportDiagnostic(ctx, DIAG_VAR_INCOMPLETE_TYPE, &coords, desc);
          return FALSE;
      }
  }

  return TRUE;
}

void verifyCallAruments(ParserContext *ctx, int so, int eo, TypeRef *functionType, AstExpressionList *aruments) {
  if (isErrorType(functionType)) return;

  if (functionType->kind == TR_POINTED)
    functionType = functionType->pointedTo;

  assert(functionType->kind == TR_FUNCTION);

  AstExpressionList *argument = aruments;
  TypeList *param = functionType->functionTypeDesc.parameters;

  while (argument && param) {
      AstExpression *arg = argument->expression;
      int so = arg->coordinates.startOffset;
      int eo = arg->coordinates.endOffset;
      TypeRef *aType = arg->type;
      TypeRef *pType = param->type;
      if (isAssignableTypes(ctx, so, eo, pType, aType, FALSE)) {
          argument = argument->next;
          param = param->next;
      } else {
          break;
      }
  }

  Coordinates coords = { so, eo };

  if (argument == NULL && param != NULL) { // fewer
      reportDiagnostic(ctx, DIAG_TOO_FEW_ARGS, &coords);
  }

  if (argument != NULL && param == NULL) {
      if (!functionType->functionTypeDesc.isVariadic) {
          reportDiagnostic(ctx, DIAG_TOO_MANY_ARGS, &coords);
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
    case SK_GOTO:
      break;
  }
}

void verifySwitchCases(ParserContext *ctx, AstStatement *switchBody, unsigned caseCount) {
  int *caseSet = heapAllocate(sizeof (int) * caseCount);
  unsigned caseIndex = 0;
  Boolean hasDefault = FALSE;

  verifySwtichCasesRec(ctx, switchBody, caseCount, &caseIndex, caseSet, &hasDefault);

  releaseHeap(caseSet);
}

void verifyGotoLabels(ParserContext *ctx, AstStatement *stmt, HashMap *labelSet) {
  switch (stmt->statementKind) {
    case SK_BLOCK: {
        AstStatementList *node = stmt->block.stmts;
        while (node) {
            verifyGotoLabels(ctx, node->stmt, labelSet);
            node = node->next;
        }
        break;
    }
    case SK_LABEL:
        verifyGotoLabels(ctx, stmt->labelStmt.body, labelSet);
        break;
    case SK_IF:
        verifyGotoLabels(ctx, stmt->ifStmt.thenBranch, labelSet);
        if (stmt->ifStmt.elseBranch) {
            verifyGotoLabels(ctx, stmt->ifStmt.elseBranch, labelSet);
        }
        break;
    case SK_WHILE:
    case SK_DO_WHILE:
        verifyGotoLabels(ctx, stmt->loopStmt.body, labelSet);;
        break;
    case SK_FOR:
        verifyGotoLabels(ctx, stmt->forStmt.body, labelSet);;
        break;
    case SK_SWITCH: // stop
        verifyGotoLabels(ctx, stmt->switchStmt.body, labelSet);;
        break;
    case SK_RETURN:
    case SK_DECLARATION:
    case SK_EXPR_STMT:
    case SK_EMPTY:
    case SK_ERROR:
    case SK_BREAK:
    case SK_CONTINUE:
      break;
    case SK_GOTO:
      if (!isInHashMap(labelSet, stmt->jumpStmt.label)) {
          reportDiagnostic(ctx, DIAG_UNDECLARED_LABEL, &stmt->coordinates, stmt->jumpStmt.label);
      }
      break;
  }
}

int stringHashCode(const void *v) {
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

int stringCmp(const void *v1, const void *v2) {
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
  return (Symbol *)getFromHashMap(scope->symbols, name);
}

Symbol* findSymbol(ParserContext *ctx, const char *name) {
    if (name) {
      Scope* s = ctx->currentScope;
      while (s != NULL) {
          Symbol *sb = (Symbol *)getFromHashMap(s->symbols, name);
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


Symbol* declareSymbol(ParserContext *ctx, SymbolKind kind, const char *name) {
    int symbolSize = sizeof(Symbol);
    Symbol *s = (Symbol *)areanAllocate(ctx->memory.typeArena, symbolSize);
    s->kind = kind;
    s->name = name;

    Scope *scope = ctx->currentScope;
    putToHashMap(scope->symbols, name, s);

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
          reportDiagnostic(ctx, DIAG_SYMBOL_REDEFINITION, &ctx->token->coordinates, name);
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
  if (typesEquals(oldType, newType)) {
      reportDiagnostic(ctx, DIAG_TYPEDEF_REDEFINITION_C11, &ctx->token->coordinates);
  } else {
      reportDiagnostic(ctx, DIAG_TYPEDEF_REDEFINITION_TYPES, &ctx->token->coordinates, oldType, newType);
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
      reportDiagnostic(ctx, DIAG_FUN_CONFLICTING_TYPES, &ctx->token->coordinates, oldDeclaration->name);
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
      // TODO: link declarations to list
  } else {
      reportDiagnostic(ctx, DIAG_VALUE_REDEFINITION_TYPES, &ctx->token->coordinates, s->name, newType, oldType);
  }
}

static int newValueProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == ValueSymbol);
  s->variableDesc = (AstValueDeclaration *)value;
}

Symbol *declareValueSymbol(ParserContext *ctx, const char *name, AstValueDeclaration *declaration) {
  return declareGenericSymbol(ctx, ValueSymbol, name, declaration, existedValueProcessor, newValueProcessor);
}

Symbol *declareSUESymbol(ParserContext *ctx, SymbolKind symbolKind, TypeId typeId, const char *symbolName, AstSUEDeclaration *declaration) {
  Symbol *s = findSymbolInScope(ctx->currentScope, symbolName); // TODO: allow local struct redeclaration

  const char *name = declaration->name;

  TypeDesc *typeDescriptor;
  if (!s) {
      s = declareSymbol(ctx, symbolKind, symbolName);
      int typeSize = UNKNOWN_SIZE;
      if (declaration->isDefinition) {
        typeSize = computeSUETypeSize(ctx, declaration);
      }
      typeDescriptor = s->typeDescriptor = createTypeDescriptor(ctx, typeId, name, typeSize);
      typeDescriptor->structInfo = declaration;
  } else {
      if (s->kind != symbolKind) {
          reportDiagnostic(ctx, DIAG_USE_WITH_DIFFERENT_TAG, &declaration->coordinates, name);
          // TODO: also point to already defined one
      } else {
          typeDescriptor = s->typeDescriptor;
          AstSUEDeclaration *existedDeclaration = typeDescriptor->structInfo;
          if (declaration->members) {
            if (existedDeclaration->members) {
              reportDiagnostic(ctx, DIAG_MEMBER_REDEFINITION, &declaration->coordinates, name);
                // TODO: also point to already defined one
            } else {
                typeDescriptor->structInfo = declaration;
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
      reportDiagnostic(ctx, diag, &ctx->token->coordinates, enumerator->name);
      return NULL; // or 's'?
  }

  s = declareSymbol(ctx, EnumConstSymbol, enumerator->name);
  s->enumerator = enumerator;
  return s;
}

// Types

static TypeDesc errorTypeImpl = { T_ERROR, "<error>", UNKNOWN_SIZE, NULL };

TypeDesc *errorTypeDescriptor = &errorTypeImpl;

TypeDesc builtInTypeDescriptors[] = {
    { T_VOID, "void", 0, NULL },

    { T_S1, "signed char", 1, NULL },
    { T_S2, "signed short", 2, NULL },
    { T_S4, "signed int", 4, NULL },
    { T_S8, "signed long", 8, NULL },

    { T_U1, "unsigned char", 1, NULL },
    { T_U2, "unsigned short", 2, NULL },
    { T_U4, "unsigned int", 4, NULL },
    { T_U8, "unsigned long", 8, NULL },

    { T_F4, "float", 4, NULL },
    { T_F8, "double", 8, NULL },
    { T_F10, "long double", 10, NULL }
};


int computeTypeSize(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      return type->descriptorDesc->size;
  }

  if (type->kind == TR_POINTED) {
      return POINTER_TYPE_SIZE;
  }

  if (type->kind == TR_ARRAY) {
      ArrayTypeDescriptor *atype = &type->arrayTypeDesc;
      return atype->size * computeTypeSize(atype->elementType);
  }

  return POINTER_TYPE_SIZE;
}

static int computeUnionTypeSize(ParserContext *ctx, AstSUEDeclaration *declaration) {
  assert(declaration->kind == DK_UNION);
  assert(declaration->isDefinition);

  int result = 0;

  AstStructMember *member = declaration->members;

  while (member) {
      assert(member->kind != SM_ENUMERATOR);
      if (member->kind == SM_DECLARATOR) {
         AstStructDeclarator *declarator = member->declarator;

         if (declarator->f_width >= 0) {
            unsigned aligned = ALIGN_SIZE(declarator->f_width, BYTE_BIT_SIZE);
            result = max(result, aligned / BYTE_BIT_SIZE);
         } else {
            int tmp = computeTypeSize(declarator->typeRef);
            if (tmp < 0) {
                reportDiagnostic(ctx, DIAG_FIELD_INCOMPLETE_TYPE, &declarator->coordinates, declarator->name, declarator->typeRef);
                return UNKNOWN_SIZE;
            }
            result = max(result, tmp);
         }
      }

      member = member->next;
  }

  return ALIGN_SIZE(result, POINTER_TYPE_SIZE);
}

static int computeStructTypeSize(ParserContext *ctx, AstSUEDeclaration *declaration) {
  assert(declaration->kind == DK_STRUCT);
  assert(declaration->isDefinition);

  int result = 0;
  unsigned bitCount = 0;


  AstStructMember *member = declaration->members;

  while (member) {
      assert(member->kind != SM_ENUMERATOR);
      if (member->kind == SM_DECLARATOR) {
         AstStructDeclarator *declarator = member->declarator;
         if (declarator->f_width >= 0) {
            bitCount += declarator->f_width;
         } else {
            int tmp = computeTypeSize(declarator->typeRef);
            if (tmp < 0) {
                reportDiagnostic(ctx, DIAG_FIELD_INCOMPLETE_TYPE, &declarator->coordinates, declarator->name, declarator->typeRef);
                return UNKNOWN_SIZE;
            }
            result += tmp;
         }
      }

      member = member->next;
  }


  bitCount = ALIGN_SIZE(bitCount, BYTE_BIT_SIZE); // align to byte (8 bits)
  result += bitCount / BYTE_BIT_SIZE;
  result = ALIGN_SIZE(result, POINTER_TYPE_SIZE); // align to word (8 bytes)

  return result;
}

int computeSUETypeSize(ParserContext *ctx, AstSUEDeclaration *declaration) {
  if (!declaration->isDefinition) {
      // report diagnostic
      return UNKNOWN_SIZE;
  }

  if (declaration->kind == DK_UNION) {
    return computeUnionTypeSize(ctx, declaration);
  }

  if (declaration->kind == DK_STRUCT) {
    return computeStructTypeSize(ctx, declaration);
  }

  assert(declaration->kind == DK_ENUM);
  return builtInTypeDescriptors[T_S4].size;
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
}

TypeRef* makePointedType(ParserContext *ctx, SpecifierFlags flags, TypeRef *pointedTo) {
    TypeRef *result = (TypeRef *)areanAllocate(ctx->memory.typeArena, sizeof(TypeRef));
    result->kind = TR_POINTED;
    result->flags.storage = flags.storage;
    result->pointedTo = pointedTo;
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

    TypeList *tail = NULL;

    while (parameter) {
      TypeList *cur = (TypeList*)areanAllocate(ctx->memory.typeArena, sizeof (TypeList));
      cur->type = parameter->type;
      parameter = parameter->next;
      if (tail) {
        tail->next = cur;
      } else {
        result->functionTypeDesc.parameters = cur;
      }
      tail = cur;
    }
    params->parameters = NULL;

    return result;
}

TypeRef *makeFunctionReturnType(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator) {

    TypeRef *type = specifiers->basicType;

    int i;
    for (i = declarator->partsCounter - 1; i >= 0; --i) {
        DeclaratorPart *part = &declarator->declaratorParts[i];
        switch (part->kind) {
        case DPK_POINTER:
            type = makePointedType(ctx, part->flags, type);
            break;
        case DPK_ARRAY:
            type = makeArrayType(ctx, part->arraySize, type);
            break;
        case DPK_FUNCTION:
            return type;
        case DPK_NONE:
        default:
            unreachable("UNKNOWN Declarator Part");
        }
    }

    reportDiagnostic(ctx, DIAG_EXPECTED_FUNCTION_DECLARATOR, &ctx->token->coordinates);
    return NULL; // return error type
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

    int i;
    for (i = declarator->partsCounter - 1; i >= 0; --i) {
        DeclaratorPart *part = &declarator->declaratorParts[i];
        switch (part->kind) {
        case DPK_POINTER:
            type = makePointedType(ctx, part->flags, type);
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
    }

    return type;
}

TypeRef *makeErrorRef(ParserContext *ctx) {
  return makeBasicType(ctx, errorTypeDescriptor, 0);
}
