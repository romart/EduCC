
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

static Boolean isErrorType(TypeRef *type) {
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

static Boolean isIntegerType(TypeRef *type) {
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

TypeRef *computeArrayAccessExpressionType(ParserContext *ctx, int so, int eo, TypeRef *arrayType, TypeRef *indexType) {
  if (isErrorType(arrayType)) return arrayType;
  if (isErrorType(indexType)) return indexType;

  if (!isIntegerType(indexType)) {
      reportError(ctx, so, eo, "array subscript is not an integer");
      return makeErrorRef(ctx);
  }

  if (arrayType->kind == TR_POINTED) {
      return arrayType->pointedTo;
  }

  if (arrayType->kind == TR_ARRAY) {
      return arrayType->arrayTypeDesc.elementType;
  }

  reportError(ctx, so, eo, "subscripted value is not an array or pointer");
  return makeErrorRef(ctx);
}


TypeRef *computeFunctionReturnType(ParserContext *ctx, int so, int eo, TypeRef *_calleeType) {
    if (isErrorType(_calleeType)) return _calleeType;

    TypeRef *calleType = _calleeType;
    if (_calleeType->kind == TR_POINTED) {
        calleType = _calleeType->pointedTo;
    }

    if (calleType->kind != TR_FUNCTION) {
        char buffer[1024] = { 0 };
        renderTypeRef(_calleeType, buffer, sizeof buffer);
        reportError(ctx, so, eo, "called object type '%s' is not a function or function pointer", buffer);
        return makeErrorRef(ctx);
    }

    return calleType->functionTypeDesc.returnType;
}



TypeRef *computeMemberAccessType(ParserContext *ctx, int so, int eo, TypeRef *_receiverType, const char *memberName, ExpressionType op) {
  assert(op == EF_DOT || op == EF_ARROW);
  if (isErrorType(_receiverType)) return _receiverType;

  TypeRef *receiverType = _receiverType;

  if (op == EF_ARROW) {
    if (_receiverType->kind == TR_POINTED) {
        receiverType = _receiverType->pointedTo;
    } else if (_receiverType->kind == TR_ARRAY) {
        receiverType = _receiverType->arrayTypeDesc.elementType;
    } else {
        char buffer[1024] = { 0 };
        renderTypeRef(_receiverType, buffer, sizeof buffer);
        reportError(ctx, so, eo, "member reference type '%s' is not a pointer", buffer);
        return makeErrorRef(ctx);
    }
  }


  if (!isStructualType(receiverType)) {
      char buffer[1024] = { 0 };
      renderTypeRef(receiverType, buffer, sizeof buffer);
      reportError(ctx, so, eo, "member reference base type '%s' is not a structure or union", buffer);
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
      char buffer[1024] = { 0 };
      renderTypeRef(receiverType, buffer, sizeof buffer);
      reportError(ctx, so, eo, "no member named '%s' in '%s'", memberName, buffer);
      return makeErrorRef(ctx);
  }


  return memberDeclarator->typeRef;
}

TypeRef *computeTernaryType(ParserContext *ctx, int so, int eo, TypeRef* cond, TypeRef* ifTrue, TypeRef *ifFalse, ExpressionType op) {
  if (isErrorType(cond)) return cond;
  if (isErrorType(ifTrue)) return ifTrue;
  if (isErrorType(ifFalse)) return ifFalse;

//  int i;
//  int *pi;
//  float f;
//  float *fp;
//  int arr[20];

//  so ? f : arr;

  if (isStructualType(ifTrue) || isStructualType(ifFalse)) {
      if (typesEquals(ifTrue, ifFalse)) {
          return ifTrue;
      } else {
        char b1[1024] = { 0 };
        char b2[1024] = { 0 };
        renderTypeRef(ifTrue, b1, sizeof b1);
        renderTypeRef(ifFalse, b2, sizeof b2);
        reportError(ctx, so, eo, "incompatible operand types ('%s' and '%s')", b1, b2);
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
              char b1[1024] = { 0 };
              char b2[1024] = { 0 };
              renderTypeRef(ifTrue, b1, sizeof b1);
              renderTypeRef(ifFalse, b2, sizeof b2);
              reportWarning(ctx, so, eo, "pointer type mismatch ('%s' and '%s')", b1, b2);
          }
          return makePointedType(ctx, ifTrue->flags, pointedT);
      } else if (isIntegerType(ifFalse)) {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(ifTrue, b1, sizeof b1);
          renderTypeRef(ifFalse, b2, sizeof b2);
          reportWarning(ctx, so, eo, "pointer/integer type mismatch in conditional expression ('%s' and '%s')", b1, b2);
          return makePointedType(ctx, ifTrue->flags, pointedT);
      } else {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(ifTrue, b1, sizeof b1);
          renderTypeRef(ifFalse, b2, sizeof b2);
          reportError(ctx, so, eo, "incompatible operand types ('%s' and '%s')", b1, b2);
          return makeErrorRef(ctx);
      }
  }

  if (isPointerLikeType(ifFalse)) {
      TypeRef *pointedF = ifFalse->kind == TR_POINTED ? ifFalse->pointedTo : ifFalse->arrayTypeDesc.elementType;
      if (isIntegerType(ifTrue)) {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(ifTrue, b1, sizeof b1);
          renderTypeRef(ifFalse, b2, sizeof b2);
          reportWarning(ctx, so, eo, "pointer/integer type mismatch in conditional expression ('%s' and '%s')", b1, b2);
          return makePointedType(ctx, ifFalse->flags, pointedF);
      } else {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(ifTrue, b1, sizeof b1);
          renderTypeRef(ifFalse, b2, sizeof b2);
          reportError(ctx, so, eo, "incompatible operand types ('%s' and '%s')", b1, b2);
          return makeErrorRef(ctx);
      }
  }

  return makeErrorRef(ctx); // Unknown situation
}

static TypeRef *reportInvalidBinaryOperand(ParserContext *ctx, int so, int eo, TypeRef *left, TypeRef *right) {
  char b1[1024] = { 0 };
  char b2[1024] = { 0 };
  renderTypeRef(left, b1, sizeof b1);
  renderTypeRef(right, b2, sizeof b2);
  reportError(ctx, so, eo, "invalid operands to binary expression ('%s' and '%s')", b1, b2);
  return makeErrorRef(ctx); // binary ops are not applicabe to structual types
}

TypeRef *computeBinaryType(ParserContext *ctx, int so, int eo, TypeRef* left, TypeRef *right, ExpressionType op) {
  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  if (isStructualType(left) || isStructualType(right)){
      return reportInvalidBinaryOperand(ctx, so, eo, left, right);  // binary ops are not applicabe to structual types
  }

  if (EB_ANDAND <= op && op <= EB_GE) {
      return makePrimitiveType(ctx, T_S4, 0);
  }

  if (EB_LHS <= op && op <= EB_XOR) {
      if (isIntegerType(left) && isIntegerType(right)) {
          return commonPrimitiveType(ctx, left, right);
      }
      return reportInvalidBinaryOperand(ctx, so, eo, left, right);
  }


  if (EB_MUL <= op && op <= EB_MOD) {
    if (isPrimitiveType(left) && isPrimitiveType(right)) {
        return commonPrimitiveType(ctx, left, right);
    }
    return reportInvalidBinaryOperand(ctx, so, eo, left, right);
  }


  if (op == EB_ADD) {
      if (isPointerLikeType(left) && isPointerLikeType(right)) {
          return reportInvalidBinaryOperand(ctx, so, eo, left, right);
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
      return reportInvalidBinaryOperand(ctx, so, eo, left, right);
  }

  if (isPointerLikeType(right)) {
      if (isIntegerType(left)) {
        TypeRef *pointedR = right->kind == TR_POINTED ? right->pointedTo : right->arrayTypeDesc.elementType;
        return makePointedType(ctx, right->flags, pointedR);
      }
      return reportInvalidBinaryOperand(ctx, so, eo, left, right);
  }

  return commonPrimitiveType(ctx, left, right);
}


TypeRef *computeIncDecType(ParserContext *ctx, int so, int eo, TypeRef *argumentType, ExpressionType op) {
  assert(op == EU_PRE_INC || op == EU_PRE_DEC || op == EU_POST_INC || op == EU_POST_DEC);

  if (isErrorType(argumentType)) return argumentType;

  if (argumentType->kind == TR_FUNCTION || argumentType->kind == TR_ARRAY || isStructualType(argumentType)) {
    const char *opName = op == EU_POST_DEC || op == EU_PRE_DEC ? "decrement" : "increment";
    char buffer[1024] = { 0 };
    renderTypeRef(argumentType, buffer, sizeof buffer);
    reportError(ctx, so, eo, "cannot %s value of type '%s'", opName, buffer);
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

  char buffer[1024] = { 0 };
  renderTypeRef(argumentType, buffer, sizeof buffer);
  reportError(ctx, so, eo, "indirection requires pointer operand ('%s' invalid)", buffer);
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
          char buffer[1024] = { 0 };
          renderTypeRef(argumentType, buffer, sizeof buffer);
          reportError(ctx, so, eo, "invalid argument type '%s' to unary expression", buffer);
          return makeErrorRef(ctx);
      }
    default:
      unreachable("Unexpected Unary operator type");
      return NULL;
    }

  return argumentType;
}

static void reportInvalidAssignTypes(ParserContext *ctx, int so, int eo, TypeRef *left, TypeRef *right) {
  char b1[1024] = { 0 };
  char b2[1024] = { 0 };
  renderTypeRef(left, b1, sizeof b1);
  renderTypeRef(right, b2, sizeof b2);
  reportError(ctx, so, eo, "assigning to '%s' from incompatible type '%s'", b1, b2);
}

static Boolean isAssignableTypes(ParserContext *ctx, int so, int eo, TypeRef *to, TypeRef *from) {
  if (isErrorType(to)) return TRUE;
  if (isErrorType(from)) return TRUE;

  if (to->flags.bits.isConst) {
      char b[1024] = { 0 };
      renderTypeRef(to, b, sizeof b);
      reportError(ctx, so, eo, "cannot assign to lvalue with const-qualified type '%s'", b);
      return FALSE;
  }

  if (to->kind == TR_ARRAY) {
      char b[1024] = { 0 };
      renderTypeRef(to, b, sizeof b);
      reportError(ctx, so, eo, "array type '%s' is not assignable", b);
      return FALSE;
  }

  if (isStructualType(to) || isStructualType(from)) {
      if (typesEquals(to, from)) {
          return TRUE;
      } else {
          reportInvalidAssignTypes(ctx, so, eo, to, from);
          return FALSE;
      }
  }

  if (to->kind == TR_POINTED) {
      TypeRef *pLeft = to->pointedTo;
      if (isPointerLikeType(from)) {
          TypeRef *pointed = from->kind == TR_POINTED ? from->pointedTo : from->arrayTypeDesc.elementType;
          if (typesEquals(pLeft, pointed)) {
              return TRUE;
          } else {
              char b1[1024] = { 0 };
              char b2[1024] = { 0 };
              renderTypeRef(to, b1, sizeof b1);
              renderTypeRef(from, b2, sizeof b2);
              reportWarning(ctx, so, eo, "incompatible pointer types assigning to '%s' from '%s'", b1, b2);
              return TRUE;
          }
      } else if (isIntegerType(from)) {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(to, b1, sizeof b1);
          renderTypeRef(from, b2, sizeof b2);
          reportWarning(ctx, so, eo, "incompatible integer to pointer conversion assigning to '%s' from '%s'", b1, b2);
          return TRUE;
      } else {
          reportInvalidAssignTypes(ctx, so, eo, to, from);
          return FALSE;
      }
  }


  if (isPointerLikeType(from)) {
      if (isIntegerType(to)) {
          char b1[1024] = { 0 };
          char b2[1024] = { 0 };
          renderTypeRef(to, b1, sizeof b1);
          renderTypeRef(from, b2, sizeof b2);
          reportWarning(ctx, so, eo, "incompatible integer to pointer conversion assigning to '%s' from '%s'", b1, b2);
          return TRUE;
      } else {
          reportInvalidAssignTypes(ctx, so, eo, to, from);
          return FALSE;
      }
  }

  return TRUE;
}

TypeRef *computeAssignmentTypes(ParserContext *ctx, int so, int eo, TypeRef *left, TypeRef *right) {
  if (isErrorType(left)) return left;
  if (isErrorType(right)) return right;

  if (isAssignableTypes(ctx, so, eo, left, right)) {
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

int foo(int *a, int *b) {}



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
      if (isAssignableTypes(ctx, so, eo, pType, aType)) {
          argument = argument->next;
          param = param->next;
      } else {
          break;
      }
  }

  if (argument == NULL && param != NULL) { // fewer
      reportError(ctx, so, eo, "too few arguments to function call");
  }

  if (argument != NULL && param == NULL) {
      if (!functionType->functionTypeDesc.isVariadic) {
          reportError(ctx, so, eo, "too many arguments to function call");
      }
  }
}

static int stringHashCode(const void *v) {
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

static int stringCmp(const void *v1, const void *v2) {
    const char *s1 = (const char *)v1;
    const char *s2 = (const char *)v2;

    return strcmp(s1, s2);
}

Scope *newScope(ParserContext *ctx, Scope *parent) {
  Scope *result = (Scope *)areanAllocate(ctx->memory.typeArena, sizeof (Scope));
  result->parent = parent;
  result->symbols = createHashMap(DEFAULT_MAP_CAPACITY, stringHashCode, stringCmp);
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
          parseError(ctx, "redefinition of '%s' as different kind of symbol", name);
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
     parseWarning(ctx, "redefinition of typedef '%s' is a C11 feature", s->name);
  } else {
    char t1[128] = { 0 }, t2[128] = { 0 };

    renderTypeRef(oldType, t1, sizeof t1);
    renderTypeRef(newType, t2, sizeof t2);
    parseError(ctx, "typedef redefinition with different types ('%s' vs '%s')", t1, t2);
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
    parseError(ctx, "conflicting types for '%s'", oldDeclaration->name);
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
    char t1[128] = { 0 }, t2[128] = { 0 };

    renderTypeRef(oldType, t1, sizeof t1);
    renderTypeRef(newType, t2, sizeof t2);
    parseError(ctx, "redefinition of '%s' with a different type: '%s' vs '%s'", s->name, t2, t1);
  }
}

static int newValueProcessor(ParserContext *ctx, Symbol *s, void *value) {
  assert(s->kind == ValueSymbol);
  s->variableDesc = (AstValueDeclaration *)value;
}

Symbol *declareValueSymbol(ParserContext *ctx, const char *name, AstValueDeclaration *declaration) {
  return declareGenericSymbol(ctx, ValueSymbol, name, declaration, existedValueProcessor, newValueProcessor);
}

Symbol *declareSUESymbol(ParserContext *ctx, SymbolKind symbolKind, TypeId typeId, const char *symbolName, AstSUEDeclaration *declaration, Symbol **ss) {
  Symbol *s = findSymbolInScope(ctx->currentScope, symbolName);
  Symbol *old = s;
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
          parseError(ctx, "use of '%s' with tag type that does not match previous declaration", name);
          // also point to already defined one
          // TODO: recovery
      } else {
          typeDescriptor = s->typeDescriptor;
          AstSUEDeclaration *existedDeclaration = typeDescriptor->structInfo;
          if (declaration->members) {
            if (existedDeclaration->members) {
                parseError(ctx, "redefinition of '%s'", name);
                // also point to already defined one
                // TODO recovery
            } else {
                typeDescriptor->structInfo = declaration;
            }
          }
      }
  }
  *ss = s;
  return old;
}

Symbol *declareEnumConstantSymbol(ParserContext *ctx, EnumConstant *enumerator) {
  Symbol *s = findSymbolInScope(ctx->currentScope, enumerator->name);
  if (s) {
      const char *suffix = s->kind == EnumConstSymbol ? "of enumerator " : "";
      parseError(ctx, "redefinition %s'%s'", suffix, enumerator->name);
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
    { T_F8, "double", 8, NULL }
};


int computeTypeSize(ParserContext *ctx, TypeRef *type) {
  if (type->kind == TR_VALUE) {
      return type->descriptorDesc->size;
  }

  if (type->kind == TR_POINTED) {
      return POINTER_TYPE_SIZE;
  }

  if (type->kind == TR_ARRAY) {
      ArrayTypeDescriptor *atype = &type->arrayTypeDesc;
      return atype->size * computeTypeSize(ctx, atype->elementType);
  }

  return POINTER_TYPE_SIZE;
}

static void reportIncompleteType(ParserContext *ctx, AstStructDeclarator *declarator) {
  int so = declarator->coordinates.startOffset;
  int eo = declarator->coordinates.endOffset;
  TypeRef *type = declarator->typeRef;
  char buffer[1024];
  renderTypeRef(type, buffer, sizeof buffer);
  reportError(ctx, so, eo, "field '%s' has incomplete type '%s'", declarator->name, buffer);
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
            int tmp = computeTypeSize(ctx, declarator->typeRef);
            if (tmp < 0) {
                reportIncompleteType(ctx, declarator);
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
            int tmp = computeTypeSize(ctx, declarator->typeRef);
            if (tmp < 0) {
                reportIncompleteType(ctx, declarator);
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

    parseError(ctx, "Expected function declarator here");
    return NULL; // return error type
}

void verifyFunctionReturnType(ParserContext *ctx, Declarator *declarator, TypeRef *returnType) {
  TypeRefKind returnRefKind = returnType->kind;
  int so = declarator->coordinates.startOffset;
  int eo = declarator->coordinates.endOffset;

  if (returnRefKind == TR_FUNCTION || returnRefKind == TR_ARRAY) {
      char buffer[1024] = { 0 };
      const char *type = returnRefKind == TR_FUNCTION ? "function" : "array";
      renderTypeRef(returnType, buffer, sizeof buffer);
      reportError(ctx, so, eo, "function cannot return %s type '%s'", type, buffer);
  }
}

static void verifyFunctionType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_FUNCTION);

  verifyFunctionReturnType(ctx, declarator, type->functionTypeDesc.returnType);
}

static void verifyArrayType(ParserContext *ctx, Declarator *declarator, TypeRef *type) {
  assert(type->kind == TR_ARRAY);
  int so = declarator->coordinates.startOffset;
  int eo = declarator->coordinates.endOffset;
  TypeRef *elementType = type->arrayTypeDesc.elementType;

  if (elementType->kind == TR_FUNCTION) {
      char buffer[1024] = { 0 };
      renderTypeRef(type, buffer, sizeof buffer);
      reportError(ctx, so, eo, "Array of functions is illegal type '%s'", buffer);
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
