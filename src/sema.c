
#include <assert.h>

#include "sema.h"
#include "treeDump.h"
#include "parser.h"


size_t computeTypeSize(TypeRef *type) {
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


TypeCastabilityKind typeCastability(TypeRef *to, TypeRef *from) {
  return TCK_UNKNOWN;
}


Boolean typesEquals(TypeRef *t1, TypeRef *t2) {
  TypeEqualityKind equality = typeEquality(t1, t2);
  return equality == TEK_EQUAL ? TRUE : FALSE;
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
    parseError(ctx, "typedef redefinition with different types ('%s' vs '%s')", t1, t2);
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
      // TODO: compute size
      typeDescriptor = s->typeDescriptor = createTypeDescriptor(ctx, typeId, name, -1);
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

static TypeDesc errorTypeImpl = { T_ERROR, "<error>", -1, NULL };

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
