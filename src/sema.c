
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

int typesEquals(TypeRef *t1, TypeRef *t2) {
  return TRUE;
}

Symbol* findSymbol(ParserContext *ctx, const char *name) {
    Scope* s = ctx->currentScope;
    while (s != NULL) {
        Symbol *sb = (Symbol *)getFromHashMap(s->symbols, name);
        if (sb != NULL) return sb;
        s = s->parent;
    }

    return NULL;
}


int isTypeName(ParserContext *ctx, const char* name, struct _Scope* scope) {
    Symbol *s = findSymbol(ctx, name);
    return s && s->kind == TypedefSymbol;
}


Symbol* declareSymbol(ParserContext *ctx, int kind, const char *name) {
    int symbolSize = sizeof(Symbol);
    Symbol *s = (Symbol *)areanAllocate(ctx->typeArena, symbolSize);
    s->kind = kind;
    s->name = name;

    Scope *scope = ctx->currentScope;
    putToHashMap(scope->symbols, name, s);

    return s;
}

Symbol* findOrDeclareSymbol(ParserContext* ctx, int kind, const char* name) {
    Symbol *existed = findSymbol(ctx, name);
    if (existed) return existed;
    return declareSymbol(ctx, kind, name);
}

static int functionsEqual(AstFunctionDeclaration *f1, AstFunctionDeclaration *f2) {
  return TRUE;
}


typedef int (*symbolProcessor)(ParserContext *, Symbol *, void *);

static Symbol *declareGenericSymbol(ParserContext *ctx, int kind, const char *name, void *value, symbolProcessor existed, symbolProcessor new) {

  Symbol *s = findSymbol(ctx, name);
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
      // parseWarning(ctx, "redefinition")
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

Symbol *declareSUESymbol(ParserContext *ctx, int symbolKind, int typeId, const char *symbolName, AstSUEDeclaration *declaration, Symbol **ss) {
  Symbol *s = findSymbol(ctx, symbolName);
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
