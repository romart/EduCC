

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
    Symbol *s = (Symbol *)areanAllocate(ctx->astArena, symbolSize);
    s->kind = kind;
    s->name = name;

    Scope *scope = ctx->currentScope;
    putToHashMap(scope->symbols, name, s);

    return s;
}


Symbol *declareTypeDef(ParserContext *ctx, const char *name, TypeRef *type) {
  Symbol *s = findSymbol(ctx, name);
  if (s) {
      if (s->kind == TypedefSymbol) {
          if (typesEquals(type, s->typeref)) {
              // parseWarning(ctx, "redefinition")
          } else {
            char t1[128] = { 0 }, t2[128] = { 0 };

            renderTypeRef(s->typeref, t1, sizeof t1);
            renderTypeRef(type, t2, sizeof t2);
            parseError(ctx, "typedef redefinition with different types ('%s' vs '%s')", t1, t2);
          }
      }
  } else {
    s = declareSymbol(ctx, TypedefSymbol, name);
    s->typeref = type;
  }

  return s;

}

Symbol* findOrDeclareSymbol(ParserContext* ctx, int kind, const char* name) {
    Symbol *existed = findSymbol(ctx, name);
    if (existed) return existed;
    return declareSymbol(ctx, kind, name);
}
