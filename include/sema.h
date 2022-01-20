
#ifndef __SEMA_H__
#define __SEMA_H__ 1

#include "common.h"
#include "tree.h"
#include "parser.h"
#include "types.h"

enum {
  POINTER_TYPE_SIZE = 8,
};

size_t computeTypeSize(TypeRef *type);

int typesEquals(TypeRef *t1, TypeRef *t2);


int isTypeName(ParserContext *ctx, const char* name, struct _Scope* scope);
Symbol* findSymbol(ParserContext *ctx, const char *name);
Symbol* declareSymbol(ParserContext *ctx, int kind, const char *name);
Symbol *declareTypeDef(ParserContext *ctx, const char *name, TypeRef *type);
Symbol* findOrDeclareSymbol(ParserContext* ctx, int kind, const char* name);




#endif // __SEMA_H__
