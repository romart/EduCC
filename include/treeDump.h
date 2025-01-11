
#ifndef __TREE_DUMP_H__
#define __TREE_DUMP_H__ 1

#include <stdio.h>

#include "tree.h"

// dump

int dumpAstFile(FILE *output, AstFile *file, TypeDefiniton *typeDefinitions);
int dumpAstExpression(FILE *output, AstExpression *expr);
int dumpAstStatement(FILE *output, AstStatement *stmt);
int dumpAstDeclaration(FILE *output, AstDeclaration *declaration);

int dumpTypeRef(FILE *output, const TypeRef *type);
int dumpTypeDesc(FILE *output, const TypeDesc *desc);

int renderTypeDesc(const TypeDesc *desc, char *buffer, int bufferSize);
int renderTypeRef(const TypeRef *type, char *buffer, int bufferSize);

void dumpLocation(FILE *output, AstExpression *expr);

#endif // __TREE_DUMP_H__
