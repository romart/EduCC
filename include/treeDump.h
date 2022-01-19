
#ifndef __TREE_DUMP_H__
#define __TREE_DUMP_H__ 1

#include <stdio.h>

#include "tree.h"

// dump

int dumpAstFile(FILE *output, AstFile *file);
int dumpAstExpression(FILE *output, AstExpression *expr);
int dumpAstStatement(FILE *output, AstStatement *stmt);
int dumpAstDeclaration(FILE *output, AstDeclaration *declaration);

int dumpTypeRef(FILE *output, TypeRef *type);
int dumpTypeDesc(FILE *output, TypeDesc *desc);

#endif // __TREE_DUMP_H__
