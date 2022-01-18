
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "lex.h"
#include "tree.h"

enum SymbolKind {
    FunctionSymbol = 1,
    StructOrUnionSymbol,
    TypedefSymbol,
    VariableSymbol,
    EnumSymbol, /** TODO: not sure about it */
};

struct VariableDesc;

typedef struct _Symbol {
    int kind;
    const char* name; /** struct/union is referenced via "struct $name" or "union $name" */
    union {
        struct {
            FunctionTypeDescriptor *typeDescriptor;
            Declaration *definition;
        } functionSymbol;
        TypeDesc *typeDescriptor; /** struct S; */
        TypeRef * typeref; /** typedef struct TS* ts_t; */
        struct VariableDesc* variableDesc; /** int a = 10; */
    };
} Symbol;

typedef struct _Scope {
    struct _Scope* parent;
    HashMap* symbols;
} Scope;

#define TokenTextCacheSize 1024

typedef struct _Token {

    int code;
    char *text;
    int pos;
} Token;

typedef struct _ParserContext {
    AstFile* parsedFile;

    yyscan_t scanner;
    int token;

    Scope* rootScope;
    Scope* currentScope;

    int anonSymbolsCounter;
} ParserContext;

#endif // __PARSER_H__
