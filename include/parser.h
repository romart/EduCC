
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "lex.h"
#include "tree.h"
#include "mem.h"

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
    int rawCode;
    const char *text;
    union {
      long long iv;
      double dv;
    } value;
    struct _Token *next;
} Token;

typedef struct _ParserContext {
    AstFile* parsedFile;

    yyscan_t scanner;

    Scope* rootScope;
    Scope* currentScope;

    Token *firstToken;
    Token *token;

    Arena *tokenArena;
    Arena *astArena;
    Arena *typeArena;

    int anonSymbolsCounter;
} ParserContext;

#endif // __PARSER_H__
