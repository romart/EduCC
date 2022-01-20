
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "mem.h"

AstConst* eval(ParserContext *ctx, AstExpression* expression);

void parseError(ParserContext *ctx, const char* fmt, ...);
void parseWarning(ParserContext *ctx, const char* fmt, ...);

enum SymbolKind {
    FunctionSymbol = 1,
    UnionSymbol,
    StructSymbol,
    TypedefSymbol,
    VariableSymbol,
    EnumSymbol, /** TODO: not sure about it */
    EnumConstSymbol
};

struct VariableDesc;

typedef struct _Symbol {
    int kind;
    const char* name; /** struct/union/enum is referenced via "$$name" or "|$name" or "#$enum"*/
    union {
        struct {
            FunctionTypeDescriptor *typeDescriptor;
            AstDeclaration *definition;
        } functionSymbol; // FunctionSymbol
        TypeDesc *typeDescriptor; // StructSymbol | UnionSymbol | EnumSymbol, struct S;
        TypeRef * typeref; // TypedefSymbol, typedef struct TS* ts_t;
        struct VariableDesc* variableDesc; // VariableSymbol, int a = 10;
        struct _EnumConstant *enumerator; // EnumConstSymbol
    };
} Symbol;

typedef struct _Scope {
    struct _Scope* parent;
    HashMap* symbols;
} Scope;

#define TokenTextCacheSize 1024

typedef struct _Token {
    Coordinates coordinates;
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

    struct {
      YYLTYPE position;
      unsigned lineno;
      unsigned lineCount;
      unsigned *linesPos;
    } locationInfo;
} ParserContext;

#endif // __PARSER_H__
