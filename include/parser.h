
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "mem.h"
#include "diagnostics.h"

typedef struct _Configuration {

  const char *fileToCompile;
  const char *dumpFileName;



  unsigned errWarns: 1;
  unsigned logTokens: 1;
  unsigned verbose: 1;
  unsigned memoryStatistics: 1;
} Configuration;


void compileFile(Configuration * config);
AstConst* eval(ParserContext *ctx, AstExpression* expression);

typedef struct _Token {
    Coordinates coordinates;
    int code;
    int rawCode;
    const char *text;
    union {
      int64_t iv;
      double dv;
    } value;
    struct _Token *next;
} Token;

struct _Scope;

typedef struct _ParserContext {
    Configuration *config;

    AstFile* parsedFile;

    yyscan_t scanner;

    struct _Scope* rootScope;
    struct _Scope* currentScope;

    struct _Scope* scopeList; // used to release scope memory

    Token *firstToken;
    Token *token;

    struct {
      Arena *tokenArena;
      Arena *astArena;
      Arena *typeArena;
      Arena *stringArena;
      Arena *diagnosticsArena;
    } memory;

    int anonSymbolsCounter;

    Diagnostics diagnostics;

    struct {
      YYLTYPE position;
      unsigned lineno;
      unsigned lineCount;
      unsigned *linesPos;
    } locationInfo;

    struct {
      unsigned inLoop: 1;
      unsigned inSwitch: 1;
      unsigned caseCount;
      HashMap *labelSet;
    } stateFlags;

    TypeRef *functionReturnType;

} ParserContext;

#endif // __PARSER_H__
