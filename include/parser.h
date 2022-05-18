
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
  const char *canonDumpFileName;



  unsigned errWarns: 1;
  unsigned logTokens: 1;
  unsigned verbose: 1;
  unsigned memoryStatistics: 1;
} Configuration;


void compileFile(Configuration * config);
void cannonizeAstFile(ParserContext *ctx, AstFile *file);
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

typedef struct _DefinedLabel {
  AstLabelStatement *label;
  struct _DefinedLabel *next;
} DefinedLabel;

enum LabelUseKind {
  LU_GOTO_USE,
  LU_REF_USE
};

typedef struct _UsedLabel {
  const char *label;
  enum LabelUseKind kind;
  union {
    AstExpression *labelRef;
    AstStatement *gotoStatement;
  };
  struct _UsedLabel *next;
} UsedLabel;

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
      Arena *codegenArena;
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
      unsigned hasDefault: 1;
      unsigned caseCount;
    } stateFlags;

    struct {
      DefinedLabel *definedLabels;
      UsedLabel *usedLabels;
    } labels;

    TypeRef *functionReturnType;

} ParserContext;

#endif // __PARSER_H__
