
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
  const char *objDirName;

  unsigned errWarns: 1;
  unsigned logTokens: 1;
  unsigned verbose: 1;
  unsigned memoryStatistics: 1;
  unsigned skipCodegen: 1;
  unsigned ppOutput: 1;
} Configuration;


typedef struct _Token {
    Coordinates coordinates;
    int code;
    int rawCode;
    const char *text;
    const char *pos;
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

enum LocationInfoKind {
  LIK_FILE,
  LIK_MACRO
};

typedef struct _LocationInfo {
  const char *fileName;
  const char *buffer;
  size_t bufferSize;
  enum LocationInfoKind kind;

  union {
    struct {
      unsigned *linesPos;
      unsigned lineno;
      unsigned lineCount;
    } fileInfo;
    struct {
      int startOffset;
      int endOffset;
    } macroInfo;
  };

  struct _LocationInfo *next;
} LocationInfo;

typedef struct _ParserContext {
    Configuration *config;

    AstFile* parsedFile;

    struct _Scope* rootScope;
    struct _Scope* currentScope;

    struct _Scope* scopeList; // used to release scope memory

    Token *firstToken;
    Token *token;

    struct {
      Arena *tokenArena;
      Arena *macroArena;
      Arena *astArena;
      Arena *typeArena;
      Arena *stringArena;
      Arena *diagnosticsArena;
      Arena *codegenArena;
    } memory;

    int anonSymbolsCounter;

    Diagnostics diagnostics;

    LocationInfo *locationInfo;

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

    HashMap *macroMap;

} ParserContext;


Token *nextToken(ParserContext *ctx);

Token *tokenizeFile(ParserContext *ctx, const char *fileName, Token *tail);
Token *tokenizeBuffer(ParserContext *ctx, LocationInfo *locInfo, Token *tail);

LocationInfo *allocateFileLocationInfo(const char *fileName, const char *buffer, size_t buffeSize, unsigned lineCount);
LocationInfo *allocateMacroLocationInfo(const char *fileName, const char *buffer, size_t buffeSize, int startOffset, int endOffset);

char *allocateString(ParserContext *ctx, size_t size);
Token *allocToken(ParserContext *ctx);


void compileFile(Configuration * config);
void cannonizeAstFile(ParserContext *ctx, AstFile *file);
AstConst* eval(ParserContext *ctx, AstExpression* expression);
AstExpression* parseConditionalExpression(ParserContext *ctx, struct _Scope* scope);

#endif // __PARSER_H__
