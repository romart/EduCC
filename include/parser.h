
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "tokens.h"
#include "tree.h"
#include "mem.h"
#include "diagnostics.h"

typedef struct _IncludePath {
  const char *path;
  struct _IncludePath *next;
} IncludePath;

typedef struct _StringList {
  const char *s;
  struct _StringList *next;
} StringList;

typedef struct _Configuration {

  const char *fileToCompile;
  const char *dumpFileName;
  const char *canonDumpFileName;
  const char *outputFile;

  IncludePath *includePath;
  StringList *macroses;

  unsigned errWarns: 1;
  unsigned logTokens: 1;
  unsigned verbose: 1;
  unsigned memoryStatistics: 1;
  unsigned skipCodegen: 1;
  unsigned ppOutput: 1;
  unsigned asmDump: 1;
} Configuration;


struct _Hideset;


typedef struct _Token {
    struct _LocationInfo *locInfo;

    int code;
    int rawCode;

    const char *pos; // startOffset = pos - coords->buffer
    size_t length;   // endOffset = startOffset + length

    const char *id;  // needs for strcmp checks

    union {
      uint64_t iv; // holds integer const
      long double ldv; // holds long double const (yes it's different from double)
      const char *text; // holds _cleared_ string literal
    } value;

    unsigned hasLeadingSpace: 1;
    unsigned startOfLine: 1;
    unsigned macroStringitize: 1;

    struct _Token *expanded;
    struct _Hideset *hs;  // used in macro expansion
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
  LIK_MACRO,
  LIK_CONST_MACRO
};

typedef struct _LineChunk {
  const char *overrideFileName;
  unsigned overrideLineNumber;
  unsigned posLineNumber;
  struct _LineChunk *next;
} LineChunk;

typedef struct _LocationInfo {
  const char *buffer;
  size_t bufferSize;
  enum LocationInfoKind kind;

  struct {
    const char *fileName;
    unsigned *linesPos;
    unsigned lineno;
    unsigned lineCount;
    struct _LineChunk *chunks; // required for #line PP directive support
  } fileInfo;

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
      unsigned hasSmallStructs: 1;
      unsigned inPP : 1;
      unsigned caseCount;
      unsigned returnStructBuffer;
    } stateFlags;

    struct {
      DefinedLabel *definedLabels;
      UsedLabel *usedLabels;
    } labels;

    TypeRef *functionReturnType;
    AstValueDeclaration *locals;
    TypeDefiniton *typeDefinitions;

    HashMap *macroMap;
    HashMap *pragmaOnceMap;

} ParserContext;


enum ParsedLoc {
  PL_OPEN,
  PL_INNER,
  PL_CLOSE,
  PL_SEPARATOR
};

typedef struct _ParsedInitializer {
  Coordinates coords;
  AstExpression *expression;
  int32_t level;
  enum ParsedLoc loc;
  struct _ParsedInitializer *next;
} ParsedInitializer;

Token *nextToken(ParserContext *ctx);

Token *tokenizeFile(ParserContext *ctx, const char *fileName, Token *tail);
Token *tokenizeFileAndPP(ParserContext *ctx, const char *fileName, Token *tail);
Token *tokenizeBuffer(ParserContext *ctx, LocationInfo *locInfo, unsigned *linePos, Token *tail);

LocationInfo *allocateFileLocationInfo(const char *fileName, const char *buffer, size_t buffeSize, unsigned lineCount);
LocationInfo *allocateMacroLocationInfo(const char *buffer, size_t buffeSize, Boolean isConst);

const char *joinToStringTokenSequence(ParserContext *ctx, Token *s);

char *allocateString(ParserContext *ctx, size_t size);
Token *allocToken(ParserContext *ctx);


void compileFile(Configuration * config);
void cannonizeAstFile(ParserContext *ctx, AstFile *file);
AstConst* eval(ParserContext *ctx, AstExpression* expression);
AstExpression* parseConditionalExpression(ParserContext *ctx, struct _Scope* scope);

#endif // __PARSER_H__
