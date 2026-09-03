
#ifndef __PARSER_H__
#define __PARSER_H__ 1

#include "tokens.h"
#include "tree.h"
#include "mem.h"
#include "diagnostics.h"
#include "pp.h"

typedef struct _IncludePath {
  const char *path;
  struct _IncludePath *next;
} IncludePath;

typedef struct _StringList {
  const char *s;
  struct _StringList *next;
} StringList;

enum Arch {
    X86_64,
    RISCV64
};

// Which stage 2 runs, selected by '-Xregalloc='. See include/ir/regalloc.h for
// what the three of them are and why all three are kept.
enum RegAllocKind {
  RA_LINEAR = 0, // stage 2B, linear scan
  RA_TRIVIAL,    // stage 2A, spill everything
  RA_COLOUR      // stage 2C, Chaitin-Briggs graph colouring, the default
};

// Bitmask values for Configuration.irDumpPhases, selected via
// '-irDump:phase[,phase...] <file>'. Each bit requests a snapshot of the IR
// taken immediately after the named pass in translateFunction() (ast2ir.c),
// so e.g. an SSA test can dump right after buildSSA without being coupled to
// whatever later passes (scp/gvn/dce) currently do. A mask of 0 (plain
// '-irDump <file>', no ':' suffix) keeps the legacy behavior of dumping only
// the fully-processed IR once translation of the whole file completes.
enum IrDumpPhase {
    IR_DUMP_PHASE_INITIAL = 1u << 0,
    IR_DUMP_PHASE_SSA     = 1u << 1,
    IR_DUMP_PHASE_SCP     = 1u << 2,
    IR_DUMP_PHASE_GVN     = 1u << 3,
    IR_DUMP_PHASE_DCE     = 1u << 4,
    // Not IR phases but machine-IR ones, dumped in their own format (see
    // dumpMachineFunction): 'mir' is the MachineFunction as stage 0 leaves it -
    // the CFG, the frame and the phi copies, with no instructions selected yet -
    // 'isel' is the same function after stage 1 has filled the blocks in, and
    // 'ra' is it once stage 2 has replaced every virtual register with a real
    // one and given the ones it could not keep a frame slot.
    IR_DUMP_PHASE_MIR     = 1u << 5,
    IR_DUMP_PHASE_ISEL    = 1u << 6,
    IR_DUMP_PHASE_RA      = 1u << 7,
};

typedef struct _Configuration {

  const char *fileToCompile;
  const char *dumpFileName;
  const char *canonDumpFileName;
  const char *irDumpFileName;
  const char *outputFile;

  IncludePath *includePath;
  StringList *macroses;

  enum Arch arch;

  unsigned irDumpPhases; // bitmask of enum IrDumpPhase values; 0 == legacy final-only dump

  unsigned errWarns: 1;
  unsigned logTokens: 1;
  unsigned verbose: 1;
  unsigned memoryStatistics: 1;
  unsigned skipCodegen: 1;
  unsigned ppOutput: 1;
  unsigned asmDump: 1;

  unsigned objOutput : 1;

  // The IR backend, which is the default; '-legacy' turns it off, and so does
  // any '-march' without a selector.
  unsigned irBackend : 1;

  // '-Xregalloc=' - which of the three allocators stage 2 runs. The linear
  // scan is the default; the other two are kept reachable because two
  // allocators over one corpus have to agree about observable behaviour, which
  // is what makes each of them the others' oracle - see include/ir/regalloc.h.
  enum RegAllocKind regAlloc;

  // A strict '-std=c...' rather than the gnu99 default. All it decides today is
  // whether __STRICT_ANSI__ is defined, which is not cosmetic: glibc's
  // features.h reads it as "hide everything that is not ISO C", so with it on,
  // <unistd.h> declares no readlink and <string.h> no strdup.
  unsigned strictAnsi : 1;

  unsigned hadError : 1;
} Configuration;


struct _Hideset;


typedef struct _Token {
    struct _LocationInfo *locInfo;

    int code;    // gets promoted later in parser if applicable into language key word like 'for', 'int', 'if', etc
    int rawCode; // actual lexed token code

    const char *pos; // startOffset = pos - coords->buffer
    size_t length;   // endOffset = startOffset + length

    const char *id;  // needs for strcmp checks

    union {
      uint64_t iv; // holds integer const
      long double ldv; // holds long double const (yes it's different from double)
      struct {
        const char *v; // holds _cleared_ string literal
        size_t l;
      } text;
    } value;

    unsigned hasLeadingSpace: 1;
    unsigned startOfLine: 1;
    unsigned macroStringitize: 1;
    unsigned isMacroParam : 1;
    unsigned disabledExpansion : 1;

    struct _Token *expanded;
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

enum LexState {
  LS_MACRO,
  LS_FILE
};

typedef struct _MacroState {
  MacroDefinition *definition;
  Token *trigger; // token which is being expanded
  Token *bodyStart;  // macro body with replaced params
  Token *bodyPtr;
} MacroState;

enum PPConditionState {
  IN_IF, IN_ELIF, IN_ELSE
};

typedef struct _PPConditionFrame {
  struct _PPConditionFrame *prev;
  Token *ifDirective;

  enum PPConditionState state;
  unsigned isTaken : 1;
  unsigned isTaking : 1;
  unsigned seenElse : 1;

} PPConditionFrame;

typedef struct _LexerState {
  enum LexState state;

  struct _LexerState *prev; // include/macro stack
  struct _LexerState *virtPrev; // to find current/base files

  union {
    struct {
      LocationInfo *locInfo;

      unsigned visibleLine;
      unsigned pos;

      PPConditionFrame *conditionStack;

      unsigned atLineStart;
    } fileContext;

    MacroState macroContext;
  };

} LexerState;

typedef struct _ParserContext {
    Configuration *config;

    AstFile* parsedFile;

    struct _Scope* rootScope;
    struct _Scope* currentScope;

    struct _Scope* scopeList; // used to release scope memory

    LexerState *lexerState;

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
      unsigned inPPExpression : 1;
      unsigned afterPPDefined : 1;
      unsigned afterPPParen : 1;
      unsigned inStaticScope : 1;
      unsigned silentMode : 1; // in this mode diagnostics are not being issued
      unsigned caseCount;
      unsigned returnStructBuffer;
      int lastLexCode;
    } stateFlags;

    struct {
      DefinedLabel *definedLabels;
      UsedLabel *usedLabels;
    } labels;

    AstFunctionDeclaration *parsingFunction;
    AstValueDeclaration *locals;
    TypeDefiniton *typeDefinitions;

    HashMap *macroMap;
    HashMap *pragmaOnceMap;

} ParserContext;


enum ParsedLoc {
  PL_OPEN,
  PL_INNER,
  PL_DESIGNATOR,
  PL_ASSIGN,
  PL_CLOSE,
  PL_SEPARATOR
};

typedef struct _ParsedInitializer {
  Coordinates coords;
  AstExpression *expression;
  int32_t level;
  enum ParsedLoc loc;
  DesignationKind kind;
  union {
    const char *identifier; // .foo
    int32_t index;  // [42]
  } designator;
  struct _ParsedInitializer *next;
} ParsedInitializer;

Token *nextToken(ParserContext *ctx);

Token *tokenizeBuffer(ParserContext *ctx);

LocationInfo *allocateFileLocationInfo(const char *fileName, const char *buffer, size_t buffeSize, unsigned lineCount);
LocationInfo *allocateMacroLocationInfo(const char *buffer, size_t buffeSize, Boolean isConst);

LexerState *allocateFileLexerState(LocationInfo *locInfo);

const char *joinToStringTokenSequence(ParserContext *ctx, Token *s);

char *allocateString(ParserContext *ctx, size_t size);
Token *allocToken(ParserContext *ctx);


void compileFile(Configuration * config);
void cannonizeAstFile(ParserContext *ctx, AstFile *file);
AstConst* eval(ParserContext *ctx, AstExpression* expression);
AstExpression* parseConditionalExpression(ParserContext *ctx);

#endif // __PARSER_H__
