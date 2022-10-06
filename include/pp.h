
#ifndef __PP_H__
#define __PP_H__ 1

#include "common.h"


struct _ParserContext;
struct _Token;
struct _LexerState;

typedef struct _MacroParam {
  const char *name;
  struct _MacroParam *next;

  unsigned isVararg : 1;
} MacroParam;

typedef struct _Token *(macroHandler)(struct _ParserContext *, struct _Token *);

typedef struct _MacroDefinition {
  const char *name;
  MacroParam *params;
  struct _Token *body;

  macroHandler *handler;

  unsigned isVararg : 1;
  unsigned isFunctional : 1;
  unsigned isEnabled : 1;
  unsigned isParamsUsed : 1;

} MacroDefinition;

void initializeProprocessor(struct _ParserContext *ctx);
struct _Token *preprocessFile(struct _ParserContext *ctx, struct _Token *s, struct _Token *tail);
struct _Token *originalToken(struct _Token *t);

MacroDefinition *findMacro(struct _ParserContext *ctx, const char *name);

struct _LexerState *allocateMacroLexerState(MacroDefinition *def, struct _Token *trigger);
struct _LexerState *popLexerState(struct _ParserContext *ctx);

struct _LexerState *loadFile(const char *fileName, struct _LexerState *prev);

Boolean isNextToken(struct _ParserContext *ctx, int code);
void handleDirective(struct _ParserContext *ctx, struct _Token *directive);
struct _Token *handleIdentifier(struct _ParserContext *ctx, struct _Token *token);
Boolean lexTokenRaw(struct _ParserContext *ctx, struct _Token *new);
struct _Token *lexToken(struct _ParserContext *ctx);
struct _Token *lexTokenNoSubstitute(struct _ParserContext *ctx);
struct _Token *lexNonExpand(struct _ParserContext *ctx, Boolean shadowNL);
void skipUntilEoL(struct _ParserContext *ctx);

#endif // __PP_H__
