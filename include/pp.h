
#ifndef __PP_H__
#define __PP_H__ 1

#include "common.h"


struct _ParserContext;
struct _Token;

struct _Token *preprocess(struct _ParserContext *ctx, struct _Token *token);
struct _Token *replaceMacro(struct _ParserContext *ctx, const struct _Token *macro, struct _Token **macroNextPtr);

//struct _Token *macroSubstitute(struct _ParserContext *ctx, struct _Token *token);

#endif // __PP_H__
