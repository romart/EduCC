
#ifndef __PP_H__
#define __PP_H__ 1

#include "common.h"


struct _ParserContext;
struct _Token;

void initializeProprocessor(struct _ParserContext *ctx);
struct _Token *preprocessFile(struct _ParserContext *ctx, struct _Token *s, struct _Token *tail);
struct _Token *originaToken(struct _Token *t);

#endif // __PP_H__
