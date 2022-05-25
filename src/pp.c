

#include <assert.h>
#include <stdio.h>

#include "pp.h"
#include "parser.h"
#include "lex.h"
#include "tree.h"


typedef struct _MacroParam {
  const char *name;
  struct _MacroParam *next;
} MacroParam;

typedef struct _MacroDefinition {
  const char *name;
  MacroParam *params;
  Token *body;
  Boolean isVararg;
  Boolean isFunctional;
} MacroDefinition;

static MacroDefinition *allocateMacroDef(ParserContext *ctx, const char *name, MacroParam *params, Token *body, Boolean isVararg, Boolean isFunc) {
  MacroDefinition *d = areanAllocate(ctx->memory.macroArena, sizeof(MacroDefinition));

  d->name = name;
  d->params = params;
  d->body = body;
  d->isVararg = isVararg;
  d->isFunctional = isFunc;

  return d;
}

typedef struct _MacroArg {
  MacroParam *param;
  Token *value;
  struct _MacroArg *next;
} MacroArg;

static MacroParam *allocateMacroParam(ParserContext *ctx, const char *name) {
  MacroParam *p = areanAllocate(ctx->memory.macroArena, sizeof(MacroParam));
  p->name = name;
  return p;
}

static MacroArg *allocateMacroArg(ParserContext *ctx, MacroParam *p, Token *v) {
  MacroArg *a = areanAllocate(ctx->memory.macroArena, sizeof(MacroArg));
  a->param = p;
  a->value = v;
  return a;
}

static Token *skipPPTokens(ParserContext *ctx, Token *token) {
  while (token->code) {
      if (token->rawCode == NEWLINE){
          return token;
      } else {
          token = token->next;
      }
  }

  return token;
}


static void joinToString(char *buffer, size_t size, Token *b, Token *e) {
  unsigned ptr = 0;
  --size;
  while (b != e && size) {
      if (b->rawCode < LAST_SIMPLE_TOKEN) {
          buffer[ptr++] = (char)b->rawCode;
          --size;
      } else if (b->rawCode != NEWLINE) {
          unsigned t = snprintf(buffer, size, "%s", b->text);
          ptr += t;
          size -= t;
      } else {
          break;
      }
  }

  buffer[ptr] = '\0';
}

static Token *findLastPPToken(ParserContext *ctx, Token *token) {
  Token *cur = token, *last = NULL;

  while (cur->rawCode) {
      if (cur->rawCode == DANGLING_NEWLINE) {
          cur = cur->next;
          if (last) last->next = cur;
      } else if (cur->rawCode == NEWLINE) {
          break;
      } else {
          last  = cur;
          cur = cur->next;
      }
  }

  return last;
}

static Boolean isMacro(ParserContext *ctx, const char *id) {
  return isInHashMap(ctx->macroMap, (intptr_t)id);
}

Boolean isSpacesBetween(const Token *l, const Token *r) {
  return r->coordinates.startOffset - l->coordinates.endOffset > 0;
}


static Boolean isVarargPosition(const Token *t) {
  return t->text && strcmp("__VA_ARGS__", t->text) == 0;
}


static Token *copyToken(ParserContext *ctx, const Token *t) {
  Token *n = allocToken(ctx);

  memcpy(n, t, sizeof(Token));
  n->next = NULL;

  return n;
}

static Token *copySequence(ParserContext *ctx, const Token *s) {

  const Token *t = s;

  Token *r = NULL, *cur = NULL;

  while (t) {
      Token *n = copyToken(ctx, t);

      if (r) {
          cur->next = n;
      } else {
          r = n;
      }

      cur = n;
      t = t->next;
  }

  return r;
}

static Token *emptyToken(ParserContext *ctx) {
  Token *r = allocToken(ctx);

  r->code = r->rawCode = IDENTIFIER;
  r->text = "";

  return r;
}

static Token *stringToken(ParserContext *ctx, Coordinates *coords, const char *s) {
  Token *r = allocToken(ctx);

  r->code = r->rawCode = STRING_LITERAL;
  r->text = s;

  return r;
}


Token *stringifySequence(ParserContext *ctx, Token *s) {
  Token *t = s, *p = NULL;

  size_t bufferSize = 0;

  while (t) {
      if (p && isSpacesBetween(p, t)) {
          ++bufferSize;
      }
      bufferSize += t->coordinates.endOffset - t->coordinates.startOffset;
      p = t;
      t = t->next;
  }
  ++bufferSize;

  char *b = allocateString(ctx, bufferSize);

  t = s;
  p = NULL;
  unsigned idx = 0;

  while (t) {
      if (p && isSpacesBetween(p, t)) {
          b[idx++] = ' ';
      }

      size_t tokenLength = t->coordinates.endOffset - t->coordinates.startOffset;
      memcpy(&b[idx], t->pos, tokenLength);
      idx += tokenLength;

      p = t;
      t = t->next;
  }

  b[idx] = '\0';

  Coordinates coords = { s->coordinates.startOffset, (p ? p : s)->coordinates.endOffset, s->coordinates.locInfo };

  return stringToken(ctx, &coords, b);
}

Token *findLastToken(Token *t) {
  Token *n = t;
  while (n) {
      t = n;
      n = n->next;
  }
  return t;
}

Token *concatTokens(ParserContext *ctx, Token *l, Token *r) {

  size_t bufferSize = 0;

  const char *lp, rp;

  // find most right token of left sequence
  l = findLastToken(l);

  size_t llen = l->coordinates.endOffset - l->coordinates.startOffset;
  size_t rlen = r->coordinates.endOffset - r->coordinates.startOffset;

  bufferSize += llen;
  bufferSize += rlen;
  bufferSize += 2;

  char *buffer = heapAllocate(bufferSize);

  unsigned i = 0, j = 0;

  while (i < llen) {
      buffer[j++] = l->pos[i++];
  }

  i = 0;
  while (i < rlen) {
      buffer[j++] = r->pos[i++];
  }

  // lex/flex requires input buffer be double-terminated
  buffer[i++] = 0;
  buffer[i++] = 0;

  LocationInfo *locInfo = allocateMacroLocationInfo(l->coordinates.locInfo->fileName, buffer, bufferSize, l->coordinates.startOffset, r->coordinates.endOffset);

  locInfo->next = ctx->locationInfo;
  ctx->locationInfo = locInfo;

  Token *t = tokenizeBuffer(ctx, locInfo, NULL);

  return t;
}

static Token *findArgument(const Token *arg, MacroArg *args) {
  const char *id = arg->text;

  if (id == NULL) return NULL;

  while (args) {
    if (args->param && strcmp(args->param->name, id) == 0) {
        return args->value;
    }
    args = args->next;
  }

  return NULL;
}

static Boolean isTokenConcat(const Token *t) {
  Token *n = t->next;

  return n && n->rawCode == DSHARP;
}

static Token *findAndCopyArgument(ParserContext *ctx, const Token *t, MacroArg *args, MacroArg *vararg, Boolean isVararg) {

  Token *tmp = NULL;

  if (isVarargPosition(t)) {
    if (vararg) {
      return copySequence(ctx, vararg->value);
    } else {
      // TODO report
      return NULL;
    }
  } else if ((tmp = findArgument(t, args)) != NULL) {
      return copySequence(ctx, tmp);
  } else {
      return copyToken(ctx, t);
  }
}

static Token* replaceTokenSequence(ParserContext *ctx, Token* s) {
  Token *t = s;
  Token *p = NULL;

  while (t) {
      Token *l = NULL;
      Token *tmp = replaceMacro(ctx, t, &l);

      if (p) p->next = tmp;
      else s = tmp;

      p = t != tmp ? findLastToken(tmp) : t;
      t = l;
  }

  return s;
}

Token *replaceMacro(ParserContext *ctx, const Token *macro, Token **macroNextPtr) {

  *macroNextPtr = macro->next;
  if (macro->rawCode != IDENTIFIER) return (Token*)macro;

  MacroDefinition *def = (MacroDefinition *)getFromHashMap(ctx->macroMap, (intptr_t)macro->text);

  if (def == NULL) return (Token*)macro;

  Token *n = macro->next;

  MacroArg *first = NULL, *cur = NULL;
  MacroArg *vararg = NULL;

  Token *macroNext = macro->next;

  if (def->isFunctional) {
    int depth = 0;
    if (n && n->rawCode == '(' && !isSpacesBetween(macro, n)) {
      MacroParam *p = def->params;
      n = n->next;
      Token *argStart = n;
      Token *pt = argStart;

      while (n) {
          Token *nn = macroNext = n->next;
          if (depth == 0) {
              int code = n->rawCode;

              if (code == '(') {
                  ++depth;
              }

              if (code == ')' || code == ',') {
                pt->next = NULL;

                MacroArg *tmp = allocateMacroArg(ctx, p, argStart);

                if (p) {
                  p = p->next;
                } else if (vararg == NULL) {
                    vararg = tmp;
                    if (!def->isVararg) {
                        reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &argStart->coordinates);
                    }
                }

                if (first) {
                  cur->next = tmp;
                } else {
                  first = tmp;
                }

                cur = tmp;
                n = nn;
              }



              if (code == ')') {
                  macroNext = nn;
                  break;
              }
          } else {
              if (n->rawCode == ')') {
                  --depth;
              }
              if (n->rawCode == '(') {
                  ++depth;
              }
          }
          pt = n;
          n = nn;
      }

      if (p) {
          reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &macro->coordinates);
      }
    } else {
        // function-like macro as an identifier
        return (Token*)macro;
    }
  }

  const Token *body = def->body;
  Token *evalBody = NULL, *bcur = NULL;

  if (def->isFunctional) {
      const Token *b = body;
      const Token *pb = NULL;

      while (b) {

          Token *tmp = findAndCopyArgument(ctx, b, first, vararg, def->isVararg);

          if (pb && pb->rawCode == '#') {
            if (tmp) {
              tmp = stringifySequence(ctx, tmp);
            } else {
               // TODO report
            }
          } else if (isTokenConcat(b)) {
            const Token *dsh = b->next;
            assert(dsh);
            const Token *nn = dsh->next;
            Token *l = tmp, *r = NULL;

            if (nn) {
                r = findAndCopyArgument(ctx, nn, first, vararg, def->isVararg);
                if (r == NULL) {
                    r = copyToken(ctx, nn);
                }
            }

            if (r == NULL) {
                // TODO report
                r = emptyToken(ctx);
            }

            tmp = concatTokens(ctx, l, r);
          } else if (b->rawCode == DSHARP) {
              // report
          }

          tmp = replaceTokenSequence(ctx, tmp);

          if (evalBody) {
              bcur->next = tmp;
          } else {
              evalBody = tmp;
          }

          bcur = findLastToken(tmp);
          pb = b;
          b = b->next;
      }
  } else {
      evalBody = copySequence(ctx, body);
  }

  Token *pped = replaceTokenSequence(ctx, evalBody);

  assert(pped != NULL);

  *macroNextPtr = macroNext;

  return pped;
}

static Token *parseInclude(ParserContext *ctx, Token *token) {
  const char *fileName = NULL;
  Token *tail = NULL;
  char b[1024];
  Coordinates coords = { 0 };
  coords.locInfo = token->coordinates.locInfo;
  if (token->rawCode == STRING_LITERAL) {
    coords = token->coordinates;
    fileName = token->text;
    tail = skipPPTokens(ctx, token->next);
  } else if (token->rawCode == '<') {
    Token *last = token;
    Token *tmp = token;

    while (tmp->rawCode && tmp->rawCode != NEWLINE && tmp->rawCode != '>') {
      last = tmp;
      tmp = tmp->next;
    }

    if (last->rawCode != '>') {
        return last;
    }

    coords.startOffset = token->coordinates.startOffset;
    coords.endOffset = last->coordinates.endOffset;

    joinToString(b, sizeof b, token->next, last);

    fileName = b;

    tail = skipPPTokens(ctx, last->next);
  } else if (token->rawCode == IDENTIFIER) {
    if (isMacro(ctx, token->text)) {
      Token *d;
      Token *rToken = replaceMacro(ctx, token, &d);
      if (rToken != token) {
          findLastToken(rToken)->next = d;
      }
      return parseInclude(ctx, rToken);
    } else {
      reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &token->coordinates);
      return token;
    }
  } else {
    reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &token->coordinates);
    return token;
  }

  Token *includeTokens = tokenizeFile(ctx, fileName, NULL);

  if (includeTokens == NULL) {
    reportDiagnostic(ctx, DIAG_INCLUDE_FILE_NOT_FOUND, &coords, fileName);
    return tail;
  }

  Token *t = includeTokens, *p = NULL;
  while (t->code) {
      p = t;
      t = t->next;
  }

  if (p) {
    p->next = tail;
    return includeTokens;
  } else {
    return tail;
  }

}

static Token *defineMacro(ParserContext *ctx, Token *token) {

  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &token->coordinates);
      return token;
  }

  Token *last = findLastPPToken(ctx, token);

  Token *tail = last->next;
  last->next = NULL;

  const char *macroName = token->text;

  Token *n = token->next;
  Token *body = NULL;

  MacroParam *sparam = NULL;
  MacroParam *lparam = NULL;

  Boolean isVarags = FALSE;
  Boolean isFunctional = FALSE;

  if (n && n->rawCode == '(' && !isSpacesBetween(token, n)) { // it's functional macro

    isFunctional = TRUE;
    n = n->next;

    while (n) {
      if (n->rawCode == IDENTIFIER) {

          MacroParam *tmp = allocateMacroParam(ctx, n->text);

          if (!sparam) sparam = tmp;
          else lparam->next = tmp;

          lparam = tmp;

          Token *nn = n->next;
          if (!nn || nn->rawCode != ',' && nn->rawCode != ')') {
              reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &nn->coordinates);
          } else if (nn->rawCode == ',') {
              n = n->next; // skip ','
          }
      } else if (n->rawCode == ELLIPSIS) {
          isVarags = TRUE;
          Token *nn = n->next;
          if (!nn || nn->rawCode != ')') {
            reportDiagnostic(ctx, DIAG_PP_MISSING_PAREN_IN_PARAMS, &nn->coordinates);
          }
      } else if (n->rawCode == ')') {
          body = n->next;
          break;
      } else {
        reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &n->coordinates);
      }
      n = n->next;
    }
  } else {
      body = n;
  }


  MacroDefinition *def = allocateMacroDef(ctx, macroName, sparam, body, isVarags, isFunctional);

  putToHashMap(ctx->macroMap, (intptr_t)macroName, (intptr_t)def);

  return tail;
}


static Token *undefMacro(ParserContext *ctx, Token *token) {
    if (token->code != IDENTIFIER) {
        reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &token->coordinates);
    } else {
        removeFromHashMap(ctx->macroMap, (intptr_t)token->text);
    }
    return token->next;
}

static Token *takeBranch(ParserContext *ctx, Token *start, Boolean isTaken);


void parseNumber(ParserContext *ctx, Token *token);

static Token *constToken(ParserContext *ctx, int v, Token *token) {
  Token *r = allocToken(ctx);
  r->coordinates = token->coordinates;
  r->rawCode = I_CONSTANT_RAW;
  r->code = I_CONSTANT;
  r->value.iv = v;
  r->next = token->next;

  return r;
}

static Token *simplifyTokenSequence(ParserContext *ctx, Token *token) {
  Token *cur = token;
  Token *last = NULL;
  while (cur) {
      if (strcmp("defined", cur->text) == 0) {
        Token *n = cur->next;
        if (n && n->rawCode == '(') {
            Token *id = n->next;
            if (id && id->rawCode == IDENTIFIER) {
                Token *nn = id->next;
                if (nn && nn->rawCode == ')') {
                    Boolean isDefined = isMacro(ctx, id->text);
                    Token *r = constToken(ctx, isDefined, nn);
                    if (last) last->next = r;
                    else token = r;
                }
            }
        } else if (n && n->rawCode == IDENTIFIER) {
          Boolean isDefined = isMacro(ctx, n->text);
          Token *r = constToken(ctx, isDefined, n);
          if (last) last->next = r;
          else token = r;

        } else {
          reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, n ? &n->coordinates : &token->coordinates);
        }
      } else if (cur->rawCode == IDENTIFIER) {
          Token *d;
          Token *replaced = replaceMacro(ctx, cur, &d);
          if (cur != replaced) {
              findLastToken(replaced)->next = d;
          }

          if (last) last->next = cur;
          else token = cur;

          continue;
      }
      last = cur;
      cur = cur->next;
  }

  return token;

}

static AstExpression *parsePPExpression(ParserContext *ctx, Token *start) {

  Token *ctxToken = ctx->token;
  ctx->token = start;

  AstExpression *expr = parseConditionalExpression(ctx, NULL);

  ctx->token = ctxToken;

  return expr;
}

static int evaluateTokenSequence(ParserContext *ctx, Token *token) {

  token = simplifyTokenSequence(ctx, token);

  AstExpression *expr = parsePPExpression(ctx, token);
  AstConst *e = eval(ctx, expr);

  if (e) {
      return e->i;
  }

  reportDiagnostic(ctx, DIAG_PP_CANNOT_EVALUATE, &expr->coordinates);

  return 0;
}


static Token *elif(ParserContext *ctx, Token *token) {
    Token *last = findLastPPToken(ctx, token);
    Token *tail = last->next;
    last->next = NULL;

    int v = evaluateTokenSequence(ctx, token);

    return takeBranch(ctx, tail, v);
}

static Token *takeBranch(ParserContext *ctx, Token *start, Boolean isTaken) {
  int depth = 0;

  Token *prev = NULL;
  Token *token  = start;

  Token *taken = isTaken ? start : NULL;
  Token *lastTaken = NULL;

  while (token->code) {
      if (token->rawCode == '#' && prev && prev->rawCode == NEWLINE) {
          token = token->next;
          if (!strcmp("if", token->text) || !strcmp("ifdef", token->text) || !strcmp("ifndef", token->text)) {
              ++depth;
          }

          if (!strcmp("else", token->text)) {
              if (depth) --depth;
              else {
                  if (taken == NULL) {
                     taken = token->next;
                  } else {
                     lastTaken = prev;
                  }
              }
          }

          if (!strcmp("elif", token->text)) {
              if (depth) --depth;
              else {
                  if (taken == NULL) {
                    return elif(ctx, token->next);
                  } else {
                    if (lastTaken == NULL)
                      lastTaken = prev;
                  }
              }
          }

          if (!strcmp("endif", token->text)) {
              if (depth) --depth;
              else {
                  if (taken == NULL) {
                    return token->next;
                  } else {
                    if (lastTaken == NULL)
                      lastTaken = prev;

                    lastTaken->next = token->next;
                    return taken;
                  }
              }
          }
      }
      prev = token;
      token = token->next;
  }


  reportDiagnostic(ctx, DIAG_PP_UNTERMINATED_COND_DIRECTIVE, &start->coordinates);

  return taken;
}

static Token *_if(ParserContext *ctx, Token *token) {
    Token *last = findLastPPToken(ctx, token);
    Token *tail = last->next;
    last->next = NULL;


    int v = evaluateTokenSequence(ctx, token);

    return takeBranch(ctx, tail, v);
}

static Token *ifdef(ParserContext *ctx, Token *token) {

  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &token->coordinates);
      return token ? token->next : token;
  }

  Boolean isDefined = isMacro(ctx, token->text);

  return takeBranch(ctx, token->next, isDefined);
}

static Token *ifndef(ParserContext *ctx, Token *token) {
  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &token->coordinates);
      return token ? token->next : token;
  }

  Boolean isDefined = isMacro(ctx, token->text);

  return takeBranch(ctx, token->next, !isDefined);
}

static Token *_else(ParserContext *ctx, Token *token) {
  reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &token->coordinates, "else");
  return token;
}

static Token *endif(ParserContext *ctx, Token *token) {
  reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &token->coordinates, "endif");
  return token;
}

Token *preprocess(ParserContext *ctx, Token *token) {

  YYSTYPE dummy = 0;

  unsigned bOffset = 0;

  assert(token->rawCode == '#');

  Token *directiveToken = token->next;

  if (directiveToken->rawCode != IDENTIFIER && directiveToken->rawCode != I_CONSTANT_RAW) {
    reportDiagnostic(ctx, DIAG_INVALID_PP_DIRECTIVE, &directiveToken->coordinates, directiveToken);
    return ctx->token = directiveToken;
  }

  if (directiveToken->rawCode == I_CONSTANT_RAW) {
      return ctx->token = skipPPTokens(ctx, directiveToken);
  }

  const char *directive = directiveToken->text;

  if (!strcmp("include", directive)) {
    return parseInclude(ctx, directiveToken->next);
  } else if (!strcmp("define", directive)) {
    return defineMacro(ctx, directiveToken->next);
  } else if (!strcmp("undef", directive)) {
    return undefMacro(ctx, directiveToken->next);
  } else if (!strcmp("if", directive)) {
    return _if(ctx, directiveToken->next);
  } else if (!strcmp("ifdef", directive)) {
    return ifdef(ctx, directiveToken->next);
  } else if (!strcmp("ifndef", directive)) {
    return ifndef(ctx, directiveToken->next);
  } else if (!strcmp("elif", directive)) {
    return elif(ctx, directiveToken->next);
  } else if (!strcmp("else", directive)) {
    return _else(ctx, directiveToken->next);
  } else if (!strcmp("endif", directive)) {
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &token->coordinates, "elif");
    return directiveToken->next;
  } else if (!strcmp("line", directive)) {

  } else if (!strcmp("error", directive)) {
    reportDiagnostic(ctx, DIAG_PP_ERROR, &token->coordinates, token->next ? token->next->text ? token->next->text : "" : "");
    return token->next ? token->next->next : NULL;
  } else if (!strcmp("pragma", directive)) {
    return skipPPTokens(ctx, token->next);
  } else {
    reportDiagnostic(ctx, DIAG_INVALID_PP_DIRECTIVE, &directiveToken->coordinates, directiveToken);
    return directiveToken;
  }
}
