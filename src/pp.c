

#include <assert.h>
#include <stdio.h>
#include <ctype.h>

#include "pp.h"
#include "parser.h"
#include "lex.h"
#include "tree.h"


static Token *expandMacro(ParserContext *ctx, const Token *macro);

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

typedef struct _Hideset {
  const char *name;
  struct _Hideset *next;
} Hideset;

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

static Hideset *allocateHideset(ParserContext *ctx, const char *name) {
  Hideset *hs = areanAllocate(ctx->memory.macroArena, sizeof(Hideset));
  hs->name = name;
  return hs;
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


static Boolean hidesetContains(Hideset *hs, const char *name) {
  while (hs) {
      if (strcmp(name, hs->name) == 0) return TRUE;
      hs = hs->next;
  }

  return FALSE;
}

static Hideset *hidesetUnion(ParserContext *ctx, Hideset *hs1, Hideset *hs2) {
  Hideset head = { 0 };
  Hideset *cur = &head;

  for (; hs1; hs1 = hs1->next) {
      cur = cur->next = allocateHideset(ctx, hs1->name);
  }

  cur->next = hs2;

  return head.next;
}

static Hideset *hidesetIntersection(ParserContext *ctx, Hideset *hs1, Hideset *hs2) {
  Hideset head = { 0 };
  Hideset *cur = &head;

  for (; hs1; hs1 = hs1->next) {
    if (hidesetContains(hs2, hs1->name)) {
        cur = cur->next = allocateHideset(ctx, hs1->name);
    }
  }

  cur->next = hs2;

  return head.next;
}

static Token *addHideset(ParserContext *ctx, const Token *body, Hideset *hs) {
  Token head = { 0 };
  Token *cur = &head;

  for (; body; body = body->next) {
      Token *t = copyToken(ctx, body);
      t->hs = hidesetUnion(ctx, body->hs, hs);
      cur = cur->next = t;
  }

  return head.next;
}

static Token *skipPPTokens(ParserContext *ctx, Token *token) {
  while (token->code) {
      if (token->rawCode == NEWLINE){
          return token->next;
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

Boolean hasSpace(const Token *t) {
  return t->coordinates.startOffset > 0 && isspace(t->pos[-1]);
}


static Boolean isVarargPosition(const Token *t) {
  return t->text && strcmp("__VA_ARGS__", t->text) == 0;
}

static Token *stringToken(ParserContext *ctx, Coordinates *coords, const char *s) {
  Token *r = allocToken(ctx);

  r->code = r->rawCode = STRING_LITERAL;
  r->text = r->pos = s;

//  r->coordinates.locInfo = NULL;
//  r->coordinates.startOffset = 0;
//  r->coordinates.endOffset = strlen(s);

  return r;
}


Token *stringifySequence(ParserContext *ctx, Token *s) {
  Token *t = s, *p = NULL;

  size_t bufferSize = 0;

  while (t) {
      if (p && hasSpace(t)) {
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
      if (p && hasSpace(t)) {
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

  Token *pl = NULL;
  Token *t = l;

  while (t->next) {
      pl = t;
      t = t->next;
  }

  // find most right token of left sequence
  l = t;

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
  buffer[j++] = 0;
  buffer[j++] = 0;

  LocationInfo *locInfo = allocateMacroLocationInfo(l->coordinates.locInfo->fileName, buffer, bufferSize, l->coordinates.startOffset, r->coordinates.endOffset);

  locInfo->next = ctx->locationInfo;
  ctx->locationInfo = locInfo;

  Token *s = tokenizeBuffer(ctx, locInfo, NULL);
  Token *p = NULL;
  t = s;

  if (pl) {
      pl->next = s;
  }

  while (t->code) {
      p = t;
      t = t->next;
  }

  if (p) {
    p->next = r->next;
  }

  return s;
}

static MacroArg *findArgument(const Token *arg, MacroArg *args) {
  const char *id = arg->text;

  if (id == NULL) return NULL;

  while (args) {
    if (args->param && strcmp(args->param->name, id) == 0) {
        return args;
    }
    args = args->next;
  }

  return NULL;
}

static Boolean isTokenConcat(const Token *t) {
  Token *n = t->next;

  return n && n->rawCode == DSHARP;
}

static MacroArg *findAndCopyArgument(ParserContext *ctx, const Token *t, MacroArg *args, MacroArg *vararg, MacroDefinition *def) {

  MacroArg *tmp = NULL;

  if (isVarargPosition(t)) {
    if (def->isVararg) {
        return vararg;
    } else {
        // it's regular ID
        return NULL;
    }
  }

  return findArgument(t, args);
}

static Token* expandEvaluatedSequence(ParserContext *ctx, Token* s) {
  Token *t = s;
  Token *p = NULL;

  while (t && t->hs) {
      Token *tmp = expandMacro(ctx, t);

      if (p) p->next = tmp;
      else s = tmp;

      if (tmp != t) {
          t = tmp;
      } else {
          p = t;
          t= t->next;
      }
  }

  return s;
}

static Token* expandSequence(ParserContext *ctx, Token* s) {
  Token *t = s;
  Token *p = NULL;

  while (t) {
      Token *tmp = expandMacro(ctx, t);

      if (p) p->next = tmp;
      else s = tmp;

      if (tmp != t) {
          t = tmp;
      } else {
          p = t;
          t= t->next;
      }
  }

  return s;
}

static MacroArg *readMacroArgument(ParserContext *ctx, Token *s, MacroParam *p, Token **tail) {
  Token *argStart = NULL, *argEnd = NULL;

  Token *n = s;
  int depth = 0;


  while (n) {
    if (depth == 0 && (p && n->rawCode == ',' || n->rawCode == ')'))  {
        break;
    }

    if (depth && n->rawCode == ')') {
        --depth;
    } else if (n->rawCode == '(') {
        ++depth;
    }

    if (argStart == NULL) {
        argStart = n;
    }

    argEnd = n;
    n = n->next;
  }

  *tail = n;
  if (argEnd) {
      argEnd->next = NULL;
  }

  return allocateMacroArg(ctx, p, argStart);
}

static Token *expandMacro(ParserContext *ctx, const Token *macro) {

  if (macro->rawCode != IDENTIFIER) return (Token*)macro;

  MacroDefinition *def = (MacroDefinition *)getFromHashMap(ctx->macroMap, (intptr_t)macro->text);

  if (def == NULL) return (Token*)macro;

  if (hidesetContains(macro->hs, def->name)) return (Token*)macro;

  Token *n = macro->next;
  Token *macroNext = macro->next;


  if (def->isFunctional && (!n || n->rawCode != '(' || hasSpace(n) && macro->hs == NULL)) {
    // #define f(x) x
    // a = foo (y + 1)
    // b = foo 10
    // b = foo <EOF>

    return (Token*)macro;
  }

  MacroArg argHead = { 0 };
  MacroArg *arg = &argHead;
  MacroArg *vararg = NULL;

  if (def->isFunctional) {
      MacroParam *p = def->params;
      Token *argStart = NULL, *argEnd = NULL;
      int depth = 0;
      n = n->next;

      while (p) {
          arg = arg->next = readMacroArgument(ctx, n, p, &n);
          p = p->next;
          if (!n || n->rawCode == ')') break;
          n = n->next;
      }

      if (p) {
          reportDiagnostic(ctx, DIAG_PP_TOO_FEW_ARGUMENTS, &macro->coordinates);
      }

      if (n && n->rawCode != ')') {
          if (!def->isVararg) {
              reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &argStart->coordinates);
          }
          vararg = readMacroArgument(ctx, n, NULL, &n);
      } else if (def->isVararg) {
          vararg = allocateMacroArg(ctx, NULL, NULL);
      }

      assert(!n || n->rawCode == ')');
      macroNext = n ? n->next : n;
  }


  Hideset *macrohs = macro->hs;
  if (def->isFunctional) {
      macrohs = hidesetIntersection(ctx, macrohs, macroNext ? macroNext->hs : NULL);
  }

  Hideset *evalhs = hidesetUnion(ctx, macrohs, allocateHideset(ctx, def->name));

  const Token *body = def->body;

  if (!def->isFunctional) {
      body = addHideset(ctx, body, evalhs);
  }

  Token evalHead = { 0 };
  Token *evalCur = &evalHead;
  Token *evalBody = NULL, *bcur = NULL;

  Boolean isFuncional = def->isFunctional;

  const Token *b = body;
  const Token *pb = NULL;

  while (b) {
      if (isFuncional && b->rawCode == '#') {
        Token *next = b->next;
        if (next) {
          MacroArg *arg = findAndCopyArgument(ctx, next, argHead.next, vararg, def);
          if (arg) {
            Token *argValue = arg->value;
            Token *evaluated = argValue ? stringifySequence(ctx, argValue) : stringToken(ctx, &next->coordinates, "");
            if (evalBody) {
                bcur = bcur->next = evaluated;
            } else {
                bcur = evalBody = evaluated;
            }

            b = next->next;
            continue;
          }
        }
      }

//      if (b->rawCode == ',' && b->next && b->next->rawCode == DSHARP) {
//          // GNU COMMA ,##__VA_ARGS__
//          Token *comma = b;
//          Token *dsh = comma->next;
//          Token *next = dsh->next;
//          if (next) {
//              MacroArg *arg = findAndCopyArgument(ctx, next, argHead.next, vararg, def);
//              if (arg && arg->param == NULL) {
//                  if (arg->value == NULL) {
//                      // expand to empty list
//                      b = next->next;
//                  } else {
//                      Token *evaluated = copyToken(ctx, b);
//                      if (evalBody) {
//                          bcur = bcur->next = evaluated;
//                      } else {
//                          bcur = evalBody = evaluated;
//                      }
//                      b = next;
//                  }
//                  continue;
//              }
//          }
//      }

      if (b->next && b->next->rawCode == DSHARP) {

        MacroArg *lhsA = findAndCopyArgument(ctx, b, argHead.next, vararg, def);
        Token *lhs = NULL;

        if (lhsA == NULL) {
            lhs = copyToken(ctx, b);
        } else {
            lhs = lhsA->value ? copySequence(ctx, lhsA->value) : NULL;
        }

        Token *sharpsharp = b->next;

        Token *origRhs = sharpsharp->next;

        if (origRhs) {
          MacroArg *rhsA = findAndCopyArgument(ctx, origRhs, argHead.next, vararg, def);
          Token *rhs = NULL;
          if (rhsA == NULL) {
              rhs = copyToken(ctx, origRhs);
          } else {
              rhs = rhsA->value ? copySequence(ctx, rhsA->value) : NULL;
          }

          Token *evaluated = NULL;
          if (rhs == NULL && lhs == NULL) {
              b = origRhs->next;
              continue;
          } else if (rhs == NULL) {
              evaluated = lhs;
          } else if (lhs == NULL) {
              evaluated = rhs;
          } else {
              evaluated = concatTokens(ctx, lhs, rhs);
          }

          if (evalBody) {
              bcur->next = evaluated;
          } else {
              evalBody = evaluated;
          }

          bcur = findLastToken(evaluated);
          b = origRhs->next;
          continue;
        } else {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macro->coordinates);
            break;
        }
      }
      if (b->rawCode == DSHARP) {
          // a##b##c
          Token *next = b->next;

          if (next == NULL) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macro->coordinates);
            break;
          }

          if (b == body) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macro->coordinates);
            break;
          }

          Token *lhs = bcur;
          MacroArg *rhsA = findAndCopyArgument(ctx, next, argHead.next, vararg, def);
          Token *rhs = NULL;

          if (rhsA == NULL) {
              rhs = copyToken(ctx, next);
          } else {
              rhs = copySequence(ctx, rhsA->value);
          }

          Token *evaluated = NULL;
          if (rhs == NULL && lhs == NULL) {
              b = next->next;
              continue;
          } else if (lhs == NULL) {
              evaluated = rhs;
          } else  if (rhs == NULL) {
              evaluated = lhs;
          } else {
              evaluated = concatTokens(ctx, lhs, rhs);
          }

          Token *i = evalBody;

          while (i) {
              if (i->next == bcur) break;
              i = i->next;
          }

          if (i) {
              i->next = evaluated;
          } else {
              evalBody = evaluated;
          }

          bcur = findLastToken(evaluated);
          b = next->next;

          continue;
      }

      MacroArg *arg = findAndCopyArgument(ctx, b, argHead.next, vararg, def);

      Token *evaluated = NULL;
      if (arg) {
        Token *argValue = arg->value;
        if (argValue) {
          argValue = copySequence(ctx, argValue);
          evaluated = expandSequence(ctx, argValue);
        } else {
          b = b->next;
          continue;
        }
      } else {
        evaluated = copyToken(ctx, b);
      }

      if (evalBody) {
          bcur->next = evaluated;
      } else {
          evalBody = evaluated;
      }

      bcur = findLastToken(evaluated);
      b = b->next;
  }

  if (def->isFunctional) {
    evalBody = addHideset(ctx, evalBody, evalhs);
  }

  if (evalBody) {
    findLastToken(evalBody)->next = macroNext;
    evalBody = expandEvaluatedSequence(ctx, evalBody);
    return evalBody;
  }

  return macroNext;
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
      Token *rToken = expandMacro(ctx, token);
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

  if (n && n->rawCode == '(' && !hasSpace(n)) { // it's functional macro

    isFunctional = TRUE;
    n = n->next;

    while (n) {
      if (n->rawCode == IDENTIFIER) {
          // TODO: maybe support GNU comma extension?

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

  intptr_t old = putToHashMap(ctx->macroMap, (intptr_t)macroName, (intptr_t)def);
  if (old) {
      reportDiagnostic(ctx, DIAG_PP_MACRO_REDEFINED, &token->coordinates, macroName);
  }

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
          cur = expandMacro(ctx, cur);

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

  token = expandSequence(ctx, token);
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

static Token *handleDirecrive(ParserContext *ctx, Token *token) {

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

Token *preprocessFile(ParserContext *ctx, Token *s, Token *tail) {
  Token *t = s, *p = NULL;

  while (t) {
    if (t->rawCode == '#' && (!p || p->rawCode == NEWLINE)) {
      Token *pp = handleDirecrive(ctx, t);
      if (pp != t) {
          t = pp;
          if (p) {
              p->next = t;
          } else {
              s = t;
          }
          continue;
      }
    } else if (t->rawCode == IDENTIFIER) {
      Token *d;
      Token *n = expandMacro(ctx, t);
      if (p) p->next = n;
      if (t != n) {
        t = n;
        continue;
      }
    }

    p = t;
    t = t->next;
  }

  if (p)
    p->next = tail;
  else
    s = tail;

  return s;
}
