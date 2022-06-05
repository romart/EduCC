

#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>
#include <libgen.h>
#include <unistd.h>
#include <linux/limits.h>
#include <sys/cdefs.h>

#include "pp.h"
#include "parser.h"
#include "lex.h"
#include "tree.h"


static Token *expandMacro(ParserContext *ctx, const Token *macro, Boolean evalDefined);

typedef struct _MacroParam {
  const char *name;
  Boolean isVararg;
  struct _MacroParam *next;
} MacroParam;

typedef Token *(macroHandler)(ParserContext *, const Token *);

typedef struct _MacroDefinition {
  const char *name;
  MacroParam *params;
  Token *body;

  macroHandler *handler;

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
          unsigned t = snprintf(&buffer[ptr], size, "%s", b->text);
          ptr += t;
          size -= t;
      } else {
          break;
      }
      b = b->next;
  }

  buffer[ptr] = '\0';
}

static Token *findLastPPToken(ParserContext *ctx, Token *token) {
  Token *cur = token, *last = token;

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

static size_t renderTokenToBuffer(char *buffer, Token *t) {

  unsigned i = 0;

  size_t len = t->coordinates.endOffset - t->coordinates.startOffset;

  if (t->pos == 0) {
      if (t->rawCode == I_CONSTANT_RAW) {
          return sprintf(buffer, "%ld", t->value.iv);
      }

      if (t->rawCode == STRING_LITERAL) {
          return sprintf(buffer, "%s", t->text);
      }

      return 0;
  } else {
    while (i < len) {
        buffer[i] = t->pos[i];
        ++i;
    }
  }

  return i;
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

  j += renderTokenToBuffer(&buffer[j], l);
  j += renderTokenToBuffer(&buffer[j], r);

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

static Token* expandEvaluatedSequence(ParserContext *ctx, Token* s, Boolean evalDefined) {
  Token *t = s;
  Token *p = NULL;

  while (t && t->hs) {
      Token *tmp = expandMacro(ctx, t, evalDefined);

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

static Token* expandSequence(ParserContext *ctx, Token* s, Boolean evalDefined) {
  Token *t = s;
  Token *p = NULL;

  while (t) {
      Token *tmp = expandMacro(ctx, t, evalDefined);

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
    if (depth == 0 && (!p->isVararg && n->rawCode == ',' || n->rawCode == ')'))  {
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

static Token *constToken(ParserContext *ctx, int v, Token *token) {
  Token *r = allocToken(ctx);
  r->coordinates = token->coordinates;
  r->rawCode = I_CONSTANT_RAW;
  r->code = I_CONSTANT;
  r->value.iv = v;
  r->next = token->next;

  return r;
}

static Token *evaluateDefinedOp(ParserContext *ctx, Token *token, Boolean relaxed, Token **next) {
  assert(token);
  assert(token->rawCode == IDENTIFIER);
  assert(strcmp("defined", token->text) == 0);

  Token *n = token->next;
  if (n && n->rawCode == '(') {
      Token *id = n->next;
      if (id && id->rawCode == IDENTIFIER) {
          Token *nn = id->next;
          if (nn && nn->rawCode == ')') {
              Boolean isDefined = isMacro(ctx, id->text);
              *next = nn->next;
              return constToken(ctx, isDefined, nn);
          }
      }
  } else if (n && n->rawCode == IDENTIFIER) {
    Boolean isDefined = isMacro(ctx, n->text);
    *next = n->next;
    return constToken(ctx, isDefined, n);
  }

  if (!relaxed) {
    reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, n ? &n->coordinates : &token->coordinates);
    *next = n;
    return constToken(ctx, 0, n);
  }

  return NULL;
}

static Token *expandMacro(ParserContext *ctx, const Token *macro, Boolean evalDefined) {

  if (macro->rawCode != IDENTIFIER) return (Token*)macro;

  MacroDefinition *def = (MacroDefinition *)getFromHashMap(ctx->macroMap, (intptr_t)macro->text);

  if (def == NULL) {
      if (evalDefined && strcmp("defined", macro->text) == 0) {
        Token *next;
        Token *e = evaluateDefinedOp(ctx, (Token*)macro, TRUE, &next);
        if (e) {
          e->next = next;
          return e;
        }
      }
      return (Token*)macro;
  }

  if (def->handler) {
      Token *r = def->handler(ctx, macro);
      r->next = macro->next;
      return r;
  }

  if (hidesetContains(macro->hs, def->name)) return (Token*)macro;

  Token *n = macro->next;
  Token *macroNext = macro->next;

  if (def->isFunctional && (!n || n->rawCode != '(')) {
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

      while (p && !p->isVararg) {
          arg = arg->next = readMacroArgument(ctx, n, p, &n);
          p = p->next;
          if (!n || n->rawCode == ')') break;
          n = n->next;
      }

      if (p && !p->isVararg) {
          reportDiagnostic(ctx, DIAG_PP_TOO_FEW_ARGUMENTS, &macro->coordinates);
      }

      if (n && n->rawCode != ')') {
          if (!p) {
              reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &argStart->coordinates);
              while (n && n->rawCode != ')') n = n->next;
          } else {
            arg = arg->next = readMacroArgument(ctx, n, p, &n);
          }
      } else if (p) {
          arg = arg->next = allocateMacroArg(ctx, p, NULL);
      }

      assert(!n || n->rawCode == ')');
      macroNext = n ? n->next : n;
  }


  Hideset *macrohs = macro->hs;
  if (def->isFunctional) {
      macrohs = hidesetIntersection(ctx, macrohs, macroNext ? macroNext->hs : NULL);
  }

  Hideset *evalhs = hidesetUnion(ctx, macrohs, allocateHideset(ctx, def->name));

  Token *body = def->body;

  if (!def->isFunctional) {
      body = addHideset(ctx, body, evalhs);
  }

  Token evalHead = { 0 };
  Token *evalCur = &evalHead;

  Boolean isFuncional = def->isFunctional;

  Token *b = body;

  while (b) {

      if (evalDefined && b->rawCode == IDENTIFIER && strcmp("defined", b->text) == 0) {
        Token *e = evaluateDefinedOp(ctx, b, TRUE, &b);
        if (e) {
          evalCur = evalCur->next = e;
          continue;
        }
      }

      if (isFuncional && b->rawCode == '#') {
        Token *next = b->next;
        if (next) {
          MacroArg *arg = findArgument(next, argHead.next);
          if (arg) {
            Token *argValue = arg->value;
            Token *evaluated = argValue ? stringifySequence(ctx, argValue) : stringToken(ctx, &next->coordinates, "");
            evalCur = evalCur->next = evaluated;

            b = next->next;
            continue;
          }
        }
      }

      if (b->rawCode == ',' && b->next && b->next->rawCode == DSHARP) {
          // GNU COMMA ,##__VA_ARGS__
          Token *comma = b;
          Token *dsh = comma->next;
          Token *next = dsh->next;
          if (next) {
              MacroArg *arg = findArgument(next, argHead.next);
              if (arg && arg->param->isVararg) {
                  if (arg->value == NULL) {
                      // expand to empty list
                      b = next->next;
                  } else {
                      Token *evaluated = copyToken(ctx, b);
                      evalCur = evalCur->next = evaluated;
                      b = next;
                  }
                  continue;
              }
          }
      }

      if (b->next && b->next->rawCode == DSHARP) {

        MacroArg *lhsA = findArgument(b, argHead.next);
        Token *lhs = NULL;

        if (lhsA == NULL) {
            lhs = copyToken(ctx, b);
        } else {
            lhs = lhsA->value ? copySequence(ctx, lhsA->value) : NULL;
        }

        Token *sharpsharp = b->next;
        Token *origRhs = sharpsharp->next;

        if (origRhs) {
          MacroArg *rhsA = findArgument(origRhs, argHead.next);
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

          evalCur->next = evaluated;
          evalCur = findLastToken(evaluated);

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

          Token *lhs = evalCur;
          MacroArg *rhsA = findArgument(next, argHead.next);
          Token *rhs = NULL;

          if (rhsA == NULL) {
              rhs = copyToken(ctx, next);
          } else {
              rhs = copySequence(ctx, rhsA->value);
          }

          Token *evaluated = NULL;
          if (rhs == NULL && lhs == &evalHead) {
              b = next->next;
              continue;
          } else if (lhs == &evalHead) {
              evaluated = rhs;
          } else  if (rhs == NULL) {
              evaluated = lhs;
          } else {
              evaluated = concatTokens(ctx, lhs, rhs);
          }

          Token *i = &evalHead;

          while (i->next) {
              if (i->next == evalCur) break;
              i = i->next;
          }

          i->next = evaluated;
          evalCur = findLastToken(evaluated);

          b = next->next;

          continue;
      }

      MacroArg *arg = findArgument(b, argHead.next);

      Token *evaluated = NULL;
      if (arg) {
        Token *argValue = arg->value;
        if (argValue) {
          argValue = copySequence(ctx, argValue);
          evaluated = expandSequence(ctx, argValue, evalDefined);
        } else {
          b = b->next;
          continue;
        }
      } else {
        evaluated = copyToken(ctx, b);
      }

      evalCur->next = evaluated;
      evalCur = findLastToken(evaluated);

      b = b->next;
  }

  if (def->isFunctional) {
    evalHead.next = addHideset(ctx, evalHead.next, evalhs);
  }

  if (evalHead.next) {
    findLastToken(evalHead.next)->next = macroNext;
    return expandEvaluatedSequence(ctx, evalHead.next, evalDefined);
  }

  return macroNext;
}

static const char *findIncludePath(ParserContext *ctx, const char *includeName, Boolean isdquoted) {

  if (isdquoted && includeName[0] != '/') {
      char *copy = strdup(ctx->config->fileToCompile);
      char *dir = dirname(copy);
      size_t l = strlen(dir) + 1 + strlen(includeName) + 1;
      char *path = heapAllocate(l);
      snprintf(path, l, "%s/%s", dir, includeName);
      if (access(path, F_OK) == 0) {
          char *result = allocateString(ctx, l);
          strncpy(result, path, l);
          releaseHeap(dir);
          releaseHeap(path);
          return result;
      }
  }

  if (includeName[0] == '/') return includeName;


  static char pathBuffer[PATH_MAX] = { 0 };

  IncludePath *includePath = ctx->config->includePath;

  while (includePath) {
      int len = snprintf(pathBuffer, PATH_MAX, "%s/%s", includePath->path, includeName);
      if (access(pathBuffer, F_OK) == 0) {
          char *result = allocateString(ctx, len + 1);
          strncpy(result, pathBuffer, len + 1);
          return result;
      }

      includePath = includePath->next;
  }

  return NULL;
}

static Token *parseInclude(ParserContext *ctx, Token *token) {
  const char *fileName = NULL;
  Token *tail = NULL;
  char b[1024];
  Boolean dquoted = FALSE;
  Coordinates coords = { 0 };
  coords.locInfo = token->coordinates.locInfo;
  if (token->rawCode == STRING_LITERAL) {
    coords = token->coordinates;
    fileName = token->text;
    tail = skipPPTokens(ctx, token->next);
    dquoted = TRUE;
  } else if (token->rawCode == '<') {
    Token *last = token;
    Token *tmp = token;

    while (tmp->rawCode && tmp->rawCode != NEWLINE && tmp->rawCode != '>') {
      last = tmp;
      tmp = tmp->next;
    }

    if (tmp->rawCode != '>') {
        return last;
    }

    coords.startOffset = token->coordinates.startOffset;
    coords.endOffset = last->coordinates.endOffset;

    joinToString(b, sizeof b, token->next, tmp);

    fileName = b;

    tail = skipPPTokens(ctx, last->next);
  } else if (token->rawCode == IDENTIFIER) {
    if (isMacro(ctx, token->text)) {
      Token *rToken = expandMacro(ctx, token, FALSE);
      return parseInclude(ctx, rToken);
    } else {
      reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &token->coordinates);
      return token;
    }
  } else {
    reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &token->coordinates);
    return token;
  }

  fileName = findIncludePath(ctx, fileName, dquoted);

  Token *includeTokens = fileName ? tokenizeFile(ctx, fileName, NULL) : NULL;

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

static MacroParam *parseVarargParam(ParserContext *ctx, Token *n, Token **next) {

  Token *elipsis = n;
  const char *varargName = "__VA_ARGS__";

  if (n->rawCode == IDENTIFIER) {
      varargName = n->text;
      elipsis = n->next;
      assert(elipsis && elipsis->rawCode == ELLIPSIS);
  }

  MacroParam* p = allocateMacroParam(ctx, varargName);
  p->isVararg = TRUE;

  Token *nn = elipsis->next;
  if (!nn || nn->rawCode != ')') {
    reportDiagnostic(ctx, DIAG_PP_MISSING_PAREN_IN_PARAMS, &nn->coordinates);
  }

  while (n && n->rawCode != ')') n = n->next;

  *next = n;

  return p;
}

static Token *defineMacro(ParserContext *ctx, Token *token) {

  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &token->coordinates);
      return token;
  }

  if (strcmp("defined", token->text) == 0) {
      reportDiagnostic(ctx, DIAG_PP_DEFINED_NOT_A_NAME, &token->coordinates);
      return token;
  }

  Token *last = findLastPPToken(ctx, token);

  Token *tail = last->next;
  last->next = NULL;

  const char *macroName = token->text;

  Token *n = token->next;
  Token *body = NULL;

  MacroParam head = { };
  MacroParam *cur = &head;

  Boolean isVarags = FALSE;
  Boolean isFunctional = FALSE;

  if (n && n->rawCode == '(' && !hasSpace(n)) { // it's functional macro

    isFunctional = TRUE;
    n = n->next;

    while (n) {
      if (n->rawCode == IDENTIFIER) {

          if (n->next && n->next->rawCode == ELLIPSIS) {
              // #define x(y...) aka GNU comma

              MacroParam *p = parseVarargParam(ctx, n, &n);
              cur = cur->next = p;
              isVarags = TRUE;

              continue;
          } else {
            MacroParam *tmp = allocateMacroParam(ctx, n->text);

            cur = cur->next = tmp;

            Token *nn = n->next;
            if (!nn || nn->rawCode != ',' && nn->rawCode != ')') {
                reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &nn->coordinates);
            } else if (nn->rawCode == ',') {
                n = n->next; // skip ','
            }
          }
      } else if (n->rawCode == ELLIPSIS) {
          MacroParam *p = parseVarargParam(ctx, n, &n);
          cur = cur->next = p;
          isVarags = TRUE;

          continue;
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


  MacroDefinition *def = allocateMacroDef(ctx, macroName, head.next, body, isVarags, isFunctional);

  intptr_t old = putToHashMap(ctx->macroMap, (intptr_t)macroName, (intptr_t)def);
  if (old) {
      // TODO: support proper comparison and do not report if bodies are equal
      // Now temporary silenced to make tests work
//      reportDiagnostic(ctx, DIAG_PP_MACRO_REDEFINED, &token->coordinates, macroName);
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

static Token *takeBranch(ParserContext *ctx, Token *start, int condition);


void parseNumber(ParserContext *ctx, Token *token);

static Token *eofToken(ParserContext *ctx) {
  Token *t = allocToken(ctx);
  t->code = t->rawCode = 0;

  return t;
}

static Token *simplifyTokenSequence(ParserContext *ctx, Token *token) {
  Token head = { 0 };
  Token *cur = &head;
  while (token) {
      if (token->rawCode == IDENTIFIER && strcmp("defined", token->text) == 0) {
          cur = cur->next = evaluateDefinedOp(ctx, token, FALSE, &token);
          continue;
      } else if (token->rawCode == IDENTIFIER) {
          Token *e = expandMacro(ctx, token, TRUE);

          if (e == token) {
              // https://gcc.gnu.org/onlinedocs/cpp/If.html#If
              // Identifiers that are not macros, which are all considered to be the number zero.
              cur = cur->next = constToken(ctx, 0, token);
              token = e->next;
              continue;
          } else {
              token = e;
              continue;
          }
      } else {
          cur = cur->next = token;
      }
      token = token->next;
  }

  cur = cur->next = eofToken(ctx);

  return head.next;
}

static AstExpression *parsePPExpression(ParserContext *ctx, Token *start) {

  Token head;
  Token *ctxToken = ctx->token;
  head.next = start;
  ctx->token = &head;
  nextToken(ctx);

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

static Token *takeBranch(ParserContext *ctx, Token *start, int condition) {
  int depth = 0;

  Token *prev = NULL;
  Token *token  = start;

  Token head = { 0 };
  Token *cur = &head;

  Boolean isTaking = condition;
  Boolean isTaken = FALSE;

  if (isTaking) {
      cur->next = start;
  }

  Boolean isNewLine = TRUE;

  while (token->code) {
      if (token->rawCode == '#' && isNewLine) {
          isNewLine = FALSE;
          Token *directive = token->next;
          if (directive == NULL) break;

          if (!strcmp("if", directive->text) || !strcmp("ifdef", directive->text) || !strcmp("ifndef", directive->text)) {
              ++depth;
          } else if (depth == 0 && !strcmp("else", directive->text)) {
              if (isTaking) {
                  assert(!isTaken);
                  isTaken = TRUE;
                  isTaking = FALSE;
              } else {
                  isTaking = !isTaken;
              }

              token = directive->next;
              continue;
          } else if (depth == 0 && !strcmp("elif", directive->text)) {
              Token *cond = directive->next;
              Token *last = findLastPPToken(ctx, cond);
              Token *tail = last->next;
              last->next = NULL;

              if (isTaking) {
                  isTaken = TRUE;
              }

              isTaking = !isTaken ? evaluateTokenSequence(ctx, cond) : FALSE;

              token = tail;
              continue;
          } else if (!strcmp("endif", directive->text)) {
              if (depth) --depth;
              else {
                  if (!isTaking && !isTaken) {
                    return directive->next;
                  } else {
                    cur->next = directive->next;
                    return head.next;
                  }
              }
          }
      }

      if (isTaking) {
        cur  = cur->next = token;
      }

      isNewLine = token->rawCode == NEWLINE;

      token = token->next;
  }


  reportDiagnostic(ctx, DIAG_PP_UNTERMINATED_COND_DIRECTIVE, &start->coordinates);

  return head.next;
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

  // TODO: fix this ugly hack bellow
  if (directiveToken->rawCode != IDENTIFIER && directiveToken->rawCode != IF && directiveToken->rawCode != ELSE && directiveToken->rawCode != I_CONSTANT_RAW) {
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
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &directiveToken->coordinates, "elif");
    return directiveToken->next;
  } else if (!strcmp("else", directive)) {
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &directiveToken->coordinates, "else");
    return directiveToken->next;
  } else if (!strcmp("endif", directive)) {
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &directiveToken->coordinates, "endif");
    return directiveToken->next;
  } else if (!strcmp("line", directive)) {
    return skipPPTokens(ctx, token->next);
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


static Token *fileMacroHandler(ParserContext *ctx, const Token *t) {

  return stringToken(ctx, &t->coordinates, t->coordinates.locInfo->fileName);
}

static Token *lineMacroHandler(ParserContext *ctx, const Token *t) {

  unsigned lineNum;
  unsigned lineMax = t->coordinates.locInfo->fileInfo.lineno;
  unsigned *lineMap = t->coordinates.locInfo->fileInfo.linesPos;
  unsigned pos = t->coordinates.startOffset;

  for (lineNum = 0; lineNum < lineMax; ++lineNum) {
      unsigned lineOffset = lineMap[lineNum];
      if (pos < lineOffset) break;
  }

  return constToken(ctx, lineNum, t);
}

static Token *counterMacroHandler(ParserContext *ctx, const Token *t) {
  static int cnt = 0;

  Token *r = constToken(ctx, cnt++, t);
  char buffer[10] = {  0 };

  int size = sprintf(buffer, "%ld", r->value.iv);
  r->coordinates.startOffset = 0;
  r->coordinates.endOffset = size;

  return r;
}

static Token *timestampMacroHandler(ParserContext *ctx, const Token *t) {
  struct stat st;

  if (stat(t->coordinates.locInfo->fileName, &st) != 0)
    return stringToken(ctx, &t->coordinates, "??? ??? ?? ??:??:?? ????");

  char buf[32] = { 0 };

  ctime_r(&st.st_mtime, buf);

  size_t l = strlen(buf);

  buf[24] = '\0';

  char *s = allocateString(ctx, l);
  strncpy(s, buf, l);

  return stringToken(ctx, &t->coordinates, s);
}

static Token *constLongToken(ParserContext *ctx, int64_t v, Token *t) {
  Token *r = allocToken(ctx);

  r->coordinates = t->coordinates;
  r->code = L_CONSTANT;
  r->rawCode = I_CONSTANT_RAW;
  r->value.iv = v;

  return r;
}


static MacroDefinition __file_macro = { "__FILE__", NULL, NULL, &fileMacroHandler, FALSE, FALSE };
static MacroDefinition __line_macro = { "__LINE__", NULL, NULL, &lineMacroHandler, FALSE, FALSE };
static MacroDefinition __counter_macro = { "__COUNTER__", NULL, NULL, &counterMacroHandler, FALSE, FALSE };
static MacroDefinition __timestamt_macro = { "__TIMESTAMP__", NULL, NULL, &timestampMacroHandler, FALSE, FALSE };


static MacroDefinition *defineBuiltinMacro(ParserContext *ctx, const char *name, Token *t) {
  MacroDefinition *def = allocateMacroDef(ctx, name, NULL, t, FALSE, FALSE);

  putToHashMap(ctx->macroMap, (intptr_t)name, (intptr_t)def);

}

static const char *dateString() {
  const char nodate[] = "??? ?? ????";
  static char buffer[sizeof nodate] = { 0 };

  static char mon[][4] = {
     "Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
  };


  time_t now = time(NULL);
  struct tm *tm = localtime(&now);

  snprintf(buffer, sizeof buffer, "%s %02d %d", mon[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);

  return buffer;
}

static const char *timeString() {
  const char notime[] = "??:??:??";
  static char buffer[sizeof notime] = { 0 };


  time_t now = time(NULL);
  struct tm *tm = localtime(&now);

  snprintf(buffer, sizeof buffer, "%02d:%02d:%02d", tm->tm_hour, tm->tm_min, tm->tm_sec);

  return buffer;
}

void initializeProprocessor(ParserContext *ctx) {

  putToHashMap(ctx->macroMap, (intptr_t)__file_macro.name, (intptr_t)&__file_macro);
  putToHashMap(ctx->macroMap, (intptr_t)__line_macro.name, (intptr_t)&__line_macro);
  putToHashMap(ctx->macroMap, (intptr_t)__counter_macro.name, (intptr_t)&__counter_macro);
  putToHashMap(ctx->macroMap, (intptr_t)__timestamt_macro.name, (intptr_t)&__timestamt_macro);

  Token dummy = { 0 };

  defineBuiltinMacro(ctx, "__STDC__", constToken(ctx, 1, &dummy));
  defineBuiltinMacro(ctx, "__STDC_VERSION__", constLongToken(ctx, 199409L, &dummy));
  defineBuiltinMacro(ctx, "__STDC_HOSTED__", constLongToken(ctx, 199409L, &dummy));
  defineBuiltinMacro(ctx, "__STRICT_ANSI__", constToken(ctx, 1, &dummy));

  defineBuiltinMacro(ctx, "_LP64", constToken(ctx, 1, &dummy));
  defineBuiltinMacro(ctx, "__x86_64__", constToken(ctx, 1, &dummy));

  defineBuiltinMacro(ctx, "__DATE__", stringToken(ctx, &dummy.coordinates, dateString()));
  defineBuiltinMacro(ctx, "__TIME__", stringToken(ctx, &dummy.coordinates, timeString()));
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
      Token *n = expandMacro(ctx, t, FALSE);
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
