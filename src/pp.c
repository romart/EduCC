

#include <assert.h>
#include <stdio.h>
#include <time.h>
#include <sys/stat.h>
#include <libgen.h>
#include <unistd.h>
#include <linux/limits.h>

#include "pp.h"
#include "parser.h"
#include "tree.h"

int isspace(int c);
extern char *strdup (const char *__s);

extern char *ctime_r (const time_t *__timer, char *__buf);

static int counterState = 0;

static Boolean expandMacro(ParserContext *ctx, Token *macro, Token **next, Boolean evalDefined);

typedef struct _MacroParam {
  const char *name;

  unsigned isVararg : 1;

  struct _MacroParam *next;
} MacroParam;

typedef Token *(macroHandler)(ParserContext *, Token *);

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
  Token *evaluated;
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

  Token head = { 0 }, *current = &head;

  while (t) {
      current = current->next = copyToken(ctx, t);
      t = t->next;
  }

  return head.next;
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
  while (token) {
      if (token->startOfLine){
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
      } else if (!b->startOfLine) {
          strncpy(&buffer[ptr], b->pos, b->length);
          ptr += b->length;
          size -= b->length;
      } else {
          break;
      }
      b = b->next;
  }

  buffer[ptr] = '\0';
}

static Token *findLastPPToken(ParserContext *ctx, Token *token) {
  Token *cur = token, *last = token;

  for (; cur; cur = cur->next) {
      if (cur->startOfLine) {
          return last;
      }
      last = cur;
  }
  return last;
}

static Boolean isMacro(ParserContext *ctx, Token *id) {
  return isInHashMap(ctx->macroMap, (intptr_t)id->id);
}

static Boolean isVarargPosition(const Token *t) {
  return t->rawCode == IDENTIFIER && strcmp("__VA_ARGS__", t->id) == 0;
}

static Token *tokenizeString(ParserContext *ctx, Token *p, const char *buffer, size_t l, Boolean isConstantBuffer) {
  LocationInfo *info = allocateMacroLocationInfo(buffer, l, isConstantBuffer);

  info->next = ctx->locationInfo;
  ctx->locationInfo = info;

  unsigned pos;
  return tokenizeBuffer(ctx, info, &pos, NULL);
}

static Token *stringToken(ParserContext *ctx, Token *p, const char *s) {
  size_t l = strlen(s);
  char *buffer = heapAllocate(l + 3);
  buffer[0] = '"';
  strncpy(&buffer[1], s, l);
  buffer[l + 1] = '"';

  return tokenizeString(ctx, p, buffer, l + 3, FALSE);
}


Token *stringifySequence(ParserContext *ctx, Token *s) {
  Token *t = s, *p = NULL;

  size_t bufferSize = 0;

  Boolean first = TRUE;

  StringBuffer sb = { 0 };

  putSymbol(&sb, '"');
  Boolean isFirst = TRUE;

  while (t) {
      if (!isFirst && t->hasLeadingSpace) {
          putSymbol(&sb, ' ');
      }
      isFirst = FALSE;
      unsigned i = 0;
      for (; i < t->length; ++i) {
          char c = t->pos[i];
          if (c == '\\' || c == '"') {
             putSymbol(&sb, '\\');
          }
          putSymbol(&sb, c);
      }
      t = t->next;
  }
  putSymbol(&sb, '"');
  putSymbol(&sb, '\0');

  Token *r = tokenizeString(ctx, s, sb.ptr, sb.idx, FALSE);
  r->startOfLine = r->hasLeadingSpace = 0;
  r->macroStringitize = 1;

  return r;
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
  strncpy(buffer, t->pos, t->length);
  return t->length;
}

Token *concatTokens(ParserContext *ctx, Token *macro, Token *l, Token *r) {

  size_t bufferSize = 0;

  const char *lp, rp;

  Token *pl = NULL;
  Token *t = l;

  Token *left = l;

  while (t->next) {
      pl = t;
      t = t->next;
  }

  // find most right token of left sequence
  l = t;

  bufferSize += l->length;
  bufferSize += r->length;
  bufferSize += 1;

  char *buffer = heapAllocate(bufferSize);

  unsigned i = 0, j = 0;

  j += renderTokenToBuffer(&buffer[j], l);
  j += renderTokenToBuffer(&buffer[j], r);

  buffer[j++] = 0;

  assert(j == bufferSize);

  Token *s = tokenizeString(ctx, macro, buffer, bufferSize, FALSE);

  s->hasLeadingSpace = l->hasLeadingSpace;
  s->startOfLine = l->startOfLine;

  if (pl) {
      pl->next = s;
  }

  s->next = r->next;

  return s;
}

static MacroArg *findArgument(const Token *arg, MacroArg *args) {
  if (arg->rawCode != IDENTIFIER) return NULL;

  while (args) {
    if (args->param && strcmp(args->param->name, arg->id) == 0) {
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

static Token* expandSequence(ParserContext *ctx, Token* s, Boolean evalDefined) {
  Token *t = s;

  Token head = { 0 }, *current = &head;

  while (t) {
      if (expandMacro(ctx, t, &t, evalDefined)) continue;
      current = current->next = t;
      t = t->next;
  }

  return head.next;
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

static Token *constToken(ParserContext *ctx, int64_t v, Token *token) {
  char buffer[32] = { 0 };
  size_t l = sprintf(buffer, "%ld", v);

  char *b = heapAllocate(l + 1);
  memcpy(b, buffer, l);

  return tokenizeString(ctx, token, b, l + 1, FALSE);
}

static Token *evaluateDefinedOp(ParserContext *ctx, Token *token, Boolean relaxed, Token **next) {
  assert(token);
  assert(token->rawCode == IDENTIFIER);
  assert(strcmp("defined", token->id) == 0);

  Token *n = token->next;
  if (n && n->rawCode == '(') {
      Token *id = n->next;
      if (id && id->rawCode == IDENTIFIER) {
          Token *nn = id->next;
          if (nn && nn->rawCode == ')') {
              Boolean isDefined = isMacro(ctx, id);
              *next = nn->next;
              Token *r = tokenizeString(ctx, token, isDefined ? "1" : "0", 2, TRUE);
              r->next = nn;
              return r;
          }
      }
  } else if (n && n->rawCode == IDENTIFIER) {
    Boolean isDefined = isMacro(ctx, n);
    *next = n->next;
    Token *r = tokenizeString(ctx, token, isDefined ? "1" : "0", 2, TRUE);
    r->hasLeadingSpace = token->hasLeadingSpace;
    r->startOfLine = token->startOfLine;
    r->next = n;
    return r;
  }

  if (!relaxed) {
    Coordinates coords;
    coords.left = coords.right = n ? n : token;
    reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &coords);
    *next = n;
    Token *r = tokenizeString(ctx, token, "0", 2, TRUE);
    r->hasLeadingSpace = token->hasLeadingSpace;
    r->startOfLine = token->startOfLine;
    r->next = n;
    return r;
  }

  return NULL;
}

static Boolean expandMacro(ParserContext *ctx, Token *macro, Token **next, Boolean evalDefined) {

  if (macro->rawCode != IDENTIFIER) return FALSE;

  MacroDefinition *def = (MacroDefinition *)getFromHashMap(ctx->macroMap, (intptr_t)macro->id);

  if (def == NULL) {
      if (evalDefined && strcmp("defined", macro->id) == 0) {
        Token *next2;
        Token *e = evaluateDefinedOp(ctx, macro, TRUE, &next2);
        if (e) {
          e->next = next2;
          *next = e;
          return TRUE;
        }
      }
      return FALSE;
  }

  if (def->handler) {
      Token *r = def->handler(ctx, macro);
      r->next = macro->next;
      r->startOfLine = macro->startOfLine;
      r->hasLeadingSpace = macro->hasLeadingSpace;

      *next = r;
      return TRUE;
  }

  if (hidesetContains(macro->hs, def->name)) return FALSE;

  Coordinates macroCoords = { macro, macro };
  Token *n = macro->next;
  Token *macroNext = macro->next;

  if (def->isFunctional && (!n || n->rawCode != '(')) {
    // #define f(x) x
    // b = foo 10
    // b = foo <EOF>

    return FALSE;
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
          reportDiagnostic(ctx, DIAG_PP_TOO_FEW_ARGUMENTS, &macroCoords);
      }

      if (n && n->rawCode != ')') {
          if (!p) {
              Coordinates argCoords = { argStart, argStart };
              reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &argCoords);
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

      if (evalDefined && b->rawCode == IDENTIFIER && strcmp("defined", b->id) == 0) {
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
            Token *evaluated = argValue ? stringifySequence(ctx, argValue) : stringToken(ctx, next, "");
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
              evaluated = concatTokens(ctx, macro, lhs, rhs);
          }

          evalCur->next = evaluated;
          evalCur = findLastToken(evaluated);

          b = origRhs->next;
          continue;
        } else {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macroCoords);
            break;
        }
      }
      if (b->rawCode == DSHARP) {
          // a##b##c
          Token *next = b->next;

          if (next == NULL) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macroCoords);
            break;
          }

          if (b == body) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &macroCoords);
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
              evaluated = concatTokens(ctx, macro, lhs, rhs);
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
        if (arg->evaluated) {
          // prevent multiple evaluation of the same argument
          // see pp-counter.c test for more details
          evaluated = copySequence(ctx, arg->evaluated);
        } else if (arg->value) {
          evaluated = expandSequence(ctx, argValue, evalDefined);
          evaluated->hasLeadingSpace = b->hasLeadingSpace;
          evaluated->startOfLine = b->startOfLine;
          arg->evaluated = copySequence(ctx, evaluated);
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
    Token *t, *p;
    for (t = evalHead.next; t; t = t->next) {
        t->expanded = macro;
        p = t;
    }
    p->next = macroNext;
    evalHead.next->hasLeadingSpace = macro->hasLeadingSpace;
    evalHead.next->startOfLine = macro->startOfLine;
    *next = evalHead.next;
  } else {
    *next = macroNext;
  }

  return TRUE;
}

static const char *findIncludePath(ParserContext *ctx, Token *include, const char *includeName, Boolean isdquoted) {
  if (isdquoted && includeName[0] != '/') {
      Token *original = originaToken(include);
      assert(original->locInfo && original->locInfo->kind == LIK_FILE);
      char *copy = strdup(original->locInfo->fileInfo.fileName);
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
  Coordinates coords = { token, token };
  if (token->rawCode == STRING_LITERAL) {
    fileName = token->value.text;
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

    joinToString(b, sizeof b, token->next, tmp);

    fileName = b;

    tail = skipPPTokens(ctx, last->next);
  } else if (token->rawCode == IDENTIFIER) {
    if (isMacro(ctx, token)) {
      Token *rToken = NULL;
      expandMacro(ctx, token, &rToken, FALSE);
      return parseInclude(ctx, rToken);
    } else {
      reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &coords);
      return token;
    }
  } else {
    reportDiagnostic(ctx, DIAG_EXPECTED_FILENAME, &coords);
    return token;
  }

  fileName = findIncludePath(ctx, token, fileName, dquoted);

  Token eof = { 0 };
  Token *includeTokens = fileName ? tokenizeFile(ctx, fileName, &eof) : NULL;
  if (includeTokens == NULL) {
    reportDiagnostic(ctx, DIAG_INCLUDE_FILE_NOT_FOUND, &coords, fileName);
    return tail;
  }

  Token *t = includeTokens, *p = NULL;
  while (t->rawCode) {
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

static const char defaultVarargName[] = "__VA_ARGS__";

static MacroParam *parseVarargParam(ParserContext *ctx, Token *n, Token **next) {

  Token *elipsis = n;
  const char *varargName = defaultVarargName;
  size_t nameSize = sizeof(defaultVarargName) - 1;

  if (n->rawCode == IDENTIFIER) {
      varargName = n->id;
      elipsis = n->next;
      assert(elipsis && elipsis->rawCode == ELLIPSIS);
  }

  MacroParam* p = allocateMacroParam(ctx, varargName);
  p->isVararg = 1;

  Token *nn = elipsis->next;
  if (!nn || nn->rawCode != ')') {
    Coordinates coords = { nn, nn };
    reportDiagnostic(ctx, DIAG_PP_MISSING_PAREN_IN_PARAMS, &coords);
  }

  while (n && n->rawCode != ')') n = n->next;

  *next = n;

  return p;
}

static Boolean cmpMacroses(MacroDefinition *old, MacroDefinition *new) {
  if (old->isFunctional != new->isFunctional) return TRUE;
  if (old->isVararg != new->isVararg) return TRUE;
  if (old->body && !new->body || !old->body && new->body) return TRUE;

  if (old->isFunctional) {
      MacroParam *oldP = old->params;
      MacroParam *newP = new->params;
      while (oldP && newP) {
          if (oldP->isVararg != new->isVararg) return TRUE;
          if (strcmp(oldP->name, newP->name)) return TRUE;
          oldP = oldP->next;
          newP = newP->next;
      }

      if (oldP != newP) return TRUE;
  }

  Token *oldB = old->body;
  Token *newB = new->body;

  const char *oldPtr = NULL;
  const char *newPtr = NULL;

  while (oldB && newB) {
      if (oldB->hasLeadingSpace != newB->hasLeadingSpace) return TRUE;
      if (oldB->startOfLine != newB->startOfLine) return TRUE;

      if (oldB->length != newB->length) return TRUE;

      if (oldPtr) {
          // #define x(y) y + y
          //   vs
          // #define x(y) y   +   y
          // gcc/clang say they are different
          ptrdiff_t oldD = oldB->pos - oldPtr;
          ptrdiff_t newD = newB->pos - newPtr;

          if (oldD != newD) return TRUE;
      }

      if (strncmp(oldB->pos, newB->pos, newB->length)) return TRUE;

      oldPtr = oldB->pos + oldB->length;
      newPtr = newB->pos + newB->length;

      oldB = oldB->next;
      newB = newB->next;
  }

  return oldB != newB;
}

static Token *defineMacro(ParserContext *ctx, Token *token) {

  Coordinates coords = { token, token };

  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &coords);
      return token;
  }

  if (strcmp("defined", token->id) == 0) {
      reportDiagnostic(ctx, DIAG_PP_DEFINED_NOT_A_NAME, &coords);
      return token;
  }

  Token *last = findLastPPToken(ctx, token);

  Token *tail = last->next;
  last->next = NULL;

  const char *macroName = token->id;

  Token *n = token->next;
  Token *body = NULL;

  MacroParam head = { };
  MacroParam *cur = &head;

  Boolean isVarags = FALSE;
  Boolean isFunctional = FALSE;

  if (n && n->rawCode == '(' && !n->hasLeadingSpace) { // it's functional macro

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
            MacroParam *tmp = allocateMacroParam(ctx, n->id);

            cur = cur->next = tmp;

            Token *nn = n->next;
            if (!nn || nn->rawCode != ',' && nn->rawCode != ')') {
                coords.left = coords.right = nn;
                reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &coords);
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
        coords.left = coords.right = n;
        reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &coords);
      }
      n = n->next;
    }
  } else {
      body = n;
  }


  if (body) {
    body->startOfLine = 0;
    body->hasLeadingSpace = 0;
  }

  MacroDefinition *def = allocateMacroDef(ctx, macroName, head.next, body, isVarags, isFunctional);

  intptr_t old = putToHashMap(ctx->macroMap, (intptr_t)macroName, (intptr_t)def);
  if (old) {
      if (cmpMacroses((MacroDefinition*)old, def)) {
        reportDiagnostic(ctx, DIAG_PP_MACRO_REDEFINED, &coords, macroName);
      }
  }

  return tail;
}


static Token *undefMacro(ParserContext *ctx, Token *token) {
    if (token->rawCode != IDENTIFIER) {
        Coordinates coords = { token, token };
        reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &coords);
    } else {
        removeFromHashMap(ctx->macroMap, (intptr_t)token->id);
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
      if (token->rawCode == IDENTIFIER && strcmp("defined", token->id) == 0) {
          cur = cur->next = evaluateDefinedOp(ctx, token, FALSE, &token);
          continue;
      } else if (token->rawCode == IDENTIFIER) {
          Token *o = token;

          if (!expandMacro(ctx, token, &token, TRUE)) {
              // https://gcc.gnu.org/onlinedocs/cpp/If.html#If
              // Identifiers that are not macros, which are all considered to be the number zero.
              cur = cur->next = constToken(ctx, 0, token);
              token = token->next;
          }
          continue;
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

  while (token->rawCode) {
      if (token->rawCode == '#' && token->startOfLine) {
          Token *directive = token->next;
          if (directive == NULL) break;

          if (!strcmp("if", directive->id) || !strcmp("ifdef", directive->id) || !strcmp("ifndef", directive->id)) {
              ++depth;
          } else if (depth == 0 && !strcmp("else", directive->id)) {
              Token *last = findLastPPToken(ctx, directive);
              if (last != directive) {
                  Coordinates coords = { directive->next, last };
                  reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, "else");
                  // report warning
              }
              if (isTaking) {
                  assert(!isTaken);
                  isTaken = TRUE;
                  isTaking = FALSE;
              } else {
                  isTaking = !isTaken;
              }

              token = last->next;
              continue;
          } else if (depth == 0 && !strcmp("elif", directive->id)) {
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
          } else if (!strcmp("endif", directive->id)) {
              if (depth) --depth;
              else {
                  Token *last = findLastPPToken(ctx, directive);
                  if (last != directive) {
                      Coordinates coords = { directive->next, last };
                      reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, "endif");
                      // report warning
                  }
                  if (!isTaking && !isTaken) {
                    return last->next;
                  } else {
                    cur->next = last->next;
                    return head.next;
                  }
              }
          }
      }

      if (isTaking) {
        cur  = cur->next = token;
      }

      token = token->next;
  }


  Coordinates coords = { start, start };
  reportDiagnostic(ctx, DIAG_PP_UNTERMINATED_COND_DIRECTIVE, &coords);

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
      Coordinates coords = { token, token };
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &coords);
      return token ? token->next : token;
  }

  Boolean isDefined = isMacro(ctx, token);

  return takeBranch(ctx, token->next, isDefined);
}

static Token *ifndef(ParserContext *ctx, Token *token) {
  if (token->rawCode != IDENTIFIER) {
      Coordinates coords = { token, token };
      reportDiagnostic(ctx, DIAG_MACRO_NAME_IS_ID, &coords);
      return token ? token->next : token;
  }

  Boolean isDefined = isMacro(ctx, token);

  return takeBranch(ctx, token->next, !isDefined);
}

static Token *_else(ParserContext *ctx, Token *token) {
  Coordinates coords = { token, token };
  reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "else");
  return token;
}

static Token *endif(ParserContext *ctx, Token *token) {
  Coordinates coords = { token, token };
  reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "endif");
  return token;
}

static Token *diagnosticDirective(ParserContext *ctx, Token *start, enum DiagnosticId id) {
  Token *last = findLastPPToken(ctx, start->next);

  Token *next = last->next;
  last->next = NULL;

  Token *messageToken = expandSequence(ctx, start->next, FALSE);

  const char *message = joinToStringTokenSequence(ctx, messageToken);
  Coordinates coords = { start->next, last };

  reportDiagnostic(ctx, id, &coords, message);

  return next;
}

static LineChunk *newLineChunk(const char *file, unsigned newLineNumber, Token *orig) {
  assert(orig->expanded == NULL);

  unsigned rawLine = tokenRawLine(orig);

  LineChunk *newChunk = heapAllocate(sizeof(LineChunk));

  newChunk->overrideFileName = file;
  newChunk->overrideLineNumber = newLineNumber;
  newChunk->posLineNumber = rawLine;

  return newChunk;
}

static Token *lineDirective(ParserContext *ctx, Token *start) {
  Token *last = findLastPPToken(ctx, start->next);

  Token *next = last->next;
  last->next = NULL;

  Token *expanded = expandSequence(ctx, start->next, FALSE);

  assert(start->rawCode == '#');
  Token *directive = start->next;
  assert(directive->rawCode == IDENTIFIER);

  Token *lineNum = directive->next;

  if (lineNum == NULL) {
      Coordinates coords = { start, directive };
      reportDiagnostic(ctx, DIAG_PP_LINE_UNEXPECTED_EOL, &coords);
      return next;
  }

  if (lineNum->rawCode != I_CONSTANT_RAW) {
      Coordinates coords = { lineNum, lineNum };
      char *copy = strndup(lineNum->pos, lineNum->length);
      reportDiagnostic(ctx, DIAG_PP_LINE_NOT_POSITIVE_INT, &coords, copy);
      free(copy);
      return next;
  }

  unsigned newLineNumber = lineNum->value.iv;

  Token *fileName = lineNum->next;
  const char *newFileName = NULL;
  if (fileName) {
      if (fileName->rawCode != STRING_LITERAL) {
          Coordinates coords = { fileName, fileName };
          char *copy = strndup(fileName->pos, fileName->length);
          reportDiagnostic(ctx, DIAG_PP_LINE_NOT_FILE_NAME, &coords, copy);
          free(copy);
          return next;
      }
      newFileName = fileName->value.text;
      if (fileName->next) {
          Coordinates coords = { fileName->next, fileName->next };
          char *copy = strndup(fileName->next->pos, fileName->next->length);
          reportDiagnostic(ctx, DIAG_PP_LINE_EXTRA_TOKEN_AT_END, &coords, copy);
          free(copy);
      }
  }


  Token *original = originaToken(directive);
  LocationInfo *locInfo = original->locInfo;

  assert(locInfo->kind == LIK_FILE);

  LineChunk *chunk = newLineChunk(newFileName, newLineNumber, original);
  chunk->next = locInfo->fileInfo.chunks;
  locInfo->fileInfo.chunks = chunk;

  return next;
}

static Token *handleDirecrive(ParserContext *ctx, Token *token) {

  unsigned bOffset = 0;

  assert(token->rawCode == '#');

  Token *directiveToken = token->next;

  // TODO: fix this ugly hack bellow
  Coordinates coords = { directiveToken, directiveToken };
  if (directiveToken->rawCode != IDENTIFIER && directiveToken->rawCode != I_CONSTANT_RAW) {
    reportDiagnostic(ctx, DIAG_INVALID_PP_DIRECTIVE, &coords, directiveToken);
    return ctx->token = directiveToken;
  }

  if (directiveToken->rawCode == I_CONSTANT_RAW) {
      return ctx->token = skipPPTokens(ctx, directiveToken);
  }

  const char *directive = directiveToken->id;

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
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "elif");
    return directiveToken->next;
  } else if (!strcmp("else", directive)) {
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "else");
    return directiveToken->next;
  } else if (!strcmp("endif", directive)) {
    reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "endif");
    return directiveToken->next;
  } else if (!strcmp("line", directive)) {
    return lineDirective(ctx, token);
  } else if (!strcmp("error", directive)) {
    return diagnosticDirective(ctx, token, DIAG_PP_ERROR);
  } else if (!strcmp("warning", directive)) {
    return diagnosticDirective(ctx, token, DIAG_PP_WARNING);
  } else if (!strcmp("pragma", directive)) {
    // TODO
    return skipPPTokens(ctx, token->next);
  } else {
    reportDiagnostic(ctx, DIAG_INVALID_PP_DIRECTIVE, &coords, directiveToken);
    return directiveToken;
  }
}

Token *originaToken(Token *t) {
  while (t->expanded) {
      t = t->expanded;
  }

  return t;
}


static Token *fileMacroHandler(ParserContext *ctx, Token *t) {

  Token *origToken = originaToken(t);

  unsigned line = 0;
  const char *file = NULL;

  fileAndLine(origToken, &line, &file);

  return stringToken(ctx, t, file);
}

static Token *lineMacroHandler(ParserContext *ctx, Token *t) {

  t = originaToken(t);

  unsigned lineNum = 0;
  const char *file = NULL;

  fileAndLine(t, &lineNum, &file);

  char b[20] = { 0 };
  size_t l = sprintf(b, "%u", lineNum);

  char *b2 = heapAllocate(l + 1);
  memcpy(b2, b, l);

  return tokenizeString(ctx, t, b2, l + 1, FALSE);
}

static Token *counterMacroHandler(ParserContext *ctx, Token *t) {
  static int cnt = 0;

  char buffer[10] = { 0 };
  size_t l = sprintf(buffer, "%u", cnt++);

  char *b2 = heapAllocate(l + 1);
  memcpy(b2, buffer, l);

  return tokenizeString(ctx, t, b2, l + 1, FALSE);
}

static Token *timestampMacroHandler(ParserContext *ctx, Token *t) {
  struct stat st;

  LocationInfo *info = originaToken(t)->locInfo;

  if (stat(info->fileInfo.fileName, &st) != 0)
    return stringToken(ctx, t, "??? ??? ?? ??:??:?? ????");

  char buf[32] = { 0 };

  ctime_r(&st.st_mtime, buf);

  size_t l = strlen(buf);

  buf[24] = '\0';

  return stringToken(ctx, t, buf);
}

static Token *constLongToken(ParserContext *ctx, int64_t v, Token *t) {
  Token *r = allocToken(ctx);

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
  static char buffer[13] = { 0 };

  static char mon[][4] = {
     "Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
  };


  time_t now = time(NULL);
  struct tm *tm = localtime(&now);

  snprintf(buffer, 12, "%s %02d %d", mon[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
  buffer[12] = '\0';

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

  defineBuiltinMacro(ctx, "__STDC__", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__STDC_VERSION__", constToken(ctx, 199409L, NULL));
  defineBuiltinMacro(ctx, "__STDC_HOSTED__", constToken(ctx, 199409L, NULL));
  defineBuiltinMacro(ctx, "__STRICT_ANSI__", constToken(ctx, 1, NULL));

  defineBuiltinMacro(ctx, "_LP64", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__x86_64__", constToken(ctx, 1, NULL));

  defineBuiltinMacro(ctx, "__DATE__", stringToken(ctx, NULL, dateString()));
  defineBuiltinMacro(ctx, "__TIME__", stringToken(ctx, NULL, timeString()));
}

Token *preprocessFile(ParserContext *ctx, Token *s, Token *tail) {
  Token head = { 0 }, *current = &head;
  Token *t = s, *p = NULL;

  while (t) {
    if (t->rawCode == '#' && t->startOfLine) {
      t = handleDirecrive(ctx, t);
      continue;
    } else if (expandMacro(ctx, t, &t, FALSE)) {
      continue;
    }

    current = current->next = t;
    t = t->next;
  }

  current->next = tail;

  return head.next;
}
