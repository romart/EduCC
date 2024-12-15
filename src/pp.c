

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

MacroDefinition *allocateMacroDef(ParserContext *ctx, const char *name, MacroParam *params, Token *body, Boolean isVararg, Boolean isFunc) {
  MacroDefinition *d = areanAllocate(ctx->memory.macroArena, sizeof(MacroDefinition));

  d->name = name;
  d->params = params;
  d->body = body;
  d->isVararg = isVararg;
  d->isFunctional = isFunc;
  d->isEnabled = 1;

  return d;
}

typedef struct _MacroArg {
  MacroParam *param;
  Token *value;
  Token *evaluated;
  struct _MacroArg *next;
} MacroArg;

MacroParam *allocateMacroParam(ParserContext *ctx, const char *name) {
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

Boolean isMacro(ParserContext *ctx, Token *id) {
  return isInHashMap(ctx->macroMap, (intptr_t)id->id);
}

MacroDefinition *findMacro(ParserContext *ctx, const char *name) {
  return (MacroDefinition*)getFromHashMap(ctx->macroMap, (intptr_t)name);
}

static Token *tokenizeString(ParserContext *ctx, Token *p, const char *buffer, size_t l, Boolean isConstantBuffer) {
  LocationInfo *info = allocateMacroLocationInfo(buffer, l, isConstantBuffer);

  LexerState *oldState = ctx->lexerState;

  info->next = ctx->locationInfo;
  ctx->locationInfo = info;
  ctx->lexerState = allocateFileLexerState(info);

  Token head = { 0 };
  Token *current = &head;

  for (;;) {
      current = current->next = lexNonExpand(ctx, TRUE);

      if (current->rawCode == END_OF_FILE) break;
  }

  ctx->lexerState = oldState;

  return head.next;
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
          if (c == '"') {
              putSymbol(&sb, '\\');
          } else if (c == '\\' && t->rawCode != '\\') {
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

  for (l = s; l->next->rawCode != END_OF_FILE; l = l->next)
    ;

  l->next = r->next;

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
    reportDiagnostic(ctx, DIAG_PP_MACRO_NAME_IS_ID, &coords);
    *next = n;
    Token *r = tokenizeString(ctx, token, "0", 2, TRUE);
    r->hasLeadingSpace = token->hasLeadingSpace;
    r->startOfLine = token->startOfLine;
    r->next = n;
    return r;
  }

  return NULL;
}

static const char defaultVarargName[] = "__VA_ARGS__";

Boolean cmpMacroses(MacroDefinition *old, MacroDefinition *new) {
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
          // gcc/clang says they are different
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

static Token *eofToken(ParserContext *ctx) {
  Token *t = allocToken(ctx);
  t->code = t->rawCode = END_OF_FILE;

  return t;
}


static AstExpression *parsePPExpression(ParserContext *ctx, Token *start) {

  Token head;

  ctx->stateFlags.inPP = 1;

  head.next = start;
  ctx->token = &head;
  nextToken(ctx);

  AstExpression *expr = parseConditionalExpression(ctx);

  ctx->token = NULL;

  ctx->stateFlags.inPP = 0;

  return expr;
}

Token *originalToken(Token *t) {
  while (t->expanded) {
      t = t->expanded;
  }

  return t;
}

static Token *fileMacroHandler(ParserContext *ctx, Token *t) {

  Token *origToken = originalToken(t);

  unsigned line = 0;
  const char *file = NULL;

  fileAndLine(origToken, &line, &file);

  return stringToken(ctx, t, file);
}

static Token *lineMacroHandler(ParserContext *ctx, Token *t) {

  t = originalToken(t);

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

  LocationInfo *info = originalToken(t)->locInfo;

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


static MacroDefinition __file_macro = { "__FILE__", NULL, NULL, &fileMacroHandler, 0, 0, 1, 0 };
static MacroDefinition __line_macro = { "__LINE__", NULL, NULL, &lineMacroHandler, 0, 0, 1, 0 };
static MacroDefinition __counter_macro = { "__COUNTER__", NULL, NULL, &counterMacroHandler, 0, 0, 1, 0 };
static MacroDefinition __timestamt_macro = { "__TIMESTAMP__", NULL, NULL, &timestampMacroHandler, 0, 0, 1, 0 };


static MacroDefinition *defineBuiltinMacro(ParserContext *ctx, const char *name, Token *t) {
  MacroDefinition *def = allocateMacroDef(ctx, name, NULL, t, FALSE, FALSE);
  def->isEnabled = 1;

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

static const char *quoteBody(const char *b) {
  StringBuffer sb = { 0 };

  unsigned idx = 1;
  unsigned l = strlen(b);

  putSymbol(&sb, '"');

  for (; idx < l; ++idx) {
      char c = b[idx++];
      if (c == '"' || c == '\\') putSymbol(&sb, '\\');
      putSymbol(&sb, c);
  }

  if (b[idx] != '"') {
      // error
  }

  putSymbol(&sb, '"');
  putSymbol(&sb, '\0');

  return sb.ptr;
}

static void defineCLIMacro(ParserContext *ctx, const char *s) {
  unsigned idx = 0;

  const char *macroName = s;
  const char *macroBody = NULL;

  for (; s[idx]; ++idx) {
      if (s[idx] == '=') {
          char *b = allocateString(ctx, idx);
          macroBody = &s[idx + 1];
          strncpy(b, s, idx);
          macroName = b;
          break;
      }
  }

  Token *body = NULL;
  if (macroBody) {
      if (s[0] == '"') {
          const char *quoted = quoteBody(macroBody);
          body = tokenizeString(ctx, NULL, quoted, strlen(quoted) + 1, FALSE);
      } else {
          body = tokenizeString(ctx, NULL, macroBody, strlen(macroBody) + 1, TRUE);
      }
  } else {
      body = constToken(ctx, 1, NULL);
  }

  defineBuiltinMacro(ctx, macroName, body);
}

void initializeProprocessor(ParserContext *ctx) {

  counterState = 0;

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
  defineBuiltinMacro(ctx, "__LP64__", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__x86_64", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__x86_64__", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__amd64", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__amd64__", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__linux", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__linux__", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__unix", constToken(ctx, 1, NULL));
  defineBuiltinMacro(ctx, "__unix__", constToken(ctx, 1, NULL));

  defineBuiltinMacro(ctx, "__SIZEOF_DOUBLE__", constToken(ctx, 8, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_FLOAT__", constToken(ctx, 4, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_INT__", constToken(ctx, 4, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_LONG_DOUBLE__", constToken(ctx, 16, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_LONG_LONG__", constToken(ctx, 8, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_LONG__", constToken(ctx, 8, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_POINTER__", constToken(ctx, 8, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_PTRDIFF_T__", constToken(ctx, 8, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_SHORT__", constToken(ctx, 2, NULL));
  defineBuiltinMacro(ctx, "__SIZEOF_SIZE_T__", constToken(ctx, 8, NULL));
  static const char ul[] = "unsigned long";
  defineBuiltinMacro(ctx, "__SIZE_TYPE__", tokenizeString(ctx, NULL, ul, sizeof (ul), TRUE));

  defineBuiltinMacro(ctx, "__DATE__", stringToken(ctx, NULL, dateString()));
  defineBuiltinMacro(ctx, "__TIME__", stringToken(ctx, NULL, timeString()));

  StringList *m = ctx->config->macroses;
  for (; m; m = m->next) {
      defineCLIMacro(ctx, m->s);
  }
}

// returns the file being parsed
static const char *getFileName(ParserContext *ctx) {
  LexerState *lex = ctx->lexerState;

  while (lex->state != LS_FILE) lex = lex->virtPrev;

  assert(lex);
  return lex->fileContext.locInfo->fileInfo.fileName;
}

// returns the file being compiled
static const char *getBaseFileName(ParserContext *ctx) {
  LexerState *lex = ctx->lexerState;

  while (lex->virtPrev) lex = lex->virtPrev;

  assert(lex->state == LS_FILE);
  return lex->fileContext.locInfo->fileInfo.fileName;
}

static const char *findIncludePath(ParserContext *ctx, const char *includeName, Boolean dquoted) {
  if (dquoted && includeName[0] != '/') {
    const char *baseFileName = getFileName(ctx);
    char *copy = strdup(baseFileName);
    char *dir = dirname(copy);
    size_t l = strlen(dir) + 1 + strlen(includeName) + 1;
    char *path = heapAllocate(l);
    snprintf(path, l, "%s/%s", dir, includeName);
    free(copy);
    if (access(path, F_OK) == 0) {
        char *result = allocateString(ctx, l);
        strncpy(result, path, l);
        releaseHeap(path);
        return result;
    }
    releaseHeap(path);
  }

  if (includeName[0] == '/') return includeName;

  IncludePath *includePath = ctx->config->includePath;
  static char pathBuffer[PATH_MAX] = { 0 };

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

static void handleIncludeDirective(ParserContext *ctx, Token *d) {
  LocationInfo *locInfo = ctx->lexerState->fileContext.locInfo;
  Token *next = lexTokenNoSubstitute(ctx);
  Boolean dquoted = FALSE, err = FALSE;
  const char *fileName = NULL;
  char b[1024] = { 0 };
  Coordinates coords = { d, next };

  if (next->rawCode == STRING_LITERAL) {
     fileName = next->value.text.v;
     skipUntilEoL(ctx);
     dquoted = TRUE;
  } else if (next->rawCode == '<') {
      next = lexTokenNoSubstitute(ctx);
      size_t ptr = 0;
      size_t size = sizeof b / sizeof b[0];
      while (ptr < size && next->rawCode != '>' && next->rawCode != NEWLINE) {
        if (next->rawCode != '>') {
          memcpy(b + ptr, next->pos, next->length);
          ptr += next->length;
        }
        next = lexTokenNoSubstitute(ctx);
      }

      if (next->rawCode != '>') {
          reportDiagnostic(ctx, DIAG_PP_EXPECTED_FILENAME, &coords);
          err = TRUE;
      }

      if (ptr >= size) {
          reportDiagnostic(ctx, DIAG_PP_FILE_NAME_TOO_LONG, &coords);
          err = TRUE;
      }

      skipUntilEoL(ctx);

      if (err) return;

      fileName = b;
  } else if (next->rawCode == IDENTIFIER) {
      // include name might be substituted as well
      Token *r = handleIdentifier(ctx, next);
      if (!r) return handleIncludeDirective(ctx, d);
      coords.left = coords.right = next;
      reportDiagnostic(ctx, DIAG_PP_EXPECTED_FILENAME, &coords);
      skipUntilEoL(ctx);
      return;
  } else {
      reportDiagnostic(ctx, DIAG_PP_EXPECTED_FILENAME, &coords);
      skipUntilEoL(ctx);
      return;
  }

  const char *includePath = findIncludePath(ctx, fileName, dquoted);

  if (includePath == NULL) {
      reportDiagnostic(ctx, DIAG_PP_INCLUDE_FILE_NOT_FOUND, &coords, fileName);
      return;
  }

  if (isInHashMap(ctx->pragmaOnceMap, (intptr_t)includePath)) {
      return;
  }

  LexerState *newLex = loadFile(includePath, ctx->lexerState);

  if (newLex == NULL) {
      reportDiagnostic(ctx, DIAG_PP_INCLUDE_FILE_NOT_FOUND, &coords, includePath);
      return;
  }

  newLex->fileContext.locInfo->next = ctx->locationInfo;
  ctx->locationInfo = newLex->fileContext.locInfo;
  ctx->lexerState = newLex;
}

static Boolean checkIsMacroParam(MacroParam *params, Token *t) {
   if (t->rawCode != IDENTIFIER) return FALSE;

   while (params) {
       if (!strcmp(params->name, t->id)) {
           return TRUE;
       }
       params = params->next;
   }

   return FALSE;
}

static MacroParam *parseVarargParam(ParserContext *ctx, const char *name) {
  MacroParam *p = NULL;
  Token *nn = lexTokenNoSubstitute(ctx);

  if (nn->rawCode != ')') {
    Coordinates coords = { nn, nn };
    reportDiagnostic(ctx, DIAG_PP_MISSING_PAREN_IN_PARAMS, &coords);
  } else {
    p = allocateMacroParam(ctx, name);
    p->isVararg = 1;
  }

  return p;
}

static MacroParam *parseMacroParam(ParserContext *ctx, Boolean *isVararg, Boolean *hasError) {
  Token *t = lexTokenNoSubstitute(ctx);
  MacroParam head = { 0 };
  MacroParam *cur = &head;

  Coordinates coords = { 0 };

  while (t->rawCode != ')') {
      if (t->rawCode == IDENTIFIER) {
          Token *n = lexTokenNoSubstitute(ctx);
          if (n->rawCode == ELLIPSIS) {
              *isVararg = TRUE;
              MacroParam *p = parseVarargParam(ctx, t->id);
              if (!p) *hasError = TRUE;
              cur = cur->next = p;
              break;
          } else {

              MacroParam *p = allocateMacroParam(ctx, t->id);
              cur = cur->next = p;

              if (n->rawCode != ',' && n->rawCode != ')') {
                  coords.left = coords.right = n;
                  reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &coords);
                  *hasError = TRUE;
                  return NULL;
              }
              t = n;
          }
      } else if (t->rawCode == ELLIPSIS) {
          *isVararg = TRUE;
          MacroParam *p = parseVarargParam(ctx, defaultVarargName);
          if (!p) *hasError = TRUE;
          cur = cur->next = p;
          break;
      } else {
          coords.left = coords.right = t;
          reportDiagnostic(ctx, DIAG_PP_INVALID_TOKEN_MACRO_PARAM, &coords);
          *hasError = TRUE;
          return NULL;
      }
      if (t->rawCode == ',') {
        t = lexTokenNoSubstitute(ctx);
      }
  }

  return head.next;
}

static MacroDefinition *parseMacro(ParserContext *ctx, const char *name) {

  Boolean isVararg = FALSE;
  Boolean isFunctional = FALSE;
  Boolean hasError = FALSE;
  MacroParam *p = NULL;

  Token *token = lexTokenNoSubstitute(ctx);

  if (token->rawCode == '(' && !token->hasLeadingSpace) {
    isFunctional = TRUE;
    p = parseMacroParam(ctx, &isVararg, &hasError);
  }

  if (hasError) {
      // error somewhere in params
      skipUntilEoL(ctx);
      return NULL;
  } else if (isFunctional) {
      // consume ')'
      token = lexTokenNoSubstitute(ctx);
  }

  Token bodyHead = { 0 };
  Token *b = &bodyHead;

  unsigned used = 0;

  unsigned prevIsHash = 0;

  Coordinates coords = { token, token };

  while (token->rawCode != NEWLINE && token->rawCode != END_OF_FILE) {
      coords.left = coords.right = token;
      if (checkIsMacroParam(p, token)) {
          token->isMacroParam = 1;
          used = 1;
      } else if (prevIsHash && isFunctional) {
          //#define x(y) # z
          reportDiagnostic(ctx, DIAG_PP_HASH_NOT_FOLLOWED_BY_PARAM, &coords);
          skipUntilEoL(ctx);
          return NULL;
      }

      if (b == &bodyHead && token->rawCode == DSHARP) {
        // #define x(y) ## y
        reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &coords);
        skipUntilEoL(ctx);
        return NULL;
      }

      prevIsHash = token->rawCode == '#';

      b = b->next = token;
      token = lexTokenNoSubstitute(ctx);
  }

  coords.left = coords.right = b;

  if (prevIsHash && isFunctional) {
      //#define x(y) #
      reportDiagnostic(ctx, DIAG_PP_HASH_NOT_FOLLOWED_BY_PARAM, &coords);
      return NULL;
  }

  if (b && b->rawCode == DSHARP) {
      // #define x(y) y ##
      reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &coords);
      return NULL;
  }

  MacroDefinition *def = allocateMacroDef(ctx, name, p, bodyHead.next, isVararg, isFunctional);
  def->isParamsUsed = used;

  return def;
}

static void handleDefineDirective(ParserContext *ctx, Token *directive) {
  LocationInfo *locInfo = ctx->lexerState->fileContext.locInfo;
  unsigned ln = locInfo->fileInfo.lineno;
  Token *token = lexTokenNoSubstitute(ctx);

  Coordinates coords = { directive, directive };

  if (token->rawCode != IDENTIFIER) {
      reportDiagnostic(ctx, DIAG_PP_MACRO_NAME_MISSING, &coords);
      skipUntilEoL(ctx);
      return;
  }

  if (strcmp("defined", token->id) == 0) {
      reportDiagnostic(ctx, DIAG_PP_DEFINED_NOT_A_NAME, &coords);
      skipUntilEoL(ctx);
      return;
  }

  const char *macroName = token->id;

  MacroDefinition *def = parseMacro(ctx, macroName);

  intptr_t old = putToHashMap(ctx->macroMap, (intptr_t)macroName, (intptr_t)def);
  if (old) {
      if (cmpMacroses((MacroDefinition*)old, def)) {
        reportDiagnostic(ctx, DIAG_PP_MACRO_REDEFINED, &coords, macroName);
      }
  }
}

static void handleUndefDirective(ParserContext *ctx, Token *directive) {
  Token *token = lexTokenNoSubstitute(ctx);

  if (token->rawCode != IDENTIFIER) {
      Coordinates coords = { directive, token };
      reportDiagnostic(ctx, DIAG_PP_MACRO_NAME_MISSING, &coords);
  } else {
      removeFromHashMap(ctx->macroMap, (intptr_t)token->id);
  }

  skipUntilEoL(ctx);
  return;
}

#define BUFFER_SIZE 4096

static void handleDiagnosticDirective(ParserContext *ctx, Token *directive, enum DiagnosticId diag) {

  Token tok = { 0 };

  static char buffer[BUFFER_SIZE];
  unsigned ptr = 0;

  unsigned skipWhitespace = 1;

  ctx->stateFlags.silentMode = 1;

  for (;;) {
    lexTokenRaw(ctx, &tok);

    if (tok.rawCode == NEWLINE || tok.rawCode == END_OF_FILE) break;

    if (ptr == BUFFER_SIZE - 1) break;

    int code = tok.rawCode;

    if (code == DANGLING_NEWLINE) {
        continue;
    }

    if (isspace(code)) {
        if (!skipWhitespace) {
          buffer[ptr++] = code;
        }
        continue;
    }

    if (ptr + tok.length > BUFFER_SIZE) break;

    memcpy(buffer + ptr, tok.pos, tok.length);
    ptr += tok.length;
    skipWhitespace = 0;
  }

  skipUntilEoL(ctx);

  ctx->stateFlags.silentMode = 0;

  buffer[ptr] = '\0';

  Coordinates coords = { directive, directive };
  reportDiagnostic(ctx, diag, &coords, buffer);
}

static PPConditionFrame *allocateConditionFrame(ParserContext *ctx, Token *open, PPConditionFrame *prev, enum PPConditionState state) {
  PPConditionFrame *r = heapAllocate(sizeof (PPConditionFrame));

  r->prev = prev;
  r->ifDirective = open;
  r->state = state;

  return r;
}

static void skipConditionalBlock(ParserContext *ctx) {

  int depth = 0;

  Token *t = NULL;
  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  ctx->stateFlags.silentMode = 1;

  for (;;) {
      unsigned pos = lex->fileContext.pos;
      unsigned vline = lex->fileContext.visibleLine;
      unsigned lineno = lex->fileContext.locInfo->fileInfo.lineno;
      unsigned ls = lex->fileContext.atLineStart;

      t = lexTokenNoSubstitute(ctx);

      if (t->rawCode == END_OF_FILE) break;
      if (t->rawCode == NEWLINE) continue;

      if (t->startOfLine && t->rawCode == '#') {
          Token *next = lexTokenNoSubstitute(ctx);
          if (next->rawCode == IDENTIFIER) {
              const char *id = next->id;
              if (strcmp("if", id) == 0 || strcmp("ifdef", id) == 0 || strcmp("ifndef", id) == 0) {
                  ++depth;
              } else if (depth && strcmp("endif", id) == 0) {
                  --depth;
              } else if (depth == 0) {
                  if (strcmp("elif", id) == 0 ||
                      strcmp("else", id) == 0 ||
                      strcmp("endif", id) == 0) {
                      lex->fileContext.pos = pos;
                      lex->fileContext.visibleLine = vline;
                      lex->fileContext.locInfo->fileInfo.lineno = lineno;
                      lex->fileContext.atLineStart = ls;
                      goto done;
                  }
              }
          }
      }
      skipUntilEoL(ctx);
  }


  popLexerState(ctx);

done:

  ctx->stateFlags.silentMode = 0;
}

static Token *simplifyTokenSequence(ParserContext *ctx, Token *token) {
  Token head = { 0 };
  Token *cur = &head;
  while (token->rawCode != END_OF_FILE) {
      if (token->rawCode == IDENTIFIER && strcmp("defined", token->id) == 0) {
          cur = cur->next = evaluateDefinedOp(ctx, token, FALSE, &token);
          continue;
      } else if (token->rawCode == IDENTIFIER) {
          // Identifiers that are not macros, which are all considered to be the number zero.
          cur = cur->next = constToken(ctx, 0, token);
      } else {
          cur = cur->next = token;
      }
      token = token->next;
  }

  return head.next;
}

static int evaluateTokenSequence(ParserContext *ctx, Token *token, int *err) {
  token = simplifyTokenSequence(ctx, token);

  AstExpression *expr = parsePPExpression(ctx, token);
  AstConst *e = eval(ctx, expr);

  if (e) {
      return e->i;
  }

  *err = 1;

  reportDiagnostic(ctx, DIAG_PP_CANNOT_EVALUATE, &expr->coordinates);

  return 0;
}

static Token *fetchConditionExpression(ParserContext *ctx, Token *directive) {

  Token exprHead = { 0 };
  Token *expr = &exprHead;
  Token *t = NULL;

  ctx->stateFlags.inPPExpression = 1;

  for (t = lexToken(ctx);
       t->rawCode != NEWLINE && t->rawCode != END_OF_FILE;
       t = lexToken(ctx)) {
      expr = expr->next = t;
  }

  ctx->stateFlags.inPPExpression = 0;

  if (expr == &exprHead) {
    // #if
    Coordinates coords = { directive, directive };
    reportDiagnostic(ctx, DIAG_PP_EXPECTED_VALUE_IN_EXPRESSION, &coords);
    return NULL;
  }

  expr->next = eofToken(ctx);

  return exprHead.next;
}

static void handleIfDirective(ParserContext *ctx, Token *directive) {
  assert(directive->rawCode == IDENTIFIER && strcmp("if", directive->id) == 0);

  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  Token *expr = fetchConditionExpression(ctx, directive);

  int err = 0;
  int cond = evaluateTokenSequence(ctx, expr, &err);

  if (err) {
      skipUntilEoL(ctx);
      return;
  }

  PPConditionFrame *frame = allocateConditionFrame(ctx, directive, lex->fileContext.conditionStack, IN_IF);

  frame->isTaking = cond;
  lex->fileContext.conditionStack = frame;

  if (!cond) {
    skipConditionalBlock(ctx);
  }
}

void handleElifDirective(ParserContext *ctx, Token *directive) {
  assert(directive->rawCode == IDENTIFIER && strcmp("elif", directive->id) == 0);

  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  Coordinates coords = { directive, directive };

  PPConditionFrame *frame = lex->fileContext.conditionStack;

  if (frame == NULL) {
      reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "elif");
      skipUntilEoL(ctx);
      return;
  }

  if (frame->seenElse) {
      reportDiagnostic(ctx, DIAG_PP_AFTER_ELSE, &coords, "elif");
      skipUntilEoL(ctx);
      return;
  }

  if (frame->isTaking) {
      frame->isTaken = 1;
      frame->isTaking = 0;
  }

  Token *expr = fetchConditionExpression(ctx, directive);

  int cond = !frame->isTaken;

  if (cond) {
    int err = 0;
    cond = evaluateTokenSequence(ctx, expr, &err);

    if (err) {
        skipUntilEoL(ctx);
        return;
    }
  }

  frame->isTaking = cond;
  frame->state = IN_ELIF;

  if (!cond) {
      skipConditionalBlock(ctx);
  }
}

static void handleElseDirective(ParserContext *ctx, Token *directive) {
  assert(directive->rawCode == IDENTIFIER && strcmp("else", directive->id) == 0);

  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  Token exprHead = { 0 };
  Token *expr = &exprHead;
  Token *t = NULL;

  Coordinates coords = { directive, directive };

  PPConditionFrame *frame = lex->fileContext.conditionStack;

  if (frame == NULL) {
      reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "else");
      skipUntilEoL(ctx);
      return;
  }

  if (frame->seenElse) {
      reportDiagnostic(ctx, DIAG_PP_AFTER_ELSE, &coords, "else");
      skipUntilEoL(ctx);
      return;
  }

  Token *nl = lexToken(ctx); // consume NEW_LINE
  if (nl->rawCode != NEWLINE && nl->rawCode != END_OF_FILE) {
      coords.left = coords.right = nl;
      reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, "else");
      skipUntilEoL(ctx);
  }

  if (frame->isTaking) {
      frame->isTaken = 1;
      frame->isTaking = 0;
  }

  frame->seenElse = 1;

  frame->state = IN_ELSE;

  if (frame->isTaken) {
      skipConditionalBlock(ctx);
  } else {
      frame->isTaking = 1;
  }
}

static void handleEndifDirective(ParserContext *ctx, Token *directive) {
  assert(directive->rawCode == IDENTIFIER && strcmp("endif", directive->id) == 0);

  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  Token exprHead = { 0 };
  Token *expr = &exprHead;
  Token *t = NULL;

  Coordinates coords = { directive, directive };

  PPConditionFrame *frame = lex->fileContext.conditionStack;

  if (frame == NULL) {
      reportDiagnostic(ctx, DIAG_PP_WITHOUT_IF, &coords, "endif");
      skipUntilEoL(ctx);
      return;
  }

  Token *nl = lexToken(ctx); // consume NEW_LINE
  if (nl->rawCode != NEWLINE && nl->rawCode != END_OF_FILE) {
      coords.left = coords.right = nl;
      reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, "endif");
      skipUntilEoL(ctx);
  }

  // pop off and release current if-endif block
  lex->fileContext.conditionStack = frame->prev;
  releaseHeap(frame);
}

void handleIfdefDirective(ParserContext *ctx, Token *directive, Boolean invert) {

  assert(directive->rawCode == IDENTIFIER && strcmp(invert ? "ifndef" : "ifdef", directive->id) == 0);

  LexerState *lex = ctx->lexerState;
  assert(lex->state == LS_FILE);

  Token *id = lexNonExpand(ctx, FALSE);

  Coordinates coords = { directive, directive };

  if (id->rawCode == NEWLINE || id->rawCode == END_OF_FILE) {
    reportDiagnostic(ctx, DIAG_PP_MACRO_NAME_MISSING, &coords);
    return;
  }

  coords.left = coords.right = id;

  if (id->rawCode != IDENTIFIER) {
    reportDiagnostic(ctx, DIAG_PP_MACRO_NAME_IS_ID, &coords);
    skipUntilEoL(ctx);
    return;
  }

  Token *nl = lexToken(ctx);

  coords.left = coords.right = nl;

  if (nl->rawCode != NEWLINE && nl->rawCode != END_OF_FILE) {
      reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, invert ? "ifndef" : "ifdef");
      skipUntilEoL(ctx);
  }

  int cond = isMacro(ctx, id) != invert;

  PPConditionFrame *frame = allocateConditionFrame(ctx, directive, lex->fileContext.conditionStack, IN_IF);

  frame->isTaking = cond;

  lex->fileContext.conditionStack = frame;

  if (!cond) {
      skipConditionalBlock(ctx);
  }
}

static void handlePragmaDirective(ParserContext *ctx, Token *directive) {
  assert(directive->rawCode == IDENTIFIER && strcmp("pragma", directive->id) == 0);

  Token *arg = lexNonExpand(ctx, FALSE);

  if (arg->rawCode == IDENTIFIER && strcmp("once", arg->id) == 0) {
      const char *f = getFileName(ctx);
      putToHashMap(ctx->pragmaOnceMap, (intptr_t)f, (intptr_t)f);
  }

  skipUntilEoL(ctx);
}

static LexerState *findFileLexer(ParserContext *ctx) {
  LexerState *lex = ctx->lexerState;
  while (lex) {
      if (lex->state == LS_FILE) return lex;
      lex = lex->prev;
  }

  unreachable("No file state found in stack");
  return NULL;
}

static LineChunk *newLineChunk(const char *file, unsigned newLineNumber, unsigned rawLine) {
  LineChunk *newChunk = heapAllocate(sizeof(LineChunk));

  newChunk->overrideFileName = file;
  newChunk->overrideLineNumber = newLineNumber;
  newChunk->posLineNumber = rawLine;

  return newChunk;
}

#define TMP_SIZE 1024
static void handleLineDirective(ParserContext *ctx, Token *directive) {
  Token *line = lexNonExpand(ctx, FALSE);

  if (line->rawCode != I_CONSTANT_RAW || line->value.iv < 0) {
      Coordinates coords = { line, line };
      char c[TMP_SIZE] = { 0 };
      memcpy(c, line->pos, min(TMP_SIZE - 1, line->length));
      reportDiagnostic(ctx, DIAG_PP_LINE_NOT_POSITIVE_INT, &coords, c);
      skipUntilEoL(ctx);
      return;
  }

  Token *fileNameToken = lexNonExpand(ctx, FALSE);
  const char *fileName = NULL;
  if (fileNameToken->rawCode != STRING_LITERAL) {
      if (fileNameToken->rawCode != END_OF_FILE && fileNameToken->rawCode != NEWLINE) {
        Coordinates coords = { line, line };
        reportDiagnostic(ctx, DIAG_PP_INVALID_FILE_LINE, &coords);
        skipUntilEoL(ctx);
        return;
      }
  } else {
      fileName = fileNameToken->value.text.v;
  }

  Token *el = lexNonExpand(ctx, FALSE); // consume \n

  if (el->rawCode != NEWLINE && el->rawCode != END_OF_FILE) {
      Coordinates coords = { el, el };
      reportDiagnostic(ctx, DIAG_PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, &coords, "line");
      skipUntilEoL(ctx);
  }

  LexerState *lex = findFileLexer(ctx);
  LocationInfo *locInfo = lex->fileContext.locInfo;

  assert(locInfo->kind == LIK_FILE);

  LineChunk *chunk = newLineChunk(fileName, line->value.iv, locInfo->fileInfo.lineno - 1);
  chunk->next = locInfo->fileInfo.chunks;
  locInfo->fileInfo.chunks = chunk;
}

void handleDirective(ParserContext *ctx, Token *directive) {
  LexerState *lexState = ctx->lexerState;
  LocationInfo *locInfo = lexState->fileContext.locInfo;
  Token tok = { 0 };
  tok.locInfo = locInfo;

  if (directive->rawCode == IDENTIFIER) {
    if (!strcmp("include", directive->id)) {
        return handleIncludeDirective(ctx, directive);
    } else if (!strcmp("include_next", directive->id)) {
        Coordinates coords = { directive, directive };
        reportDiagnostic(ctx, DIAG_PP_UNSUPPORTED_DIRECTIVE, &coords, directive->id);
    } else if (!strcmp("define", directive->id)) {
        return handleDefineDirective(ctx, directive);
    } else if (!strcmp("undef", directive->id)) {
        return handleUndefDirective(ctx, directive);
    } else if (!strcmp("if", directive->id)) {
        return handleIfDirective(ctx, directive);
    } else if (!strcmp("elif", directive->id)) {
        return handleElifDirective(ctx, directive);
    } else if (!strcmp("else", directive->id)) {
        return handleElseDirective(ctx, directive);
    } else if (!strcmp("endif", directive->id)) {
        return handleEndifDirective(ctx, directive);
    } else if (!strcmp("ifdef", directive->id)) {
        return handleIfdefDirective(ctx, directive, FALSE);
    } else if (!strcmp("ifndef", directive->id)) {
        return handleIfdefDirective(ctx, directive, TRUE);
    } else if (!strcmp("pragma", directive->id)) {
        return handlePragmaDirective(ctx, directive);
    } else if (!strcmp("line", directive->id)) {
        return handleLineDirective(ctx, directive);
    } else if (!strcmp("error", directive->id)) {
        return handleDiagnosticDirective(ctx, directive, DIAG_PP_ERROR);
    } else if (!strcmp("warning", directive->id)) {
        return handleDiagnosticDirective(ctx, directive, DIAG_PP_WARNING);
    } else {
        Coordinates coords = { directive, directive };
        reportDiagnostic(ctx, DIAG_PP_INVALID_PP_DIRECTIVE, &coords, directive);
    }
  }

  // invalid pp directive
  skipUntilEoL(ctx);
}

MacroArg *readMacroArg(ParserContext *ctx, MacroParam *p, Token **n) {
  Token *t = *n = lexNonExpand(ctx, TRUE);

  Token head = { 0 };
  Token *cur = &head;
  Token *argStart = NULL;
  Token *argEnd = NULL;
  int depth = 0;

  while (t->rawCode != END_OF_FILE) {
      if (depth == 0 && (!p->isVararg && t->rawCode == ',' || t->rawCode == ')'))  {
          break;
      }

      if (depth && t->rawCode == ')') {
          --depth;
      } else if (t->rawCode == '(') {
          ++depth;
      }

      if (t->rawCode == IDENTIFIER) {
          MacroDefinition *def = findMacro(ctx, t->id);
          if (def && !def->isEnabled) {
              t->disabledExpansion = 1;
          }
      }


      cur = cur->next = t;
      *n = t = lexNonExpand(ctx, TRUE);
  }

  return allocateMacroArg(ctx, p, head.next);
}

MacroArg *parseMacroArgs(ParserContext *ctx, Token *macro, MacroDefinition *def) {
  MacroParam *p = def->params;

  MacroArg argHead = { 0 };
  MacroArg *arg = &argHead;
  MacroArg *vararg = NULL;

  Token *t = NULL;

  lexNonExpand(ctx, TRUE); // consume '('

  while (p && !p->isVararg) {
    arg = arg->next = readMacroArg(ctx, p, &t);
    p = p->next;
  }

  if (!t && !p) {
      // #define x() y
      // x()
      //  ^
      t = lexNonExpand(ctx, TRUE); // consume ')'
  }

  Coordinates coords = { macro, macro };
  if (p && !p->isVararg) {
      // #define x(y) z
      // x()
      //   ^
      reportDiagnostic(ctx, DIAG_PP_TOO_FEW_ARGUMENTS, &coords);
      while (t->rawCode != ')') t = lexTokenNoSubstitute(ctx);
  } else if (!p && t->rawCode != ')') {
      // #define x(y) z
      // x(a, b)
      //      ^
      reportDiagnostic(ctx, DIAG_PP_TOO_MANY_ARGUMENTS, &coords);
      while (t->rawCode != ')') t = lexTokenNoSubstitute(ctx);
  } else if (p) {
      if (t && t->rawCode == ')') {
        // #define x(...)
        // x( )
        //    ^
        arg = arg->next = allocateMacroArg(ctx, p, NULL);
      } else {
        // #define x(...)
        // x( ) | x(a)
        //    ^ |   ^
        arg = arg->next = readMacroArg(ctx, p, &t);
        if (t->rawCode != ')') {
            // #define x(...)
            // x(a, b, c,
            // report ...
            reportDiagnostic(ctx, DIAG_PP_UNTERMINATED_MACRO_INVOCATION, &coords);
        }
      }
  }

  return argHead.next;
}

static Boolean needExpansion(ParserContext *ctx, Token *arg) {
  while (arg) {
      if (arg->rawCode == IDENTIFIER) {
          if (isMacro(ctx, arg)) return TRUE;
      }
      arg = arg->next;
  }
  return FALSE;
}

static Token *expandArgument(ParserContext *ctx, MacroDefinition *def, MacroArg *arg, Token *macro) {

  Token *evaluated = arg->evaluated;

  if (!evaluated) {
    LexerState *state = allocateMacroLexerState(NULL, macro);
    Token *bodyCopy = arg->value;
    state->macroContext.bodyStart = state->macroContext.bodyPtr = bodyCopy;

    LexerState *old = state->virtPrev = ctx->lexerState;
    ctx->lexerState = state;

    MacroState *ms = &state->macroContext;

    Token head = { 0 };
    Token *cur = &head;
    Token *t;

    for (t = lexToken(ctx); t->rawCode != END_OF_FILE; t = lexToken(ctx)) {
        cur = cur->next = t;
    }

    ctx->lexerState = old;

    evaluated = arg->evaluated = head.next;
  }

  return evaluated;
}

static Token* expandArguments(ParserContext *ctx, MacroDefinition *def, MacroArg *args, Token *macro) {

  Token *body = def->body;

  Token head = { 0 };
  Token *cur = &head;

  Coordinates coords = { macro, macro };

  while (body) {
      if (body->rawCode == '#') {
          // stringify
          Token *next = body->next;
          if (next) {
              MacroArg *arg = findArgument(next, args);
              if (arg) {
                  Token *argValue = arg->value;
                  Token *evaluated = argValue ? stringifySequence(ctx, argValue) : stringToken(ctx, next, "");

                  cur = cur->next = evaluated;

                  evaluated->hasLeadingSpace = body->hasLeadingSpace;

                  for (; evaluated->rawCode != END_OF_FILE; evaluated = evaluated->next) {
                      cur = evaluated;
                  }

                  cur->next = NULL;

                  body = next->next;
                  continue;
              }
          }
      }

      if (body->rawCode == ',' && body->next && body->next->rawCode == DSHARP) {
          // GNU COMMA ,##__VA_ARGS__
          Token *comma = body;
          Token *dsh = comma->next;
          Token *next = dsh->next;

          if (next && next->isMacroParam) {
              MacroArg *arg = findArgument(next, args);
              if (arg && arg->param->isVararg) {
                  if (arg->value == NULL) {
                      // expand to empty list
                      body = next->next;
                  } else {
                      // TODO: check this
                      Token *evaluated = copyToken(ctx, body);
                      cur = cur->next = evaluated;
                      body = next;
                  }
                  continue;
              }
          }

      }

      if (body->next && body->next->rawCode == DSHARP) {
          MacroArg *lhsA = findArgument(body, args);
          Token *lhs = NULL;

          if (lhsA == NULL) {
              lhs = copyToken(ctx, body);
          } else {
              lhs = lhsA->value ? copySequence(ctx, lhsA->value) : NULL;
          }

          Token *sharpsharp = body->next;
          Token *origRhs = sharpsharp->next;

          if (origRhs) {
            MacroArg *rhsA = findArgument(origRhs, args);
            Token *rhs = NULL;
            if (rhsA == NULL) {
                rhs = copyToken(ctx, origRhs);
            } else {
                rhs = rhsA->value ? copySequence(ctx, rhsA->value) : NULL;
            }

            Token *evaluated = NULL;
            if (rhs == NULL && lhs == NULL) {
                body = origRhs->next;
                continue;
            } else if (rhs == NULL) {
                evaluated = lhs;
            } else if (lhs == NULL) {
                evaluated = rhs;
            } else {
                evaluated = concatTokens(ctx, macro, lhs, rhs);
            }

            cur->next = evaluated;
            for (cur = evaluated; evaluated; evaluated = evaluated->next) {
                evaluated->expanded = macro;
                cur = evaluated;
            }

            body = origRhs->next;
            continue;
          } else {
              reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &coords);
              break;
          }

      }

      if (body->rawCode == DSHARP) { // pasting
          // a##b##c
          Token *next = body->next;

          if (next == NULL) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &coords);
            break;
          }

          if (body == def->body) {
            reportDiagnostic(ctx, DIAG_PP_WRONG_CONCAT_OP_PLACE, &coords);
            break;
          }

          Token *lhs = cur;
          MacroArg *rhsA = findArgument(next, args);
          Token *rhs = NULL;

          if (rhsA == NULL) {
              rhs = copyToken(ctx, next);
          } else {
              rhs = copySequence(ctx, rhsA->value);
          }

          Token *evaluated = NULL;
          if (rhs == NULL && lhs == &head) {
              body = next->next;
              continue;
          } else if (lhs == &head) {
              evaluated = rhs;
          } else  if (rhs == NULL) {
              evaluated = lhs;
          } else {
              evaluated = concatTokens(ctx, macro, lhs, rhs);
          }

          Token *i = &head;

          while (i->next) {
              if (i->next == cur) break;
              i = i->next;
          }

          i->next = evaluated;
          for (cur = evaluated; evaluated; evaluated = evaluated->next) {
              evaluated->expanded = macro;
              cur = evaluated;
          }

          body = next->next;

          continue;
      }

      if (body->isMacroParam) {
          MacroArg *arg = findArgument(body, args);
          if (arg->value) {
            if (needExpansion(ctx, arg->value)) {
              Token *expanded = expandArgument(ctx, def, arg, macro);
              cur = cur->next = copySequence(ctx, expanded);
            } else {
              cur = cur->next = copySequence(ctx, arg->value);
            }

            cur->hasLeadingSpace = body->hasLeadingSpace;
            cur->startOfLine = body->startOfLine;

            for (; cur->next; cur = cur->next) {
                if (cur->rawCode == DSHARP) {
                    // #define x(y) y
                    // x(a##b) expands into 'a##b', not 'ab'
                    cur->rawCode = cur->code = BAD_CHARACTER;
                }
            }
          }

          body = body->next;
          continue;
      }

      cur = cur->next = copyToken(ctx, body);
      body = body->next;
  }

  return head.next;

}

void skipMacroArgs(ParserContext *ctx) {
  int depth = 0;
  Token *t = lexTokenNoSubstitute(ctx);

  assert(t->rawCode == '(');

  for (t = lexTokenNoSubstitute(ctx); t->rawCode != END_OF_FILE; t = lexTokenNoSubstitute(ctx)) {
      if (t->rawCode == '(') ++depth;
      if (depth && t->rawCode == ')') --depth;
      if (t->rawCode == ')') break;
  }
}

static void expandMacro(ParserContext *ctx, Token *t, MacroDefinition *def) {

  Token *body = NULL;

  if (def->isFunctional) {
      MacroArg *args = parseMacroArgs(ctx, t, def);
      if (def->isParamsUsed) {
          body = expandArguments(ctx, def, args, t);
      } else {
          body = copySequence(ctx, def->body);
      }
  } else {
      body = copySequence(ctx, def->body);
  }

  if (!body) return;

  body->hasLeadingSpace = t->hasLeadingSpace;
  body->startOfLine = t->startOfLine;

  LexerState *lex = allocateMacroLexerState(def, t);
  lex->macroContext.bodyStart = lex->macroContext.bodyPtr = body;

  def->isEnabled = 0;
  lex->prev = lex->virtPrev = ctx->lexerState;
  ctx->lexerState = lex;

  for (; body; body = body->next)
    body->expanded = t;
}

// returns NULL if macro enqueued into expansion stack
Token *handleIdentifier(ParserContext *ctx, Token *token) {

  if (ctx->stateFlags.inPPExpression) {
      if (token->rawCode == IDENTIFIER && strcmp("defined", token->id) == 0) {
          ctx->stateFlags.afterPPDefined = 1;
          return token;
      }
  }

  if (ctx->stateFlags.afterPPDefined) {
      ctx->stateFlags.afterPPDefined = 0;
      ctx->stateFlags.afterPPParen = 0;
      return token;
  }

  MacroDefinition *def = findMacro(ctx, token->id);
  if (def) {
    if (def->isEnabled && !token->disabledExpansion) {
      if (def->handler) {
          Token *r = def->handler(ctx, token);
          r->startOfLine = token->startOfLine;
          r->hasLeadingSpace = token->hasLeadingSpace;
          assert(r->next->rawCode == END_OF_FILE);
          r->next = NULL;
          return r;
      }
      if (!def->isFunctional || isNextToken(ctx, '(')) {
          expandMacro(ctx, token, def);
          return NULL;
      }
    } else {
        token->disabledExpansion = 1;
    }
  }
  return token;
}
