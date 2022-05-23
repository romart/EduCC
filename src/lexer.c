
#include <assert.h>

#include "parser.h"
#include "sema.h"
#include "lex.h"


char *allocateString(ParserContext *ctx, size_t size) {
  return (char *)areanAllocate(ctx->memory.stringArena, size);
}

static const char* copyLiteralString(ParserContext *ctx, yyscan_t scanner) {

    int yyleng = yyget_leng(scanner);
    const char* yytext = yyget_text(scanner);

    char* r = (char*)allocateString(ctx, yyleng + 1);
    strncpy(r, yytext, yyleng + 1);

    return r;
}

static Token *allocToken(ParserContext *ctx) {
  return (Token *)areanAllocate(ctx->memory.tokenArena, sizeof(Token));
}

static EnumConstant *enumConstant(ParserContext *ctx, const char* name) {
  Symbol *s = findSymbol(ctx, name);
  if (s && s->kind == EnumConstSymbol) return s->enumerator;
  return NULL;
}

static void dumpToken(char *buffer, size_t bsize, Token *token) {

  char *bf = buffer;
  size_t l = snprintf(bf, bsize, "Token '%s':%d", tokenName(token->code), token->code);
  bf += l; bsize -= l;

  if (token->code != token->rawCode) {
    l = snprintf(bf, bsize, " (raw '%s': %d)", tokenName(token->rawCode), token->rawCode);
    bf += l; bsize -= l;
  }

  l = snprintf(bf, bsize, ", coordinates [%d, %d]", token->coordinates.startOffset, token->coordinates.endOffset);
  bf += l; bsize -= l;

  if (token->text) {
    l = snprintf(bf, bsize, ", text \"%s\"", token->text);
    bf += l; bsize -= l;
  }

  if (token->rawCode == I_CONSTANT_RAW) {
      l = snprintf(bf, bsize, ", integer value '%ld'", token->value.iv);
      bf += l; bsize -= l;
  }

  if (token->rawCode == F_CONSTANT_RAW) {
      l = snprintf(bf, bsize, ", float value '%f'", token->value.dv);
      bf += l; bsize -= l;
  }
}

static int parseCharSymbol(ParserContext *ctx, Coordinates *coords, const char *text, size_t length, Boolean isWide) {
  char buffer[8] = { 0 };
  buffer[0] = '0';

  Boolean isHex = FALSE;
  unsigned idx = isWide ? 1 : 0;
  assert(text[idx++] == '\'');

  if (text[idx] == '\\') {
    idx++;
    switch (text[idx]) {
        case '0':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\0';
        case 'a':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\a';
        case 'b':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\b';
        case 'e':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\e';
        case 'f':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\f';
        case 'n':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\n';
        case 'r':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\r';
        case 't':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\t';
        case 'v':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\v';
        case '\\':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\\';
        case '\'':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\'';
        case '\"':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\"';
        case '?':
          if (text[idx+1] != '\'') reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
          return '\?';
        case 'x': isHex = TRUE;
        default: {
          unsigned i = 1;
          while (idx < length && text[idx] != '\'') {
              buffer[i++] = text[idx++];
              if (i > 6) break; // too large sequence
          }
          int32_t v = -1;
          int r = sscanf(buffer, isHex ? "%x" : "%o", &v);
          int limit = 0x7f;
          if (isWide) {
              limit = 0x7fff;
          }
          if (v > limit) {
              enum DiagnosticId diag = isHex ? DIAG_ESCAPE_SEC_OOR_HEX : DIAG_ESCAPE_SEC_OOR_OCT;
              reportDiagnostic(ctx, diag, coords);
          }
          return v;
        }
    }
  } else {
    if ((length - idx) > 3) {
        reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, coords);
    }
    return text[idx] - '\0';
  }
}


static void parseNumber(ParserContext *ctx, Token *token) {
  int code = token->rawCode;
  assert(code == I_CONSTANT_RAW || code == F_CONSTANT_RAW);

  const char *text = token->text;
  size_t length = strlen(token->text);

  assert(length > 0);

  if (text[0] == 'L') {
      // char16_t
      assert(code == I_CONSTANT_RAW);
      assert(length > 1);
      assert(text[1] == '\'');
      token->code = C16_CONSTANT;
      token->value.iv = parseCharSymbol(ctx, &token->coordinates, text, length, TRUE);
      return;
  } else if (text[0] == '\'') {
      // char
      assert(code == I_CONSTANT_RAW);
      token->code = C_CONSTANT;
      token->value.iv = parseCharSymbol(ctx, &token->coordinates, text, length, FALSE);
      return;
  }

  int suffix = text[length - 1];

  if (code == F_CONSTANT_RAW) {
    double v = atof(text);
    if (suffix == 'f' || suffix == 'F') {
        // float
        token->code = F_CONSTANT;
        if ((double)(float) v != v) {
            // TODO: report float precision violation warning
        }
        token->value.dv = (double)(float)v;
    } else {
        // double
        token->code = D_CONSTANT;
        token->value.dv = v;
    }
    return;
  }

  assert(code == I_CONSTANT_RAW);

  int sign = 0;
  int wide = 4;

  if (suffix == 'u' || suffix == 'U') {
      sign = 1;
      int suffix2 = text[length - 2];
      if (suffix == 'l' || suffix == 'L') {
          int suffix3 = text[length - 3];
          if (suffix == 'l' || suffix == 'L') {
            wide = 8;
          }
      }
  }

  if (suffix == 'l' || suffix == 'L') {
      int suffix2 = text[length - 2];
      if (suffix == 'l' || suffix == 'L') {
          wide = 8;
          int suffix3 = text[length - 3];
          if (suffix == 'u' || suffix == 'U') {
              sign = 1;
          }
      }
  }

  int prefix = text[0];
  int64_t c = 0;
  if (prefix == '0') {
      if (text[1] == 'x') {
        sscanf(text, "%lx", &c);
      } else if (text[1] == 'b' || text[1] == 'B') {
        u_int64_t r = 0;
        unsigned i;
        for (i = 2; i < length; ++i) {
            u_int64_t old = r;
            r <<= 1;
            r += (text[i] - '0');
            if (old > r) {
                // integer overflow
                reportDiagnostic(ctx, DIAG_INTEGER_BIN_CONST_OVERFLOW, &token->coordinates);
            }
        }
        c = (int64_t)r;
      } else {
        sscanf(text, "%lo", &c);
      }
  } else {
    sscanf(text, "%ld", &c);
  }

  if (wide == 8) {
      token->value.iv = c;
      if (sign == 1) {
          token->code = UL_CONSTANT;
      } else {
          token->code = L_CONSTANT;
      }
  } else {
      if (sign == 1) {
          u_int32_t uc = (u_int32_t)c;
          if (c != (int64_t)uc) {
              reportDiagnostic(ctx, DIAG_IMPLICIT_CONVERSION, &token->coordinates, "long", "unsigned int", c, uc);
          }
          token->code = U_CONSTANT;
          token->value.iv = (int64_t)uc;
      } else {
          int32_t ic = (int32_t)c;
          if (c != (int64_t)ic) {
              reportDiagnostic(ctx, DIAG_IMPLICIT_CONVERSION, &token->coordinates, "long", "int", c, ic);
          }
          token->code = I_CONSTANT;
          token->value.iv = (int64_t)ic;
      }
  }
}



Token *tokenizeFile(ParserContext *ctx, const char *fileName, unsigned *lineNum) {

  FILE* opened = fopen(fileName, "r");

  if (opened == NULL) return NULL;

  yyscan_t scanner;

  yylex_init(&scanner);

  Token *start = NULL, *current;

  int token = -1;
  YYSTYPE dummy = 0;
  int endOffset;
  YYLTYPE position = 0;

  unsigned lineCount = 1;

  yyset_in(opened, scanner);

  do {
      token = yylex(&dummy, &position, scanner);

      endOffset = position;
      Token *tmp = allocToken(ctx);

      size_t tokenLength = yyget_leng(scanner);

      tmp->code = tmp->rawCode = token;

      tmp->coordinates.fileName = fileName;
      tmp->coordinates.endOffset = endOffset;
      tmp->coordinates.startOffset = endOffset - tokenLength;

      if (token == IDENTIFIER)  {
          tmp->text = copyLiteralString(ctx, scanner);
      } else if (token == I_CONSTANT_RAW || token == F_CONSTANT_RAW) {
          tmp->text = copyLiteralString(ctx, scanner);
      } else if (token == STRING_LITERAL) {
          tmp->coordinates.startOffset -= 1; // "..
          tmp->coordinates.endOffset += 1; // .."
          tmp->text = copyLiteralString(ctx, scanner);
      } else if (token == EMPTY_STRING_LITERAL) {
          tmp->code = STRING_LITERAL;
          tmp->text = "";
      }

      if (token == NEWLINE || token == DANGLING_NEWLINE) ++lineCount;

      if (!start) {
          start = tmp;
      } else {
          current->next = tmp;
      }

      current = tmp;
  } while (token);

  yylex_destroy(scanner);

  fclose(opened);

  *lineNum = lineCount;

  return start;
}


Token *nextToken(ParserContext *ctx) {

  Token *cur = ctx->token;
  Token *next = cur ? cur->next : ctx->firstToken;
  Token *prev = cur;

  while (next) {
    int rawToken = next->rawCode;

    if (rawToken == DANGLING_NEWLINE || rawToken == NEWLINE) {
        struct LocationInfo *locInfo = ctx->locationInfo;
        assert(strcmp(locInfo->fileName, next->coordinates.fileName) == 0);
        assert(locInfo->lineno < locInfo->lineCount);
        locInfo->linesPos[locInfo->lineno++] = next->coordinates.endOffset;
    } else break;

    if (cur) cur->next = next->next;
    prev = next;
    next = next->next;
  }

  if (!next) return NULL;

  if (next->code == '#' && (!prev || prev->code == NEWLINE)) {
      // looks like a PP-directive
  }

  if (next->rawCode == IDENTIFIER) {
      if (isTypeName(ctx, next->text, ctx->currentScope)) {
          next->code = TYPE_NAME;
      } else {
        EnumConstant *enumerator = enumConstant(ctx, next->text);
        if (enumerator) {
          next->code = ENUM_CONST;
          next->value.iv = enumerator->value;
        }
      }
  } else if (next->rawCode == I_CONSTANT_RAW || next->rawCode == F_CONSTANT_RAW) {
    parseNumber(ctx, next);
  }

  if (ctx->config->logTokens) {
    char buffer[1024];
    dumpToken(buffer, sizeof buffer, next);
    printf("%s\n", buffer); fflush(stdout);
  }

  ctx->token = next;
  return next;
}
