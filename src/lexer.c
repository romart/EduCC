
#include <assert.h>

#include "parser.h"
#include "sema.h"
#include "pp.h"

static int isalfanum(char c) {
  return '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
}

char *allocateString(ParserContext *ctx, size_t size) {
  return (char *)areanAllocate(ctx->memory.stringArena, size);
}

Token *allocToken(ParserContext *ctx) {
  return (Token *)areanAllocate(ctx->memory.tokenArena, sizeof(Token));
}

Token *allocToken2(ParserContext *ctx, LocationInfo *locInfo) {
  Token *new = allocToken(ctx);
  new->locInfo = locInfo;
  return new;
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

  if (token->pos) {
    ptrdiff_t startOffset = token->pos - token->locInfo->buffer;
    ptrdiff_t endOffset = startOffset + token->length;
    const char *place = token->locInfo->kind == LIK_FILE ? token->locInfo->fileInfo.fileName : "macro";
    l = snprintf(bf, bsize, ", coordinates [%s -- %ld, %ld]", place, startOffset, endOffset);
    bf += l; bsize -= l;

    l = snprintf(bf, bsize, ", text \"");
    bf += l; bsize -= l;
    unsigned i = 0;
    while (i < token->length && bsize > 0) {
        *bf = token->pos[i++];
        ++bf; --bsize;
    }

    *bf++ = '"';
    --bsize;
  }


  if (token->rawCode == I_CONSTANT_RAW) {
      l = snprintf(bf, bsize, ", integer value '%ld'", token->value.iv);
      bf += l; bsize -= l;
  }

  if (token->rawCode == F_CONSTANT_RAW) {
      l = snprintf(bf, bsize, ", float value '%Lf'", token->value.ldv);
      bf += l; bsize -= l;
  }

  *bf = '\0';
}

static unsigned needSpace(char c, const Token *p, const Token *t) {
  if (t == NULL || p == NULL) return 0;
  if (t->length == 0 || t->startOfLine) return 0;

  char nc = t->pos[0];

  int pcode = p->rawCode;
  int ncode = t->rawCode;

  return 'E' == c ? '+' == ncode || '-' == ncode // 0x1E + 1 vs 0x1E+1
      : '+' == pcode ? INC_OP == ncode || '+' == ncode // + ++ vs +++ or + + vs ++
      : '-' == pcode ? DEC_OP == ncode || '-' == ncode // - -- vs --- or - - vs --
      : pcode == IDENTIFIER ? ncode == IDENTIFIER // a b vs ab
      : pcode == I_CONSTANT_RAW || pcode == F_CONSTANT_RAW ? ncode == IDENTIFIER || ncode == F_CONSTANT_RAW || ncode == I_CONSTANT_RAW // 0 x12 vs 0x12 or 1 2 vs 12
      : 0;
}

const char *joinToStringTokenSequence(ParserContext *ctx, Token *s) {

  StringBuffer sb = { 0 };

  Token *t = s, *p = NULL;

  while (t && t->rawCode) {
      if (sb.idx && t->startOfLine) {
          putSymbol(&sb, '\n');
      }

      if (t->hasLeadingSpace || sb.ptr && needSpace(sb.ptr[sb.idx - 1], p, t)) {
          putSymbol(&sb, ' ');
      }

      unsigned idx;
      for (idx = 0; idx < t->length; ++idx) {
          putSymbol(&sb, t->pos[idx]);
      }

      p = t;
      t = t->next;
  }

  return sb.ptr;
}


static char *readFileToBuffer(const char *fileName, size_t *bufferSize) {

  FILE* opened = fopen(fileName, "r");

  if (opened == NULL) return NULL;

  fseek(opened, 0L, SEEK_END);
  size_t size = ftell(opened);

  rewind(opened);

  char *b = heapAllocate(size + 1);

  size_t readed = fread(b, 1, size, opened);

  assert(readed == size);

  fclose(opened);

  *bufferSize = size + 1;

  return b;
}

static unsigned countLinesInBuffer(const char *buffer) {
  unsigned result = 1;
  unsigned idx = 0;

  while (buffer[idx]) {
    int ch = buffer[idx++];
    if(ch == '\n') {
      result++;
    }
  }

  return result;
}

LocationInfo *allocateFileLocationInfo(const char *fileName, const char *buffer, size_t buffeSize, unsigned lineCount) {
  LocationInfo *locInfo = heapAllocate(sizeof(LocationInfo));

  locInfo->kind = LIK_FILE;

  locInfo->fileInfo.linesPos = heapAllocate(sizeof(unsigned) * lineCount);
  locInfo->fileInfo.linesPos[locInfo->fileInfo.lineno++] = 0;
  locInfo->fileInfo.lineCount = lineCount;

  locInfo->fileInfo.fileName = fileName;

  LineChunk *newChunk = heapAllocate(sizeof(LineChunk));
  newChunk->overrideFileName = fileName;
  newChunk->posLineNumber = 0;
  newChunk->overrideLineNumber = 1;

  locInfo->fileInfo.chunks = newChunk;

  locInfo->buffer = buffer;
  locInfo->bufferSize = buffeSize;

  return locInfo;
}

LocationInfo *allocateMacroLocationInfo(const char *buffer, size_t buffeSize, Boolean isConst) {
  LocationInfo *locInfo = heapAllocate(sizeof(LocationInfo));

  locInfo->kind = isConst ? LIK_CONST_MACRO : LIK_MACRO;

  locInfo->buffer = buffer;
  locInfo->bufferSize = buffeSize;

  return locInfo;
}

Token *tokenizeFile(ParserContext *ctx, const char *fileName, Token *tail) {

  size_t bufferSize = 0;

  char *buffer = readFileToBuffer(fileName, &bufferSize);

  if (buffer == NULL) return NULL;

  unsigned lineCount = countLinesInBuffer(buffer);

  LocationInfo *locInfo = allocateFileLocationInfo(fileName, buffer, bufferSize, lineCount);

  locInfo->next = ctx->locationInfo;
  ctx->locationInfo = locInfo;

  Token *s = tokenizeBuffer(ctx, locInfo, locInfo->fileInfo.linesPos, tail);

  return s;
}

Token *tokenizeFileAndPP(ParserContext *ctx, const char *fileName, Token *tail) {

  const char *oldfile = ctx->config->fileToCompile;
  ctx->config->fileToCompile = fileName;

  Token *tokenized = tokenizeFile(ctx, fileName, NULL);
  Token *preprocessed = preprocessFile(ctx, tokenized, tail);

  ctx->config->fileToCompile = oldfile;

  return preprocessed;
}

static Boolean isDigit(char c) {
  return '0' <= c && c <= '9';
}

static Boolean isOctalDigit(char c) {
  return '0' <= c && c <= '7';
}

static Boolean isHexDigit(char c) {
  return isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F';
}

static int hexToNum(char c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('a' <= c && c <= 'f') return 10 + (c - 'a');
  if ('A' <= c && c <= 'F') return 10 + (c - 'A');

  return 0;
}

#define IS_UNSIGNED BIT(0) // 1u | 1U
#define IS_LONG BIT(1) // 1L | 1l;
#define IS_LONGLONG BIT(2) // 1LL | 1ll | 1lL | 1Ll
#define IS_FLOAT BIT(3) // 1.f || 1.0F
//#define IS_INTEGER BIT(4)
#define HAS_ERROR BIT(5)
#define IS_BINARY BIT(6)
#define SEEN_BIN BIT(7)
#define IS_OCTAL BIT(8)
#define IS_HEX BIT(9)
#define SEEN_HEX BIT(10)
#define SEEN_DOT BIT(11)
#define SEEN_EXP BIT(12)
#define IS_BINEXP BIT(13)
#define IS_DECIMAL BIT(14)
#define SEEN_FSUFFIX BIT(15)
#define SEEN_LSUFFIX BIT(16)

static unsigned verifyNumberLiteral(ParserContext *ctx, Token *new, const char *buffer, size_t *size_ptr) {
  unsigned flags = 0;

  Token dummy = { 0 };
  dummy.locInfo = new->locInfo;
  dummy.pos = buffer;
  Coordinates coords = { &dummy, &dummy };

  char bx[32] = { 0 };
  memcpy(bx, buffer, max(31, *size_ptr));
//  printf("verify %s\n", bx);

  size_t size = *size_ptr;

  unsigned i = 0;
  if (buffer[i] == '0') {
      // hex or octal or binary
      ++i;
      if (i < size) {
         char c2 = buffer[i];
         if (c2 == 'b' || c2 == 'B') {
             if (i + 1 < size) {
               char c3 = buffer[i + 1];
               if (c3 == '1' || c3 == '0') {
                 ++i;
                 flags |= IS_BINARY;
                 while (i < size) {
                   char cb = buffer[i];
                   if (cb != '1' && cb != '0') {
                     break;
                   }
                   ++i;
                 }
               }
             }
         } else if (c2 == 'x' || c2 == 'X') {
             flags |= IS_HEX;
             ++i;
             while (i < size) {
                 char cb = buffer[i];
                 if (!isHexDigit(cb)) {
                     break;
                 }
                 ++i;
                 flags |= SEEN_HEX;
             }
         } else {
             while (i < size) {
                 char cb = buffer[i];
                 if (!isOctalDigit(cb)) {
                     break;
                 }
                 ++i;
                 flags |= IS_OCTAL;
             }
             if (!(flags & IS_OCTAL)) {
                 // if it just a single 0
                 flags |= IS_DECIMAL;
             }
         }
      } else {
          flags |= IS_DECIMAL;
      }
  } else {
      flags |= IS_DECIMAL;
      while (i < size) {
          char cb = buffer[i];
          if (!isDigit(cb)) {
              break;
          }
          ++i;
      }
  }

  if (i == size) {
    return flags;
  }

  if (buffer[i] == '.') {
      if (flags & IS_BINARY) {
          goto invalid_int_suffix;
      } else {
          ++i;
          flags |= (SEEN_DOT | IS_FLOAT);
          if (flags & IS_HEX) {
              while (i < size && isHexDigit(buffer[i])) ++i;
          } else {
              while (i < size && isDigit(buffer[i])) ++i;
          }
      }
  }

  if (flags & IS_HEX) {
      if (!(flags & (SEEN_HEX | SEEN_DOT))) {
//          0xu; 0x; 0xp;
          goto invalid_int_suffix;
      }
  }

  if (i == size) {
      return flags;
  }

  char c = buffer[i];

  if (c == '-' || c == '+') {
      *size_ptr = i - 1;
      return flags;
  }

  if (c == 'e' || c == 'E') {
      if (flags & IS_BINARY) {
          goto invalid_int_suffix;
      }
      ++i;
      flags |= SEEN_EXP;
  } else if (c == 'p' || c == 'P') { // binary exp
      if (!(flags & IS_HEX)) {
          goto invalid_int_suffix;
      }
      ++i;
      flags |= (SEEN_EXP | IS_BINEXP);
  }

//  12p+12;

  if (flags & SEEN_EXP) {
      flags |= IS_FLOAT;

      if (i == size) {
          // 1e;
          dummy.length = i;
          reportDiagnostic(ctx, DIAG_EXP_NO_DIGIT, &coords);
          return flags | HAS_ERROR;
      }

      char ec = buffer[i];
      if (ec == '-' || ec == '+') {
          ++i;
      }

      if (i == size || !isDigit(buffer[i])) {
          // 1e-;
          dummy.length = i;
          reportDiagnostic(ctx, DIAG_EXP_NO_DIGIT, &coords);
          return flags | HAS_ERROR;
      }

      while (i < size && isDigit(buffer[i])) ++i;
  }

  unsigned suffixStart = i;

  while (i < size) {
      char c = buffer[i++];
      switch (c) {
      case 'f':
      case 'F':
        if (!(flags & IS_FLOAT)) {
            if (flags & (IS_DECIMAL | IS_OCTAL)) {
                const char *t = flags & IS_DECIMAL ? "decimal" : "octal";
                dummy.length = i;
                reportDiagnostic(ctx, DIAG_INVALID_DIGIT_INT, &coords, c, t);
                return flags | HAS_ERROR;
            }
        } else if (flags & (SEEN_FSUFFIX | SEEN_LSUFFIX | IS_LONG | IS_LONGLONG | IS_UNSIGNED)) {
            // 1.ff
            i = suffixStart;
            goto invalid_float_suffix;

        }
        flags |= (IS_FLOAT | SEEN_FSUFFIX);
        continue;
      case 'l':
      case 'L':
          if (flags & IS_FLOAT) {
              if (flags & (SEEN_FSUFFIX | SEEN_LSUFFIX)) {
                i = suffixStart;
                goto invalid_float_suffix;
              } else {
                // double 1.0L
                flags |= SEEN_LSUFFIX;
              }
          }

          if (flags & IS_LONG || flags & IS_LONGLONG) {
              i = suffixStart;
              goto invalid_int_suffix;
          }

          if (i < size) {
              char ccc = buffer[i];
              if (ccc == 'L' || ccc == 'l') {
                  ++i;
                  flags |= IS_LONGLONG;
              } else {
                  flags |= IS_LONG;
              }
          } else {
              flags |= IS_LONG;
          }
          continue;
       case 'u':
       case 'U':
          if (flags & IS_FLOAT) {
              i = suffixStart;
              goto invalid_float_suffix;
          }

          if (flags & IS_UNSIGNED) {
              i = suffixStart;
              goto invalid_int_suffix;
          }

          flags |= IS_UNSIGNED;

          continue;
      }
      break;
  }

  if (i == size) {
      return flags;
  }


  if (flags & IS_FLOAT) goto invalid_float_suffix;
  else goto invalid_int_suffix;


  const char *typeString = NULL;
invalid_int_suffix:
  typeString = "integer";
  goto invalid_const_suffix;

invalid_float_suffix:
  typeString = "floating";
  goto invalid_const_suffix;

  char *suffix = NULL;

invalid_const_suffix:

  dummy.pos = &buffer[i];
  dummy.length = size - i;
  suffix = strndup(&buffer[i], size - i);
  reportDiagnostic(ctx, DIAG_INVALID_CONST_SUFFIX, &coords, suffix, typeString);
  free(suffix);
  return flags | HAS_ERROR;
}

static void parseFloatLiteral(Token *new, unsigned flags, const char *buffer, size_t size)  {

  char tmp[64] = { 0 };

  char *copy;

  if (size < 64) {
    copy = strncpy(tmp, buffer, size);
  } else {
    copy = strndup(buffer, size);
  }

  long double r;
  sscanf(copy, "%Lf", &r);
  new->code = flags & SEEN_FSUFFIX ? F_CONSTANT : D_CONSTANT;
  new->rawCode = F_CONSTANT_RAW;
  new->value.ldv = r;

  if (size >= 64) {
    free(copy);
  }
}

static void parseNumericLiteral(ParserContext *ctx, Token *new, unsigned flags, const char *buffer, size_t size, int radix) {

  unsigned i = 0;

  Token dummy = { 0 };
  dummy.locInfo = new->locInfo;
  dummy.pos = buffer;
  dummy.length = size;
  Coordinates coords = { &dummy, &dummy };
  uint64_t result = 0;
  unsigned overflow = 0;
  for (;i < size && isHexDigit(buffer[i]); ++i) {
      uint64_t old = result;
      result *= radix;

      overflow |= (result / radix) != old;

      uint64_t v = hexToNum(buffer[i]);
      result += v;

      overflow |= result < v;
  }

  if (overflow) {
      // overflow;
      // uint64_t f = 7778213193418937241981798831231;
      reportDiagnostic(ctx, DIAG_INT_TOO_LARGE, &coords);
      new->value.iv = 0;
      return;
  } /* else {
      int64_t sResult = 0;
      switch (new->code) {
      case I_CONSTANT:
          sResult = (int32_t)result;
          if ((int32_t)result < 0) {
              // report conversion from
              reportDiagnostic(ctx, DIAG_IMPLICIT_CONVERSION, &coords, "long", "int", result, sResult);
          }
      case U_CONSTANT:
          sResult = (uint32_t)result;
          if (result != (uint64_t)sResult) {
              reportDiagnostic(ctx, DIAG_IMPLICIT_CONVERSION, &coords, "long", "unsigned int", result, sResult);
          }
      case L_CONSTANT:
          sResult = (int64_t)result;
          if (sResult < 0) {
              reportDiagnostic(ctx, DIAG_SIGNED_LITERAL_TOO_LARGE, &coords);
          }
      }
  } */

  new->value.iv = result;
}

static void parseIntegerLiteral(ParserContext *ctx, Token *new, unsigned flags, const char *buffer, size_t size) {

  new->rawCode = I_CONSTANT_RAW;

  unsigned isLong = flags & (IS_LONG | IS_LONGLONG);
  unsigned isU = flags & IS_UNSIGNED;

  if (isU) new->code = isLong ? UL_CONSTANT : U_CONSTANT;
  else new->code = isLong ? L_CONSTANT : I_CONSTANT;

  if (flags & IS_BINARY) {
      parseNumericLiteral(ctx, new, flags, buffer + 2, size, 2);
  } else if (flags & IS_OCTAL) {
      parseNumericLiteral(ctx, new, flags, buffer + 1, size, 8);
  } else if (flags & IS_DECIMAL) {
      parseNumericLiteral(ctx, new, flags, buffer, size, 10);
  } else {
      assert(flags & IS_HEX);
      parseNumericLiteral(ctx, new, flags, buffer + 2, size, 16);
  }
}

static void parseNumberLiteral(ParserContext *ctx, Token *new, unsigned flags, const char *buffer, size_t size) {
  if (flags & IS_FLOAT) {
      parseFloatLiteral(new, flags, buffer, size);
  } else {
      parseIntegerLiteral(ctx, new, flags, buffer, size);
  }
}

static unsigned lexNumber(ParserContext *ctx, Token *new, const char *buffer, size_t size) {
  size_t i = 0;

  while (i < size) {
    if (buffer[i] && buffer[i+1] && strchr("eEpP", buffer[i]) && strchr("+-", buffer[i+1])) {
      i += 2;
    } else if (isalfanum(buffer[i]))
      ++i;
    else if (buffer[i] == '.') {
      ++i;
    } else break;
  }

  unsigned flags = verifyNumberLiteral(ctx, new, buffer, &i);

  if (flags & HAS_ERROR) {
      // some error found
      new->value.iv = 0;
      new->code = new->rawCode = I_CONSTANT;
      return i;
  }

  parseNumberLiteral(ctx, new, flags, buffer, i);

  return i;
}


static unsigned skipUntilNewLineOrSymbol(const char *buffer, size_t size, char cc) {
  unsigned i = 0;

  while (i < size) {
      char c = buffer[i];
      if (c == cc || c == '\n') break;
      ++i;
  }

  return i;
}

static unsigned lexEscapedLiteralSymbol(ParserContext *ctx, LocationInfo *locInfo, int *v, const char *buffer, size_t size, Boolean isWide) {
  unsigned i = 0;

  assert(buffer[i] == '\\');
  ++i;

  Boolean isHex = FALSE;

  Token dummy = { 0 };
  dummy.locInfo = locInfo;
  dummy.pos = buffer;
  Coordinates coords = { &dummy, &dummy };

  uint64_t result = 0;
  const uint64_t max = isWide ? 0xFFFF : 0xFF;

  if (i < size) {
    char c = buffer[i++];

    // TODO: suppot wide codes
    switch (c) {
      case '0': case '1': case '2': case '3':
      case '4': case '5': case '6': case '7':
          result = c - '0';
          while (i < size && isOctalDigit(buffer[i]) && result < max) {
            result <<= 3;
            result += buffer[i++] - '0';
          }
          *v = result;
          break;
        case 'a':
          *v = '\a';
          break;
        case 'b':
          *v ='\b';
          break;
        case 'f':
          *v = '\f';
          break;
        case 'n':
          *v = '\n';
          break;
        case 'r':
          *v = '\r';
          break;
        case 't':
          *v = '\t';
          break;
        case 'v':
          *v = '\v';
          break;
        case '\\':
          *v = '\\';
          break;
        case '\'':
          *v = '\'';
          break;
        case '"':
          *v = '"';
          break;
        case '?':
          *v = '\?';
          break;
        case 'x': { // \x12 -- \xff - max
          if (i < size && isHexDigit(buffer[i])) {
            result = hexToNum(buffer[i++]);
            while (i < size && isHexDigit(buffer[i]) && result < max) {
              char c = buffer[i++];
              result <<= 4;
              result += hexToNum(c);
            }
          } else {
            dummy.length = i;
            reportDiagnostic(ctx, DIAG_NO_ESACPED_HEX_DIGITS, &coords);
          }
          *v = result;
          break;
        }
        default: {
          *v = '\\';
          dummy.length = i;
          reportDiagnostic(ctx, DIAG_UNKNOWN_ESCAPED_SEQ, &coords, c);
          break;
        }
    }
  } else {
      *v = '\\';
  }

  return i;
}

static unsigned lexChar(ParserContext *ctx, Token *new, const char *buffer, size_t size) {

  unsigned i = 0;

  Boolean isWide = FALSE;

  if (buffer[i] == 'L') {
      isWide = TRUE;
      ++i;
  }

  assert(buffer[i] == '\'');
  ++i;

  new->code = C_CONSTANT;
  new->rawCode = I_CONSTANT_RAW;

  Token dummy = { 0 };
  dummy.locInfo = new->locInfo;
  dummy.pos = buffer;

  Coordinates coords = { &dummy, &dummy };

  int64_t result = 0;
  uint64_t limit = 0;

  if (i < size) {
    char c = buffer[i++];

    if (c == '\\') {
      c = buffer[i++];
      switch (c) {
          case '0': case '1': case '2': case '3':
          case '4': case '5': case '6': case '7':
            result = c - '0';
            while (i < size && isOctalDigit(buffer[i])) {
              result <<= 3;
              result += buffer[i++] - '0';
              uint16_t check = result & (isWide ? 0xFFFF : 0xFF);
              if (check != result) {
                  dummy.length = i;
                  reportDiagnostic(ctx, DIAG_INTEGER_CONST_OVERFLOW, &coords);
                  break;
              }
            }
            if (buffer[i] != '\'') {
                goto check_multichar;
            }

            limit = isWide ? 0xFFFF : 0xFF;
            if (result > limit) {
                dummy.length = i;
                reportDiagnostic(ctx, DIAG_ESCAPE_SEC_OOR_OCT, &coords);
            }
            ++i; // consume '\'';
            break;
          case 'a': result = '\a'; goto check_multichar;
          case 'b': result ='\b'; goto check_multichar;
          case 'f': result = '\f'; goto check_multichar;
          case 'n': result = '\n'; goto check_multichar;
          case 'r': result = '\r'; goto check_multichar;
          case 't': result = '\t'; goto check_multichar;
          case 'v': result = '\v'; goto check_multichar;
          case '\\': result = '\\'; goto check_multichar;
          case '\'': result = '\''; goto check_multichar;
          case '"': result = '"'; goto check_multichar;
          case '?': result = '\?'; goto check_multichar;
          check_multichar:
          if (i < size && buffer[i] != '\'') {
              unsigned ii = skipUntilNewLineOrSymbol(buffer + i, size - i, '\'') + i;
              if (buffer[ii] != '\'') {
                dummy.length = ii;
                i = ii;
                reportDiagnostic(ctx, DIAG_UNTERMINATED_CHAR_STRING, &coords, '\'');
              } else {
                result = 0;
                unsigned k = isWide ? 2 : 1;
                while (buffer[k] != '\'') {
                    result <<= 8;
                    result += buffer[k++];
                }
                dummy.length = ii + 1;
                i = ii + 1;
                reportDiagnostic(ctx, DIAG_MULTI_CHAR_CONST, &coords);
              }
          } else {
              ++i; // eat '
          }
          break;

          case 'x': { // \x12 -- \xff - max
            result = 0;
            if (i < size && isHexDigit(buffer[i])) {
              while (i < size && isHexDigit(buffer[i])) {
                  char c = buffer[i++];
                  result <<= 4;
                  result += hexToNum(c);
                  uint16_t check = result & (isWide ? 0xFFFF : 0xFF);
                  if (check != result) {
                      dummy.length = i;
                      reportDiagnostic(ctx, DIAG_INTEGER_CONST_OVERFLOW, &coords);
                      break;
                  }
              }

              if (buffer[i] != '\'') {
                  goto check_multichar;
              }

              limit = isWide ? 0xFFFF : 0xFF;
              if (result > limit) {
                  dummy.length = i;
                  reportDiagnostic(ctx, DIAG_ESCAPE_SEC_OOR_HEX, &coords);
              }
              ++i; // consume '\'';
            } else {
              dummy.length = i;
              reportDiagnostic(ctx, DIAG_NO_ESACPED_HEX_DIGITS, &coords);
            }
            break;
          }
          default: {
            result = c;
            dummy.length = i;
            reportDiagnostic(ctx, DIAG_UNKNOWN_ESCAPED_SEQ, &coords, c);
            if (buffer[i] != '\'') {
                goto check_multichar;
            }
            ++i; // eat '
            break;
          }
      }
    } else {
        result = c;
        if (buffer[i] != '\''){
          goto check_multichar;
        } else ++i;
    }
  }

  new->value.iv = result;

  return i;

}

static unsigned lexStringLiteral(ParserContext *ctx, Token *new, LocationInfo *locInfo, const char *buffer, size_t size) {
  unsigned l = 0;

  Boolean isWide = FALSE;

  if (buffer[l] == 'L') {
      ++l;
      isWide = TRUE; // actually is not yet supported
  }

  assert(buffer[l] == '"');
  ++l;

  Token dummy = { 0 };
  dummy.locInfo = new->locInfo;
  dummy.pos = buffer;
  Coordinates coords = { &dummy, &dummy };

  StringBuffer sb = { 0 };

  size_t stringSize = 0;

  while (l < size) {
      char c = buffer[l];
      if (c == '\\') {
          if (buffer[l+1] == '\n') {
            l += 2;
            locInfo->fileInfo.linesPos[locInfo->fileInfo.lineno++] = l;
          } else {
            int v = 0;
            l += lexEscapedLiteralSymbol(ctx, new->locInfo, &v, &buffer[l], size - l, isWide);
            putSymbol(&sb, (char)v);
          }
          continue;
      }
      if (c == '\n') {
          dummy.length = l;
          reportDiagnostic(ctx, DIAG_UNTERMINATED_CHAR_STRING, &coords, '"');
          break;
      }
      if (c == '"') {
          ++l;
          break;
      }

      putSymbol(&sb, c);
      ++l;
  }

  if (buffer[l - 1] != '"') {
      dummy.length = l;
      reportDiagnostic(ctx, DIAG_UNTERMINATED_CHAR_STRING, &coords, '"');
  }

  char *result = allocateString(ctx, sb.idx + 1);
  if (sb.ptr) {
    memcpy(result, sb.ptr, sb.idx);
    releaseHeap(sb.ptr);
  }
  result[sb.idx] = '\0';

  new->code = new->rawCode = STRING_LITERAL;
  new->value.text.v = result;
  new->value.text.l = sb.idx + 1;

  return l;
}

static Boolean isValidIdStart(char c) {
  if (c == '_') return TRUE;
  if ('a' <= c && c <= 'z') return TRUE;
  if ('A' <= c && c <= 'Z') return TRUE;
  return FALSE;
}

static Boolean isValidIdSymbol(char c) {
  return isValidIdStart(c) || isDigit(c);
}

static unsigned lexIdentifier(ParserContext *ctx, Token *new, const char *buffer, size_t size) {

  assert(isValidIdStart(buffer[0]));
  unsigned i = 1;

  new->code = new->rawCode = IDENTIFIER;

  while (i < size) {
      if (isValidIdSymbol(buffer[i])) {
          ++i;
          continue;
      }
      break;
  }

  char *id = allocateString(ctx, i + 1);
  strncpy(id, buffer, i);
  new->id = id;

  return i;
}

static unsigned skipWhiteSpace(const char *buffer, size_t size) {
  unsigned i = 0;
  while (i < size) {
      char c = buffer[i];
      if (c == ' ' || c == '\t' || c == '\f' || c == '\v') {
          ++i;
          continue;
      }
      break;
  }

  return i;
}

static unsigned skipLineComment(const char *buffer, size_t size) {
  unsigned i = 1;

  for (; i < size; ++i) {
      if (buffer[i] == '\n') {
          break;
      }
  }

  return i;
}

static unsigned skipBlockComment(ParserContext *ctx, LocationInfo *locInfo, const char *buffer, unsigned start, size_t size) {
  unsigned i = start + 2; // skip /*

  unsigned *linePos = locInfo->fileInfo.linesPos;
  unsigned lineCounter = locInfo->fileInfo.lineno;

  while (i < size) {
      char c = buffer[i];
      if (c == '\n') {
          linePos[lineCounter++] = i;
          locInfo->fileInfo.lineno = lineCounter;
      } else if (c == '*') {
          if (i + 1 < size) {
              if (buffer[i + 1] == '/') {
                  i += 2;
                  break;
              }
          } else {
              Token dummy = { 0 };
              dummy.locInfo = locInfo;
              dummy.pos = buffer;
              dummy.length = 2;
              Coordinates coords = { &dummy, &dummy };
              reportDiagnostic(ctx, DIAG_UNTERMINATED_COMMENT, &coords);
              break;
          }
      }
      ++i;
  }

  return i;
}

Token *tokenizeBuffer(ParserContext *ctx, LocationInfo *locInfo, unsigned *linePos, Token *tail) {

  unsigned int i = 0;

  unsigned startOfLine = 1;
  unsigned lineCounter = 0;

  linePos[lineCounter++] = 0;

  const char *buffer = locInfo->buffer;

  Token head = { 0 }, *current = &head;
  head.locInfo = locInfo;
  Token *new = allocToken2(ctx, locInfo);

  const size_t bufferSize = locInfo->bufferSize - 1;
  Coordinates coords = { &head, &head };


  while (i < bufferSize) {

      new->startOfLine = startOfLine;

      head.pos = &buffer[i];

      unsigned tokenStart = i;
      unsigned tokenEnd;

      int code;

      Boolean isWide = FALSE;
      char c = buffer[i];

      switch (c) {
        case '\\': { // escaping
            unsigned skipped = skipWhiteSpace(&buffer[i], bufferSize - i);
            if ((i + skipped + 1) < bufferSize) {
                if (buffer[i + skipped + 1] == '\n') {
                    i += (skipped + 2);
                    linePos[lineCounter++] = i;
                    locInfo->fileInfo.lineno = lineCounter;
                    if (skipped) {
                        head.length = &buffer[i] - head.pos;
                        reportDiagnostic(ctx, DIAG_SPACE_SEPARATED, &coords);
                    }
                    continue;
                }
            }
          }
          new->code = new->rawCode = '\\';
          tokenEnd = ++i;
          break;
        case '\r':
        case '\n':
          ++i;
          linePos[lineCounter++] = i;
          locInfo->fileInfo.lineno = lineCounter;
          startOfLine = 1;
          new->hasLeadingSpace = 0;
          continue;
        case ' ':
        case '\t':
        case '\f':
        case '\v':
          new->hasLeadingSpace = 1;
          ++i;
          continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          i += lexNumber(ctx, new, &buffer[i], bufferSize - tokenStart);
          tokenEnd = i;
          break;
        case 'L': // wide char or identifier or wide string
          if ((i + 1) == bufferSize || buffer[i + 1] != '\'') {
              goto lexID;
          }
        case '\'':
          i += lexChar(ctx, new, &buffer[i], bufferSize - tokenStart);
          tokenEnd = i;
          break;
        case '"':
          i += lexStringLiteral(ctx, new, locInfo, &buffer[i], bufferSize - tokenStart);
          tokenEnd = i;
          break;
        case '/': // or /* or /= or /
          code = '/';
          if ((i + 1) < bufferSize) {
            char c = buffer[i+1];
            if (c == '/') {
                i += skipLineComment(&buffer[i], bufferSize - i);
                continue;
            } else if (c == '*') {
                i = skipBlockComment(ctx, locInfo, buffer, i, bufferSize);
                continue;
            } else if (c == '=') {
                code = DIV_ASSIGN;
                ++i;
            }
          }
          tokenEnd = ++i;
          new->code = new->rawCode = code;
          break;
        case '.': // ... or .. or x.y or .1
          new->code = new->rawCode = '.';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if ('0' <= buffer[i] && buffer[i] <= '9') { // .1
                --i;
                i += lexNumber(ctx, new, &buffer[i], bufferSize - i);
                tokenEnd = i;
              } else if (buffer[i] == '.') { // .. or ...
                  new->code = new->rawCode = DDOT; // ..
                  tokenEnd = ++i;
                  if (i < bufferSize && buffer[i] == '.') {
                      new->code = new->rawCode = ELLIPSIS; // ...
                      tokenEnd = ++i;
                  }
              }
          }
          break;
        case '>': // >>= or >> or >= or >
          new->code = new->rawCode = '>';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') { // >=
                new->code = new->rawCode = GE_OP;
                tokenEnd = ++i;
              } else if (buffer[i] == '>') { // >> or >>=
                  new->code = new->rawCode = RIGHT_OP;
                  tokenEnd = ++i;
                  if (i < bufferSize && buffer[i] == '=') {
                      new->code = new->rawCode = RIGHT_ASSIGN;
                      tokenEnd = ++i;
                  }
              }
          }
          break;
        case '<': // <<= or << or <% == { or <: == [ or <= or <
          new->code = new->rawCode = '<';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') { // <=
                new->code = new->rawCode = LE_OP;
                tokenEnd = ++i;
              } else if (buffer[i] == ':') { // :
                  new->code = new->rawCode = '[';
                  tokenEnd = ++i;
              } else if (buffer[i] == '%') { // %
                  new->code = new->rawCode = '{';
                  tokenEnd = ++i;
              } else if (buffer[i] == '<') { // << or <<=
                  new->code = new->rawCode = LEFT_OP;
                  tokenEnd = ++i;
                  if (i < bufferSize && buffer[i] == '=') {
                      new->code = new->rawCode = LEFT_ASSIGN;
                      tokenEnd = ++i;
                  }
              }
          }
          break;
        case '+': // += or ++ or +
          new->code = new->rawCode = '+';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') {
                new->code = new->rawCode = ADD_ASSIGN;
                tokenEnd = ++i;
              } else if (buffer[i] == '+') {
                  new->code = new->rawCode = INC_OP;
                  tokenEnd = ++i;
              }
          }
          break;
        case '-': // -= or -- or -> or -
          new->code = new->rawCode = '-';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') {
                new->code = new->rawCode = SUB_ASSIGN;
                tokenEnd = ++i;
              } else if (buffer[i] == '>') {
                  new->code = new->rawCode = PTR_OP;
                  tokenEnd = ++i;
              } else if (buffer[i] == '-') {
                  new->code = new->rawCode = DEC_OP;
                  tokenEnd = ++i;
              }
          }
          break;
        case '*': // *= or *
          new->code = new->rawCode = '*';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '=') {
              new->code = new->rawCode = MUL_ASSIGN;
              tokenEnd = ++i;
          }
          break;
        case '%': // %= or %> == } or %
          new->code = new->rawCode = '%';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') {
                new->code = new->rawCode = MOD_ASSIGN;
                tokenEnd = ++i;
              } else if (buffer[i] == '>') {
                  new->code = new->rawCode = '}';
                  tokenEnd = ++i;
              }
          }
          break;
        case '&': // &= or && or &
          new->code = new->rawCode = '&';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') {
                new->code = new->rawCode = AND_ASSIGN;
                tokenEnd = ++i;
              } else if (buffer[i] == '&') {
                  new->code = new->rawCode = AND_OP;
                  tokenEnd = ++i;
              }
          }
          break;
        case '^': // ^= or ^
          new->code = new->rawCode = '^';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '=') {
              new->code = new->rawCode = XOR_ASSIGN;
              tokenEnd = ++i;
          }
          break;
        case '|': // |= or || or |
          new->code = new->rawCode = '|';
          tokenEnd = ++i;
          if (i < bufferSize) {
              if (buffer[i] == '=') {
                new->code = new->rawCode = OR_ASSIGN;
                tokenEnd = ++i;
              } else if (buffer[i] == '|') {
                  new->code = new->rawCode = OR_OP;
                  tokenEnd = ++i;
              }
          }
          break;
        case '=': // '==' or '='
          new->code = new->rawCode = '=';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '=') {
              new->code = new->rawCode = EQ_OP;
              tokenEnd = ++i;
          }
          break;
        case '!': // != or !
          new->code = new->rawCode = '!';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '=') {
              new->code = new->rawCode = NE_OP;
              tokenEnd = ++i;
          }
          break;
        case ':': // : or :> == ]
          new->code = new->rawCode = ':';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '>') {
              new->code = new->rawCode = ']';
              tokenEnd = ++i;
          }
          break;
        case ';': // ;
        case '{': // {
        case '}': // }
        case ',': // ,
        case '(': // (
        case ')': // )
        case '[': // [
        case ']': // ]
        case '~': // ~
        case '?': // ?
          new->code = new->rawCode = buffer[i];
          tokenEnd = ++i;
          break;
        case '#': // # or ##
          new->code = new->rawCode = '#';
          tokenEnd = ++i;
          if (i < bufferSize && buffer[i] == '#') {
              new->code = new->rawCode = DSHARP;
              tokenEnd = ++i;
          }
          break;
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
        case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case '_': // identifier
        lexID:
          i += lexIdentifier(ctx, new, &buffer[i], bufferSize - tokenStart);
          tokenEnd = i;
          break;
        default: // bad character
          tokenEnd = ++i;
          new->code = new->rawCode = BAD_CHARACTER;
          break;
      }

      new->pos = &buffer[tokenStart];
      new->length = tokenEnd - tokenStart;

      current = current->next = new;

      new = allocToken2(ctx, locInfo);
      startOfLine = 0;
  }


  current->next = tail;

  return head.next;
}

static const char breakWord[] = "break";
static const char caseWord[] = "case";
static const char charWord[] = "char";
static const char constWord[] = "const";
static const char continueWord[] = "continue";
static const char doubleWord[] = "double";
static const char defaultWord[] = "default";
static const char elseWord[] = "else";
static const char enumWord[] = "enum";
static const char externWord[] = "extern";
static const char floatWord[] = "float";
static const char gotoWord[] = "goto";
static const char longWord[] = "long";
static const char returnWord[] = "return";
static const char registerWord[] = "register";
static const char volatileWord[] = "volatile";
static const char voidWord[] = "void";
static const char shortWord[] = "short";
static const char sizeofWord[] = "sizeof";
static const char staticWord[] = "static";
static const char signedWord[] = "signed";
static const char structWord[] = "struct";
static const char switchWord[] = "switch";
static const char alignofWord[] = "_Alignof";
static const char typedefWord[] = "typedef";
static const char unionWord[] = "union";
static const char unsignedWord[] = "unsigned";
static const char whileWord[] = "while";

static void maybeSetupKeyword(Token *token) {
  if (token->code != IDENTIFIER) return;
  const char *pos = token->pos;
  size_t length = token->length;
  char c = pos[0];
  uint32_t code, tokenCode;
  uint64_t code8, tokenCode8;
  switch (c) {
  case 'b': // break
      if (length == 5) {
          code = *(uint32_t*)(&breakWord[1]);
          tokenCode = *(uint32_t*)(&pos[1]);
          if (tokenCode == code) {
              token->code = BREAK;
          }
      }
      return;
  case 'c': // case | char | const | continue
      if (length == 4) {
          tokenCode = *(uint32_t*)(&pos[0]);
          code = *(uint32_t*)caseWord;
          if (code == tokenCode) {
              token->code = CASE;
              return;
          }
          code = *(uint32_t*)charWord;
          if (code == tokenCode) {
            token->code = CHAR;
            return;
          }
      } else if (length == 5) {
          tokenCode = *(uint32_t*)(&pos[1]);
          code = *(uint32_t*)(&constWord[1]);
          if (code == tokenCode) {
              token->code = CONST;
          }
      } else if (length == 8) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          code8 = *(uint64_t*)continueWord;
          if (tokenCode8 == code8) {
              token->code = CONTINUE;
          }
      }
      return;
  case 'd': // default | do | double
      if (length == 2 && pos[1] == 'o') {
          token->code = DO;
      } else if (length == 6) {
          tokenCode = *(uint32_t*)(&pos[2]);
          code = *(uint32_t*)(&doubleWord[2]);
          if (pos[1] == 'o' && tokenCode == code) {
              token->code = DOUBLE;
          }
      } else if (length == 7) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          tokenCode8 &= ~(0xFFUL << 56);
          code8 = *(uint64_t*)defaultWord;
          if (tokenCode8 == code8) {
              token->code = DEFAULT;
          }
      }
      return;
  case 'e': // else | enum | extern
      if (length == 4) {
          tokenCode = *(uint32_t*)(&pos[0]);
          code = *(uint32_t*)elseWord;
          if (code == tokenCode) {
              token->code = ELSE;
              return;
          }
          code = *(uint32_t*)enumWord;
          if (code == tokenCode) {
            token->code = ENUM;
            return;
          }
      } else if (length == 6) {
          tokenCode = *(uint32_t*)(&pos[2]);
          code = *(uint32_t*)(&externWord[2]);
          if (pos[1] == 'x' && tokenCode == code) {
              token->code = EXTERN;
          }
      }
      return;
  case 'f': // float | for
      if (length == 5) {
          code = *(uint32_t*)(&floatWord[1]);
          tokenCode = *(uint32_t*)(&pos[1]);
          if (tokenCode == code) {
              token->code = FLOAT;
          }
      } else if (length == 3) {
          if (pos[1] == 'o' && pos[2] == 'r') {
              token->code = FOR;
          }
      }
      return;
  case 'g': // goto
      if (length == 4) {
          tokenCode = *(uint32_t*)(&pos[0]);
          code = *(uint32_t*)(&gotoWord[0]);
          if (code == tokenCode) {
              token->code = GOTO;
          }
      }
      return;
  case 'i': // if | int
      if (length == 2 && pos[1] == 'f') {
          token->code = IF;
      } else if (length == 3 && pos[1] == 'n' && pos[2] == 't') {
          token->code = INT;
      }
      return;
  case 'l': // long
      if (length == 4) {
          tokenCode = *(uint32_t*)(&pos[0]);
          code = *(uint32_t*)(&longWord[0]);
          if (code == tokenCode) {
              token->code = LONG;
          }
      }
      return;
  case 'r': // register | return
      if (length == 6 && pos[1] == 'e') {
          tokenCode = *(uint32_t*)(&pos[2]);
          code = *(uint32_t*)(&returnWord[2]);
          if (code == tokenCode) {
              token->code = RETURN;
          }
      } else if (length == 8) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          code8 = *(uint64_t*)registerWord;
          if (tokenCode8 == code8) {
              token->code = REGISTER;
          }
      }
      return;
  case 'v': // volatile | void
      if (length == 4) {
          tokenCode = *(uint32_t*)(&pos[0]);
          code = *(uint32_t*)(&voidWord[0]);
          if (code == tokenCode) {
              token->code = VOID;
          }
      } else if (length == 8) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          code8 = *(uint64_t*)volatileWord;
          if (tokenCode8 == code8) {
              token->code = VOLATILE;
          }
      }
      return;
  case 's': // short | signed | sizeof | static | struct | switch
      if (length == 5) {
          code = *(uint32_t*)(&shortWord[1]);
          tokenCode = *(uint32_t*)(&pos[1]);
          if (tokenCode == code) {
              token->code = SHORT;
          }
      } else if (length == 6) {
          tokenCode = *(uint32_t*)(&pos[2]);
          if (pos[1] == 'i') {
              code = *(uint32_t*)(&signedWord[2]);
              if (tokenCode == code) {
                  token->code = SIGNED;
                  return;
              }
              code = *(uint32_t*)(&sizeofWord[2]);
              if (tokenCode == code) {
                  token->code = SIZEOF;
                  return;
              }
          } else if (pos[1]== 't') {
              code = *(uint32_t*)(&staticWord[2]);
              if (tokenCode == code) {
                  token->code = STATIC;
                  return;
              }
              code = *(uint32_t*)(&structWord[2]);
              if (tokenCode == code) {
                  token->code = STRUCT;
                  return;
              }
          } else if (pos[1] == 'w') {
              code = *(uint32_t*)(&switchWord[2]);
              if (tokenCode == code) {
                  token->code = SWITCH;
                  return;
              }
          }
      }
      return;
  case '_': // _Alignof
      if (length == 8) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          code8 = *(uint64_t*)alignofWord;
          if (tokenCode8 == code8) {
              token->code = ALIGNOF;
          }
      }
      return;
  case 't': // typedef
      if (length == 7) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          tokenCode8 &= ~(0xFFUL << 56);
          code8 = *(uint64_t*)typedefWord;
          if (tokenCode8 == code8) {
              token->code = TYPEDEF;
          }
      }
      return;
  case 'u': // union | unsigned
      if (length == 5) {
          code = *(uint32_t*)(&unionWord[1]);
          tokenCode = *(uint32_t*)(&pos[1]);
          if (tokenCode == code) {
              token->code = UNION;
          }
      } else if (length == 8) {
          tokenCode8 = *(uint64_t*)(&pos[0]);
          code8 = *(uint64_t*)unsignedWord;
          if (tokenCode8 == code8) {
              token->code = UNSIGNED;
          }
      }
      return;
  case 'w': // while
      if (length == 5) {
          code = *(uint32_t*)(&whileWord[1]);
          tokenCode = *(uint32_t*)(&pos[1]);
          if (tokenCode == code) {
              token->code = WHILE;
          }
      }
  }
}

Token *nextToken(ParserContext *ctx) {
  Token *cur = ctx->token;
  Token *next = cur ? cur->next : ctx->firstToken;
  Token *prev = cur;

  if (!next) {
      return prev;
  }

  if (next->rawCode == IDENTIFIER && !ctx->stateFlags.inPP) {
      if (isTypeName(ctx, next->id, ctx->currentScope)) {
          next->code = TYPE_NAME;
      } else {
        EnumConstant *enumerator = enumConstant(ctx, next->id);
        if (enumerator) {
          next->code = ENUM_CONST;
          next->value.iv = enumerator->value;
        } else {
          maybeSetupKeyword(next);
        }
      }
  }

  if (ctx->config->logTokens) {
    char buffer[1024];
    dumpToken(buffer, sizeof buffer, next);
    printf("%s\n", buffer); fflush(stdout);
  }

  ctx->token = next;
  return next;
}
