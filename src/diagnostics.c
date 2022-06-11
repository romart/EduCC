
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>

#include "diagnostics.h"
#include "pp.h"
#include "parser.h"
#include "mem.h"
#include "treeDump.h"


static Severity severities[] = {
  { DSK_INFO, "info", FALSE },
  { DSK_WARNING, "warning", FALSE },
  { DSK_ERROR, "error", TRUE },
  { DSK_CRITICAL_ERROR, "critical error", TRUE }
};

static Severity *infoSeverity = &severities[DSK_INFO];
static Severity *warningSeverity = &severities[DSK_WARNING];
static Severity *errorSeverity = &severities[DSK_ERROR];
static Severity *criticalErrorSeverity = &severities[DSK_CRITICAL_ERROR];

static DiagnosticDescriptor descriptors[] = {
#define DIAGNOSTIC_DEF(s, type, id, fmt) { DSK_##s, IDT_##type, DIAG_##id, #id, fmt }
  #include "diagnosticList.h"
  DIAGNOSTICS
#undef DIAGNOSTIC_DEF
};

static Diagnostic *allocDiagnostic(ParserContext *ctx) {
  return (Diagnostic *)areanAllocate(ctx->memory.diagnosticsArena, sizeof (Diagnostic));
}

static char *allocMessageString(ParserContext *ctx, size_t size) {
  return (char *)areanAllocate(ctx->memory.diagnosticsArena, sizeof(char) * size);
}

static LocationInfo *findLocationInfo(ParserContext *ctx, const char *fileName) {
  LocationInfo *locInfo = ctx->locationInfo;

  while (locInfo) {
      if (strcmp(fileName, locInfo->fileInfo.fileName) == 0) break;
  }

  return locInfo;
}

static void computeLineAndCollumn(ParserContext *ctx, LocationInfo *locInfo, int _pos, int *line, int *col, int *lineStartOffset) {
  if (_pos < 0) {
      *line = NO_LOC;
      *col = NO_LOC;
      return;
  }

  if (locInfo->kind == LIK_MACRO) {
      *line = NO_LOC;
      *col = _pos;
      *lineStartOffset = 0;
      return;
  }

  assert(locInfo->kind == LIK_FILE);

  unsigned *lineMap = locInfo->fileInfo.linesPos;
  assert(lineMap != NULL);

  unsigned pos = (unsigned)_pos;

  unsigned lineMax = locInfo->fileInfo.lineno;
  unsigned lineNum = 0;
  unsigned lineOffset = 0;
  unsigned previousLine = 0;


  for (lineNum = 0; lineNum < lineMax; ++lineNum) {
      lineOffset = lineMap[lineNum];
      if (pos < lineOffset) break;
  }

  --lineNum;
  lineOffset = lineMap[lineNum];

  *line = lineNum + 1;
  *col = pos - lineOffset + 1;
  *lineStartOffset = lineOffset;
}

const Severity *getSeverity(enum DiagSeverityKind id) {
  assert(0 <= id && id < DSK_TOTAL_SEVERITY_COUNT);
  return &severities[id];
}

void reportDiagnostic(ParserContext *ctx, enum DiagnosticId diag, const Coordinates *location, ...) {
  Diagnostic *newDiagnostic = allocDiagnostic(ctx);
  char buffer[1024] = { 0 };

  assert(0 <= diag && diag < DIAG_TOTAL_COUNT);
  const DiagnosticDescriptor *descriptor = &descriptors[diag];
  newDiagnostic->descriptor = descriptor;

  int r = 0;
  unsigned i = 0, j = 0;
  const char *fmt = descriptor->formatString;
  size_t bufferSize = sizeof (buffer);

  va_list args;
  va_start(args, location);

  // TODO: don't render message on creation, do so during output

  for (;;) {
      char fc = fmt[i++];

      if (fc == '\0' || j >= sizeof buffer) break;

      char fmtBuf[10] = { 0 };
      unsigned k = 0;

      if (fc == '%') {
        fmtBuf[k++] = fc;
        char fc2 = fmt[i++];
        if (fc2 == 't') {
            char fc3 = fmt[i++];
            if (fc3 == 'r') {
                // type ref
                TypeRef *ref = va_arg(args, TypeRef *);
                r = renderTypeRef(ref, buffer + j, bufferSize - j);
            } else if (fc3 == 'd') {
                // type desc
                TypeDesc *desc = va_arg(args, TypeDesc *);
                r = renderTypeDesc(desc, buffer + j, bufferSize - j);
            } else if (fc3 == 'k') {
                int tokenCode = va_arg(args, int);
                char tb[2];
                const char *tokenName = tokenNameInBuffer(tokenCode, tb);
                r = snprintf(buffer + j, bufferSize - j, "%s", tokenName);
            } else {
                buffer[j] = fc;
                buffer[j + 1] = fc2;
                buffer[j + 2] = fc3;
                r = 3;
            }
            // types
        } else {
            if (fc2 == 'l') {
                // long numerical formats
                fmtBuf[k++] = fc2;
                int fc3 = fmt[i++];
                fmtBuf[k++] = fc3;
                if (fc3 == 'd') {
                    // long decimal
                    int64_t v = va_arg(args, int64_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc3 == 'x') {
                    // long hex
                    int64_t v = va_arg(args, int64_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc3 == 'f') {
                    // double
                    double v = va_arg(args, double);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc3 == 'u') {
                    // unsigned long decimal
                    uint64_t v = va_arg(args, uint64_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else {
                    buffer[j] = fc;
                    buffer[j + 1] = fc2;
                    buffer[j + 2] = fc3;
                    r = 3;
                }

            } else {
                fmtBuf[k++] = fc2;
                if (fc2 == 's') {
                    const char *s = va_arg(args, const char *);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, s);
                } else if (fc2 == 'd') {
                    int32_t v = va_arg(args, int32_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc2 == 'x') {
                    // long hex
                    int32_t v = va_arg(args, int32_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc2 == 'f') {
                    // double
                    double v = va_arg(args, double);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc2 == 'u') {
                    // unsigned long decimal
                    uint32_t v = va_arg(args, uint32_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else if (fc2 == 'c') {
                    // char
                    uint32_t v = va_arg(args, int32_t);
                    r = snprintf(buffer + j, bufferSize - j, fmtBuf, v);
                } else {
                    buffer[j] = fc;
                    buffer[j + 1] = fc2;
                    r = 2;
                }
            }

        }
        if (r < 0) {
            j = r;
            break;
        }
        j += r;
      } else {
        buffer[j++] = fc;
      }
  }

  va_end(args);

  if (j > 0) {
    char *message = allocMessageString(ctx, j + 1);
    strncpy(message, buffer, j);
    newDiagnostic->message = message;
  } else {
    newDiagnostic->message = "<cannot render a diagnostic message>";
  }

  if (location) {
    Token *origLeft = originaToken(location->left);
    Token *origRight = originaToken(location->right);

    if (origLeft->locInfo->kind == LIK_FILE && origLeft->locInfo == origRight->locInfo) {
      LocationInfo *locInfo = origLeft->locInfo;
      ptrdiff_t startOffset = origLeft->pos - locInfo->buffer;
      ptrdiff_t endOffset = origRight->pos - locInfo->buffer + origRight->length;

      newDiagnostic->location.file = locInfo->fileInfo.fileName;
      computeLineAndCollumn(ctx, locInfo, startOffset, &newDiagnostic->location.lineStart, &newDiagnostic->location.colStart, &newDiagnostic->location.lineStartOffset);
      computeLineAndCollumn(ctx, locInfo, endOffset, &newDiagnostic->location.lineEnd, &newDiagnostic->location.colEnd, &newDiagnostic->location.lineEndOffset);
    }
  }

  if (ctx->diagnostics.tail) {
      ctx->diagnostics.tail->next = newDiagnostic;
  } else {
      ctx->diagnostics.head = newDiagnostic;
  }

  ctx->diagnostics.tail = newDiagnostic;
  ctx->diagnostics.count += 1;
}

#define ANSI_COLOR_RESET   "\x1b[0m"
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_PURPLE  "\x1b[95m"
#define ANSI_COLOR_BOLD    "\x1b[1m"

static void printVerboseDiagnostic(FILE *output, Diagnostic *diagnostic) {
  int toTerminal = isTerminal(output);
  const Severity *severity = getSeverity(diagnostic->descriptor->severityKind);
  const char *typeColor = severity->isError ? ANSI_COLOR_RED : ANSI_COLOR_PURPLE;
  int lineStart = diagnostic->location.lineStart;

  if (lineStart >= 0 && lineStart == diagnostic->location.lineEnd) {
    fprintf(output, "\n%5d | ", lineStart);

    FILE *file = fopen(diagnostic->location.file, "r");
    fseek(file, diagnostic->location.lineStartOffset, SEEK_SET);

    int outputCount = 0;

    int startHightLight = diagnostic->location.colStart - 1;
    int endHightLight = diagnostic->location.colEnd - 1;

    while (!feof(file)) {
        int c = fgetc(file);
        if (c && c != '\n') {
            if (outputCount == startHightLight) {
              if (toTerminal) {
                  fprintf(output, "%s%s", ANSI_COLOR_BOLD, typeColor);
              }
            }

            if (outputCount == endHightLight) {
                if (toTerminal) {
                    fprintf(output, ANSI_COLOR_RESET);
                }
            }

            outputCount++;
            fputc(c, output);
        } else {
            if (toTerminal) {
                fprintf(output, ANSI_COLOR_RESET);
            }
            break;
        }
    }

    fclose(file);

    int underlineCount = 0;
    fprintf(output, "\n      | ");

    Boolean underLine = FALSE;
    int printedUnderLine = 0;

    while (underlineCount < endHightLight) {
        if (underlineCount == startHightLight) {
          if (toTerminal) {
              fprintf(output, "%s%s", ANSI_COLOR_BOLD, typeColor);
          }
          underLine = TRUE;
        }

        if (underLine) {
          if (printedUnderLine == 0) {
              fputc('^', output);
          } else {
              fputc('~', output);
          }
          ++printedUnderLine;
        } else {
            fputc(' ', output);
        }
        ++underlineCount;
    }
    if (toTerminal) {
        fprintf(output, ANSI_COLOR_RESET);
    }
  }
}

void printDiagnostic(FILE *output, Diagnostic *diagnostic, Boolean verbose) {
  int toTerminal = isTerminal(output);

  if (toTerminal) {
      fprintf(output, ANSI_COLOR_BOLD);
  }

  fprintf(output, "%s:", diagnostic->location.file);

  if (diagnostic->location.lineStart >= 0) {
      fprintf(output, "%d:%d:", diagnostic->location.colStart, diagnostic->location.lineStart);
  }

  const Severity *severity = getSeverity(diagnostic->descriptor->severityKind);

  const char *typeColor = severity->isError ? ANSI_COLOR_RED : ANSI_COLOR_PURPLE;

  if (toTerminal) {
      fprintf(output, "%s", typeColor);
  }

  fprintf(output, " %s: ", severity->name);

  if (toTerminal) {
      fprintf(output, ANSI_COLOR_RESET);
  }

  fprintf(output, "%s", diagnostic->message);

  if (verbose) {
    printVerboseDiagnostic(output, diagnostic);
  }
}


