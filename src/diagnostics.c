
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>

#include "diagnostics.h"
#include "parser.h"
#include "mem.h"


static Severity infoSeverityImpl = { DSK_INFO, "info", FALSE };
static Severity warningSeverityImpl = { DSK_WARNING, "warning", FALSE };
static Severity errorSeverityImpl = { DSK_ERROR, "error", TRUE };
static Severity criticalErrorSeverityImpl = { DSK_CRITICAL_ERROR, "critical error", TRUE };

static Severity *infoSeverity = &infoSeverityImpl;
static Severity *warningSeverity = &warningSeverityImpl;
static Severity *errorSeverity = &errorSeverityImpl;
static Severity *criticalErrorSeverity = &criticalErrorSeverityImpl;


static Diagnostic *allocDiagnostic(ParserContext *ctx) {
  return (Diagnostic *)areanAllocate(ctx->memory.diagnosticsArena, sizeof (Diagnostic));
}

static char *allocMessageString(ParserContext *ctx, size_t size) {
  return (char *)areanAllocate(ctx->memory.diagnosticsArena, sizeof(char) * size);
}

static void computeLineAndCollumn(ParserContext *ctx, int _pos, int *line, int *col, int *lineStartOffset) {
  if (_pos < 0) {
      *line = NO_LOC;
      *col = NO_LOC;
      return;
  }

  unsigned *lineMap = ctx->locationInfo.linesPos;
  assert(lineMap != NULL);

  unsigned pos = (unsigned)_pos;

  unsigned lineMax = ctx->locationInfo.lineno;
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

static void reportDiagnostic(ParserContext *ctx, Severity *severity, int start, int end, const char *format, va_list args) {

  Diagnostic *newDiagnostic = allocDiagnostic(ctx);

  char buffer[1024] = { 0 };

  int stringSize = vsnprintf(buffer, sizeof buffer, format, args);

  newDiagnostic->severity = severity;
  if (stringSize >= 0) {
    char *message = allocMessageString(ctx, stringSize + 1);
    strncpy(message, buffer, stringSize);
    newDiagnostic->message = message;
  } else {
    newDiagnostic->message = "Cannot render a diagnostic message";
  }

  newDiagnostic->location.file = ctx->parsedFile->fileName;

  computeLineAndCollumn(ctx, start, &newDiagnostic->location.lineStart, &newDiagnostic->location.colStart, &newDiagnostic->location.lineStartOffset);
  computeLineAndCollumn(ctx, end, &newDiagnostic->location.lineEnd, &newDiagnostic->location.colEnd, &newDiagnostic->location.lineEndOffset);

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
  const char *typeColor = diagnostic->severity->isError ? ANSI_COLOR_RED : ANSI_COLOR_PURPLE;
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

  const char *typeColor = diagnostic->severity->isError ? ANSI_COLOR_RED : ANSI_COLOR_PURPLE;

  if (toTerminal) {
      fprintf(output, "%s", typeColor);
  }

  fprintf(output, " %s: ", diagnostic->severity->name);

  if (toTerminal) {
      fprintf(output, ANSI_COLOR_RESET);
  }

  fprintf(output, "%s", diagnostic->message);

  if (verbose) {
    printVerboseDiagnostic(output, diagnostic);
  }
}

void reportInfo(ParserContext *ctx, int start, int end, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  reportDiagnostic(ctx, infoSeverity, start, end, fmt, args);
  va_end(args);
}

void reportWarning(ParserContext *ctx, int start, int end, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  reportDiagnostic(ctx, warningSeverity, start, end, fmt, args);
  va_end(args);
}

void reportError(ParserContext *ctx, int start, int end, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  reportDiagnostic(ctx, errorSeverity, start, end, fmt, args);
  va_end(args);
}

void parseWarning(ParserContext *ctx, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  reportDiagnostic(ctx, warningSeverity, ctx->token->coordinates.startOffset, ctx->token->coordinates.endOffset, fmt, args);
  va_end(args);
}

void parseError(ParserContext *ctx, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  reportDiagnostic(ctx, errorSeverity, ctx->token->coordinates.startOffset, ctx->token->coordinates.endOffset, fmt, args);
  va_end(args);
}

