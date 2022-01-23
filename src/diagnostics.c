
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

static void computeLineAndCollumn(ParserContext *ctx, int _pos, int *line, int *col) {
  if (_pos < 0) {
      *line = NO_LOC;
      *col = NO_LOC;
      return;
  }

  unsigned *lineMap = ctx->locationInfo.linesPos;
  assert(lineMap != NULL);

  unsigned pos = (unsigned)_pos;

  unsigned lineMax = ctx->locationInfo.lineno;
  unsigned lineNum = lineMax / 2;
  unsigned lineOffset = 0;

  for (;;) {
     lineOffset = lineMap[lineNum];

     if (pos < lineOffset) {
       if (lineNum > 0) {
           unsigned prevLineOffset = lineMap[lineNum - 1];
           if (pos < prevLineOffset) {
               lineNum = lineNum / 2;
               continue;
           }
           lineOffset = prevLineOffset;
           break;
       }
     } else {
       if (lineNum + 1 < lineMax) {
         unsigned nextLineOffset = lineMap[lineNum + 1];
         if (pos >= nextLineOffset) {
             lineNum = (lineNum + lineMax) / 2;
             continue;
         }
       }
       break;
     }
  }

  *line = lineNum + 1;
  *col = pos - lineOffset + 1;
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

  computeLineAndCollumn(ctx, start, &newDiagnostic->location.lineStart, &newDiagnostic->location.colStart);
  computeLineAndCollumn(ctx, end, &newDiagnostic->location.lineEnd, &newDiagnostic->location.colEnd);

  if (ctx->diagnostics.tail) {
      ctx->diagnostics.tail->next = newDiagnostic;
  } else {
      ctx->diagnostics.head = newDiagnostic;
  }

  ctx->diagnostics.tail = newDiagnostic;
  ctx->diagnostics.count += 1;
}

void printDiagnostic(FILE *output, Diagnostic *diagnostic) {
  fprintf(output, "%s:", diagnostic->location.file);

  if (diagnostic->location.lineStart >= 0) {
      fprintf(output, "%d:%d:", diagnostic->location.colStart, diagnostic->location.lineStart);
  }

  fprintf(output, " %s: %s", diagnostic->severity->name, diagnostic->message);
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

