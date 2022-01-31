#ifndef __DIAGNOSTICS_H__
#define __DIAGNOSTICS_H__

#include <stdio.h>
#include "common.h"

enum DiagSeverityKind {
  DSK_INFO = 0,
  DSK_WARNING,
  DSK_ERROR,
  DSK_CRITICAL_ERROR
};

#define NO_LOC -1

typedef struct _Severity {
  int kind;
  const char *name;
  unsigned isError : 1;
} Severity;

enum DiagnosticId {
#include "diagnosticList.h"
#define DIAGNOSTIC_DEF(s, id, fmt) DIAG_##id
  DIAGNOSTICS
  DIAG_TOTAL_COUNT
#undef DIAGNOSTIC_DEF
};


typedef struct _Diagnostic {
  Severity *severity;
  struct {
    const char *file;
    int lineStartOffset;
    int lineStart;
    int colStart;
    int lineEndOffset;
    int lineEnd;
    int colEnd;
  } location;

  const char *message;

  struct _Diagnostic *next;
} Diagnostic;

typedef struct _Diagnostics {
  unsigned count;
  Diagnostic *head, *tail;
} Diagnostics;

struct _ParserContext;
struct _Coordinates;

void reportInfo(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);
void reportWarning(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);
void reportError(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);

void parseInfo(struct _ParserContext *ctx, const char* fmt, ...);
void parseWarning(struct _ParserContext *ctx, const char* fmt, ...);
void parseError(struct _ParserContext *ctx, const char* fmt, ...);

void printDiagnostic(FILE *output, Diagnostic *diagnostic, Boolean verbose);

void reportDiagnostic(struct _ParserContext *ctx, enum DiagnosticId diag, struct _Coordinates *location, ...);

#endif // __DIAGNOSTICS_H__
