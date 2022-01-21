#ifndef __DIAGNOSTICS_H__
#define __DIAGNOSTICS_H__

#include "common.h"
#include <stdio.h>

enum DiagSeverityKind {
  DSK_INFO,
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

typedef struct _Diagnostic {
  Severity *severity;
  struct {
    const char *file;
    int lineStart;
    int colStart;
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


void reportInfo(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);
void reportWarning(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);
void reportError(struct _ParserContext *ctx, int start, int end, const char* fmt, ...);

void parseInfo(struct _ParserContext *ctx, const char* fmt, ...);
void parseWarning(struct _ParserContext *ctx, const char* fmt, ...);
void parseError(struct _ParserContext *ctx, const char* fmt, ...);

void printDiagnostic(FILE *output, Diagnostic *diagnostic);

#endif // __DIAGNOSTICS_H__
