#ifndef __DIAGNOSTICS_H__
#define __DIAGNOSTICS_H__

#include <stdio.h>
#include "common.h"

enum DiagSeverityKind {
  DSK_INFO = 0,
  DSK_WARNING,
  DSK_ERROR,
  DSK_CRITICAL_ERROR,
  DSK_TOTAL_SEVERITY_COUNT
};

#define NO_LOC -1

typedef struct _Severity {
  int kind;
  const char *name;
  unsigned isError : 1;
} Severity;

enum IssueDiagType {
  IDT_PP,
  IDT_LEXICAL,
  IDT_SYNTAX,
  IDT_SEMANTHICAL
};

enum DiagnosticId {
#define DIAGNOSTIC_DEF(s, type, id, fmt) DIAG_##id
#include "diagnosticList.h"
  DIAGNOSTICS,
  DIAG_TOTAL_COUNT
#undef DIAGNOSTIC_DEF
};

typedef struct _DiagnosticDescriptor {
  enum DiagSeverityKind severityKind;
  enum IssueDiagType type;
  enum DiagnosticId id;
  const char *mnemonic;
  const char *formatString;
} DiagnosticDescriptor;

typedef struct _Diagnostic {
  const DiagnosticDescriptor *descriptor;

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

void printDiagnostic(FILE *output, Diagnostic *diagnostic, Boolean verbose);

void reportDiagnostic(struct _ParserContext *ctx, enum DiagnosticId diag, struct _Coordinates *location, ...);

#endif // __DIAGNOSTICS_H__
