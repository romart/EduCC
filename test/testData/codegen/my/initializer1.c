#include <stdio.h>


typedef enum _TypeId {
  T_VOID,

  T_S1,
  T_S2,
  T_S4,
  T_S8,


  T_U1,
  T_U2,
  T_U4,
  T_U8,

  T_F4,
  T_F8,

  T_F10,

  T_BUILT_IN_TYPES,

  T_STRUCT,
  T_UNION,

  T_ENUM,

  T_ERROR
} TypeId;


typedef struct _TypeDesc {
  TypeId typeId;
  int size;
  const char *name;
  void *structInfo;
} TypeDesc;

static TypeDesc builtInTypeDescriptors[] = {
    { T_VOID, 0, "void", NULL },

    { T_S1, 1, "signed char", NULL },
    { T_S2, 2, "signed short", NULL },
    { T_S4, 4, "signed int", NULL },
    { T_S8, 8, "signed long", NULL },

    { T_U1, 1, "unsigned char", NULL },
    { T_U2, 2, "unsigned short", NULL },
    { T_U4, 4, "unsigned int", NULL },
    { T_U8, 8, "unsigned long", NULL },

    { T_F4, 4, "float", NULL },
    { T_F8, 8, "double", NULL },
    { T_F10, 16, "long double", NULL }
};



TypeDesc *p_void = &builtInTypeDescriptors[T_VOID];
TypeDesc *p_s1 = &builtInTypeDescriptors[T_S1];
TypeDesc *p_s2 = &builtInTypeDescriptors[T_S2];
TypeDesc *p_s4 = &builtInTypeDescriptors[T_S4];
TypeDesc *p_s8 = &builtInTypeDescriptors[T_S8];
TypeDesc *p_u1 = &builtInTypeDescriptors[T_U1];
TypeDesc *p_u2 = &builtInTypeDescriptors[T_U2];
TypeDesc *p_u4 = &builtInTypeDescriptors[T_U4];
TypeDesc *p_u8 = &builtInTypeDescriptors[T_U8];
TypeDesc *p_f4 = &builtInTypeDescriptors[T_F4];
TypeDesc *p_f8 = &builtInTypeDescriptors[T_F8];
TypeDesc *p_f10 = &builtInTypeDescriptors[T_F10];



int test(TypeDesc *p, TypeId id, int size, void *info) {
  if (p->typeId != id) return 1;
  if (p->size != size) return 2;
  if (p->structInfo != info) return 3;
  printf("%s\n", p->name);
  return 0;
}


int testStatic() {
  int r = 0;
  r = test(p_void, T_VOID, 0, NULL);
  if (r) return 10+r;
  r = test(p_s1, T_S1, 1, NULL);
  if (r) return 20+r;
  r = test(p_s2, T_S2, 2, NULL);
  if (r) return 30+r;
  r = test(p_s4, T_S4, 4, NULL);
  if (r) return 40+r;
  r = test(p_s8, T_S8, 8, NULL);
  if (r) return 50+r;
  r = test(p_u1, T_U1, 1, NULL);
  if (r) return 60+r;
  r = test(p_u2, T_U2, 2, NULL);
  if (r) return 70+r;
  r = test(p_u4, T_U4, 4, NULL);
  if (r) return 80+r;
  r = test(p_u8, T_U8, 8, NULL);
  if (r) return 90+r;
  r = test(p_f4, T_F4, 4, NULL);
  if (r) return 100+r;
  r = test(p_f8, T_F8, 8, NULL);
  if (r) return 110+r;
  r = test(p_f10, T_F10, 16, NULL);
  if (r) return 120+r;

  return 0;
}


int test2(TypeId id, int size, void *info) {
  return test(&builtInTypeDescriptors[id], id, size, info);
}

int testRef() {
  int r = 0;
  r = test2(T_VOID, 0, NULL);
  if (r) return 10+r;
  r = test2(T_S1, 1, NULL);
  if (r) return 20+r;
  r = test2(T_S2, 2, NULL);
  if (r) return 30+r;
  r = test2(T_S4, 4, NULL);
  if (r) return 40+r;
  r = test2(T_S8, 8, NULL);
  if (r) return 50+r;
  r = test2(T_U1, 1, NULL);
  if (r) return 60+r;
  r = test2(T_U2, 2, NULL);
  if (r) return 70+r;
  r = test2(T_U4, 4, NULL);
  if (r) return 80+r;
  r = test2(T_U8, 8, NULL);
  if (r) return 90+r;
  r = test2(T_F4, 4, NULL);
  if (r) return 100+r;
  r = test2(T_F8, 8, NULL);
  if (r) return 110+r;
  r = test2(T_F10, 16, NULL);
  if (r) return 120+r;

  return 0;
}

typedef union {
    unsigned storage;
    struct {
        unsigned isConst : 1;
        unsigned isVolatile : 1;

        unsigned isStatic : 1;
        unsigned isExternal : 1;
        unsigned isRegister : 1;
        unsigned isTypedef : 1;

        unsigned isLocal : 1;
    } bits;
} SpecifierFlags;

typedef struct _Coordinates {
  int startOffset;
  int endOffset;
  void *locInfo;
} Coordinates;

typedef struct _DeclarationSpecifiers {
  Coordinates coordinates;
  SpecifierFlags flags;
  void *basicType;
  void *defined;
} DeclarationSpecifiers;

int testInitZero() {
  DeclarationSpecifiers spec = { 0 };

  return spec.flags.storage;
}

unsigned testCopyUnion() {
  DeclarationSpecifiers spec = { 0 };
  SpecifierFlags x = spec.flags;
  return x.storage;
}

typedef struct _Severity {
  int kind;
  const char *name;
  int isError;
//  unsigned isError : 1;
} Severity;

enum DiagSeverityKind {
  DSK_INFO = 0,
  DSK_WARNING,
  DSK_ERROR,
  DSK_CRITICAL_ERROR,
  DSK_TOTAL_SEVERITY_COUNT
};

static Severity severities[] = {
  { DSK_INFO, "info", 0 },
  { DSK_WARNING, "warning", 0 },
  { DSK_ERROR, "error", 1 },
  { DSK_CRITICAL_ERROR, "critical error", 1 }
};

static Severity *infoSeverity = &severities[DSK_INFO];
static Severity *warningSeverity = &severities[DSK_WARNING];
static Severity *errorSeverity = &severities[DSK_ERROR];
static Severity *criticalErrorSeverity = &severities[DSK_CRITICAL_ERROR];

int testSeverity(Severity *s, enum DiagSeverityKind id, int errorness) {
  printf("%s\n", s->name);
  if (s->kind != id) return 1;
  if (s->isError != errorness) return 2;

  return 0;
}

int testSeverities() {
  int r = testSeverity(infoSeverity, DSK_INFO, 0);
  if (r) return r + 10;
  r = testSeverity(warningSeverity, DSK_WARNING, 0);
  if (r) return r + 20;
  r = testSeverity(errorSeverity, DSK_ERROR, 1);
  if (r) return r + 30;
  r = testSeverity(criticalErrorSeverity, DSK_CRITICAL_ERROR, 1);
  if (r) return r + 40;

  return 0;
}

int main() {
  int r = testStatic();

  if (r) return 1000 + r;

  r = testRef();

  if (r) return 2000 + r;

  if (testInitZero()) return 3;

  if (testCopyUnion()) return 4;

  r = testSeverities();
  if (r) return r + 3000;



  return 0;
}
