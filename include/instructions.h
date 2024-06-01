
#ifndef __INSTR_H__
#define __INSTR_H__ 1

#include <stdint.h>
#include <stddef.h>
#include "common.h"

typedef unsigned char *address;

enum RelocKind {
  RK_SYMBOL, // by symbol for call
  RK_CONST,
  RK_REF, // absolute
  RK_RIP, // rip-relative
};

struct _GeneratedFunction;
struct _Section;

typedef struct _Relocation {
  enum RelocKind kind;

  struct _Section *applySection;
  ptrdiff_t applySectionOffset;

  ptrdiff_t addend;

  union {
    struct {
      const char *symbolName;
      struct _Symbol *symbol;
    } symbolData;
    struct {
      struct _Section *dataSection;
      ptrdiff_t dataSectionOffset;
    } sectionData;
  };

  struct _Relocation *next;
} Relocation;


struct LabelJump {
  ptrdiff_t instruction_cp;
  size_t instSize;
  struct LabelJump *next;
};

struct LabelRef {
  ptrdiff_t offset_cp;
  struct LabelRef *next;
};

struct Label {
  const char *name;
  int binded;
  ptrdiff_t label_cp;
  struct LabelJump *jumps;
  struct LabelRef *refs;
};


enum OperandKind {
  OPK_ILLEGAL = 0,
  OPK_REGISTER,
  OPK_ADDRESS,
  OPK_CONSTANT,
  OPK_LABEL,
  OPK_RELOCATION
};

#endif // __INSTR_H__
