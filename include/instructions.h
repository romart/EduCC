
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
  RK_GOT, // the symbol's GOT slot, rip-relative; '-fPIC' only

  // riscv64 only, and the reason this enum is not simply "an address goes
  // here": RV64 has no instruction that takes a full address, so one is built
  // from a high and a low half in two instructions, and each half is its own
  // relocation over its own four bytes.
  //
  // These are the *absolute* forms - lui/addi - which is the medlow code
  // model, where an address is a constant the link knows. The pc-relative
  // forms an auipc pair would need are a different pair again, and they are
  // what '-fPIC' would want; riscv64 refuses '-fPIC' rather than pretend
  // otherwise.
  RK_RISCV_HI20,   // the lui: bits 31:12 of the address, rounded for the low half
  RK_RISCV_LO12_I, // the I-type half - addi, a load
  RK_RISCV_LO12_S, // the S-type half, whose immediate is split across two fields

  // The same pair against a *section* and an offset into it rather than a
  // named symbol - a string literal, which has no name. x86-64 makes the same
  // distinction between RK_SYMBOL and RK_RIP and for the same reason: the two
  // halves of Relocation's union are not interchangeable.
  RK_RISCV_SEC_HI20,
  RK_RISCV_SEC_LO12_I,
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
