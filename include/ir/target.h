#ifndef __IR_TARGET_H__
#define __IR_TARGET_H__ 1

#include "common.h"
#include "tree.h"
#include "parser.h"

// Physical registers live in ONE flat namespace per target, shared by the IR
// (IR_P_REG), and later by the instruction selector and register allocator.
//
// That flatness is the whole point. Before it existed, IR_P_REG ids were
// per-class ordinals handed out by independent counters, so "integer argument
// 0", "SSE argument 0" and "the stack pointer" were all spelled $0 or $3. GVN
// keys IR_P_REG on (kind, type, physReg), so a pointer parameter landing on
// integer ordinal 3 was indistinguishable from the stack pointer and got CSE'd
// into it. A single namespace makes that unrepresentable rather than merely
// unlikely.
//
// Layouts: x86-64 puts the 16 GP registers at 0..15 (matching enum Registers)
// and xmm0..xmm15 at 16..31; riscv64 puts x0..x31 at 0..31 and f0..f31 at
// 32..63.
#define IR_PHYS_REG_MAX 64

enum RegClass {
  // Zero on purpose: a target's regClass table is a sparse designated
  // initializer, so every id it does not mention has to come out as "not a
  // register" rather than as whichever class happens to be listed first.
  RC_NONE = 0,
  RC_GP,  // general purpose / integer
  RC_FP,  // floating point

  RC_CLASS_COUNT
};

// Where one parameter arrives, as decided by the target's ABI.
typedef struct {
  AstValueDeclaration *declaration;
  struct _LocalValueInfo *lvi;
  union {
    int32_t stackOffset;  // when !isRegister, relative to the frame pointer
    uint32_t physReg;     // when isRegister, an id in the flat namespace above
  } loc;
  uint32_t idx;
  Boolean isRegister;
} ParamtersABIInfo;

typedef struct _TargetDescriptor {
  const char *name;

  uint32_t numPhysRegs;
  const enum RegClass *regClass;  // [IR_PHYS_REG_MAX]
  const char *const *regName;     // [IR_PHYS_REG_MAX], for IR dumps

  uint32_t sp;  // stack pointer
  uint32_t fp;  // frame pointer

  const uint32_t *intArgRegs;
  uint32_t intArgRegCount;
  const uint32_t *fpArgRegs;
  uint32_t fpArgRegCount;

  uint32_t intRetReg;
  uint32_t fpRetReg;

  // Both targets currently point this at classifyParametersGeneric(): they
  // differ only in which registers they use and how many, and that is data,
  // not code. The hook exists because the aggregate-passing rules they both
  // stub out today do *not* agree - SysV classifies a struct field-by-field
  // into two eightbytes, riscv64 LP64D has its own rules - so that is where
  // they will stop sharing an implementation.
  void (*classifyParameters)(const struct _TargetDescriptor *target,
                             AstFunctionDeclaration *declaration,
                             ParamtersABIInfo *infos, size_t numberOfParams);
} TargetDescriptor;

extern const TargetDescriptor targetX86_64;
extern const TargetDescriptor targetRiscv64;

const TargetDescriptor *getTargetDescriptor(enum Arch arch);

// Shared, deliberately simple classification: scalars go to the next free
// register of their class and fall back to the stack once those run out;
// anything aggregate and larger than a word goes to the stack. See the hook
// comment above for what this does not yet do.
void classifyParametersGeneric(const TargetDescriptor *target,
                               AstFunctionDeclaration *declaration,
                               ParamtersABIInfo *infos, size_t numberOfParams);

const char *physRegName(const TargetDescriptor *target, uint32_t reg);

#endif  // __IR_TARGET_H__
