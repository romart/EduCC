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

// No register. Spelled here rather than in ir/machine.h, which is where
// MachineOperand's NO_REG comes from, because a TargetDescriptor field can
// need it too and target.h is the header machine.h includes and not the other
// way round; the two are the same value on purpose.
#define IR_NO_PHYS_REG ((uint32_t)-1)

enum RegClass {
  // Zero on purpose: a target's regClass table is a sparse designated
  // initializer, so every id it does not mention has to come out as "not a
  // register" rather than as whichever class happens to be listed first.
  RC_NONE = 0,
  RC_GP,  // general purpose / integer
  RC_FP,  // floating point

  // The condition flags, as one register. Its own class because it is nothing
  // like the other two: there is exactly one of it, no value has this type, and
  // an allocator has nothing to hand out - which is what scratchRegCount of
  // zero for this class says. It exists so that "cmp writes it and setcc reads
  // it" is a fact about registers like any other rather than a fact about the
  // order the two happened to be emitted in.
  RC_FLAGS,

  RC_CLASS_COUNT
};

// What an instruction does to the condition flags. Derived from the opcode and
// not stored per instruction: unlike MachineInstr.flags.isCall, which is a
// claim about this machine function, "add writes the flags" is a fact about the
// ISA that every add shares, and a second copy of it is a second thing that can
// disagree.
//
// The two kinds of write are the reason this is not one bit. An 'add' leaves
// the flags holding something new that nothing is entitled to read; a 'cmp'
// leaves them holding its answer, and something has to. Both are writes to
// whoever asks "may this move past that one", and telling them apart is what
// lets verifyFlagsDependencies() notice a clobber that has landed between a
// compare and its reader - the exact damage this model exists to make
// impossible to do silently.
enum MachineFlagsEffect {
  // Zero on purpose, and not "touches nothing": a target's table is a sparse
  // designated initializer, so an opcode added without an entry has to come
  // out as "nobody has answered for this one" rather than as a claim that it
  // leaves the flags alone. verifyFlagsDependencies() is what refuses it.
  MFE_UNKNOWN = 0,

  MFE_NONE = 1u << 0,     // answered: does not touch them
  MFE_CLOBBER = 1u << 1,  // writes them incidentally; the result is nobody's
  MFE_PRODUCE = 1u << 2,  // writes them as its answer, for something to read
  MFE_READ = 1u << 3,

  MFE_WRITE = MFE_CLOBBER | MFE_PRODUCE
};

// What one eightbyte of an aggregate is made of, and so which register file it
// travels in. SysV's X87, X87UP and COMPLEX_X87 classes are not here: nothing
// with a long double in it goes in a register at all, and classifyComposite
// says so by returning no eightbytes rather than by naming a class.
enum EightbyteClass {
  EB_NONE = 0,  // no member reached this eightbyte
  EB_INTEGER,
  EB_SSE
};

// How a composite travels: the class of each of its eightbytes, and how many of
// them there are - one or two. Zero means it goes in memory, because it is
// larger than two eightbytes or because something in it has no register class.
//
// A scalar answers zero too: it is not an aggregate, and isRealType already
// decides where a bare float goes.
uint32_t classifyComposite(const TypeRef *type, enum EightbyteClass classes[2]);

// Where one parameter arrives, as decided by the target's ABI.
typedef struct {
  AstValueDeclaration *declaration;
  struct _LocalValueInfo *lvi;
  union {
    int32_t stackOffset;  // when !isRegister, relative to the frame pointer
    uint32_t physReg;     // when isRegister, an id in the flat namespace above
  } loc;

  // A composite of two eightbytes arrives in two registers, each of its own
  // class - {double,double} in xmm0:xmm1, {long,long} in rdi:rsi. 'regCount' is
  // 1 for everything else and 'physReg2' is only read when it is 2. SysV passes
  // such an aggregate all in registers or all on the stack and never splits it,
  // so one 'isRegister' still answers for both halves.
  uint32_t physReg2;
  enum EightbyteClass classes[2];
  uint32_t regCount;

  uint32_t idx;
  Boolean isRegister;
} ParamtersABIInfo;

// What a variadic definition needs and no per-parameter answer can give: how
// many argument registers of each class the *named* parameters used up, and
// where the unnamed ones start on the stack. va_start writes exactly these
// three numbers into the va_list.
typedef struct {
  uint32_t intRegParams;
  uint32_t fpRegParams;
  int32_t stackParamOffset;
} ParametersABISummary;

typedef struct _TargetDescriptor {
  const char *name;

  uint32_t numPhysRegs;
  const enum RegClass *regClass;  // [IR_PHYS_REG_MAX]
  const char *const *regName;     // [IR_PHYS_REG_MAX], for IR dumps

  // Mnemonics for this target's own machine opcodes, indexed by
  // (opcode - MOP_TARGET_FIRST); NULL for a target with no selector yet.
  // Naming is the only thing the target-independent side ever needs to do with
  // a target opcode, so a table is enough and there is no descriptor per
  // instruction to keep in step with it.
  const char *const *opcodeName;
  uint32_t numOpcodes;

  // What each of them does to flagsReg, indexed the same way; NULL for a
  // target with no selector yet. Read it through targetOpcodeFlagsEffect().
  const uint8_t *opcodeFlagsEffect;

  uint32_t sp;  // stack pointer
  uint32_t fp;  // frame pointer

  // The condition-flags register, or IR_NO_PHYS_REG for a target that has
  // none - riscv64 compares and branches in one instruction and keeps no
  // flags at all, which is why this is a target's answer and not a constant.
  uint32_t flagsReg;

  const uint32_t *intArgRegs;
  uint32_t intArgRegCount;
  const uint32_t *fpArgRegs;
  uint32_t fpArgRegCount;

  uint32_t intRetReg;
  uint32_t fpRetReg;

  // Where the second eightbyte of an aggregate returned in registers goes -
  // rdx for an INTEGER one, xmm1 for an SSE one. A target that returns nothing
  // in two registers leaves these IR_NO_PHYS_REG, and classifyComposite never
  // hands it a second eightbyte to place.
  uint32_t intRetReg2;
  uint32_t fpRetReg2;

  // Every register a call destroys, of either class. Read together with
  // MachineInstr.flags.isCall, which is what marks the instructions this
  // applies to; see the comment there for why a call does not list them as
  // operands instead.
  const uint32_t *callerSavedRegs;
  uint32_t callerSavedRegCount;

  // Registers the trivial "spill everything" allocator may use as scratch,
  // per register class (see docs/ir-codegen-design.md section 7 stage A). They
  // have to be registers *instruction selection never names itself*: the
  // allocator leaves every physical operand alone, so an ABI argument
  // register, a return register, or one the ISA fixes for a divide or a shift
  // would be reloaded over while the value it holds is still wanted. That
  // constraint is what keeps this a target's answer and not a generic one.
  //
  // Indexed by enum RegClass; the RC_NONE entry is unused and NULL. A target
  // with no entries has no backend yet and is skipped by the allocator.
  const uint32_t *scratchRegs[RC_CLASS_COUNT];
  uint32_t scratchRegCount[RC_CLASS_COUNT];

  // Every register stage 2B and later may hand out, per class, in the order it
  // should prefer them - which is the whole of this target's allocation
  // policy. Caller-saved first, so that a value only lands somewhere the
  // prologue has to preserve once the caller-saved half is full.
  //
  // Unlike scratchRegs this may name registers selection uses itself: an
  // interval asks liveness whether a register is busy over its own range
  // rather than assuming it is free, so rax being the divide's quotient does
  // not disqualify it everywhere else. What it may not name is the stack or
  // frame pointer, or anything of a class with no values in it.
  const uint32_t *allocatableRegs[RC_CLASS_COUNT];
  uint32_t allocatableRegCount[RC_CLASS_COUNT];

  // Both targets currently point this at classifyParametersGeneric(): they
  // differ only in which registers they use and how many, and that is data,
  // not code. The hook exists because the aggregate-passing rules they both
  // stub out today do *not* agree - SysV classifies a struct field-by-field
  // into two eightbytes, riscv64 LP64D has its own rules - so that is where
  // they will stop sharing an implementation.
  void (*classifyParameters)(const struct _TargetDescriptor *target,
                             AstFunctionDeclaration *declaration,
                             ParamtersABIInfo *infos, size_t numberOfParams,
                             ParametersABISummary *summary);
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
                               ParamtersABIInfo *infos, size_t numberOfParams,
                               ParametersABISummary *summary);

// Whether a function with this return type is handed a buffer to write the
// result into, rather than handing the result back in a register.
//
// The threshold is the same one classifyParametersGeneric uses for an
// aggregate parameter and the same one the legacy backend uses, which is what
// lets a function from either backend call one from the other. Real SysV
// splits an aggregate of up to sixteen bytes into two eightbytes and returns
// those in rax:rdx; that is the same approximation the parameter side makes,
// and the two have to move together.
Boolean returnsThroughHiddenPointer(const TypeRef *returnType);

// Whether a composite that fits in one register travels in the SSE file rather
// than the integer one - SysV's SSE class, for the single-eightbyte case. The
// caller decides that it fits; this decides which file.
//
// Read by the IR backend only. See the definition for why the legacy backend
// does not, and what that costs.
Boolean isCompositeInSSERegister(const TypeRef *type);

const char *physRegName(const TargetDescriptor *target, uint32_t reg);

// The mnemonic for one of this target's machine opcodes, or NULL if the target
// does not name it. Generic opcodes are not this function's business - see
// MACHINE_GENERIC_OPCODES.
const char *targetOpcodeName(const TargetDescriptor *target, uint32_t opcode);

// What one of this target's machine opcodes does to the condition flags.
// MFE_NONE for a generic opcode - see the note on MACHINE_GENERIC_OPCODES -
// and MFE_UNKNOWN for a target that names no effects at all.
enum MachineFlagsEffect targetOpcodeFlagsEffect(const TargetDescriptor *target, uint32_t opcode);

#endif  // __IR_TARGET_H__
