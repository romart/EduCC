#include "ir/target.h"
#include "instructions_x86_64.h"
#include "machine_x86_64.h"

// Flat physical register namespace for x86-64:
//
//   0..15   general purpose, in enum Registers order (rax, rcx, rdx, rbx,
//           rsp, rbp, rsi, rdi, r8..r15) - so a GP id is already the encoding
//           the assembler in instructions_x86_64.c wants.
//   16..31  xmm0..xmm15. enum Registers spells these 0..15 as well, which is
//           exactly the ambiguity this namespace exists to remove; subtract
//           X86_FP_BASE (machine_x86_64.h) to get back to the encoding.
#define X86_PHYS_REG_COUNT 32

#define FP(n) (X86_FP_BASE + (n))

static const enum RegClass x86RegClass[IR_PHYS_REG_MAX] = {
  [R_EAX] = RC_GP, [R_ECX] = RC_GP, [R_EDX] = RC_GP, [R_EBX] = RC_GP,
  [R_ESP] = RC_GP, [R_EBP] = RC_GP, [R_ESI] = RC_GP, [R_EDI] = RC_GP,
  [R_R8]  = RC_GP, [R_R9]  = RC_GP, [R_R10] = RC_GP, [R_R11] = RC_GP,
  [R_R12] = RC_GP, [R_R13] = RC_GP, [R_R14] = RC_GP, [R_R15] = RC_GP,

  [FP(0)]  = RC_FP, [FP(1)]  = RC_FP, [FP(2)]  = RC_FP, [FP(3)]  = RC_FP,
  [FP(4)]  = RC_FP, [FP(5)]  = RC_FP, [FP(6)]  = RC_FP, [FP(7)]  = RC_FP,
  [FP(8)]  = RC_FP, [FP(9)]  = RC_FP, [FP(10)] = RC_FP, [FP(11)] = RC_FP,
  [FP(12)] = RC_FP, [FP(13)] = RC_FP, [FP(14)] = RC_FP, [FP(15)] = RC_FP
};

static const char *const x86RegName[IR_PHYS_REG_MAX] = {
  [R_EAX] = "rax", [R_ECX] = "rcx", [R_EDX] = "rdx", [R_EBX] = "rbx",
  [R_ESP] = "rsp", [R_EBP] = "rbp", [R_ESI] = "rsi", [R_EDI] = "rdi",
  [R_R8]  = "r8",  [R_R9]  = "r9",  [R_R10] = "r10", [R_R11] = "r11",
  [R_R12] = "r12", [R_R13] = "r13", [R_R14] = "r14", [R_R15] = "r15",

  [FP(0)]  = "xmm0",  [FP(1)]  = "xmm1",  [FP(2)]  = "xmm2",  [FP(3)]  = "xmm3",
  [FP(4)]  = "xmm4",  [FP(5)]  = "xmm5",  [FP(6)]  = "xmm6",  [FP(7)]  = "xmm7",
  [FP(8)]  = "xmm8",  [FP(9)]  = "xmm9",  [FP(10)] = "xmm10", [FP(11)] = "xmm11",
  [FP(12)] = "xmm12", [FP(13)] = "xmm13", [FP(14)] = "xmm14", [FP(15)] = "xmm15"
};

// Indexed by (opcode - MOP_TARGET_FIRST). Built from the same X-macros as
// enum X86Opcode and in the same order, so the two cannot drift apart.
const char *const x86OpcodeName[X86_OPCODE_NUM] = {
#define X86_OPCODE_DEF(m, n) n
  X86_OPCODES,
#undef X86_OPCODE_DEF

#define X86_CC_DEF(m, n) "set" n
  X86_CONDITIONS,
#undef X86_CC_DEF

#define X86_CC_DEF(m, n) "j" n
  X86_CONDITIONS
#undef X86_CC_DEF
};

// SysV AMD64: rdi, rsi, rdx, rcx, r8, r9 then the stack.
static const uint32_t x86IntArgRegs[] = {
  R_EDI, R_ESI, R_EDX, R_ECX, R_R8, R_R9
};

// SysV AMD64: xmm0..xmm7 then the stack.
static const uint32_t x86FpArgRegs[] = {
  FP(0), FP(1), FP(2), FP(3), FP(4), FP(5), FP(6), FP(7)
};

// Scratch for the trivial allocator - see TargetDescriptor.scratchRegs for
// what disqualifies a register. On x86-64 that rules out a lot: rdi/rsi/rdx/
// rcx/r8/r9 carry arguments, rax the return value and the quotient, rdx the
// remainder, rcx a variable shift count, and rsp/rbp are the frame. What is
// left is rbx and r10..r15.
//
// r10 and r11 come first because they are the only two of those that are
// caller-saved, so a function that needs no more than two - which is every
// function stage 1 can currently emit, since nothing it selects names more
// than two distinct virtual registers - costs the prologue nothing. rbx is
// the headroom for a three-operand instruction, and being callee-saved it
// shows up in MachineFunction.usedPhysRegs for stage 3 to preserve.
//
// The three suggested in docs/ir-codegen-design.md section 7 - rax, rdx, rsi -
// cannot be used: all three are named by selection itself.
static const uint32_t x86GpScratchRegs[] = {
  R_R10, R_R11, R_EBX
};

// xmm0..xmm7 are the argument registers and xmm0 the return register, so
// scratch starts above them. All of xmm8..xmm15 are caller-saved.
static const uint32_t x86FpScratchRegs[] = {
  FP(8), FP(9), FP(10)
};

const TargetDescriptor targetX86_64 = {
  .name = "x86_64",

  .numPhysRegs = X86_PHYS_REG_COUNT,
  .regClass = x86RegClass,
  .regName = x86RegName,

  .opcodeName = x86OpcodeName,
  .numOpcodes = X86_OPCODE_NUM,

  .sp = R_ESP,
  .fp = R_EBP,

  .intArgRegs = x86IntArgRegs,
  .intArgRegCount = sizeof(x86IntArgRegs) / sizeof(x86IntArgRegs[0]),
  .fpArgRegs = x86FpArgRegs,
  .fpArgRegCount = sizeof(x86FpArgRegs) / sizeof(x86FpArgRegs[0]),

  .intRetReg = R_EAX,
  .fpRetReg = FP(0),

  .scratchRegs = { [RC_GP] = x86GpScratchRegs, [RC_FP] = x86FpScratchRegs },
  .scratchRegCount = {
    [RC_GP] = sizeof(x86GpScratchRegs) / sizeof(x86GpScratchRegs[0]),
    [RC_FP] = sizeof(x86FpScratchRegs) / sizeof(x86FpScratchRegs[0])
  },

  .classifyParameters = &classifyParametersGeneric
};
