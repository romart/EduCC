#include "ir/target.h"
#include "instructions_riscv64.h"
#include "machine_riscv64.h"

// Flat physical register namespace for riscv64:
//
//   0..31   x0..x31, in enum XRegister order.
//   32..63  f0..f31. enum FRegister spells these 0..31 too, so the offset is
//           what keeps an integer register distinct from a float one; subtract
//           RISCV_FP_BASE (machine_riscv64.h) to get back to the encoding.
//
// This is the whole of IR_PHYS_REG_MAX, with nothing left over - see the note
// on the allocatable lists below for what that costs.
#define RISCV_PHYS_REG_COUNT 64

#define FR(n) (RISCV_FP_BASE + (n))

static const enum RegClass riscvRegClass[IR_PHYS_REG_MAX] = {
  [X_ZERO] = RC_GP, [X_RA] = RC_GP, [X_SP] = RC_GP, [X_GP] = RC_GP,
  [X_TP] = RC_GP,   [X_T0] = RC_GP, [X_T1] = RC_GP, [X_T2] = RC_GP,
  [X_S0] = RC_GP,   [X_S1] = RC_GP,
  [X_A0] = RC_GP,   [X_A1] = RC_GP, [X_A2] = RC_GP, [X_A3] = RC_GP,
  [X_A4] = RC_GP,   [X_A5] = RC_GP, [X_A6] = RC_GP, [X_A7] = RC_GP,
  [X_S2] = RC_GP,   [X_S3] = RC_GP, [X_S4] = RC_GP, [X_S5] = RC_GP,
  [X_S6] = RC_GP,   [X_S7] = RC_GP, [X_S8] = RC_GP, [X_S9] = RC_GP,
  [X_S10] = RC_GP,  [X_S11] = RC_GP,
  [X_T3] = RC_GP,   [X_T4] = RC_GP, [X_T5] = RC_GP, [X_T6] = RC_GP,

  [FR(F_T0)] = RC_FP,  [FR(F_T1)] = RC_FP,  [FR(F_T2)] = RC_FP,
  [FR(F_T3)] = RC_FP,  [FR(F_T4)] = RC_FP,  [FR(F_T5)] = RC_FP,
  [FR(F_T6)] = RC_FP,  [FR(F_T7)] = RC_FP,
  [FR(F_S0)] = RC_FP,  [FR(F_S1)] = RC_FP,
  [FR(F_A0)] = RC_FP,  [FR(F_A1)] = RC_FP,  [FR(F_A2)] = RC_FP,
  [FR(F_A3)] = RC_FP,  [FR(F_A4)] = RC_FP,  [FR(F_A5)] = RC_FP,
  [FR(F_A6)] = RC_FP,  [FR(F_A7)] = RC_FP,
  [FR(F_S2)] = RC_FP,  [FR(F_S3)] = RC_FP,  [FR(F_S4)] = RC_FP,
  [FR(F_S5)] = RC_FP,  [FR(F_S6)] = RC_FP,  [FR(F_S7)] = RC_FP,
  [FR(F_S8)] = RC_FP,  [FR(F_S9)] = RC_FP,  [FR(F_S10)] = RC_FP,
  [FR(F_S11)] = RC_FP,
  [FR(F_T8)] = RC_FP,  [FR(F_T9)] = RC_FP,  [FR(F_T10)] = RC_FP,
  [FR(F_T11)] = RC_FP
};

static const char *const riscvRegName[IR_PHYS_REG_MAX] = {
  [X_ZERO] = "zero", [X_RA] = "ra", [X_SP] = "sp", [X_GP] = "gp",
  [X_TP] = "tp",     [X_T0] = "t0", [X_T1] = "t1", [X_T2] = "t2",
  [X_S0] = "s0",     [X_S1] = "s1",
  [X_A0] = "a0",     [X_A1] = "a1", [X_A2] = "a2", [X_A3] = "a3",
  [X_A4] = "a4",     [X_A5] = "a5", [X_A6] = "a6", [X_A7] = "a7",
  [X_S2] = "s2",     [X_S3] = "s3", [X_S4] = "s4", [X_S5] = "s5",
  [X_S6] = "s6",     [X_S7] = "s7", [X_S8] = "s8", [X_S9] = "s9",
  [X_S10] = "s10",   [X_S11] = "s11",
  [X_T3] = "t3",     [X_T4] = "t4", [X_T5] = "t5", [X_T6] = "t6",

  [FR(F_T0)] = "ft0",  [FR(F_T1)] = "ft1",  [FR(F_T2)] = "ft2",
  [FR(F_T3)] = "ft3",  [FR(F_T4)] = "ft4",  [FR(F_T5)] = "ft5",
  [FR(F_T6)] = "ft6",  [FR(F_T7)] = "ft7",
  [FR(F_S0)] = "fs0",  [FR(F_S1)] = "fs1",
  [FR(F_A0)] = "fa0",  [FR(F_A1)] = "fa1",  [FR(F_A2)] = "fa2",
  [FR(F_A3)] = "fa3",  [FR(F_A4)] = "fa4",  [FR(F_A5)] = "fa5",
  [FR(F_A6)] = "fa6",  [FR(F_A7)] = "fa7",
  [FR(F_S2)] = "fs2",  [FR(F_S3)] = "fs3",  [FR(F_S4)] = "fs4",
  [FR(F_S5)] = "fs5",  [FR(F_S6)] = "fs6",  [FR(F_S7)] = "fs7",
  [FR(F_S8)] = "fs8",  [FR(F_S9)] = "fs9",  [FR(F_S10)] = "fs10",
  [FR(F_S11)] = "fs11",
  [FR(F_T8)] = "ft8",  [FR(F_T9)] = "ft9",  [FR(F_T10)] = "ft10",
  [FR(F_T11)] = "ft11"
};

// Indexed by (opcode - MOP_TARGET_FIRST). Built from the same X-macros as
// enum RiscvOpcode and in the same order, so the two cannot drift apart.
const char *const riscvOpcodeName[RISCV_OPCODE_NUM] = {
#define RISCV_OPCODE_DEF(m, n) n
  RISCV_OPCODES,
#undef RISCV_OPCODE_DEF

#define RISCV_BR_DEF(m, n) n
  RISCV_BRANCHES
#undef RISCV_BR_DEF
};

// RV64 has no condition flags, so every opcode's answer is MFE_NONE and the
// table exists only to say so - MFE_UNKNOWN is what an opcode nobody has
// answered for reads back as, and a uniform table keeps that distinction alive
// if a future extension ever does introduce a status register.
static const uint8_t riscvOpcodeFlags[RISCV_OPCODE_NUM] = {
#define RISCV_OPCODE_DEF(m, _) [RISCV_##m - MOP_TARGET_FIRST] = MFE_NONE
  RISCV_OPCODES,
#undef RISCV_OPCODE_DEF

#define RISCV_BR_DEF(m, _) [RISCV_B##m - MOP_TARGET_FIRST] = MFE_NONE
  RISCV_BRANCHES
#undef RISCV_BR_DEF
};

// LP64D: a0..a7 then the stack.
static const uint32_t riscvIntArgRegs[] = {
  X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7
};

// LP64D: fa0..fa7 then the stack.
static const uint32_t riscvFpArgRegs[] = {
  FR(F_A0), FR(F_A1), FR(F_A2), FR(F_A3),
  FR(F_A4), FR(F_A5), FR(F_A6), FR(F_A7)
};

// LP64D: ra, the temporaries and the argument registers of both files. sp, gp
// and tp are left out because a call does not destroy them in any sense an
// allocator cares about, and s0..s11/fs0..fs11 because the callee restores
// them.
static const uint32_t riscvCallerSavedRegs[] = {
  X_RA, X_T0, X_T1, X_T2, X_T3, X_T4, X_T5, X_T6,
  X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7,

  FR(F_T0), FR(F_T1), FR(F_T2),  FR(F_T3),  FR(F_T4),  FR(F_T5),
  FR(F_T6), FR(F_T7), FR(F_T8),  FR(F_T9),  FR(F_T10), FR(F_T11),
  FR(F_A0), FR(F_A1), FR(F_A2),  FR(F_A3),
  FR(F_A4), FR(F_A5), FR(F_A6),  FR(F_A7)
};

// Scratch for the trivial allocator - see TargetDescriptor.scratchRegs for what
// disqualifies a register. Selection itself names a0..a7, sp, s0, ra and x0, and
// emission names t0 (below), so the temporaries above it are what is left.
static const uint32_t riscvGpScratchRegs[] = {
  X_T4, X_T5, X_T6
};

// fa0..fa7 are the argument and return registers, so scratch starts above them.
static const uint32_t riscvFpScratchRegs[] = {
  FR(F_T9), FR(F_T10), FR(F_T11)
};

// What stages 2B and 2C hand out, caller-saved first. Five of the 32 x
// registers never appear:
//
//   x0   is hardwired to zero and holds nothing,
//   gp   and tp belong to the ABI and to the thread,
//   sp   and s0 are the frame,
//
// and t0 is reserved as *emission's* temporary. RV64 immediates are twelve
// bits, so a frame offset past 2047, a constant wider than that, or a far
// branch needs a register to build the value in, and stage 3 has no allocator
// left to ask for one - this is the same role GNU as keeps t0/t1 for when it
// expands a pseudo-instruction. Reserving it costs one register out of 27 and
// is what stops a large frame from being a failed assertion in the emitter.
static const uint32_t riscvGpAllocatableRegs[] = {
  X_T1, X_T2, X_T3, X_T4, X_T5, X_T6,
  X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7,
  X_S1, X_S2, X_S3, X_S4, X_S5, X_S6, X_S7, X_S8, X_S9, X_S10, X_S11
};

// All thirty-two, since nothing reserves an f register: the whole file is
// values, and emission's temporary above only ever holds an address.
static const uint32_t riscvFpAllocatableRegs[] = {
  FR(F_T0), FR(F_T1), FR(F_T2),  FR(F_T3),  FR(F_T4),  FR(F_T5),
  FR(F_T6), FR(F_T7), FR(F_T8),  FR(F_T9),  FR(F_T10), FR(F_T11),
  FR(F_A0), FR(F_A1), FR(F_A2),  FR(F_A3),
  FR(F_A4), FR(F_A5), FR(F_A6),  FR(F_A7),
  FR(F_S0), FR(F_S1), FR(F_S2),  FR(F_S3),  FR(F_S4),  FR(F_S5),
  FR(F_S6), FR(F_S7), FR(F_S8),  FR(F_S9),  FR(F_S10), FR(F_S11)
};

const TargetDescriptor targetRiscv64 = {
  .name = "riscv64",

  .numPhysRegs = RISCV_PHYS_REG_COUNT,
  .regClass = riscvRegClass,
  .regName = riscvRegName,

  .opcodeName = riscvOpcodeName,
  .numOpcodes = RISCV_OPCODE_NUM,
  .opcodeFlagsEffect = riscvOpcodeFlags,

  .sp = X_SP,
  .fp = X_S0,  // s0/fp is the frame pointer in the standard ABI

  // s0 points at the *top* of the frame and the return address lives in ra
  // rather than on the stack, so an argument the caller pushed is at s0 + 0.
  // x86-64 has a saved rbp and a return address in between and answers 16.
  .incomingStackParamOffset = 0,
  // No sub-registers: a spill saves the whole register or it cannot be undone.
  .spillRegisterWidth = sizeof(intptr_t),

  // riscv64 has no condition flags at all - a compare and a branch are one
  // instruction, and a materialized boolean comes out of 'slt' into an
  // ordinary register - so there is nothing here to name.
  .flagsReg = IR_NO_PHYS_REG,

  .intArgRegs = riscvIntArgRegs,
  .intArgRegCount = sizeof(riscvIntArgRegs) / sizeof(riscvIntArgRegs[0]),
  .fpArgRegs = riscvFpArgRegs,
  .fpArgRegCount = sizeof(riscvFpArgRegs) / sizeof(riscvFpArgRegs[0]),

  .vaListKind = VA_FLAT_POINTER,
  .largeAggregatesByReference = TRUE,

  // LP64D's long double is IEEE binary128, and RV64G has no instruction that
  // touches one: every operation on the type is a call into libgcc, and a
  // value of it travels in a pair of integer registers.
  .longDoubleFormat = LDF_BINARY128,

  .intRetReg = X_A0,
  .fpRetReg = FR(F_A0),
  // LP64D returns a two-eightbyte aggregate in a0:a1 or fa0:fa1, as SysV does.
  // Which eightbyte goes to which file is still decided by classifyComposite,
  // which is SysV's rule and not LP64D's float flattening - the one deliberate
  // ABI divergence left on this target, recorded in the plan's R6.
  .intRetReg2 = X_A1,
  .fpRetReg2 = FR(F_A1),

  .callerSavedRegs = riscvCallerSavedRegs,
  .callerSavedRegCount = sizeof(riscvCallerSavedRegs) / sizeof(riscvCallerSavedRegs[0]),

  .scratchRegs = { [RC_GP] = riscvGpScratchRegs, [RC_FP] = riscvFpScratchRegs },
  .scratchRegCount = {
    [RC_GP] = sizeof(riscvGpScratchRegs) / sizeof(riscvGpScratchRegs[0]),
    [RC_FP] = sizeof(riscvFpScratchRegs) / sizeof(riscvFpScratchRegs[0])
  },

  .allocatableRegs = { [RC_GP] = riscvGpAllocatableRegs, [RC_FP] = riscvFpAllocatableRegs },
  .allocatableRegCount = {
    [RC_GP] = sizeof(riscvGpAllocatableRegs) / sizeof(riscvGpAllocatableRegs[0]),
    [RC_FP] = sizeof(riscvFpAllocatableRegs) / sizeof(riscvFpAllocatableRegs[0])
  },

  .classifyParameters = &classifyParametersGeneric
};
