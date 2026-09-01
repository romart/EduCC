#include "ir/target.h"
#include "instructions_riscv64.h"

// Flat physical register namespace for riscv64:
//
//   0..31   x0..x31, in enum XRegister order.
//   32..63  f0..f31. enum FRegister spells these 0..31 too, so the offset is
//           what keeps an integer register distinct from a float one; subtract
//           RISCV_FP_BASE to get back to the encoding.
#define RISCV_FP_BASE 32
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

// LP64D: a0..a7 then the stack.
static const uint32_t riscvIntArgRegs[] = {
  X_A0, X_A1, X_A2, X_A3, X_A4, X_A5, X_A6, X_A7
};

// LP64D: fa0..fa7 then the stack.
static const uint32_t riscvFpArgRegs[] = {
  FR(F_A0), FR(F_A1), FR(F_A2), FR(F_A3),
  FR(F_A4), FR(F_A5), FR(F_A6), FR(F_A7)
};

const TargetDescriptor targetRiscv64 = {
  .name = "riscv64",

  .numPhysRegs = RISCV_PHYS_REG_COUNT,
  .regClass = riscvRegClass,
  .regName = riscvRegName,

  .sp = X_SP,
  .fp = X_S0,  // s0/fp is the frame pointer in the standard ABI

  // riscv64 has no condition flags at all - a compare and a branch are one
  // instruction, and a materialized boolean comes out of 'slt' into an
  // ordinary register - so there is nothing here to name.
  .flagsReg = IR_NO_PHYS_REG,

  .intArgRegs = riscvIntArgRegs,
  .intArgRegCount = sizeof(riscvIntArgRegs) / sizeof(riscvIntArgRegs[0]),
  .fpArgRegs = riscvFpArgRegs,
  .fpArgRegCount = sizeof(riscvFpArgRegs) / sizeof(riscvFpArgRegs[0]),

  .intRetReg = X_A0,
  .fpRetReg = FR(F_A0),

  .classifyParameters = &classifyParametersGeneric
};
