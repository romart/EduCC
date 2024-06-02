
#ifndef __INSTR_RISCV64_H__
#define __INSTR_RISCV64_H__ 1

#include "instructions.h"

enum XRegister {
  X_BAD = -1,

  X_ZERO = 0,
  X_RA = 1,    // X1, return address
  X_SP = 2,    // X2, stack pointer
  X_GP = 3,    // X3, global pointer
  X_TP = 4,    // X4, thread pointer

  X_T0 = 5,  // X5, temporary 0
  X_T1 = 6,  // X6, temporary 1
  X_T2 = 7,  // X7, temporary 2

  X_S0 = 8,  // X8, callee-saved 0
  X_S1 = 9,  // X9, callee-saved 1

  X_A0 = 10,  // X10, argument 0 / return value 0
  X_A1 = 11,  // X11, argument 1 / return value 1
  X_A2 = 12,  // X12, argument 2
  X_A3 = 13,  // X13, argument 3
  X_A4 = 14,  // X14, argument 4
  X_A5 = 15,  // X15, argument 5
  X_A6 = 16,  // X16, argument 6
  X_A7 = 17,  // X17, argument 7

  X_S2 = 18,   // X18, callee-saved 2
  X_S3 = 19,   // X19, callee-saved 3
  X_S4 = 20,   // X20, callee-saved 4
  X_S5 = 21,   // X21, callee-saved 5
  X_S6 = 22,   // X22, callee-saved 6
  X_S7 = 23,   // X23, callee-saved 7
  X_S8 = 24,   // X24, callee-saved 8
  X_S9 = 25,   // X25, callee-saved 9
  X_S10 = 26,  // X26, callee-saved 10
  X_S11 = 27,  // X27, callee-saved 11

  X_T3 = 28,  // X28, temporary 3
  X_T4 = 29,  // X29, temporary 4
  X_T5 = 30,  // X30, temporary 5
  X_T6 = 31,  // X31, temporary 6

  X_COUNT,
  X_GP_PARAM_COUNT = 8
};

enum FRegister {
  F_BAD = -1,

  F_T0 = 0,  // F0, temporary 0
  F_T1 = 1,  // F1, temporary 1
  F_T2 = 2,  // F2, temporary 2
  F_T3 = 3,  // F3, temporary 3
  F_T4 = 4,  // F4, temporary 4
  F_T5 = 5,  // F5, temporary 5
  F_T6 = 6,  // F6, temporary 6
  F_T7 = 7,  // F7, temporary 7

  F_S0 = 8,  // F8, callee-saved 0
  F_S1 = 9,  // F9, callee-saved 1

  F_A0 = 10,  // F10, argument 0 / return value 0
  F_A1 = 11,  // F11, argument 1 / return value 1
  F_A2 = 12,  // F12, argument 2
  F_A3 = 13,  // F13, argument 3
  F_A4 = 14,  // F14, argument 4
  F_A5 = 15,  // F15, argument 5
  F_A6 = 16,  // F16, argument 6
  F_A7 = 17,  // F17, argument 7

  F_S2 = 18,   // F18, callee-saved 2
  F_S3 = 19,   // F19, callee-saved 3
  F_S4 = 20,   // F20, callee-saved 4
  F_S5 = 21,   // F21, callee-saved 5
  F_S6 = 22,   // F22, callee-saved 6
  F_S7 = 23,   // F23, callee-saved 7
  F_S8 = 24,   // F24, callee-saved 8
  F_S9 = 25,   // F25, callee-saved 9
  F_S10 = 26,  // F26, callee-saved 10
  F_S11 = 27,  // F27, callee-saved 11

  F_T8 = 28,   // F28, temporary 8
  F_T9 = 29,   // F29, temporary 9
  F_T10 = 30,  // F30, temporary 10
  F_T11 = 31,  // F31, temporary 11

  F_COUNT,
  F_FP_PARAM_COUNT = 8
};

enum VRegister {
  V_BAD = -1,

  V0 = 0,  // V0, argument 0
  V1 = 1,  // V1, callee-saved 0
  V2 = 2,  // V2, callee-saved 1
  V3 = 3,  // V3, callee-saved 2
  V4 = 4,  // V4, callee-saved 3
  V5 = 5,  // V5, callee-saved 4
  V6 = 6,  // V6, callee-saved 5
  V7 = 7,  // V7, callee-saved 6

  V8 = 8,    // V8, argument 1
  V9 = 9,    // V9, argument 2
  V10 = 10,  // V10, argument 3
  V11 = 11,  // V11, argument 4
  V12 = 12,  // V12, argument 5
  V13 = 13,  // V13, argument 6
  V14 = 14,  // V14, argument 7
  V15 = 15,  // V15, argument 8

  V16 = 16,  // V16, argument 9
  V17 = 17,  // V17, argument 10
  V18 = 18,  // V18, argument 11
  V19 = 19,  // V19, argument 12
  V20 = 20,  // V20, argument 13
  V21 = 21,  // V21, argument 14
  V22 = 22,  // V22, argument 15
  V23 = 23,  // V23, argument 16

  V24 = 24,  // V24, callee-saved 7
  V25 = 25,  // V25, callee-saved 8
  V26 = 26,  // V26, callee-saved 9
  V27 = 27,  // V27, callee-saved 10
  V28 = 28,  // V28, callee-saved 11
  V29 = 29,  // V29, callee-saved 12
  V30 = 30,  // V30, callee-saved 13
  V31 = 31,  // V31, callee-saved 14

  V_COUNT
};

#endif // __INSTR_RISCV64_H__
