#ifndef __MACHINE_RISCV64_H__
#define __MACHINE_RISCV64_H__ 1

#include "ir/machine.h"

// riscv64's own machine opcodes, numbered from MOP_TARGET_FIRST upwards (see
// the namespace split in include/ir/machine.h).
//
// Two things differ from the x86-64 list next door, and both make this one
// shorter. RV64G is three-address, so no opcode reads its own destination and
// the selector emits no 'dst <- lhs' copy; and there are no condition flags at
// all, so a comparison either lands in an ordinary register (slt, feq) or is
// part of the branch itself (beq) - there is no setcc/jcc pair to generate two
// of.
//
// An immediate does not get an opcode of its own. 'addi' is 'add' with an
// MO_IMM operand, and emission picks the encoding, exactly as x86's does; the
// operations RV64 has no immediate form for - mul, div, sub - say so through
// ArchSelector.isLegalImmediate instead, and the selector turns 'sub imm' into
// an add of the negation.

// Where the f-register half of the flat physical namespace starts. An x-
// register id is already the encoding, an f-register id needs exactly this
// subtracted. Defined here rather than in target_riscv64.c because selection,
// allocation and emission all have to agree on it and the register tables are
// only one of the three.
#define RISCV_FP_BASE 32

#define RISCV_OPCODES                                                          \
  RISCV_OPCODE_DEF(LI, "li"),   /* dst <- an integer constant, in as many      \
                                   instructions as it takes */                 \
  RISCV_OPCODE_DEF(LA, "la"),   /* dst <- an address, not what is at it: a     \
                                   frame slot, a global, a constant-pool entry \
                                   or a block. The counterpart of x86's lea,   \
                                   and under medlow a lui/addi pair */         \
  RISCV_OPCODE_DEF(LOAD, "l"),  /* dst <- [address], sign-extended to the      \
                                   register: lb/lh/lw/ld by opSize */          \
  RISCV_OPCODE_DEF(LOADU, "lu"), /* the same, zero-extended: lbu/lhu/lwu.      \
                                   Two opcodes rather than a flag because RV64 \
                                   has two instructions, where x86 has one mov \
                                   and says it with movsx/movzx afterwards */  \
  RISCV_OPCODE_DEF(STORE, "s"), /* [address] <- src: sb/sh/sw/sd */            \
  RISCV_OPCODE_DEF(SEXT, "sext"), /* widen keeping the sign; opSize is the     \
                                     destination and srcSize the source. A     \
                                     shift pair, or addiw for 4 -> 8 */        \
  RISCV_OPCODE_DEF(ZEXT, "zext"), /* widen filling with zeroes; likewise, and  \
                                     always a shift pair on RV64G - zext.w is  \
                                     Zba, which is not assumed here */         \
                                                                               \
  /* Integer arithmetic. The 'w' forms are the 32-bit ones, which sign-extend  \
     their result into the whole register - that is what makes a 32-bit value  \
     in a 64-bit register have exactly one representation, and why 'int'       \
     arithmetic selects them rather than the full-width form plus a cast. */   \
  RISCV_OPCODE_DEF(ADD, "add"),                                                \
  RISCV_OPCODE_DEF(ADDW, "addw"),                                              \
  RISCV_OPCODE_DEF(SUB, "sub"),                                                \
  RISCV_OPCODE_DEF(SUBW, "subw"),                                              \
  RISCV_OPCODE_DEF(MUL, "mul"),                                                \
  RISCV_OPCODE_DEF(MULW, "mulw"),                                              \
  RISCV_OPCODE_DEF(DIV, "div"),                                                \
  RISCV_OPCODE_DEF(DIVU, "divu"),                                              \
  RISCV_OPCODE_DEF(DIVW, "divw"),                                              \
  RISCV_OPCODE_DEF(DIVUW, "divuw"),                                            \
  RISCV_OPCODE_DEF(REM, "rem"),                                                \
  RISCV_OPCODE_DEF(REMU, "remu"),                                              \
  RISCV_OPCODE_DEF(REMW, "remw"),                                              \
  RISCV_OPCODE_DEF(REMUW, "remuw"),                                            \
  RISCV_OPCODE_DEF(AND, "and"),                                                \
  RISCV_OPCODE_DEF(OR, "or"),                                                  \
  RISCV_OPCODE_DEF(XOR, "xor"),                                                \
  RISCV_OPCODE_DEF(SLL, "sll"),                                                \
  RISCV_OPCODE_DEF(SRL, "srl"),                                                \
  RISCV_OPCODE_DEF(SRA, "sra"),                                                \
  RISCV_OPCODE_DEF(SLLW, "sllw"),                                              \
  RISCV_OPCODE_DEF(SRLW, "srlw"),                                              \
  RISCV_OPCODE_DEF(SRAW, "sraw"),                                              \
  RISCV_OPCODE_DEF(SLT, "slt"),   /* dst <- lhs < rhs, signed, as 0 or 1 */    \
  RISCV_OPCODE_DEF(SLTU, "sltu"), /* likewise unsigned. These two and the      \
                                     branches below are the whole of integer   \
                                     comparison: == and != are a subtraction   \
                                     against zero, and the other four orders   \
                                     come from swapping the operands or        \
                                     inverting the answer */                   \
  RISCV_OPCODE_DEF(NEG, "neg"),   /* sub from x0 */                            \
  RISCV_OPCODE_DEF(NOT, "not"),   /* xori -1 */                                \
                                                                               \
  /* Scalar float, F and D chosen by opSize exactly as x86's ss/sd are. */     \
  RISCV_OPCODE_DEF(FADD, "fadd"),                                              \
  RISCV_OPCODE_DEF(FSUB, "fsub"),                                              \
  RISCV_OPCODE_DEF(FMUL, "fmul"),                                              \
  RISCV_OPCODE_DEF(FDIV, "fdiv"),                                              \
  RISCV_OPCODE_DEF(FNEG, "fneg"), /* fsgnjn rd, rs, rs */                      \
  RISCV_OPCODE_DEF(FABS, "fabs"), /* fsgnjx rd, rs, rs */                      \
  RISCV_OPCODE_DEF(FLOAD, "fl"),                                               \
  RISCV_OPCODE_DEF(FSTORE, "fs"),                                              \
  /* The comparisons answer into an *integer* register, which is the whole of  \
     why this target needs no flags: there is nothing to keep live between a   \
     compare and its reader, because the answer is an ordinary value. All      \
     three are the quiet forms C's == and != want; the ordered predicates      \
     raise on a signalling NaN only, which is what flt/fle already do. */      \
  RISCV_OPCODE_DEF(FEQ, "feq"),                                                \
  RISCV_OPCODE_DEF(FLT, "flt"),                                                \
  RISCV_OPCODE_DEF(FLE, "fle"),                                                \
  RISCV_OPCODE_DEF(FCVT_F2F, "fcvt.f2f"), /* float <-> double */               \
  RISCV_OPCODE_DEF(FCVT_I2F, "fcvt.i2f"), /* signed integer -> float; srcSize  \
                                             is the integer's width */         \
  RISCV_OPCODE_DEF(FCVT_U2F, "fcvt.u2f"), /* unsigned, likewise */             \
  RISCV_OPCODE_DEF(FCVT_F2I, "fcvt.f2i"), /* float -> signed integer,          \
                                             truncating as a C cast does;      \
                                             srcSize is the float's width */   \
  RISCV_OPCODE_DEF(FCVT_F2U, "fcvt.f2u"), /* unsigned, likewise */             \
  RISCV_OPCODE_DEF(FMV_X, "fmv.x"),       /* the bits of an f register into an \
                                             x one unchanged - how a float is  \
                                             passed to a variadic callee */    \
  RISCV_OPCODE_DEF(FMV_F, "fmv.f"),       /* and back, which is how a float    \
                                             constant is materialized without  \
                                             a constant pool */                \
                                                                               \
  RISCV_OPCODE_DEF(J, "j"),       /* unconditional */                          \
  RISCV_OPCODE_DEF(JR, "jr"),     /* through a register: a computed goto, and  \
                                     the tail of a jump table */               \
  RISCV_OPCODE_DEF(CALL, "call"), /* operand 0 is the callee: a symbol for a   \
                                     direct call, a register for an indirect   \
                                     one */                                    \
  RISCV_OPCODE_DEF(RET, "ret")    /* the return itself; stage 3 puts the       \
                                     epilogue in front of it, once it knows    \
                                     the frame */

// The conditional branches, which on RV64 are the comparison and the branch at
// once - so unlike x86's X86_CONDITIONS this list generates one opcode each
// rather than a setcc and a jcc. There is no unsigned >= / > pair because
// there is no instruction for one: the selector swaps the operands.
#define RISCV_BRANCHES                                                         \
  RISCV_BR_DEF(EQ, "beq"), RISCV_BR_DEF(NE, "bne"),                            \
  RISCV_BR_DEF(LT, "blt"), RISCV_BR_DEF(GE, "bge"),                            \
  RISCV_BR_DEF(LTU, "bltu"), RISCV_BR_DEF(GEU, "bgeu")

enum RiscvOpcode {
  // The list below has to start at MOP_TARGET_FIRST, and an enum continues
  // from its predecessor, so this names the slot before it.
  RISCV_OPCODE_BASE = MOP_TARGET_FIRST - 1,

#define RISCV_OPCODE_DEF(m, _) RISCV_##m
  RISCV_OPCODES,
#undef RISCV_OPCODE_DEF

#define RISCV_BR_DEF(m, _) RISCV_B##m
  RISCV_BRANCHES,
#undef RISCV_BR_DEF

  RISCV_OPCODE_COUNT
};

#define RISCV_OPCODE_NUM (RISCV_OPCODE_COUNT - MOP_TARGET_FIRST)

// Defined in target_riscv64.c, alongside the register tables it is the
// counterpart of, and reached through TargetDescriptor.opcodeName.
extern const char *const riscvOpcodeName[RISCV_OPCODE_NUM];

#endif // __MACHINE_RISCV64_H__
