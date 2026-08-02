#ifndef __MACHINE_X86_64_H__
#define __MACHINE_X86_64_H__ 1

#include "ir/machine.h"

// x86-64's own machine opcodes, numbered from MOP_TARGET_FIRST upwards (see
// the namespace split in include/ir/machine.h).
//
// These are not the assembler's enum Opcodes. That one names an *operation*
// and leaves the operand form to the emit* call taking it; this one names an
// instruction the way selection has to think about it, which is one opcode per
// distinct thing the ISA can do. The two meet in stage 3, where each opcode
// below turns into one emit* call.
//
// Everything here is two-address where the ISA is: operand 0 is the
// destination and is also read, so the selector emits the 'dst <- lhs' copy
// itself. Doing that here rather than in a generic fixup pass is a departure
// from the pipeline sketch, and deliberate - "add reads its destination" is
// target knowledge, and we have no per-instruction descriptor table for a
// target-independent pass to learn it from.

// Where the xmm half of the flat physical namespace starts. The GP half is
// already the encoding enum Registers uses, so a GP id needs no translation
// and an FP id needs exactly this subtracted. Defined here rather than in
// target_x86_64.c because selection, allocation and emission all have to agree
// on it, and the register tables are only one of the three.
#define X86_FP_BASE 16

#define X86_OPCODES                                                            \
  X86_OPCODE_DEF(MOV, "mov"),   /* dst <- immediate; reg-reg is MOP_COPY */    \
  X86_OPCODE_DEF(ADD, "add"),                                                  \
  X86_OPCODE_DEF(SUB, "sub"),                                                  \
  X86_OPCODE_DEF(IMUL, "imul"), /* two-operand form: signed and unsigned       \
                                   low halves agree, so one opcode covers      \
                                   both IR_E_MUL signednesses */               \
  X86_OPCODE_DEF(AND, "and"),                                                  \
  X86_OPCODE_DEF(OR, "or"),                                                    \
  X86_OPCODE_DEF(XOR, "xor"),                                                  \
  X86_OPCODE_DEF(SHL, "shl"),                                                  \
  X86_OPCODE_DEF(SHR, "shr"), /* logical, for unsigned IR_E_SHR */             \
  X86_OPCODE_DEF(SAR, "sar"), /* arithmetic, for signed IR_E_SHR */            \
  X86_OPCODE_DEF(NEG, "neg"),                                                  \
  X86_OPCODE_DEF(NOT, "not"),                                                  \
  X86_OPCODE_DEF(CMP, "cmp"),                                                  \
  X86_OPCODE_DEF(TEST, "test"),                                                \
  X86_OPCODE_DEF(CDQ, "cdq"),   /* sign-extend the dividend into rdx:rax */    \
  X86_OPCODE_DEF(IDIV, "idiv"),                                                \
  X86_OPCODE_DEF(DIV, "div"),                                                  \
  X86_OPCODE_DEF(JMP, "jmp"),                                                  \
  X86_OPCODE_DEF(RET, "ret") /* the return itself; stage 3 puts the epilogue   \
                                in front of it, once it knows the frame */

// The conditions the integer comparisons need, and no others: signed and
// unsigned orderings plus equality. Each generates both a setcc and a jcc,
// because that is exactly the pair a compare gets used by - materialize the
// boolean, or branch on it.
#define X86_CONDITIONS                                                         \
  X86_CC_DEF(E, "e"),   /* == */                                               \
  X86_CC_DEF(NE, "ne"), /* != */                                               \
  X86_CC_DEF(L, "l"), X86_CC_DEF(LE, "le"), /* signed <, <= */                 \
  X86_CC_DEF(G, "g"), X86_CC_DEF(GE, "ge"), /* signed >, >= */                 \
  X86_CC_DEF(B, "b"), X86_CC_DEF(BE, "be"), /* unsigned <, <= */               \
  X86_CC_DEF(A, "a"), X86_CC_DEF(AE, "ae")  /* unsigned >, >= */

enum X86Opcode {
  // The list below has to start at MOP_TARGET_FIRST, and an enum continues
  // from its predecessor, so this names the slot before it.
  X86_OPCODE_BASE = MOP_TARGET_FIRST - 1,

#define X86_OPCODE_DEF(m, _) X86_##m
  X86_OPCODES,
#undef X86_OPCODE_DEF

#define X86_CC_DEF(m, _) X86_SET##m
  X86_CONDITIONS,
#undef X86_CC_DEF

#define X86_CC_DEF(m, _) X86_J##m
  X86_CONDITIONS,
#undef X86_CC_DEF

  X86_OPCODE_COUNT
};

#define X86_OPCODE_NUM (X86_OPCODE_COUNT - MOP_TARGET_FIRST)

// Defined in target_x86_64.c, alongside the register tables it is the
// counterpart of, and reached through TargetDescriptor.opcodeName.
extern const char *const x86OpcodeName[X86_OPCODE_NUM];

#endif // __MACHINE_X86_64_H__
