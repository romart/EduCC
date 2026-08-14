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
  X86_OPCODE_DEF(LEA, "lea"),   /* dst <- an address, not what is at it: the   \
                                   only way to get a frame slot's or a         \
                                   global's address into a register */         \
  X86_OPCODE_DEF(LOAD, "mov"),  /* dst <- [address] */                         \
  X86_OPCODE_DEF(STORE, "mov"), /* [address] <- src. Spelled "mov" like the    \
                                   load, because that is the mnemonic and the  \
                                   operands say which way it goes - a dump     \
                                   reads '%v2 = mov.4 [%v1]' one way and       \
                                   'mov.4 [%v1], %v2' the other */             \
  X86_OPCODE_DEF(MOVSX, "movsx"), /* widen, keeping the sign; opSize is the    \
                                     destination and srcSize the source */     \
  X86_OPCODE_DEF(MOVZX, "movzx"), /* widen, filling with zeroes */             \
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
                                                                               \
  /* SSE scalar arithmetic. One opcode per operation rather than one per       \
     operation and width: opSize picks between the 'ss' and 'sd' encodings,    \
     exactly as it picks between the integer widths above. */                  \
  X86_OPCODE_DEF(FADD, "fadd"),                                                \
  X86_OPCODE_DEF(FSUB, "fsub"),                                                \
  X86_OPCODE_DEF(FMUL, "fmul"),                                                \
  X86_OPCODE_DEF(FDIV, "fdiv"),                                                \
  X86_OPCODE_DEF(FCMP, "comis"),   /* ordered: raises on a quiet NaN, which is \
                                      what C's <, <=, > and >= are defined to  \
                                      do */                                    \
  X86_OPCODE_DEF(FUCMP, "ucomis"), /* unordered: quiet on a quiet NaN, for ==  \
                                      and != */                                \
  X86_OPCODE_DEF(MOVD, "movd"),    /* the bits of a GP register into an xmm    \
                                      one unchanged, which is how a float      \
                                      constant is materialized here without a  \
                                      constant pool */                         \
  X86_OPCODE_DEF(CVTF2F, "cvtf2f"),   /* float <-> double */                   \
  X86_OPCODE_DEF(CVTSI2F, "cvtsi2f"), /* integer -> float; srcSize is the      \
                                         integer's width */                    \
  X86_OPCODE_DEF(CVTF2SI, "cvtf2si"), /* float -> integer, truncating, as a C  \
                                         cast does; srcSize is the float's */  \
  X86_OPCODE_DEF(CDQ, "cdq"),   /* sign-extend the dividend into rdx:rax */    \
  X86_OPCODE_DEF(IDIV, "idiv"),                                                \
  X86_OPCODE_DEF(DIV, "div"),                                                  \
  X86_OPCODE_DEF(JMP, "jmp"),                                                  \
  X86_OPCODE_DEF(PUSH, "push"), /* an outgoing stack argument, and nothing     \
                                   else - see selectCall */                    \
  X86_OPCODE_DEF(CALL, "call"), /* operand 0 is the callee: a symbol for a     \
                                   direct call, a register for an indirect     \
                                   one */                                      \
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
  X86_CC_DEF(A, "a"), X86_CC_DEF(AE, "ae"), /* unsigned >, >= */                \
  /* Parity, which after a float compare means "unordered": one of the two     \
     operands was a NaN. Only == and != need it - the ordered comparisons get  \
     the answer they want out of the carry and zero flags alone. */            \
  X86_CC_DEF(P, "p"), X86_CC_DEF(NP, "np")

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
