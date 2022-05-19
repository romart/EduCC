
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


enum JumpCondition {
  JC_BAD = -1,
  JC_OVERFLOW = 0x0,
  JC_NOT_OVERFLOW = 0x1,
  JC_BELOW = 0x2,
  JC_NOT_A_E = JC_BELOW,
  JC_CARRY = JC_BELOW,
  JC_A_E = 0x3,
  JC_NOT_CARRY = JC_A_E,
  JC_EQ = 0x4,
  JC_ZERO = JC_EQ,
  JC_NE = 0x5,
  JC_NOT_ZERO = JC_NE,
  JC_B_E = 0x6,
  JC_NOT_A = JC_B_E,
  JC_A = 0x7,
  JC_NOT_B_E = JC_A,
  JC_SIGN = 0x8,
  JC_NOT_SIGN = 0x9,
  JC_PARITY = 0xa,
  JC_PARITY_EVEN = JC_PARITY,
  JC_PARITY_ODD = 0xb,
  JC_NOT_PARITY = JC_PARITY_ODD,
  JC_L = 0xc,
  JC_NOT_GE = JC_L,
  JC_GE = 0xd,
  JC_NOT_L = JC_GE,
  JC_LE = 0xe,
  JC_NOT_G = JC_LE,
  JC_G = 0xf,
  JC_NOT_LE = JC_G
};

enum Registers {
  R_BAD = -1,
  R_EAX = 0,
  R_ECX,
  R_EDX,
  R_EBX,
  R_ESP,
  R_EBP,
  R_ESI,
  R_EDI,

  R_R8,
  R_R9,
  R_R10,
  R_R11,
  R_R12,
  R_R13,
  R_R14,
  R_R15,

  R_COUNT,

  R_XMM0 = 0,
  R_XMM1,
  R_XMM2,
  R_XMM3,
  R_XMM4,
  R_XMM5,
  R_XMM6,
  R_XMM7,
  R_XMM8,
  R_XMM9,
  R_XMM10,
  R_XMM11,
  R_XMM12,
  R_XMM13,
  R_XMM14,
  R_XMM15,


  R_ACC = R_EAX,
  R_TMP = R_EDX,

  R_FACC = R_XMM0,
  R_FTMP = R_XMM1,

  R_ARG_0 = R_EDI,
  R_ARG_1 = R_ESI,
  R_ARG_2 = R_EDX,
  R_ARG_3 = R_ECX,
  R_ARG_4 = R_R8,
  R_ARG_5 = R_R9,

  R_RIP = -2,

  R_PARAM_COUNT = 6,
  R_FP_PARAM_COUNT = R_XMM7 - R_XMM0 + 1



};

#define OPCODES \
  OPCODE_DEF(I_ADD, "addl", I, 2), \
  OPCODE_DEF(L_ADD, "addq", L, 2), \
  OPCODE_DEF(F_ADD, "fadd", F, 2), \
  OPCODE_DEF(D_ADD, "dadd", D, 2), \
  OPCODE_DEF(I_SUB, "subl", I, 2), \
  OPCODE_DEF(L_SUB, "subq", L, 2), \
  OPCODE_DEF(F_SUB, "fsub", F, 2), \
  OPCODE_DEF(D_SUB, "dsub", D, 2), \
  OPCODE_DEF(I_MUL, "mull", I, 2), \
  OPCODE_DEF(L_MUL, "mulq", L, 2), \
  OPCODE_DEF(F_MUL, "fmul", F, 2), \
  OPCODE_DEF(D_MUL, "dmul", D, 2), \
  OPCODE_DEF(I_DIV, "divl", I, 2), \
  OPCODE_DEF(L_DIV, "divq", L, 2), \
  OPCODE_DEF(F_DIV, "fdiv", F, 2), \
  OPCODE_DEF(D_DIV, "ddiv", D, 2), \
  OPCODE_DEF(I_MOD, "modl", I, 2), \
  OPCODE_DEF(L_MOD, "modq", L, 2), \
  OPCODE_DEF(F_MOD, "fmod", F, 2), \
  OPCODE_DEF(D_MOD, "dmod", D, 2), \
  OPCODE_DEF(I_SHR, "shrl", L, 2), \
  OPCODE_DEF(L_SHR, "shrq", L, 2), \
  OPCODE_DEF(I_SAR, "sarl", L, 2), \
  OPCODE_DEF(L_SAR, "sarq", L, 2), \
  OPCODE_DEF(I_SHL, "shll", L, 2), \
  OPCODE_DEF(L_SHL, "shlq", L, 2), \
  OPCODE_DEF(I_AND, "andl", I, 2), \
  OPCODE_DEF(L_AND, "andq", L, 2), \
  OPCODE_DEF(I_OR, "orl", I, 2), \
  OPCODE_DEF(L_OR, "orq", L, 2), \
  OPCODE_DEF(I_XOR, "xorl", I, 2), \
  OPCODE_DEF(L_XOR, "xorq", L, 2), \
  OPCODE_DEF(I_CMP, "cmpl", I, 2), \
  OPCODE_DEF(L_CMP, "cmpq", L, 2), \
  OPCODE_DEF(F_CMP, "fcmp", F, 2), \
  OPCODE_DEF(D_CMP, "dcmp", D, 2)

enum Opcodes {
  #define OPCODE_DEF(m, _, __, ___) OP_##m

  OPCODES,

  #undef OPCODE_DEF

  OP_NUM
};

typedef struct _Address  {
  enum Registers base;
  enum Registers index;
  int64_t scale;
  int32_t imm;

  Relocation *reloc;
  struct Label *label;
} Address;

struct _GeneratedFunction;

void emitCondJump(struct _GeneratedFunction *f, struct Label *label, enum JumpCondition cond);

void emitPushReg(struct _GeneratedFunction *f, enum Registers reg);
void emitPopReg(struct _GeneratedFunction *f, enum Registers reg);

void emitLea(struct _GeneratedFunction *f, Address *from, enum Registers to);

void emitMoveRR(struct _GeneratedFunction *f, enum Registers from, enum Registers to, size_t size);
void emitMoveAR(struct _GeneratedFunction *f, Address* addr, enum Registers to, size_t size);
void emitMoveRA(struct _GeneratedFunction *f, enum Registers from, Address* addr, size_t size);
void emitMoveCR(struct _GeneratedFunction *f, intptr_t c, enum Registers to, size_t size);

void emitArithRR(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers l, enum Registers r);
void emitArithConst(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers r, int64_t c, size_t size);
void emitArithAR(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers r, Address *addr, size_t size);
void emitNot(struct _GeneratedFunction *f, enum Registers reg, size_t size);
void emitZeroReg(struct _GeneratedFunction *f, enum Registers reg);

void emitSetccR(struct _GeneratedFunction *f, enum JumpCondition cc, enum Registers reg);

void emitTestRR(struct _GeneratedFunction *f, enum Registers l, enum Registers r, size_t s);
void emitSar(struct _GeneratedFunction *f, enum Registers r, int s);
void emitShr(struct _GeneratedFunction *f, enum Registers r, int s);
void emitShl(struct _GeneratedFunction *f, enum Registers r, int s);
void emitBitwiseNot(struct _GeneratedFunction *f, enum Registers reg);

void emitJumpTo(struct _GeneratedFunction *f, struct Label *l);
void emitJumpByReg(struct _GeneratedFunction *f, enum Registers reg);
void emitCall(struct _GeneratedFunction *f, enum Registers reg);
void emitCallLiteral(struct _GeneratedFunction *f, Relocation *reloc);

void emitRet(struct _GeneratedFunction *f, uint16_t s);

void patchJumpTo(struct _GeneratedFunction *f, ptrdiff_t inst_cp, size_t instSize, ptrdiff_t label_cp);
void patchRefTo(struct _GeneratedFunction *f, ptrdiff_t literal_cp, ptrdiff_t label_cp);

void emitMovxxRR(struct _GeneratedFunction *f, uint8_t opcode, enum Registers from, enum Registers to);
void emitMovxxAR(struct _GeneratedFunction *f, uint8_t opcode, Address *from, enum Registers to);
void emitConvertWDQ(struct _GeneratedFunction *f, uint8_t opSize);
void emitConvertFP(struct _GeneratedFunction *f, uint8_t prefix, uint8_t opcode, enum Registers from, enum Registers to, Boolean isW);

void emitMovfpRR(struct _GeneratedFunction *f, enum Registers from, enum Registers to, Boolean isD);
void emitMovfpRA(struct _GeneratedFunction *f, enum Registers from, Address *to, Boolean isD);
void emitMovfpAR(struct _GeneratedFunction *f, Address *from, enum Registers to, Boolean isD);

void emitMovdq(struct _GeneratedFunction *f, uint8_t prefix, uint8_t opcode, uint8_t opcode2, enum Registers from, enum Registers to, Boolean isW);
void emitCmovRR(struct _GeneratedFunction *f, enum JumpCondition cc, enum Registers from, enum Registers to, Boolean isW);

void emitMoveCR_Reloc(struct _GeneratedFunction *f, Relocation *reloc, enum Registers reg);

#endif // __INSTR_H__
