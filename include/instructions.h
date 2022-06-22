
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
  R_TMP2 = R_ESI,

  R_FACC = R_XMM0,
  R_FTMP = R_XMM1,
  R_FTMP2 = R_XMM2,

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
  OPCODE_DEF(ADD, "add", I, 2), \
  OPCODE_DEF(FADD, "fadd", F, 2), \
  OPCODE_DEF(SUB, "sub", I, 2), \
  OPCODE_DEF(FSUB, "fsub", F, 2), \
  OPCODE_DEF(SMUL, "smul", I, 2), \
  OPCODE_DEF(UMUL, "umul", I, 2), \
  OPCODE_DEF(FMUL, "fmul", F, 2), \
  OPCODE_DEF(SDIV, "sdiv", I, 2), \
  OPCODE_DEF(UDIV, "udiv", L, 2), \
  OPCODE_DEF(FDIV, "fdiv", F, 2), \
  OPCODE_DEF(FMOD, "fmod", F, 2), \
  OPCODE_DEF(SHR, "shr", L, 2), \
  OPCODE_DEF(SAR, "sar", L, 2), \
  OPCODE_DEF(SHL, "shl", L, 2), \
  OPCODE_DEF(AND, "and", I, 2), \
  OPCODE_DEF(OR, "or", I, 2), \
  OPCODE_DEF(XOR, "xor", I, 2), \
  OPCODE_DEF(CMP, "cmp", I, 2), \
  OPCODE_DEF(FOCMP, "focmp", F, 2), \
  OPCODE_DEF(FUCMP, "fucmp", F, 2), \
  OPCODE_DEF(PXOR, "fxor", F, 2)

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

void emitCondJump(struct _GeneratedFunction *f, struct Label *label, enum JumpCondition cond, Boolean isNear);

void emitPushReg(struct _GeneratedFunction *f, enum Registers reg);
void emitPopReg(struct _GeneratedFunction *f, enum Registers reg);

void emitLea(struct _GeneratedFunction *f, Address *from, enum Registers to);

void emitMoveRR(struct _GeneratedFunction *f, enum Registers from, enum Registers to, size_t size);
void emitMoveAR(struct _GeneratedFunction *f, Address* addr, enum Registers to, size_t size);
void emitMoveRA(struct _GeneratedFunction *f, enum Registers from, Address* addr, size_t size);
void emitMoveCR(struct _GeneratedFunction *f, intptr_t c, enum Registers to, int tid);

void emitArithRR(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers l, enum Registers r, size_t size);
void emitArithConst(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers r, int64_t c, int _tid);
void emitArithAR(struct _GeneratedFunction *f, enum Opcodes opcode, enum Registers r, Address *addr, size_t size);
void emitNot(struct _GeneratedFunction *f, enum Registers reg, size_t size);
void emitNegR(struct _GeneratedFunction *f, enum Registers reg, size_t size);
void emitNegA(struct _GeneratedFunction *f, Address *addr, size_t size);
void emitZeroReg(struct _GeneratedFunction *f, enum Registers reg);

void emitFPArith(struct _GeneratedFunction *f, enum Opcodes opcode, uint8_t stId, Boolean doPop);
void emitFPArithMem(struct _GeneratedFunction *f, enum Opcodes opcode, Address *addr);
void emitFPLoad(struct _GeneratedFunction *f, Address *addr, int tid);
void emitFPIntLoad(struct _GeneratedFunction *f, Address *addr, int32_t size);
void emitFPStore(struct _GeneratedFunction *f, Address *addr, int tid);
void emitFPIntStore(struct _GeneratedFunction *f, Address *addr, int32_t size);
void emitFPPop(struct _GeneratedFunction *f, uint8_t stId);
void emitFPnoArg(struct _GeneratedFunction *f, uint8_t opByte);
void emitFPnoArgMem(struct _GeneratedFunction *f, Address *addr, int digit);


void emitSetccR(struct _GeneratedFunction *f, enum JumpCondition cc, enum Registers reg);

void emitTestRR(struct _GeneratedFunction *f, enum Registers l, enum Registers r, size_t s);

void emitBitwiseNotR(struct _GeneratedFunction *f, enum Registers reg, size_t size);
void emitBitwiseNotA(struct _GeneratedFunction *f, Address *addr, size_t size);

void emitJumpTo(struct _GeneratedFunction *f, struct Label *l, Boolean isNear);
void emitJumpByReg(struct _GeneratedFunction *f, enum Registers reg);
void emitCall(struct _GeneratedFunction *f, enum Registers reg);
void emitCallLiteral(struct _GeneratedFunction *f, Relocation *reloc);

void emitRet(struct _GeneratedFunction *f, uint16_t s);

void emitLeave(struct _GeneratedFunction *f);

void patchJumpTo(struct _GeneratedFunction *f, ptrdiff_t inst_cp, size_t instSize, ptrdiff_t label_cp);
void patchRefTo(struct _GeneratedFunction *f, ptrdiff_t literal_cp, ptrdiff_t label_cp);

void emitMovsxdRR(struct _GeneratedFunction *f, enum Registers from, enum Registers to, size_t s);
void emitMovxxRR(struct _GeneratedFunction *f, uint8_t opcode, enum Registers from, enum Registers to);
void emitMovxxAR(struct _GeneratedFunction *f, uint8_t opcode, Address *from, enum Registers to);
void emitConvertWDQ(struct _GeneratedFunction *f, uint8_t opcode, uint8_t opSize);
void emitConvertFP(struct _GeneratedFunction *f, uint8_t prefix, uint8_t opcode, enum Registers from, enum Registers to, Boolean isW);

void emitMovfpRR(struct _GeneratedFunction *f, enum Registers from, enum Registers to, size_t size);
void emitMovfpRA(struct _GeneratedFunction *f, enum Registers from, Address *to, size_t size);
void emitMovfpAR(struct _GeneratedFunction *f, Address *from, enum Registers to, size_t size);

void emitMovdq(struct _GeneratedFunction *f, uint8_t prefix, uint8_t opcode, uint8_t opcode2, enum Registers from, enum Registers to, Boolean isW);
void emitCmovRR(struct _GeneratedFunction *f, enum JumpCondition cc, enum Registers from, enum Registers to, Boolean isW);

void emitMoveCR_Reloc(struct _GeneratedFunction *f, Relocation *reloc, enum Registers reg);

#endif // __INSTR_H__
