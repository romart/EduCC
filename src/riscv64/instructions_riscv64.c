#include <stdint.h>
#include <stddef.h>
#include <memory.h>
#include <assert.h>

#include "common.h"
#include "codegen.h"
#include "mem.h"
#include "instructions_riscv64.h"
#include "_elf.h"

static void emit16(GeneratedFunction *f, uint32_t value) { emitShort(f, (uint16_t)(value)); }
static void emit32(GeneratedFunction *f, uint32_t value) { emitDWord(f, value); }

uint32_t bitFieldExtract(uint32_t value, size_t lsb, size_t width) {
  uint32_t mask = (1u << width) - 1u;
  return (value >> lsb) & mask;
}

uint32_t bitFieldInsert(uint32_t value, uint32_t data, size_t lsb, size_t width) {
  uint32_t mask = (1u << width) - 1u;
  return (value & ~(mask << lsb)) | ((data & mask) << lsb);
}

// Whether 'v' fits in a signed field of this many bits. The bound is computed
// at int64_t and not at unsigned int: '-(1u << (bits - 1))' wraps to a large
// positive value, which then widens to int64_t rather than going negative, so
// the old test rejected every value it was asked about - including zero, and
// including every register number the formats below check.
int isInt(int64_t v, size_t bits) {
  if (bits >= 64) return 1;

  int64_t bound = INT64_C(1) << (bits - 1);
  return -bound <= v && v < bound;
}

// And in an unsigned field. '(1u << (bits - 1)) * 2u - 1u' is one short: at
// five bits it admitted 0..30 and refused x31, which is a register RV64 very
// much has.
int isUint(uint64_t v, size_t bits) {
  if (bits >= 64) return 1;

  return v < (UINT64_C(1) << bits);
}

// 'align' is a *byte* count, and a power of two. It used to be read as a bit
// count - '(1U << align) - 1' - so the two callers that pass 2 meaning "an
// even address", which is what the B- and J-type encodings drop the low bit
// of, were checking four-byte alignment instead. With no compressed
// instructions emitted every target happens to be four-byte aligned, so this
// was a check that passed for the wrong reason.
int isAligned(uint64_t v, size_t align) {
  return (v & (align - 1)) == 0;
}

int isShortReg(enum XRegister reg) {
    uint32_t uv = (uint32_t)(reg) - 8u;
    return isUint(uv, 3);
}

// I-type instruction:
//
//    31                   20 19     15 14 12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . . . . . . . . | . . . . | . . | . . . . | . . . . . . ]
//   [        imm11:0            rs1   funct3     rd        opcode   ]
//   -----------------------------------------------------------------
void EmitI(GeneratedFunction *f, int32_t imm12, uint32_t rs1, uint32_t funct3, uint32_t rd, uint32_t opcode) {
  assert(isInt(imm12, 12));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd), 5));
  // Seven, as every other format has it. At five this refused jalr's 0x67.
  assert(isUint(opcode, 7));
  uint32_t encoding = ((uint32_t)(imm12) & 0xfffu) << 20 | rs1 << 15 | funct3 << 12 | rd << 7 | opcode;
  emit32(f, encoding);
}

// R-type instruction:
//
//    31         25 24     20 19     15 14 12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . . . | . . . . | . . . . | . . | . . . . | . . . . . . ]
//   [   funct7        rs2       rs1   funct3     rd        opcode   ]
//   -----------------------------------------------------------------
void EmitR(GeneratedFunction *f,
           uint32_t funct7, uint32_t rs2, uint32_t rs1, uint32_t funct3, uint32_t rd, uint32_t opcode) {
  assert(isUint(funct7, 7));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 7));
  uint32_t encoding = funct7 << 25 | rs2 << 20 | rs1 << 15 | funct3 << 12 | rd << 7 | opcode;
  emit32(f, encoding);
}

// R-type instruction variant for floating-point fused multiply-add/sub (F[N]MADD/ F[N]MSUB):
//
//    31     27  25 24     20 19     15 14 12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . | . | . . . . | . . . . | . . | . . . . | . . . . . . ]
//   [  rs3     fmt    rs2       rs1   funct3     rd        opcode   ]
//   -----------------------------------------------------------------
void EmitR4(GeneratedFunction *f,
    uint32_t rs3, uint32_t fmt, uint32_t rs2, uint32_t rs1, uint32_t funct3, uint32_t rd, uint32_t opcode) {
  assert(isUint((uint32_t)(rs3), 5));
  assert(isUint(fmt, 2));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 7));
  uint32_t encoding = rs3 << 27 | fmt << 25 | rs2 << 20 | rs1 << 15 | funct3 << 12 | rd << 7 | opcode;
  emit32(f, encoding);
}

// S-type instruction:
//
//    31         25 24     20 19     15 14 12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . . . | . . . . | . . . . | . . | . . . . | . . . . . . ]
//   [   imm11:5       rs2       rs1   funct3   imm4:0      opcode   ]
//   -----------------------------------------------------------------
void EmitS(GeneratedFunction *f,
    int32_t imm12, uint32_t rs2, uint32_t rs1, uint32_t funct3, uint32_t opcode) {
  assert(isInt(imm12, 12));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint(opcode, 7));
  uint32_t encoding = ((uint32_t)(imm12) & 0xFE0) << 20 | rs2 << 20 | rs1 << 15 | funct3 << 12 |
                      ((uint32_t)(imm12) & 0x1F) << 7 | opcode;
  emit32(f, encoding);
}

// I-type instruction variant for shifts (SLLI / SRLI / SRAI):
//
//    31       26 25       20 19     15 14 12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . . | . . . . . | . . . . | . . | . . . . | . . . . . . ]
//   [  imm11:6  imm5:0(shamt)   rs1   funct3     rd        opcode   ]
//   -----------------------------------------------------------------
void EmitI6(GeneratedFunction *f,
            uint32_t funct6,
            uint32_t imm6,
            uint32_t rs1,
            uint32_t funct3,
            uint32_t rd,
            uint32_t opcode) {
  assert(isUint(funct6, 6));
  assert(isUint(imm6, 6));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 7));
  uint32_t encoding = funct6 << 26 | imm6 << 20 | rs1 << 15 | funct3 << 12 | rd << 7 | opcode;
  emit32(f, encoding);
}

// B-type instruction:
//
//   31 30       25 24     20 19     15 14 12 11    8 7 6           0
//   -----------------------------------------------------------------
//   [ | . . . . . | . . . . | . . . . | . . | . . . | | . . . . . . ]
//  imm12 imm11:5      rs2       rs1   funct3 imm4:1 imm11  opcode   ]
//   -----------------------------------------------------------------
void EmitB(GeneratedFunction *f, int32_t offset, uint32_t rs2, uint32_t rs1, uint32_t funct3, uint32_t opcode) {
  assert(isAligned(offset, 2));
  assert(isInt(offset, 13));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint(opcode, 7));
  uint32_t imm12 = ((uint32_t)(offset) >> 1) & 0xfffu;
  uint32_t encoding = (imm12 & 0x800u) << (31 - 11) | (imm12 & 0x03f0u) << (25 - 4) |
                      rs2 << 20 | rs1 << 15 |
                      funct3 << 12 |
                      (imm12 & 0xfu) << 8 | (imm12 & 0x400u) >> (10 - 7) | opcode;
  emit32(f, encoding);
}

// U-type instruction:
//
//    31                                   12 11      7 6           0
//   -----------------------------------------------------------------
//   [ . . . . . . . . . . . . . . . . . . . | . . . . | . . . . . . ]
//   [                imm31:12                    rd        opcode   ]
//   -----------------------------------------------------------------
void EmitU(GeneratedFunction *f, uint32_t imm20, uint32_t rd, uint32_t opcode) {
  assert(isUint(imm20, 20));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 7));
  uint32_t encoding = imm20 << 12 | rd << 7 | opcode;
  emit32(f, encoding);
}

// J-type instruction:
//
//   31 30               21   19           12 11      7 6           0
//   -----------------------------------------------------------------
//   [ | . . . . . . . . . | | . . . . . . . | . . . . | . . . . . . ]
//  imm20    imm10:1      imm11   imm19:12        rd        opcode   ]
//   -----------------------------------------------------------------
void EmitJ(GeneratedFunction *f, int32_t offset, uint32_t rd, uint32_t opcode) {
  assert(isAligned(offset, 2));
  assert(isInt(offset, 21));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 7));
  uint32_t imm20 = ((uint32_t)(offset) >> 1) & 0xfffffu;
  uint32_t encoding = (imm20 & 0x80000u) << (31 - 19) | (imm20 & 0x03ffu) << 21 |
                      (imm20 & 0x400u) << (20 - 10) | (imm20 & 0x7f800u) << (12 - 11) |
                      rd << 7 | opcode;
  emit32(f, encoding);
}

static uint32_t encodeShortReg(enum XRegister reg) {
  assert(isShortReg(reg));
  return (uint32_t)(reg) - 8u;
}

// Compressed Instruction Encodings

// CR-type instruction:
//
//   15    12 11      7 6       2 1 0
//   ---------------------------------
//   [ . . . | . . . . | . . . . | . ]
//   [ func4   rd/rs1      rs2    op ]
//   ---------------------------------
//
void EmitCR(GeneratedFunction *f, uint32_t funct4, uint32_t rd_rs1, uint32_t rs2, uint32_t opcode) {
  assert(isUint(funct4, 4));
  assert(isUint((uint32_t)(rd_rs1), 5));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint(opcode, 2));

  uint32_t encoding = funct4 << 12 | rd_rs1 << 7 | rs2 << 2 | opcode;
  emit16(f, encoding);
}

// CI-type instruction:
//
//   15  13   11      7 6       2 1 0
//   ---------------------------------
//   [ . . | | . . . . | . . . . | . ]
//   [func3 imm rd/rs1     imm    op ]
//   ---------------------------------
//
void EmitCI(GeneratedFunction *f, uint32_t funct3, uint32_t rd_rs1, uint32_t imm6, uint32_t opcode) {
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd_rs1), 5));
  assert(isUint(imm6, 6));
  assert(isUint(opcode, 2));

  uint32_t immH1 = bitFieldExtract(imm6, 5, 1);
  uint32_t immL5 = bitFieldExtract(imm6, 0, 5);

  uint32_t encoding = funct3 << 13 | immH1 << 12 | rd_rs1 << 7 | immL5 << 2 | opcode;
  emit16(f, encoding);
}

// CSS-type instruction:
//
//   15  13 12        7 6       2 1 0
//   ---------------------------------
//   [ . . | . . . . . | . . . . | . ]
//   [func3     imm6      rs2     op ]
//   ---------------------------------
//
void EmitCSS(GeneratedFunction *f, uint32_t funct3, uint32_t offset6, uint32_t rs2, uint32_t opcode) {
  assert(isUint(funct3, 3));
  assert(isUint(offset6, 6));
  assert(isUint((uint32_t)(rs2), 5));
  assert(isUint(opcode, 2));

  uint32_t encoding = funct3 << 13 | offset6 << 7 | rs2 << 2 | opcode;
  emit16(f, encoding);
}

// CIW-type instruction:
//
//   15  13 12            5 4   2 1 0
//   ---------------------------------
//   [ . . | . . . . . . . | . . | . ]
//   [func3     imm8         rd'  op ]
//   ---------------------------------
//
void EmitCIW(GeneratedFunction *f, uint32_t funct3, uint32_t imm8, uint32_t rd_s, uint32_t opcode) {
  assert(isUint(funct3, 3));
  assert(isUint(imm8, 8));
  assert(isShortReg(rd_s));
  assert(isUint(opcode, 2));

  uint32_t encoding = funct3 << 13 | imm8 << 5 | encodeShortReg(rd_s) << 2 | opcode;
  emit16(f, encoding);
}

// CL/S-type instruction:
//
//   15  13 12  10 9  7 6 5 4   2 1 0
//   ---------------------------------
//   [ . . | . . | . . | . | . . | . ]
//   [func3  imm   rs1' imm rds2' op ]
//   ---------------------------------
//
void EmitCM(GeneratedFunction *f, uint32_t funct3, uint32_t imm5, uint32_t rs1_s, uint32_t rd_rs2_s, uint32_t opcode) {
  assert(isUint(funct3, 3));
  assert(isUint(imm5, 5));
  assert(isShortReg(rs1_s));
  assert(isShortReg(rd_rs2_s));
  assert(isUint(opcode, 2));

  uint32_t immH3 = bitFieldExtract(imm5, 2, 3);
  uint32_t immL2 = bitFieldExtract(imm5, 0, 2);

  uint32_t encoding = funct3 << 13 | immH3 << 10 | encodeShortReg(rs1_s) << 7 | immL2 << 5 |
                      encodeShortReg(rd_rs2_s) << 2 | opcode;
  emit16(f, encoding);
}

// CA-type instruction:
//
//   15         10 9  7 6 5 4   2 1 0
//   ---------------------------------
//   [ . . . . . | . . | . | . . | . ]
//   [    funct6 rds1' funct2 rs2' op]
//   ---------------------------------
//
void EmitCA(GeneratedFunction *f,
    uint32_t funct6, uint32_t rd_rs1_s, uint32_t funct2, uint32_t rs2_v, uint32_t opcode) {
  assert(isUint(funct6, 6));
  assert(isShortReg(rd_rs1_s));
  assert(isUint(funct2, 2));
  assert(isUint(rs2_v, 3));
  assert(isUint(opcode, 2));

  uint32_t encoding =
      funct6 << 10 | encodeShortReg(rd_rs1_s) << 7 | funct2 << 5  | rs2_v << 2 | opcode;
  emit16(f, encoding);
}

void EmitCAReg(GeneratedFunction *f,
    uint32_t funct6, enum XRegister rd_rs1_s, uint32_t funct2, enum XRegister rs2_s, uint32_t opcode) {
  assert(isShortReg(rs2_s));
  EmitCA(f, funct6, rd_rs1_s, funct2, encodeShortReg(rs2_s), opcode);
}

void EmitCAImm(GeneratedFunction *f,
    uint32_t funct6, enum XRegister rd_rs1_s, uint32_t funct2, uint32_t funct3, uint32_t opcode) {
  EmitCA(f, funct6, rd_rs1_s, funct2, funct3, opcode);
}

// CB-type instruction:
//
//   15  13 12  10 9  7 6       2 1 0
//   ---------------------------------
//   [ . . | . . | . . | . . . . | . ]
//   [func3 offset rs1'   offset  op ]
//   ---------------------------------
//
void EmitCB(GeneratedFunction *f, uint32_t funct3, int32_t offset8, uint32_t rd_rs1_s, uint32_t opcode) {
  assert(isUint(funct3, 3));
  assert(isUint(offset8, 8));
  assert(isShortReg(rd_rs1_s));
  assert(isUint(opcode, 2));

  uint32_t offsetH3 = bitFieldExtract(offset8, 5, 3);
  uint32_t offsetL5 = bitFieldExtract(offset8, 0, 5);

  uint32_t encoding =
      funct3 << 13 | offsetH3 << 10 | encodeShortReg(rd_rs1_s) << 7 | offsetL5 << 2 | opcode;
  emit16(f, encoding);
}

// Wrappers for EmitCB with different imm bit permutation
void EmitCBBranch(GeneratedFunction *f, uint32_t funct3, int32_t offset, uint32_t rs1_s, uint32_t opcode) {
  assert(isInt(offset, 9));
  assert(isAligned(offset, 2));

  uint32_t u_offset = (uint32_t)offset;

  // offset[8|4:3]
  uint32_t offsetH3 = (bitFieldExtract(u_offset, 8, 1) << 2) |
                       bitFieldExtract(u_offset, 3, 2);
  // offset[7:6|2:1|5]
  uint32_t offsetL5 = (bitFieldExtract(u_offset, 6, 2) << 3) |
                      (bitFieldExtract(u_offset, 1, 2) << 1) |
                       bitFieldExtract(u_offset, 5, 1);

  EmitCB(f, funct3, bitFieldInsert(offsetL5, offsetH3, 5, 3), rs1_s, opcode);
}

void EmitCBArithmetic(GeneratedFunction *f,
    uint32_t funct3, uint32_t funct2, uint32_t imm, uint32_t rd_s, uint32_t opcode) {
  uint32_t imm_5 = bitFieldExtract(imm, 5, 1);
  uint32_t immH3 = bitFieldInsert(funct2, imm_5, 2, 1);
  uint32_t immL5 = bitFieldExtract(imm, 0, 5);

  EmitCB(f, funct3, bitFieldInsert(immL5, immH3, 5, 3), rd_s, opcode);
}

// CJ-type instruction:
//
//   15  13 12                  2 1 0
//   ---------------------------------
//   [ . . | . . . . . . . . . . | . ]
//   [func3    jump target 11     op ]
//   ---------------------------------
//
void EmitCJ(GeneratedFunction *f, uint32_t funct3, int32_t offset, uint32_t opcode) {
  assert(isAligned(offset, 2));
  assert(isInt(offset, 12));
  assert(isUint(funct3, 3));
  assert(isUint(opcode, 2));

  uint32_t uoffset = (uint32_t)offset;
  // offset[11|4|9:8|10|6|7|3:1|5]
  uint32_t jumpt = (bitFieldExtract(uoffset, 11, 1) << 10) |
                   (bitFieldExtract(uoffset, 4, 1) << 9)   |
                   (bitFieldExtract(uoffset, 8, 2) << 7)   |
                   (bitFieldExtract(uoffset, 10, 1) << 6)  |
                   (bitFieldExtract(uoffset, 6, 1) << 5)   |
                   (bitFieldExtract(uoffset, 7, 1) << 4)   |
                   (bitFieldExtract(uoffset, 1, 3) << 1)   |
                    bitFieldExtract(uoffset, 5, 1);

  assert(isUint(jumpt, 11));

  uint32_t encoding = funct3 << 13 | jumpt << 2 | opcode;
  emit16(f, encoding);
}



// -------------------------------------------------------------------------
// The mnemonic layer.
//
// Everything above encodes a *format*; everything below names an instruction.
// The split is the same one instructions_x86_64.h makes, and it is what lets
// stage 3 be a switch whose every arm is one call.
//
// The names carry a 'riscv' prefix because both assemblers are linked into one
// binary and share a namespace - x86-64 owns the plain emit* spelling.
// -------------------------------------------------------------------------

#define OP_LOAD     0x03
#define OP_LOAD_FP  0x07
#define OP_IMM      0x13
#define OP_AUIPC    0x17
#define OP_IMM_32   0x1b
#define OP_STORE    0x23
#define OP_STORE_FP 0x27
#define OP_REG      0x33
#define OP_LUI      0x37
#define OP_REG_32   0x3b
#define OP_FP       0x53
#define OP_BRANCH   0x63
#define OP_JALR     0x67
#define OP_JAL      0x6f

void riscvLui(GeneratedFunction *f, enum XRegister rd, uint32_t imm20) {
  EmitU(f, imm20, rd, OP_LUI);
}

void riscvAuipc(GeneratedFunction *f, enum XRegister rd, uint32_t imm20) {
  EmitU(f, imm20, rd, OP_AUIPC);
}

void riscvAddi(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x0, rd, OP_IMM);
}

void riscvAddiw(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x0, rd, OP_IMM_32);
}

void riscvSlti(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x2, rd, OP_IMM);
}

void riscvSltiu(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x3, rd, OP_IMM);
}

void riscvXori(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x4, rd, OP_IMM);
}

void riscvOri(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x6, rd, OP_IMM);
}

void riscvAndi(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t imm12) {
  EmitI(f, imm12, rs1, 0x7, rd, OP_IMM);
}

void riscvSlli(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  EmitI6(f, 0x0, shamt, rs1, 0x1, rd, OP_IMM);
}

void riscvSrli(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  EmitI6(f, 0x0, shamt, rs1, 0x5, rd, OP_IMM);
}

void riscvSrai(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  EmitI6(f, 0x10, shamt, rs1, 0x5, rd, OP_IMM);
}

// The 32-bit shift-immediates take a five-bit amount, not six, so they are
// R-type with the shift where rs2 goes rather than EmitI6.
void riscvSlliw(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  assert(isUint(shamt, 5));
  EmitR(f, 0x00, shamt, rs1, 0x1, rd, OP_IMM_32);
}

void riscvSrliw(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  assert(isUint(shamt, 5));
  EmitR(f, 0x00, shamt, rs1, 0x5, rd, OP_IMM_32);
}

void riscvSraiw(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t shamt) {
  assert(isUint(shamt, 5));
  EmitR(f, 0x20, shamt, rs1, 0x5, rd, OP_IMM_32);
}

// Register-register arithmetic. A named wrapper per instruction over one entry
// point, so a typo shows up as a wrong funct rather than a wrong mnemonic.
#define RISCV_REG_OP(name, opcode, funct7, funct3)                             \
  void riscv##name(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, \
                   enum XRegister rs2) {                                       \
    EmitR(f, funct7, rs2, rs1, funct3, rd, opcode);                            \
  }

RISCV_REG_OP(Add,   OP_REG,    0x00, 0x0)
RISCV_REG_OP(Sub,   OP_REG,    0x20, 0x0)
RISCV_REG_OP(Sll,   OP_REG,    0x00, 0x1)
RISCV_REG_OP(Slt,   OP_REG,    0x00, 0x2)
RISCV_REG_OP(Sltu,  OP_REG,    0x00, 0x3)
RISCV_REG_OP(Xor,   OP_REG,    0x00, 0x4)
RISCV_REG_OP(Srl,   OP_REG,    0x00, 0x5)
RISCV_REG_OP(Sra,   OP_REG,    0x20, 0x5)
RISCV_REG_OP(Or,    OP_REG,    0x00, 0x6)
RISCV_REG_OP(And,   OP_REG,    0x00, 0x7)

// The M extension. Present by definition - the target is RV64G.
RISCV_REG_OP(Mul,   OP_REG,    0x01, 0x0)
RISCV_REG_OP(Mulh,  OP_REG,    0x01, 0x1)
RISCV_REG_OP(Mulhu, OP_REG,    0x01, 0x3)
RISCV_REG_OP(Div,   OP_REG,    0x01, 0x4)
RISCV_REG_OP(Divu,  OP_REG,    0x01, 0x5)
RISCV_REG_OP(Rem,   OP_REG,    0x01, 0x6)
RISCV_REG_OP(Remu,  OP_REG,    0x01, 0x7)

// The 'w' forms: 32-bit operations whose result is sign-extended through the
// whole register, which is what keeps a C 'int' canonically represented.
RISCV_REG_OP(Addw,  OP_REG_32, 0x00, 0x0)
RISCV_REG_OP(Subw,  OP_REG_32, 0x20, 0x0)
RISCV_REG_OP(Sllw,  OP_REG_32, 0x00, 0x1)
RISCV_REG_OP(Srlw,  OP_REG_32, 0x00, 0x5)
RISCV_REG_OP(Sraw,  OP_REG_32, 0x20, 0x5)
RISCV_REG_OP(Mulw,  OP_REG_32, 0x01, 0x0)
RISCV_REG_OP(Divw,  OP_REG_32, 0x01, 0x4)
RISCV_REG_OP(Divuw, OP_REG_32, 0x01, 0x5)
RISCV_REG_OP(Remw,  OP_REG_32, 0x01, 0x6)
RISCV_REG_OP(Remuw, OP_REG_32, 0x01, 0x7)

#undef RISCV_REG_OP

// Loads and stores. 'width' is in bytes and 'isUnsigned' picks between the
// sign- and zero-extending load; there is no unsigned store, a store writing
// exactly its width.
void riscvLoad(GeneratedFunction *f, enum XRegister rd, enum XRegister base, int32_t off,
               uint32_t width, Boolean isUnsigned) {
  uint32_t funct3;

  switch (width) {
    case 1: funct3 = isUnsigned ? 0x4 : 0x0; break;
    case 2: funct3 = isUnsigned ? 0x5 : 0x1; break;
    case 4: funct3 = isUnsigned ? 0x6 : 0x2; break;
    case 8: funct3 = 0x3; break;
    default: unreachable("no load of this width");
  }

  EmitI(f, off, base, funct3, rd, OP_LOAD);
}

void riscvStore(GeneratedFunction *f, enum XRegister src, enum XRegister base, int32_t off,
                uint32_t width) {
  uint32_t funct3;

  switch (width) {
    case 1: funct3 = 0x0; break;
    case 2: funct3 = 0x1; break;
    case 4: funct3 = 0x2; break;
    case 8: funct3 = 0x3; break;
    default: unreachable("no store of this width");
  }

  EmitS(f, off, src, base, funct3, OP_STORE);
}

void riscvFLoad(GeneratedFunction *f, enum FRegister rd, enum XRegister base, int32_t off,
                uint32_t width) {
  assert(width == 4 || width == 8);
  EmitI(f, off, base, width == 4 ? 0x2 : 0x3, rd, OP_LOAD_FP);
}

void riscvFStore(GeneratedFunction *f, enum FRegister src, enum XRegister base, int32_t off,
                 uint32_t width) {
  assert(width == 4 || width == 8);
  EmitS(f, off, src, base, width == 4 ? 0x2 : 0x3, OP_STORE_FP);
}

// -------------------------------------------------------------------------
// The F and D extensions.
//
// One OP-FP opcode covers all of it, with funct7 naming the operation *and*
// its precision - the '.s' and '.d' forms differ by one in funct7, which is
// what 'fmt' below adds. funct3 is the rounding mode where an instruction has
// one: 0x7 asks for whatever the fcsr says, which is what C arithmetic wants,
// and 0x1 is round-toward-zero, which is what a C cast to an integer wants and
// the one place this backend names a mode explicitly.
// -------------------------------------------------------------------------

#define FP_RM_DYNAMIC 0x7
#define FP_RM_RTZ     0x1
#define FP_RM_RNE     0x0

// 0 for single precision, 1 for double. Every funct7 below is the '.s' form.
static uint32_t fpFmt(uint32_t width) {
  assert(width == 4 || width == 8);
  return width == 4 ? 0u : 1u;
}

void riscvFpArith(GeneratedFunction *f, uint32_t funct7, uint32_t width, enum FRegister rd,
                  enum FRegister rs1, enum FRegister rs2) {
  EmitR(f, funct7 + fpFmt(width), rs2, rs1, FP_RM_DYNAMIC, rd, OP_FP);
}

// fsgnj and its variants: rd takes rs1's magnitude and a sign built from rs2's.
// With rs1 == rs2 they are the three moves - fmv, fneg and fabs - which is how
// RV64 spells all three without an instruction of their own.
void riscvFpSignInject(GeneratedFunction *f, uint32_t funct3, uint32_t width, enum FRegister rd,
                       enum FRegister rs1, enum FRegister rs2) {
  EmitR(f, 0x10 + fpFmt(width), rs2, rs1, funct3, rd, OP_FP);
}

// The comparisons, whose answer is an *integer* register.
void riscvFpCompare(GeneratedFunction *f, uint32_t funct3, uint32_t width, enum XRegister rd,
                    enum FRegister rs1, enum FRegister rs2) {
  EmitR(f, 0x50 + fpFmt(width), rs2, rs1, funct3, rd, OP_FP);
}

// float <-> double. The destination's width picks the instruction and the
// source's is encoded where rs2 would go.
void riscvFpConvertFp(GeneratedFunction *f, uint32_t toWidth, uint32_t fromWidth,
                      enum FRegister rd, enum FRegister rs) {
  assert(toWidth != fromWidth);

  // Widening is exact, so there is nothing for a rounding mode to decide and
  // the canonical encoding says so: binutils refuses an fcvt.d.s with the
  // dynamic mode outright, and a dynamic one is an illegal instruction
  // whenever fcsr's frm holds a reserved value. Narrowing really does round.
  uint32_t rm = toWidth > fromWidth ? FP_RM_RNE : FP_RM_DYNAMIC;

  EmitR(f, 0x20 + fpFmt(toWidth), fpFmt(fromWidth), rs, rm, rd, OP_FP);
}

// integer -> float. 'intWidth' is 4 or 8 and 'isUnsigned' picks between the
// w/l and wu/lu forms, which is the rs2 field: 0 w, 1 wu, 2 l, 3 lu.
void riscvFpFromInt(GeneratedFunction *f, uint32_t fpWidth, uint32_t intWidth,
                    Boolean isUnsigned, enum FRegister rd, enum XRegister rs) {
  uint32_t which = (intWidth == 8 ? 2u : 0u) + (isUnsigned ? 1u : 0u);

  // A 32-bit integer is exact in a double, so those two have nothing to round
  // and take the canonical RNE the way fcvt.d.s does; binutils refuses them
  // otherwise. Everything wider, and everything into a float, really rounds.
  uint32_t rm = intWidth == 4 && fpWidth == 8 ? FP_RM_RNE : FP_RM_DYNAMIC;

  EmitR(f, 0x68 + fpFmt(fpWidth), which, rs, rm, rd, OP_FP);
}

// float -> integer, truncating toward zero, which is what a C cast does.
void riscvFpToInt(GeneratedFunction *f, uint32_t intWidth, uint32_t fpWidth,
                  Boolean isUnsigned, enum XRegister rd, enum FRegister rs) {
  uint32_t which = (intWidth == 8 ? 2u : 0u) + (isUnsigned ? 1u : 0u);
  EmitR(f, 0x60 + fpFmt(fpWidth), which, rs, FP_RM_RTZ, rd, OP_FP);
}

// The bits across, unchanged, in either direction. Only the same width: there
// is no fmv between a float register and half an integer one.
void riscvFpMoveToInt(GeneratedFunction *f, uint32_t width, enum XRegister rd, enum FRegister rs) {
  EmitR(f, 0x70 + fpFmt(width), 0, rs, 0x0, rd, OP_FP);
}

void riscvFpMoveFromInt(GeneratedFunction *f, uint32_t width, enum FRegister rd, enum XRegister rs) {
  EmitR(f, 0x78 + fpFmt(width), 0, rs, 0x0, rd, OP_FP);
}

void riscvBranch(GeneratedFunction *f, uint32_t funct3, enum XRegister rs1, enum XRegister rs2,
                 int32_t offset) {
  EmitB(f, offset, rs2, rs1, funct3, OP_BRANCH);
}

void riscvJal(GeneratedFunction *f, enum XRegister rd, int32_t offset) {
  EmitJ(f, offset, rd, OP_JAL);
}

void riscvJalr(GeneratedFunction *f, enum XRegister rd, enum XRegister rs1, int32_t off) {
  EmitI(f, off, rs1, 0x0, rd, OP_JALR);
}

// 'ret' is 'jalr x0, 0(ra)' - jump to the return address and keep no link.
void riscvRet(GeneratedFunction *f) {
  riscvJalr(f, X_ZERO, X_RA, 0);
}

// The signed low twelve bits of a value, as the number an I- or S-type field
// will be read back as. Every split of a constant into a high and a low half
// starts here, because the low half is added sign-extended and the high half
// has to be pre-compensated for it.
static int64_t signExtend12(int64_t v) {
  return (int64_t)((uint64_t)v << 52) >> 52;
}

static uint32_t trailingZeros64(uint64_t v) {
  uint32_t n = 0;
  while ((v & 1) == 0) {
    v >>= 1;
    ++n;
  }
  return n;
}

// 'li' - a constant into a register, in as many instructions as it takes.
//
// Up to 32 bits that is lui plus addiw, with the lui rounded up by 0x800 so
// that the sign-extended addiw lands on the value rather than 4096 below it.
// Beyond that the value is built from the top down: construct everything above
// the low twelve bits, shift it up, and add them back. The shift absorbs the
// run of zeros under the high part, which is what keeps a value like
// 0x1000000000000 to three instructions rather than six.
void riscvLoadImmediate(GeneratedFunction *f, int64_t c, enum XRegister to) {
  if (isInt(c, 12)) {
    riscvAddi(f, to, X_ZERO, (int32_t)c);
    return;
  }

  if (isInt(c, 32)) {
    int64_t lo12 = signExtend12(c);
    uint32_t hi20 = (uint32_t)(((uint64_t)c - (uint64_t)lo12) >> 12) & 0xfffffu;

    riscvLui(f, to, hi20);
    if (lo12 != 0) {
      riscvAddiw(f, to, to, (int32_t)lo12);
    }
    return;
  }

  int64_t lo12 = signExtend12(c);
  int64_t rest = (int64_t)((uint64_t)c - (uint64_t)lo12) >> 12;
  assert(rest != 0 && "a value whose high part is zero fits in twelve bits");

  // Arithmetic, so that a negative high part stays negative: shifting it back
  // up is what reconstructs the value, and the bits discarded here are the
  // zeros the shift will put back.
  uint32_t shift = 12 + trailingZeros64((uint64_t)rest);
  rest >>= (shift - 12);

  riscvLoadImmediate(f, rest, to);
  riscvSlli(f, to, to, shift);

  if (lo12 != 0) {
    riscvAddi(f, to, to, (int32_t)lo12);
  }
}
