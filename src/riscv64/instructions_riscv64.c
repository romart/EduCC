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

int isInt(int64_t v, size_t bits) {
    return (bits == sizeof(v) * 8u) || ((-(1u << (bits - 1)) <= v) && (v < (1u << (bits - 1))));
}

int isUint(uint64_t v, size_t bits) {
    return (bits == sizeof(v) * 8u) || (v < ((1u << (bits - 1)) * 2u - 1u));
}

int isAligned(uint64_t v, size_t align) {
    uint64_t mask = (1U << align) - 1;
    return (v & ~mask) == v;
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
void emitI(GeneratedFunction *f, int32_t imm12, uint32_t rs1, uint32_t funct3, uint32_t rd, uint32_t opcode) {
  assert(isInt(imm12, 12));
  assert(isUint((uint32_t)(rs1), 5));
  assert(isUint(funct3, 3));
  assert(isUint((uint32_t)(rd), 5));
  assert(isUint(opcode, 5));
  uint32_t encoding = (uint32_t)(imm12) << 20 | rs1 << 15 | funct3 << 12 | rd << 7 | opcode;
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
