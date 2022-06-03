
#include <stdint.h>
#include <stddef.h>
#include <memory.h>
#include <assert.h>

#include "common.h"
#include "mem.h"
#include "instructions.h"
#include "_elf.h"
#include "sema.h"
#include "codegen.h"


#define EX_BYTE (0x0F)
#define REX_BYTE (0x04)

static uint8_t register_encodings[R_COUNT] = { 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b110, 0b111, 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b110, 0b111 };

typedef union {
  uint8_t v;
  struct {
    uint8_t b:1;
    uint8_t x:1;
    uint8_t r:1;
    uint8_t w:1;
    uint8_t fixed:4;
  } bits;
} Rex;

typedef union {
  uint8_t v;
  struct {
    uint8_t rm:3;
    uint8_t regOp:3;
    uint8_t mod:2;
  } bits;
} ModRM;

typedef union {
  uint8_t v;
  struct {
    uint8_t base:3;
    uint8_t index:3;
    uint8_t ss:2;
  } bits;
} SIB;

enum OpcWidth {
  OW_DOUBLE = 4,
  OW_QUAD = 8,

  OW_I = OW_DOUBLE,
  OW_L = OW_QUAD,
  OW_F = OW_DOUBLE,
  OW_D = OW_QUAD,
};

enum OpcWidth op_width[] = {
  #define OPCODE_DEF(_, __, w, ___) OW_##w
  OPCODES
  #undef OPCODE_DEF
};

static const char *opcodeNames[] = {
  #define OPCODE_DEF(_, s, __, ___) s
  OPCODES
  #undef OPCODE_DEF
};


static void emitRex(GeneratedFunction *f, enum Registers rReg, enum Registers bReg, enum Registers xReg, Boolean isWide) {
  Rex rex = { 0 };
  rex.bits.fixed = REX_BYTE;

  int needRex = 0;

  if (rReg >= R_R8) {
      rex.bits.r = 1;
      needRex = 1;
  }

  if (bReg >= R_R8) {
      rex.bits.b = 1;
      needRex = 1;
  }

  if (xReg >= R_R8) {
      rex.bits.x = 1;
      needRex = 1;
  }

  if (isWide) {
      rex.bits.w = 1;
      needRex = 1;
  }

  if (needRex) {
      emitByte(f, rex.v);
  }

}

void emitPushReg(GeneratedFunction *f, enum Registers reg) {

  f->stackOffset += sizeof(intptr_t);

  emitRex(f, R_BAD, reg, R_BAD, FALSE);
  emitByte(f, 0x50 + register_encodings[reg]);
}

void emitPopReg(GeneratedFunction *f, enum Registers reg) {
  f->stackOffset -= sizeof(intptr_t);

  emitRex(f, R_BAD, reg, R_BAD, FALSE);
  emitByte(f, 0x58 + register_encodings[reg]);
}

void emitMoveRR(GeneratedFunction *f, enum Registers from, enum Registers to, size_t size) {

  if (size == 2) emitByte(f, 0x66);

  emitRex(f, from, to, R_BAD, size == 8);

  emitByte(f, size == 1 ? 0x88 : 0x89);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.regOp = register_encodings[from];
  modrm.bits.rm = register_encodings[to];

  emitByte(f, modrm.v);
}


static void encodeAR(GeneratedFunction *f, Address *from, uint8_t regOp) {
  ModRM modrm = { 0 };

  modrm.bits.regOp = regOp & 0x7;

  if (from->base == R_RIP) {

    modrm.bits.rm = 0b101;
    modrm.bits.mod = 0b00;

    emitByte(f, modrm.v);

    if (from->reloc) {

      Relocation *reloc = from->reloc;

      reloc->addend = -sizeof(int32_t);
      reloc->applySectionOffset = f->section->pc - f->section->start;

      // 0xDEADBEFF;
      emitByte(f, 0xFF);
      emitByte(f, 0xBE);
      emitByte(f, 0xAD);
      emitByte(f, 0x7E);
    } else {
      assert(from->label);

      struct Label *l = from->label;

      ptrdiff_t literalOffset = f->section->pc - f->section->start;

      if (l->binded) {
        ptrdiff_t fromOffset = literalOffset + sizeof(int32_t);
        ptrdiff_t toOffset = l->label_cp;

        int32_t delta = toOffset - fromOffset;

        emitByte(f, (uint8_t) (delta >> 0));
        emitByte(f, (uint8_t) (delta >> 8));
        emitByte(f, (uint8_t) (delta >> 16));
        emitByte(f, (uint8_t) (delta >> 24));
      } else {
        struct LabelRef *lr = areanAllocate(f->arena, sizeof (struct LabelRef));

        emitByte(f, 0xFF);
        emitByte(f, 0xBE);
        emitByte(f, 0xAD);
        emitByte(f, 0x7E);

        lr->offset_cp = literalOffset;
        lr->next = l->refs;
        l->refs = lr;
      }
    }

    return;
  }



  if (from->index == R_BAD) {
      // [%reg + disp]
      enum Registers reg = from->base;
      int32_t disp = from->imm;
      if ((int32_t)(int8_t)disp == disp) {
          modrm.bits.mod = 1;
      } else {
          modrm.bits.mod = 2;
      }
      if (reg == R_ESP) {
          modrm.bits.rm = 0b100;
          emitByte(f, modrm.v);
          SIB sib = { 0 };
          sib.bits.index = 0b100;
          sib.bits.base = register_encodings[reg];
          emitByte(f, sib.v);
      } else {
          modrm.bits.rm = register_encodings[reg];
          emitByte(f, modrm.v);
      }

      if (modrm.bits.mod == 1) {
          emitByte(f, disp);
      } else {
          emitDisp32(f, disp);
      }
  } else if (from->index != R_BAD && from->imm == 0) {
      // [%reg + %reg * scale]
      enum Registers index = from->index;
      enum Registers base = from->base;
      assert(index != R_ESP);

      modrm.bits.rm = 0b100;
      emitByte(f, modrm.v);

      SIB sib = { 0 };
      sib.bits.ss = from->scale;
      sib.bits.base = register_encodings[base];
      sib.bits.index = register_encodings[index];
      emitByte(f, sib.v);
  } else {
      // [%reg + %reg * scale + disp]
      enum Registers index = from->index;
      enum Registers base = from->base;
      uint32_t disp = from->imm;
      assert(index != R_ESP);

      if ((uint32_t)(uint8_t)disp == disp) {
          modrm.bits.mod = 1;
      } else {
          modrm.bits.mod = 2;
      }

      modrm.bits.rm = 0b100;
      emitByte(f, modrm.v);

      SIB sib = { 0 };
      sib.bits.ss = from->scale;
      sib.bits.base = register_encodings[from->base];
      sib.bits.index = register_encodings[from->index];
      emitByte(f, sib.v);

      if (modrm.bits.mod == 1) {
          emitByte(f, disp);
      } else {
          emitDisp32(f, disp);
      }
  }
}

void emitLea(GeneratedFunction *f, Address *from, enum Registers to) {
  emitRex(f, to, from->base, from->index, TRUE);

  emitByte(f, 0x8d);

  encodeAR(f, from, register_encodings[to]);
}

void emitMoveAR(GeneratedFunction *f, Address* addr, enum Registers to, size_t size) {

  if (size == 2) emitByte(f, 0x66);

  emitRex(f, to, addr->base, addr->index, size > 4);

  uint8_t code = size == 1 ? 0x8A : 0x8B;
  emitByte(f, code);

  encodeAR(f, addr, register_encodings[to]);
}

void emitMoveRA(GeneratedFunction *f, enum Registers from, Address* addr, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, from, addr->base, addr->index, size > 4);

  uint8_t code = size == 1 ? 0x88 : 0x89;
  emitByte(f, code);

  encodeAR(f, addr, register_encodings[from]);
}

void emitMoveCR_Reloc(GeneratedFunction *f, Relocation *reloc, enum Registers reg) {

}

void emitMoveCR(GeneratedFunction *f, intptr_t c, enum Registers to, size_t size) {

  if (size == 2)  emitByte(f, 0x66);

  emitRex(f, R_BAD, to, R_BAD, size > 4);

  if (size == 8 && (uint64_t)(uint32_t)c == (uint64_t)c) {
      emitByte(f, 0xC7);
      ModRM modrm = { 0 };
      modrm.bits.mod = 0b11;
      modrm.bits.regOp = 0;
      modrm.bits.rm = register_encodings[to];
      emitByte(f, modrm.v);
      emitByte(f, (uint8_t)c);
      emitByte(f, (uint8_t)(c >> 8));
      emitByte(f, (uint8_t)(c >> 16));
      emitByte(f, (uint8_t)(c >> 24));
  } else {
      uint8_t code = size == 1 ? 0xB0 : 0xB8;
      emitByte(f, code + register_encodings[to]);
      emitByte(f, (uint8_t)c);

      if (size > 1) {
          emitByte(f, (uint8_t)(c >> 8));
      }

      if (size > 2) {
          emitByte(f, (uint8_t)(c >> 16));
          emitByte(f, (uint8_t)(c >> 24));
      }

      if (size > 4) {
          emitByte(f, (uint8_t)(c >> 32));
          emitByte(f, (uint8_t)(c >> 40));
          emitByte(f, (uint8_t)(c >> 48));
          emitByte(f, (uint8_t)(c >> 56));
      }
  }
}



void emitCmovRR(GeneratedFunction *f, enum JumpCondition cc, enum Registers from, enum Registers to, Boolean isW) {

  emitRex(f, to, from, R_BAD, isW);
  emitByte(f, 0x0F);
  emitByte(f, 0x40 + cc);

  ModRM rm = { 0 };

  rm.bits.mod = 0b11;
  rm.bits.rm = register_encodings[from];
  rm.bits.regOp = register_encodings[to];

  emitByte(f, rm.v);

}

void emitSetccR(GeneratedFunction *f, enum JumpCondition cc, enum Registers reg) {
  emitByte(f, 0x0F);
  emitByte(f, 0x90 + cc);

  ModRM rm = { 0 };
  rm.bits.mod = 0b11;
  rm.bits.regOp = 0;
  rm.bits.rm = register_encodings[reg];

  emitByte(f, rm.v);
}

static void emitSimpleArithRC(GeneratedFunction *f, uint8_t  opcode, uint8_t opcode8, uint8_t digit, enum Registers r, int64_t c, size_t size) {

  if (size == 2) emitByte(f, 0x66);

  emitRex(f, r, R_BAD, R_BAD, size == 8);

  emitByte(f, size == 1 ? opcode - 1 : opcode);

  ModRM rm = { 0 };

  rm.bits.regOp = digit & 0x7;
  rm.bits.mod = 0b11;
  rm.bits.rm = register_encodings[r];

  emitByte(f, rm.v);
  emitByte(f, c);

  // TODO: it could be optimized using imm8
  if (size > 1) {
      emitByte(f, (uint8_t)(c >> 8));
  }

  if (size > 2) {
      emitByte(f, (uint8_t)(c >> 16));
      emitByte(f, (uint8_t)(c >> 24));
  }
}

void emitSimpleArithRR(GeneratedFunction *f, uint8_t code, enum Registers l, enum Registers r, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, r, l, R_BAD, size == 8);

  emitByte(f, size == 1 ? code - 1 : code);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.rm = register_encodings[l];
  modrm.bits.regOp = register_encodings[r];

  emitByte(f, modrm.v);
}

void emitSimpleArithR(GeneratedFunction *f, uint8_t code, uint8_t digit, enum Registers r, size_t size) {
  if (size == 2) emitByte(f, 0x66);
  emitRex(f, R_BAD, r, R_BAD, size == 8);

  emitByte(f, size == 1 ? code - 1 : code);

  ModRM modrm = { 0 };
  modrm.bits.mod = 0b11;
  modrm.bits.rm = register_encodings[r];
  modrm.bits.regOp = digit & 0x7;

  emitByte(f, modrm.v);
}

void emitSimpleArithA(GeneratedFunction *f, uint8_t code, uint8_t digit, Address *addr, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, R_BAD, addr->base, addr->index, size == 8);

  emitByte(f, size == 1 ? code - 1 : code);

  encodeAR(f, addr, digit);
}

void emitSMulR(GeneratedFunction *f, enum Registers l, enum Registers r, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, R_BAD, r, R_BAD, size == 8);

  emitByte(f, 0x0F);
  emitByte(f, 0xAF);

  ModRM rm = { 0 };

  rm.bits.mod = 0b11;
  rm.bits.regOp = register_encodings[l];
  rm.bits.rm = register_encodings[r];

  emitByte(f, rm.v);
}

void emitSMulAR(GeneratedFunction *f, Address *addr, enum Registers r, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, R_BAD, r, R_BAD, size == 8);

  emitByte(f, 0x0F);
  emitByte(f, 0xAF);

  encodeAR(f, addr, register_encodings[r]);
}

void emitShiftRC(GeneratedFunction *f, uint8_t code, uint8_t digit, enum Registers r, int64_t c, size_t size) {
  if (size == 2) emitByte(f, 0x66);

  emitRex(f, r, R_BAD, R_BAD, size == 8);

  emitByte(f, size == 1 ? code - 1 : code);

  ModRM rm = { 0 };

  rm.bits.mod = 0b11;
  rm.bits.regOp = digit;
  rm.bits.rm = register_encodings[r];

  emitByte(f, rm.v);

  emitByte(f, c);

}

void emitShiftRR(GeneratedFunction *f, uint8_t code, uint8_t digit, enum Registers l, enum Registers r, size_t size) {

  if (r != R_ECX) {
      emitPushReg(f, R_ECX);
      emitMoveRR(f, r, R_ECX, size);
  }

  if (size == 2) emitByte(f, 0x66);

  emitRex(f, R_BAD, l, R_BAD, size == 8);

  emitByte(f, size == 1 ? code : code - 1);

  ModRM rm = { 0 };

  rm.bits.regOp = digit & 7;
  rm.bits.mod = 0b11;
  rm.bits.rm = register_encodings[l];

  emitByte(f, rm.v);

  if (r != R_ECX) {
      emitPopReg(f, R_ECX);
  }
}

void emitSimpleFPArithRR(GeneratedFunction *f, int16_t prefix, uint8_t opcode1, uint8_t opcode2, enum Registers l, enum Registers r) {

  if (prefix >= 0) emitByte(f, (uint8_t)prefix);

  emitRex(f, l, r, R_BAD, FALSE);

  emitByte(f, opcode1);
  emitByte(f, opcode2);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.regOp = register_encodings[l];
  modrm.bits.rm = register_encodings[r];

  emitByte(f, modrm.v);
}

void emitSimpleFPArightRA(GeneratedFunction *f, uint8_t prefix, uint8_t opcode1, uint8_t opcode2, enum Registers r, Address *addr) {
  emitByte(f, prefix);

  emitRex(f, r, addr->base, addr->index, FALSE);

  emitByte(f, opcode1);
  emitByte(f, opcode2);

  encodeAR(f, addr, register_encodings[r]);
}

static void emitSimpleArithAR(GeneratedFunction *f, uint8_t code, int16_t code2, enum Registers r, Address *addr, size_t size) {

  if (size == 2) emitByte(f, 0x66);

  emitRex(f, r, addr->base, addr->scale, size == 8);

  emitByte(f, size == 1 ? code - 1 : code);

  if (code2 >= 0) emitByte(f, (uint8_t)code2);

  encodeAR(f, addr, register_encodings[r]);
}

static void emitSimpleFPArithAR(GeneratedFunction *f, int16_t prefix, uint8_t opcode1, uint8_t opcode2, enum Registers r, Address *addr, size_t size) {
  if (prefix >= 0) emitByte(f, (uint8_t)prefix);

  emitRex(f, r, addr->base, addr->index, size == 8);

  emitByte(f, opcode1);
  emitByte(f, opcode2);

  encodeAR(f, addr, register_encodings[r]);
}

void emitArithAR(GeneratedFunction *f, enum Opcodes opcode, enum Registers r, Address *addr, size_t size) {
  switch (opcode) {
    case OP_ADD: return emitSimpleArithAR(f, 0x03, -1, r, addr, size);
    case OP_SUB: return emitSimpleArithAR(f, 0x2B, -1, r, addr, size);
    case OP_AND: return emitSimpleArithAR(f, 0x23, -1, r, addr, size);
    case OP_OR:  return emitSimpleArithAR(f, 0x0B, -1, r, addr, size);
    case OP_XOR: return emitSimpleArithAR(f, 0x33, -1, r, addr, size);
    case OP_CMP:  return emitSimpleArithAR(f, 0x3B, -1, r, addr, size);
    case OP_SMUL: return emitSimpleArithAR(f, 0x0F, 0xAF, r, addr, size);
    case OP_UMUL: assert(r == R_EAX); return emitSimpleArithA(f, 0xF7, 4, addr, size);
    case OP_SDIV: return emitSimpleArithA(f, 0xF7, 7, addr, size);
    case OP_UDIV: return emitSimpleArithA(f, 0xF7, 6, addr, size);

    case OP_FADD: return emitSimpleFPArithAR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x58, r, addr, size);
    case OP_FSUB: return emitSimpleFPArithAR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x5C, r, addr, size);
    case OP_FMUL: return emitSimpleFPArithAR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x59, r, addr, size);
    case OP_FDIV: return emitSimpleFPArithAR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x5E, r, addr, size);
    case OP_FOCMP: return emitSimpleFPArithAR(f, size == 8 ? 0x66 : -1, 0x0F, 0x2F, r, addr, size); // comiss/comisd
    case OP_FUCMP: return emitSimpleFPArithAR(f, size == 8 ? 0x66 : -1, 0x0F, 0x2E, r, addr, size); // ucomiss/ucomisd

    case OP_SHR:
    case OP_SAR:
    case OP_SHL:
      emitMoveAR(f, addr, R_R8, size);
      emitArithRR(f, opcode, r, R_R8, size);
      break;
    default:
      // TODO:
      unreachable("unsupported operation");
  }
}

void emitArithRR(GeneratedFunction *f, enum Opcodes opcode, enum Registers l, enum Registers r, size_t size) {
  switch (opcode) {
  case OP_ADD: return emitSimpleArithRR(f, 0x01, l, r, size);
  case OP_SUB: return emitSimpleArithRR(f, 0x29, l, r, size);
  case OP_AND: return emitSimpleArithRR(f, 0x21, l, r, size);
  case OP_OR:  return emitSimpleArithRR(f, 0x09, l, r, size);
  case OP_XOR: return emitSimpleArithRR(f, 0x31, l, r, size);
  case OP_CMP: return emitSimpleArithRR(f, 0x39, l, r, size);

  case OP_SMUL: return emitSMulR(f, l, r, size);
  case OP_UMUL: assert(l == R_EAX); return emitSimpleArithR(f, 0xF7, 4, r, size);
  case OP_SDIV: return emitSimpleArithR(f, 0xF7, 7, r, size);
  case OP_UDIV: return emitSimpleArithR(f, 0xF7, 6, r, size);
  case OP_SHR: return emitShiftRR(f, 0xD3, 5, l, r, size);
  case OP_SAR: return emitShiftRR(f, 0xD3, 7, l, r, size);
  case OP_SHL: return emitShiftRR(f, 0xD3, 4, l, r, size);

  // FP
  case OP_FADD: return emitSimpleFPArithRR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x58, l, r);
  case OP_FSUB: return emitSimpleFPArithRR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x5C, l, r);
  case OP_FMUL: return emitSimpleFPArithRR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x59, l, r);
  case OP_FDIV: return emitSimpleFPArithRR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x5E, l, r);
  case OP_FMOD: unreachable("TODO");
  case OP_FOCMP: return emitSimpleFPArithRR(f, size == 8 ? 0x66 : -1, 0x0F, 0x2F, l, r); // comiss/comisd
  case OP_FUCMP: return emitSimpleFPArithRR(f, size == 8 ? 0x66 : -1, 0x0F, 0x2E, l, r); // ucomiss/ucomisd

  default: unreachable("unreachable");
    }
}


void emitArithConst(GeneratedFunction *f, enum Opcodes opcode, enum Registers r, int64_t c, size_t size) {
  if ((uint64_t)(uint32_t)c != (uint32_t)c) {
    emitMoveCR(f, c, R_R8, size);
    emitArithRR(f, opcode, r, R_R8, size);
  } else {
      switch (opcode) {
      case OP_ADD: return emitSimpleArithRC(f, 0x81, 0x83, 0, r, c, size);
      case OP_SUB: return emitSimpleArithRC(f, 0x81, 0x83, 5, r, c, size);
      case OP_AND: return emitSimpleArithRC(f, 0x81, 0x83, 4, r, c, size);
      case OP_OR:  return emitSimpleArithRC(f, 0x81, 0x83, 1, r, c, size);
      case OP_XOR: return emitSimpleArithRC(f, 0x81, 0x83, 6, r, c, size);
      case OP_SHR: return emitShiftRC(f, 0xC1, 5, r, c, size);
      case OP_SAR: return emitShiftRC(f, 0xC1, 7, r, c, size);
      case OP_SHL: return emitShiftRC(f, 0xC1, 4, r, c, size);
      case OP_CMP: return emitSimpleArithRC(f, 0x81, 0x83, 7, r, c, size);
      case OP_SMUL:
      case OP_UMUL:
      case OP_SDIV:
      case OP_UDIV:
          emitMoveCR(f, c, R_R8, size);
          emitArithRR(f, opcode, r, R_R8, size);
          break;
      default: unreachable("unreachable");
        }
  }
}

void emitNot(GeneratedFunction *f, enum Registers reg, size_t size) {
  emitTestRR(f, reg, reg, size);
  emitSetccR(f, JC_EQ, R_ACC);
  emitMovxxRR(f, 0xB6, R_ACC, R_ACC);
}

void emitNegR(GeneratedFunction *f, enum Registers reg, size_t size) {
  emitSimpleArithR(f, 0xF7, 3, reg, size);
}


void emitNegA(GeneratedFunction *f, Address *addr, size_t size) {
  emitSimpleArithA(f, 0xF7, 3, addr, size);
}

void emitZeroReg(GeneratedFunction *f, enum Registers reg) {

  emitRex(f, reg, R_BAD, R_BAD, FALSE);

  emitByte(f, 0x31);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.regOp = register_encodings[reg];
  emitByte(f, modrm.v);
}

static void emitShift(GeneratedFunction *f, uint8_t op1, uint8_t opImm, uint8_t digit, enum Registers r, int s) {
  assert((int8_t)s == s);

  emitRex(f, r, R_BAD, R_BAD, FALSE);

  emitByte(f, s == 1 ? op1 : opImm);

  ModRM modrm = { 0 };
  modrm.bits.regOp = digit;
  modrm.bits.rm = register_encodings[r];
  modrm.bits.mod = 0b11;
  emitByte(f, modrm.v);

  if (s != 1) {
      emitByte(f, s);
  }

}

void emitTestRR(GeneratedFunction *f, enum Registers l, enum Registers r, size_t s) {

  if (s == 2) emitByte(f, 0x66);

  emitRex(f, r, l, R_BAD, s > sizeof(int32_t));

  emitByte(f, s == 1 ? 0x84 : 0x85);

  ModRM rm = { 0 };

  rm.bits.mod = 0b11;
  rm.bits.rm = register_encodings[l];
  rm.bits.regOp = register_encodings[r];

  emitByte(f, rm.v);
}

void emitSar(GeneratedFunction *f, enum Registers r, int s) {
  emitShift(f, 0xD1, 0xC1, 7, r, s);
}

void emitShr(GeneratedFunction *f, enum Registers r, int s) {
  emitShift(f, 0xD1, 0xC1, 5, r, s);
}

void emitShl(GeneratedFunction *f, enum Registers r, int s) {
  emitShift(f, 0xD1, 0xC1, 4, r, s);
}

void emitBitwiseNotR(GeneratedFunction *f, enum Registers reg, size_t size) {
  emitSimpleArithR(f, 0xF7, 2, reg, size);
}

void emitBitwiseNotA(GeneratedFunction *f, Address *addr, size_t size) {
  emitSimpleArithA(f, 0xF7, 2, addr, size);
}

void emitMovfpRR(GeneratedFunction *f, enum Registers from, enum Registers to, size_t size) {
  emitSimpleFPArithRR(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x10, to, from);
}

void emitMovfpRA(GeneratedFunction *f, enum Registers from, Address *to, size_t size) {
  emitSimpleFPArightRA(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x11, from, to);
}

void emitMovfpAR(GeneratedFunction *f, Address *from, enum Registers to, size_t size) {
  emitSimpleFPArightRA(f, size == 8 ? 0xF2 : 0xF3, 0x0F, 0x10, to, from);
}

void emitMovxxRR(GeneratedFunction *f, uint8_t opcode, enum Registers from, enum Registers to) {
  emitRex(f, from, to, R_BAD, FALSE);

  emitByte(f, 0x0F);

  emitByte(f, opcode);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.rm = register_encodings[from];
  modrm.bits.regOp = register_encodings[to];

  emitByte(f, modrm.v);
}

void emitMovxxAR(GeneratedFunction *f, uint8_t opcode, Address *from, enum Registers to) {
  emitRex(f, to, from->base, from->index, FALSE);

  emitByte(f, 0x0F);

  emitByte(f, opcode);

  encodeAR(f, from, register_encodings[to]);
}

void emitMovdq(GeneratedFunction *f, uint8_t prefix, uint8_t opcode, uint8_t opcode2, enum Registers r1, enum Registers r2, Boolean isW) {
  emitByte(f, prefix);

  emitRex(f, r1, r2, R_BAD, isW);

  emitByte(f, opcode);
  emitByte(f, opcode2);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.rm = register_encodings[r1];
  modrm.bits.regOp = register_encodings[r2];

  emitByte(f, modrm.v);
}

void emitConvertWDQ(GeneratedFunction *f, uint8_t opcode, uint8_t opSize) {
  switch (opSize) {
    case 2: emitByte(f, 0x66);
    case 4: emitByte(f, opcode); //, 0x98);
      break;
    case 8:
      emitRex(f, R_BAD, R_BAD, R_BAD, TRUE);
      emitByte(f, opcode);//, 0x98);
      break;
    default:
      unreachable("Unexptected operand size");
    }
}

void emitConvertFP(GeneratedFunction *f, uint8_t prefix, uint8_t opcode, enum Registers from, enum Registers to, Boolean isW) {

  emitByte(f, prefix);

  emitRex(f, from, to, R_BAD, isW);

  emitByte(f, 0x0F);
  emitByte(f, opcode);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.rm = register_encodings[from];
  modrm.bits.regOp = register_encodings[to];

  emitByte(f, modrm.v);

}

static void emitByte_pc(address p, uint8_t b) {
   *(uint8_t*)p = b;
}

static void emitWord_pc(address p, uint16_t w) {
   emitByte_pc(p, (uint8_t)w);

   if ((uint16_t)(uint8_t)w != w) {
       emitByte_pc(p + sizeof (uint8_t), (uint8_t)(w >> 8));
   }
}


static void emitDouble_pc(address p, uint32_t w) {
    emitWord_pc(p, (uint16_t) w);

    if ((uint32_t)(uint16_t)w != w) {
        emitWord_pc(p + sizeof (uint16_t), (uint16_t)(w >> 16));
    }
}

static void emitDisp32_pc(address p, uint32_t w) {
  if ((uint32_t)(uint16_t)w != w) {
      emitDouble_pc(p, w);
  } else {
      if ((uint32_t)(uint8_t)w != w) {
          emitWord_pc(p, w);
      } else {
          emitByte_pc(p, (uint8_t)w);
          emitByte_pc(p + sizeof (uint8_t), 0);
      }
      emitByte_pc(p + sizeof (uint16_t), 0);
      emitByte_pc(p + sizeof (uint16_t) + sizeof (uint8_t), 0);
  }
}

void emitCondJump(GeneratedFunction *f, struct Label *l, enum JumpCondition cond) {
  // jcc l
  Section *s = f->section;
  address pc = s->pc;
  ptrdiff_t instrOff = pc - s->start;
  if (l->binded) {
      address l_pc = s->start + l->label_cp;
      ptrdiff_t d = l_pc - pc - 2;
      if ((ptrdiff_t)(int8_t)d == d) {
          emitByte(f, 0x70 | cond);
          emitByte(f, d);
      } else {
          emitByte(f, 0x0F);
          emitByte(f, 0x80 | cond);
          emitDisp32(f, d - 4);
      }
  } else {
    emitByte(f, 0x0F);
    emitByte(f, 0x80 | cond);
    emitDisp32(f, 0xDEADBEEF);

    struct LabelJump *lj = areanAllocate(f->arena, sizeof (struct LabelJump));
    lj->instSize = 6;
    lj->instruction_cp = instrOff;
    lj->next = l->jumps;
    l->jumps = lj;
  }
}


void emitJumpTo(GeneratedFunction *f, struct Label *l) {
  // jmp l
  Section *s = f->section;
  address pc = s->pc;
  address start = s->start;
  if (l->binded) {
      address l_pc = s->start + l->label_cp;
      ptrdiff_t d = l_pc - pc - 2;
      if ((ptrdiff_t)(int8_t)d == d) {
          emitByte(f, 0xEB);
          emitByte(f, d);
      } else {
          emitByte(f, 0xE9);
          emitDisp32(f, d - 3);
      }
  } else {
    emitByte(f, 0xE9);
    emitDisp32(f, 0xDEADBEEF);

    struct LabelJump *lj = areanAllocate(f->arena, sizeof (struct LabelJump));
    lj->instSize = 5;
    lj->instruction_cp = pc - start;
    lj->next = l->jumps;
    l->jumps = lj;
  }
}

void emitJumpByReg(GeneratedFunction *f, enum Registers reg) {
  // jmp %reg

  emitRex(f, R_BAD, reg, R_BAD, FALSE);

  emitByte(f, 0xFF);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.regOp = 4;
  modrm.bits.rm = register_encodings[reg];

  emitByte(f, modrm.v);
}

void patchRefTo(GeneratedFunction *f, ptrdiff_t literal_cp, ptrdiff_t label_cp) {
  ptrdiff_t fromOffset = literal_cp + sizeof(int32_t);
  ptrdiff_t toOffset = label_cp;

  int32_t delta = toOffset - fromOffset;
  address literalAddress = f->section->start + literal_cp;

  emitByte_pc(literalAddress++, (uint8_t) (delta >> 0));
  emitByte_pc(literalAddress++, (uint8_t) (delta >> 8));
  emitByte_pc(literalAddress++, (uint8_t) (delta >> 16));
  emitByte_pc(literalAddress++, (uint8_t) (delta >> 24));
}

void patchJumpTo(GeneratedFunction *f, ptrdiff_t inst_cp, size_t instSize, ptrdiff_t label_cp) {
  Section *s = f->section;
  ptrdiff_t d = label_cp - inst_cp - 2;
  address instpc = s->start + inst_cp;
  void * instpc_d = instpc;
  uint8_t opc = *instpc;

  if ((ptrdiff_t)(int8_t)d == d) {
      if (opc == 0xE9) {
        // jmp
        assert(instSize == 5);
        emitByte_pc(instpc++, 0xEB);
        emitByte_pc(instpc++, d);
      } else {
        // jcc
        assert(*instpc == 0x0F);
        assert(instSize == 6);
        uint8_t opc2 = *(instpc + 1);
        uint8_t cc = opc2 & 0x0F;
        emitByte_pc(instpc++, 0x70 | cc);
        emitByte_pc(instpc++, d);
        emitByte_pc(instpc++, 0x90);
      }

      emitByte_pc(instpc++, 0x90);
      emitByte_pc(instpc++, 0x90);
      emitByte_pc(instpc++, 0x90);
  } else {
     if (opc == 0xE9) {
        emitDisp32_pc(instpc + 1, d - 3);
     } else {
        assert(opc == 0x0F);
        emitDisp32_pc(instpc + 2, d - 4);
     }
  }
}

void emitCall(GeneratedFunction *f, enum Registers reg) {
  // call %reg
  emitRex(f, R_BAD, reg, R_BAD, FALSE);

  emitByte(f, 0xFF);

  ModRM modrm = { 0 };

  modrm.bits.mod = 0b11;
  modrm.bits.regOp = 2;
  modrm.bits.rm = register_encodings[reg];

  emitByte(f, modrm.v);
}

void emitCallLiteral(GeneratedFunction *f, Relocation *reloc) {
  Symbol *s = reloc->symbolData.symbol;

  emitByte(f, 0xE8);

  reloc->applySectionOffset = f->section->pc - f->section->start;
  reloc->addend = -sizeof(int32_t);

  emitDisp32(f, 0x7EADBEEF);

}

void emitRet(struct _GeneratedFunction *f, uint16_t s) {
  if (s == 0) {
    emitByte(f, 0xC3);
  } else {
    emitByte(f, 0xC2);
    emitWord(f, s);
  }
}

void emitLeave(GeneratedFunction *f) {
  emitByte(f, 0xC9);
}
