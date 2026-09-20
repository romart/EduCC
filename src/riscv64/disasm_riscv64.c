// RV64G, decoded. What '-S' prints on this target.
//
// x86-64 hands the job to Zydis, which is a library because x86 instructions
// are between one and fifteen bytes and their meaning depends on prefixes that
// came earlier. RV64G is fixed-width and orthogonal: every instruction is four
// bytes, the opcode is always the low seven bits, and the register fields are
// always in the same place. So this is a switch rather than a dependency, and
// it is the whole of what Zydis does here.
//
// The spelling is GNU's, because the point of -S is to be read beside
// 'riscv64-linux-gnu-objdump -d' - ABI register names, and the handful of
// pseudo-instructions objdump always prints ('mv', 'ret', 'nop'), which are
// what most of a prologue turns into.
//
// Not necessarily all code: a caller that hands over a function body runs into
// its jump tables. A word that decodes to nothing is printed as '(bad)' and
// stepped over, the way the x86 side does it, so what follows stays readable.

#include <stdio.h>

#include "instructions_riscv64.h"

static const char *const xRegNames[32] = {
  "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

static const char *const fRegNames[32] = {
  "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
  "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5",
  "fa6", "fa7", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"
};

#define X(r) xRegNames[(r) & 31]
#define F(r) fRegNames[(r) & 31]

static uint32_t opcodeOf(uint32_t w) { return w & 0x7f; }
static uint32_t rdOf(uint32_t w) { return (w >> 7) & 0x1f; }
static uint32_t funct3Of(uint32_t w) { return (w >> 12) & 0x7; }
static uint32_t rs1Of(uint32_t w) { return (w >> 15) & 0x1f; }
static uint32_t rs2Of(uint32_t w) { return (w >> 20) & 0x1f; }
static uint32_t funct7Of(uint32_t w) { return (w >> 25) & 0x7f; }

static int32_t signExtend(uint32_t v, unsigned bits) {
  uint32_t sign = 1u << (bits - 1);
  return (int32_t)((v ^ sign) - sign);
}

static int32_t immI(uint32_t w) { return signExtend(w >> 20, 12); }

static int32_t immS(uint32_t w) {
  return signExtend(((w >> 25) << 5) | ((w >> 7) & 0x1f), 12);
}

static int32_t immB(uint32_t w) {
  uint32_t v = (((w >> 31) & 1) << 12) | (((w >> 7) & 1) << 11) |
               (((w >> 25) & 0x3f) << 5) | (((w >> 8) & 0xf) << 1);
  return signExtend(v, 13);
}

static int32_t immJ(uint32_t w) {
  uint32_t v = (((w >> 31) & 1) << 20) | (((w >> 12) & 0xff) << 12) |
               (((w >> 20) & 1) << 11) | (((w >> 21) & 0x3ff) << 1);
  return signExtend(v, 21);
}

// 's' or 'd', from the two format bits every OP-FP instruction carries.
static char fpFmt(uint32_t w) { return ((w >> 25) & 1) != 0 ? 'd' : 's'; }

// The integer width an fcvt names in rs2: w, wu, l, lu.
static const char *const fcvtIntTypes[4] = { "w", "wu", "l", "lu" };

static void formatOpImm(char *out, size_t n, uint32_t w, Boolean isWord) {
  const char *suffix = isWord ? "w" : "";
  uint32_t shamt = isWord ? (rs2Of(w)) : ((w >> 20) & 0x3f);

  switch (funct3Of(w)) {
  case 0x0:
    // 'addi rd,zero,imm' is 'li' and 'addi rd,rs,0' is 'mv', both of which
    // objdump prints and both of which most of a prologue is made of.
    if (!isWord && rs1Of(w) == X_ZERO && rdOf(w) == X_ZERO && immI(w) == 0) {
      snprintf(out, n, "nop");
    } else if (!isWord && rs1Of(w) == X_ZERO) {
      snprintf(out, n, "li %s,%d", X(rdOf(w)), immI(w));
    } else if (!isWord && immI(w) == 0) {
      snprintf(out, n, "mv %s,%s", X(rdOf(w)), X(rs1Of(w)));
    } else if (isWord && immI(w) == 0) {
      snprintf(out, n, "sext.w %s,%s", X(rdOf(w)), X(rs1Of(w)));
    } else {
      snprintf(out, n, "addi%s %s,%s,%d", suffix, X(rdOf(w)), X(rs1Of(w)), immI(w));
    }
    return;
  case 0x1: snprintf(out, n, "slli%s %s,%s,0x%x", suffix, X(rdOf(w)), X(rs1Of(w)), shamt); return;
  case 0x2: snprintf(out, n, "slti %s,%s,%d", X(rdOf(w)), X(rs1Of(w)), immI(w)); return;
  case 0x3:
    // 'sltiu rd,rs,1' is 'rs == 0', which is how every test against zero that
    // wants an answer rather than a branch comes out.
    if (immI(w) == 1) {
      snprintf(out, n, "seqz %s,%s", X(rdOf(w)), X(rs1Of(w)));
    } else {
      snprintf(out, n, "sltiu %s,%s,%d", X(rdOf(w)), X(rs1Of(w)), immI(w));
    }
    return;
  case 0x4:
    if (immI(w) == -1) {
      snprintf(out, n, "not %s,%s", X(rdOf(w)), X(rs1Of(w)));
    } else {
      snprintf(out, n, "xori %s,%s,%d", X(rdOf(w)), X(rs1Of(w)), immI(w));
    }
    return;
  case 0x5:
    snprintf(out, n, "%s%s %s,%s,0x%x", ((w >> 30) & 1) != 0 ? "srai" : "srli", suffix,
             X(rdOf(w)), X(rs1Of(w)), shamt);
    return;
  case 0x6: snprintf(out, n, "ori %s,%s,%d", X(rdOf(w)), X(rs1Of(w)), immI(w)); return;
  case 0x7:
    // 'andi rd,rs,255' is the zero-extension of a byte, which is what a
    // narrow load out of a register comes to.
    if (immI(w) == 255) {
      snprintf(out, n, "zext.b %s,%s", X(rdOf(w)), X(rs1Of(w)));
    } else {
      snprintf(out, n, "andi %s,%s,%d", X(rdOf(w)), X(rs1Of(w)), immI(w));
    }
    return;
  default: snprintf(out, n, "(bad)"); return;
  }
}

static void formatOp(char *out, size_t n, uint32_t w, Boolean isWord) {
  static const char *const base[8] = { "add", "sll", "slt", "sltu", "xor", "srl", "or", "and" };
  static const char *const muldiv[8] = { "mul", "mulh", "mulhsu", "mulhu",
                                         "div", "divu", "rem", "remu" };
  const char *suffix = isWord ? "w" : "";
  uint32_t f3 = funct3Of(w);
  const char *name = NULL;

  switch (funct7Of(w)) {
  case 0x00: name = base[f3]; break;
  case 0x01: name = muldiv[f3]; break;
  case 0x20: name = f3 == 0 ? "sub" : f3 == 5 ? "sra" : NULL; break;
  default: break;
  }

  if (name == NULL) {
    snprintf(out, n, "(bad)");
    return;
  }

  // 'sub rd,zero,rs' is a negation and 'sltu rd,zero,rs' is 'rs != 0', which
  // is how the selector spells both.
  if (funct7Of(w) == 0x20 && f3 == 0 && rs1Of(w) == X_ZERO) {
    snprintf(out, n, "neg%s %s,%s", suffix, X(rdOf(w)), X(rs2Of(w)));
    return;
  }

  if (funct7Of(w) == 0x00 && f3 == 3 && !isWord && rs1Of(w) == X_ZERO) {
    snprintf(out, n, "snez %s,%s", X(rdOf(w)), X(rs2Of(w)));
    return;
  }

  snprintf(out, n, "%s%s %s,%s,%s", name, suffix, X(rdOf(w)), X(rs1Of(w)), X(rs2Of(w)));
}

static void formatOpFp(char *out, size_t n, uint32_t w) {
  char fmt = fpFmt(w);
  uint32_t top = funct7Of(w) >> 2;
  uint32_t f3 = funct3Of(w);

  switch (top) {
  case 0x00: snprintf(out, n, "fadd.%c %s,%s,%s", fmt, F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w))); return;
  case 0x01: snprintf(out, n, "fsub.%c %s,%s,%s", fmt, F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w))); return;
  case 0x02: snprintf(out, n, "fmul.%c %s,%s,%s", fmt, F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w))); return;
  case 0x03: snprintf(out, n, "fdiv.%c %s,%s,%s", fmt, F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w))); return;
  case 0x0b: snprintf(out, n, "fsqrt.%c %s,%s", fmt, F(rdOf(w)), F(rs1Of(w))); return;

  case 0x04: {
    // fsgnj with the same source twice is the move, the negation and the
    // absolute value - which is all this backend ever emits of the three.
    static const char *const injected[3] = { "fsgnj", "fsgnjn", "fsgnjx" };
    static const char *const pseudo[3] = { "fmv", "fneg", "fabs" };

    if (f3 > 2) {
      snprintf(out, n, "(bad)");
    } else if (rs1Of(w) == rs2Of(w)) {
      snprintf(out, n, "%s.%c %s,%s", pseudo[f3], fmt, F(rdOf(w)), F(rs1Of(w)));
    } else {
      snprintf(out, n, "%s.%c %s,%s,%s", injected[f3], fmt, F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w)));
    }
    return;
  }

  case 0x05:
    snprintf(out, n, "%s.%c %s,%s,%s", f3 == 0 ? "fmin" : "fmax", fmt,
             F(rdOf(w)), F(rs1Of(w)), F(rs2Of(w)));
    return;

  case 0x08:
    // The destination format is the instruction's; the source is named in rs2.
    snprintf(out, n, "fcvt.%c.%c %s,%s", fmt, rs2Of(w) == 1 ? 'd' : 's',
             F(rdOf(w)), F(rs1Of(w)));
    return;

  case 0x14: {
    static const char *const compare[3] = { "fle", "flt", "feq" };

    if (f3 > 2) {
      snprintf(out, n, "(bad)");
    } else {
      snprintf(out, n, "%s.%c %s,%s,%s", compare[f3], fmt,
               X(rdOf(w)), F(rs1Of(w)), F(rs2Of(w)));
    }
    return;
  }

  case 0x18:
    // A float-to-integer conversion is the one place the rounding mode is
    // worth printing: C truncates and everything else rounds, so 'rtz' is
    // what says which of the two this is. objdump names it the same way.
    snprintf(out, n, "fcvt.%s.%c %s,%s%s", fcvtIntTypes[rs2Of(w) & 3], fmt,
             X(rdOf(w)), F(rs1Of(w)), f3 == 1 ? ",rtz" : "");
    return;

  case 0x1a:
    snprintf(out, n, "fcvt.%c.%s %s,%s", fmt, fcvtIntTypes[rs2Of(w) & 3],
             F(rdOf(w)), X(rs1Of(w)));
    return;

  case 0x1c:
    if (f3 == 0) {
      snprintf(out, n, "fmv.x.%c %s,%s", fmt == 'd' ? 'd' : 'w', X(rdOf(w)), F(rs1Of(w)));
    } else {
      snprintf(out, n, "fclass.%c %s,%s", fmt, X(rdOf(w)), F(rs1Of(w)));
    }
    return;

  case 0x1e:
    snprintf(out, n, "fmv.%c.x %s,%s", fmt == 'd' ? 'd' : 'w', F(rdOf(w)), X(rs1Of(w)));
    return;

  default: snprintf(out, n, "(bad)"); return;
  }
}

// 'at' is where this word sits, so a branch or a jump prints where it goes
// rather than how far - which is what makes a disassembly readable against the
// block boundaries the emitter chose.
static void formatInstruction(char *out, size_t n, uint32_t w, size_t at) {
  switch (opcodeOf(w)) {
  case 0x37: snprintf(out, n, "lui %s,0x%x", X(rdOf(w)), w >> 12); return;
  case 0x17: snprintf(out, n, "auipc %s,0x%x", X(rdOf(w)), w >> 12); return;

  case 0x6f:
    if (rdOf(w) == X_ZERO) {
      snprintf(out, n, "j 0x%lx", (unsigned long)(at + (long)immJ(w)));
    } else {
      snprintf(out, n, "jal %s,0x%lx", X(rdOf(w)), (unsigned long)(at + (long)immJ(w)));
    }
    return;

  case 0x67:
    if (funct3Of(w) != 0) {
      snprintf(out, n, "(bad)");
    } else if (rdOf(w) == X_ZERO && rs1Of(w) == X_RA && immI(w) == 0) {
      snprintf(out, n, "ret");
    } else if (rdOf(w) == X_ZERO && immI(w) == 0) {
      snprintf(out, n, "jr %s", X(rs1Of(w)));
    } else if (rdOf(w) == X_RA && immI(w) == 0) {
      snprintf(out, n, "jalr %s", X(rs1Of(w)));
    } else {
      snprintf(out, n, "jalr %s,%d(%s)", X(rdOf(w)), immI(w), X(rs1Of(w)));
    }
    return;

  case 0x63: {
    static const char *const branches[8] = { "beq", "bne", NULL, NULL,
                                             "blt", "bge", "bltu", "bgeu" };
    const char *name = branches[funct3Of(w)];

    if (name == NULL) {
      snprintf(out, n, "(bad)");
    } else if (rs2Of(w) == X_ZERO && funct3Of(w) < 2) {
      // beqz / bnez, which is how every test of a condition comes out.
      snprintf(out, n, "%sz %s,0x%lx", name, X(rs1Of(w)),
               (unsigned long)(at + (long)immB(w)));
    } else {
      snprintf(out, n, "%s %s,%s,0x%lx", name, X(rs1Of(w)), X(rs2Of(w)),
               (unsigned long)(at + (long)immB(w)));
    }
    return;
  }

  case 0x03: {
    static const char *const loads[8] = { "lb", "lh", "lw", "ld",
                                          "lbu", "lhu", "lwu", NULL };
    const char *name = loads[funct3Of(w)];

    if (name == NULL) {
      snprintf(out, n, "(bad)");
    } else {
      snprintf(out, n, "%s %s,%d(%s)", name, X(rdOf(w)), immI(w), X(rs1Of(w)));
    }
    return;
  }

  case 0x23: {
    static const char *const stores[8] = { "sb", "sh", "sw", "sd",
                                           NULL, NULL, NULL, NULL };
    const char *name = stores[funct3Of(w)];

    if (name == NULL) {
      snprintf(out, n, "(bad)");
    } else {
      snprintf(out, n, "%s %s,%d(%s)", name, X(rs2Of(w)), immS(w), X(rs1Of(w)));
    }
    return;
  }

  case 0x07:
    snprintf(out, n, "%s %s,%d(%s)", funct3Of(w) == 3 ? "fld" : "flw",
             F(rdOf(w)), immI(w), X(rs1Of(w)));
    return;

  case 0x27:
    snprintf(out, n, "%s %s,%d(%s)", funct3Of(w) == 3 ? "fsd" : "fsw",
             F(rs2Of(w)), immS(w), X(rs1Of(w)));
    return;

  case 0x13: formatOpImm(out, n, w, FALSE); return;
  case 0x1b: formatOpImm(out, n, w, TRUE); return;
  case 0x33: formatOp(out, n, w, FALSE); return;
  case 0x3b: formatOp(out, n, w, TRUE); return;
  case 0x53: formatOpFp(out, n, w); return;

  case 0x0f: snprintf(out, n, "%s", funct3Of(w) == 1 ? "fence.i" : "fence"); return;

  case 0x73:
    snprintf(out, n, "%s", immI(w) == 0 ? "ecall" : immI(w) == 1 ? "ebreak" : "(bad)");
    return;

  default: snprintf(out, n, "(bad)"); return;
  }
}

void disassembleRiscv64(FILE *output, const uint8_t *buffer, size_t size) {
  size_t offset = 0;

  while (offset < size) {
    char text[128];
    unsigned c;

    // A trailing byte or three is not an instruction; say so rather than
    // reading past the end of what was handed over.
    size_t length = size - offset >= 4 ? 4 : 1;
    uint32_t word = 0;

    if (length == 4) {
      word = (uint32_t)buffer[offset] | ((uint32_t)buffer[offset + 1] << 8) |
             ((uint32_t)buffer[offset + 2] << 16) | ((uint32_t)buffer[offset + 3] << 24);
      formatInstruction(text, sizeof text, word, offset);
    } else {
      snprintf(text, sizeof text, "(bad)");
    }

    fprintf(output, "<%08lx>\t", (unsigned long)offset);

    for (c = 0; c < length; ++c) {
      fprintf(output, "%.2x", buffer[offset + c]);
    }

    while (c < 10) {
      fputc(' ', output);
      fputc(' ', output);
      ++c;
    }

    fprintf(output, "%s\n", text);
    offset += length;
  }

  fflush(output);
}
