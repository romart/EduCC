#include <string.h>

#include "link/target.h"

// The two machines this linker writes for, as data. What is *not* here is as
// much of the point as what is: every decision the link makes before it writes
// a byte reaches a target through 'role', and only the final bytes - a PLT
// stub, a relocated field - are spelled per machine.

static void write32le(uint8_t *at, uint32_t value) {
  at[0] = (uint8_t)value;
  at[1] = (uint8_t)(value >> 8);
  at[2] = (uint8_t)(value >> 16);
  at[3] = (uint8_t)(value >> 24);
}

static void write64le(uint8_t *at, uint64_t value) {
  write32le(at, (uint32_t)value);
  write32le(at + 4, (uint32_t)(value >> 32));
}

/* ------------------------------------------------------------------ x86-64 */

static enum RelocRole x86Role(uint32_t type) {
  switch (type) {
    case R_X86_64_NONE:
    case R_X86_64_GOTPC32: // the GOT's own address, which needs nothing arranged
      return RELOC_ROLE_OTHER;

    case R_X86_64_64:  return RELOC_ROLE_ABS64;
    case R_X86_64_32:
    case R_X86_64_32S: return RELOC_ROLE_ABS32;
    case R_X86_64_PC32: return RELOC_ROLE_PCREL;
    case R_X86_64_PLT32: return RELOC_ROLE_CALL;

    case R_X86_64_GOTPCREL:
    case R_X86_64_GOTPCRELX:
    case R_X86_64_REX_GOTPCRELX:
      return RELOC_ROLE_GOT;

    default: return RELOC_ROLE_UNSUPPORTED;
  }
}

static const char *x86RelocName(uint32_t type) {
  switch (type) {
    case R_X86_64_64: return "R_X86_64_64";
    case R_X86_64_PC32: return "R_X86_64_PC32";
    case R_X86_64_PLT32: return "R_X86_64_PLT32";
    case R_X86_64_32: return "R_X86_64_32";
    case R_X86_64_32S: return "R_X86_64_32S";
    case R_X86_64_GOTPCREL: return "R_X86_64_GOTPCREL";
    case R_X86_64_GOTPCRELX: return "R_X86_64_GOTPCRELX";
    case R_X86_64_REX_GOTPCRELX: return "R_X86_64_REX_GOTPCRELX";
    case R_X86_64_GOTPC32: return "R_X86_64_GOTPC32";
    default: return "relocation";
  }
}

static uint32_t x86FieldWidth(uint32_t type) {
  return type == R_X86_64_64 ? 8 : 4;
}

static void x86WritePlt(uint8_t *plt, uint64_t pltAddress, uint8_t *gotPlt,
                        uint64_t gotPltAddress, size_t count) {
  // PLT0 pushes the loader's bookkeeping word and jumps to its resolver, which
  // ld.so leaves in the second and third reserved .got.plt slots.
  plt[0] = 0xff;
  plt[1] = 0x35;
  write32le(plt + 2, (uint32_t)(gotPltAddress + 8 - (pltAddress + 6)));
  plt[6] = 0xff;
  plt[7] = 0x25;
  write32le(plt + 8, (uint32_t)(gotPltAddress + 16 - (pltAddress + 12)));
  plt[12] = 0x0f;
  plt[13] = 0x1f;
  plt[14] = 0x40;
  plt[15] = 0x00;

  size_t i;
  for (i = 0; i < count; ++i) {
    uint8_t *entry = plt + 16 * (i + 1);
    uint64_t address = pltAddress + 16 * (i + 1);
    uint64_t slot = gotPltAddress + 8 * (3 + i);

    entry[0] = 0xff;
    entry[1] = 0x25;
    write32le(entry + 2, (uint32_t)(slot - (address + 6)));
    entry[6] = 0x68;
    write32le(entry + 7, (uint32_t)i);
    entry[11] = 0xe9;
    write32le(entry + 12, (uint32_t)(pltAddress - (address + 16)));

    // Until ld.so has resolved it, the slot points back at the push - which is
    // what makes the first call go through PLT0 and the rest not.
    write64le(gotPlt + 8 * (3 + i), address + 6);
  }
}

static const char *const x86CrtDirs[] = {
  "/usr/lib/x86_64-linux-gnu", "/usr/lib64", "/usr/lib"
};

static const char *const x86GccGlobs[] = {
  "/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
  "/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o", // For Gentoo and Arch
  "/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o", // For Fedora
};

static const char *const x86LibDirs[] = {
  "/usr/lib/x86_64-linux-gnu",
  "/usr/lib64",
  "/lib64",
  "/usr/lib/x86_64-pc-linux-gnu",
  "/usr/lib/x86_64-redhat-linux",
  "/usr/lib",
  "/lib",
};

/* ----------------------------------------------------------------- riscv64 */

static enum RelocRole riscvRole(uint32_t type) {
  switch (type) {
    case R_RISCV_NONE:
    // Both are annotations rather than edits, and both are safe to ignore for
    // the same reason: this linker never relaxes, so nothing shrinks and the
    // padding an assembler already inserted stays the right size.
    case R_RISCV_RELAX:
    case R_RISCV_ALIGN:
    // A branch reaches within its own function, and a label difference is
    // between two addresses in the same section.
    case R_RISCV_BRANCH:
    case R_RISCV_RVC_BRANCH:
    case R_RISCV_RVC_JUMP:
    case R_RISCV_ADD8: case R_RISCV_ADD16:
    case R_RISCV_ADD32: case R_RISCV_ADD64:
    case R_RISCV_SUB6: case R_RISCV_SUB8: case R_RISCV_SUB16:
    case R_RISCV_SUB32: case R_RISCV_SUB64:
    case R_RISCV_SET6: case R_RISCV_SET8: case R_RISCV_SET16:
    case R_RISCV_SET32:
      return RELOC_ROLE_OTHER;

    case R_RISCV_64: return RELOC_ROLE_ABS64;

    // medlow reaches a global by computing its absolute address in two halves,
    // so the pair is a 32-bit absolute reference in everything but spelling.
    case R_RISCV_32:
    case R_RISCV_HI20:
    case R_RISCV_LO12_I:
    case R_RISCV_LO12_S:
      return RELOC_ROLE_ABS32;

    // R_RISCV_JAL is here rather than under OTHER because nothing stops an
    // assembler naming an external symbol with one, and then it needs a stub
    // like any other call.
    case R_RISCV_CALL:
    case R_RISCV_CALL_PLT:
    case R_RISCV_JAL:
      return RELOC_ROLE_CALL;

    case R_RISCV_PCREL_HI20:
    case R_RISCV_32_PCREL:
      return RELOC_ROLE_PCREL;

    case R_RISCV_PCREL_LO12_I:
    case R_RISCV_PCREL_LO12_S:
      return RELOC_ROLE_PAIRED_LO;

    case R_RISCV_GOT_HI20: return RELOC_ROLE_GOT;

    default: return RELOC_ROLE_UNSUPPORTED;
  }
}

static const char *riscvRelocName(uint32_t type) {
  switch (type) {
    case R_RISCV_32: return "R_RISCV_32";
    case R_RISCV_64: return "R_RISCV_64";
    case R_RISCV_BRANCH: return "R_RISCV_BRANCH";
    case R_RISCV_JAL: return "R_RISCV_JAL";
    case R_RISCV_CALL: return "R_RISCV_CALL";
    case R_RISCV_CALL_PLT: return "R_RISCV_CALL_PLT";
    case R_RISCV_GOT_HI20: return "R_RISCV_GOT_HI20";
    case R_RISCV_PCREL_HI20: return "R_RISCV_PCREL_HI20";
    case R_RISCV_PCREL_LO12_I: return "R_RISCV_PCREL_LO12_I";
    case R_RISCV_PCREL_LO12_S: return "R_RISCV_PCREL_LO12_S";
    case R_RISCV_HI20: return "R_RISCV_HI20";
    case R_RISCV_LO12_I: return "R_RISCV_LO12_I";
    case R_RISCV_LO12_S: return "R_RISCV_LO12_S";
    case R_RISCV_RVC_BRANCH: return "R_RISCV_RVC_BRANCH";
    case R_RISCV_RVC_JUMP: return "R_RISCV_RVC_JUMP";
    case R_RISCV_RELAX: return "R_RISCV_RELAX";
    case R_RISCV_ALIGN: return "R_RISCV_ALIGN";
    case R_RISCV_32_PCREL: return "R_RISCV_32_PCREL";
    default: return "relocation";
  }
}

static uint32_t riscvFieldWidth(uint32_t type) {
  switch (type) {
    case R_RISCV_NONE:
    case R_RISCV_RELAX:
    case R_RISCV_ALIGN:
      return 0;

    case R_RISCV_SUB6:
    case R_RISCV_SET6:
    case R_RISCV_ADD8:
    case R_RISCV_SUB8:
    case R_RISCV_SET8:
      return 1;

    case R_RISCV_ADD16:
    case R_RISCV_SUB16:
    case R_RISCV_SET16:
    case R_RISCV_RVC_BRANCH:
    case R_RISCV_RVC_JUMP:
      return 2;

    case R_RISCV_64:
    case R_RISCV_ADD64:
    case R_RISCV_SUB64:
    // auipc and jalr together, which one entry covers.
    case R_RISCV_CALL:
    case R_RISCV_CALL_PLT:
      return 8;

    default: return 4;
  }
}

enum { X_T0 = 5, X_T1 = 6, X_T2 = 7, X_T3 = 28, X_ZERO = 0 };

static uint32_t riscvUType(uint32_t opcode, uint32_t rd, uint32_t imm) {
  return (imm & 0xfffff000u) | (rd << 7) | opcode;
}

static uint32_t riscvIType(uint32_t opcode, uint32_t funct3, uint32_t rd,
                           uint32_t rs1, int32_t imm) {
  return (((uint32_t)imm & 0xfffu) << 20) | (rs1 << 15) | (funct3 << 12) |
         (rd << 7) | opcode;
}

static uint32_t riscvRType(uint32_t funct7, uint32_t rs2, uint32_t rs1,
                           uint32_t funct3, uint32_t rd) {
  return (funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) |
         (rd << 7) | 0x33u;
}

// A PC-relative reference is written as a 20-bit high half and a 12-bit signed
// low one, so the high half carries the rounding the low one's sign costs.
static uint32_t riscvHi20(int64_t offset) {
  return (uint32_t)((offset + 0x800) & ~0xfffLL);
}

static int32_t riscvLo12(int64_t offset) {
  return (int32_t)(offset - (int64_t)(int32_t)riscvHi20(offset));
}

static void riscvWritePlt(uint8_t *plt, uint64_t pltAddress, uint8_t *gotPlt,
                          uint64_t gotPltAddress, size_t count) {
  // PLT0 leaves the resolver's two reserved slots where ld.so put them and
  // works out which entry it was entered from: the stub's 'jalr t1, t3' left
  // its own address behind, and t3 holds what the slot pointed at, which until
  // a symbol is bound is PLT0 itself.
  int64_t toGot = (int64_t)gotPltAddress - (int64_t)pltAddress;

  uint32_t header[8] = {
    riscvUType(0x17u, X_T2, riscvHi20(toGot)),              // auipc t2, %hi
    riscvRType(0x20u, X_T3, X_T1, 0u, X_T1),                // sub   t1, t1, t3
    riscvIType(0x03u, 3u, X_T3, X_T2, riscvLo12(toGot)),    // ld    t3, %lo(t2)
    riscvIType(0x13u, 0u, X_T1, X_T1, -(32 + 12)),          // addi  t1, t1, -44
    riscvIType(0x13u, 0u, X_T0, X_T2, riscvLo12(toGot)),    // addi  t0, t2, %lo
    riscvIType(0x13u, 5u, X_T1, X_T1, 1),                   // srli  t1, t1, 1
    riscvIType(0x03u, 3u, X_T0, X_T0, 8),                   // ld    t0, 8(t0)
    riscvIType(0x67u, 0u, X_ZERO, X_T3, 0),                 // jr    t3
  };

  unsigned k;
  for (k = 0; k < 8; ++k) write32le(plt + 4 * k, header[k]);

  size_t i;
  for (i = 0; i < count; ++i) {
    uint8_t *entry = plt + 32 + 16 * i;
    uint64_t address = pltAddress + 32 + 16 * i;
    uint64_t slot = gotPltAddress + 8 * (2 + i);
    int64_t toSlot = (int64_t)slot - (int64_t)address;

    write32le(entry + 0, riscvUType(0x17u, X_T3, riscvHi20(toSlot)));
    write32le(entry + 4, riscvIType(0x03u, 3u, X_T3, X_T3, riscvLo12(toSlot)));
    write32le(entry + 8, riscvIType(0x67u, 0u, X_T1, X_T3, 0));
    write32le(entry + 12, 0x00000013u); // nop, so an entry is four instructions

    // Unbound, the slot sends the call to PLT0, which is the only place that
    // knows how to ask ld.so a question.
    write64le(gotPlt + 8 * (2 + i), pltAddress);
  }
}

// Arch's cross toolchain, which is the one this was brought up against. The
// sysroot holds glibc and the CRT objects; gcc's own half stays in /usr/lib.
static const char *const riscvCrtDirs[] = {
  "/usr/riscv64-linux-gnu/usr/lib",
  "/usr/riscv64-linux-gnu/lib",
};

static const char *const riscvGccGlobs[] = {
  "/usr/lib/gcc/riscv64-linux-gnu/*/crtbegin.o",
};

static const char *const riscvLibDirs[] = {
  "/usr/riscv64-linux-gnu/usr/lib",
  "/usr/riscv64-linux-gnu/lib",
};

/* ------------------------------------------------------------------ tables */

static const LinkTarget targets[] = {
  {
    .machine = EM_X86_64,
    .name = "x86-64",
    .interpreter = "/lib64/ld-linux-x86-64.so.2",
    .sysroot = NULL,

    .crtDirs = x86CrtDirs,
    .crtDirCount = sizeof x86CrtDirs / sizeof x86CrtDirs[0],
    .gccGlobs = x86GccGlobs,
    .gccGlobCount = sizeof x86GccGlobs / sizeof x86GccGlobs[0],
    .libDirs = x86LibDirs,
    .libDirCount = sizeof x86LibDirs / sizeof x86LibDirs[0],

    .plt0Size = 16,
    .pltEntrySize = 16,
    .pltAlign = 16,
    .gotPltReserved = 3,

    .relocNone = R_X86_64_NONE,
    .relocAbs64 = R_X86_64_64,
    .relocRelative = R_X86_64_RELATIVE,
    .relocJumpSlot = R_X86_64_JUMP_SLOT,
    .relocCopy = R_X86_64_COPY,
    .relocGlobDat = R_X86_64_GLOB_DAT,

    .role = x86Role,
    .relocName = x86RelocName,
    .fieldWidth = x86FieldWidth,
    .writePlt = x86WritePlt,
  },
  {
    .machine = EM_RISCV,
    .name = "riscv64",
    .interpreter = "/lib/ld-linux-riscv64-lp64d.so.1",
    .sysroot = "/usr/riscv64-linux-gnu",

    .crtDirs = riscvCrtDirs,
    .crtDirCount = sizeof riscvCrtDirs / sizeof riscvCrtDirs[0],
    .gccGlobs = riscvGccGlobs,
    .gccGlobCount = sizeof riscvGccGlobs / sizeof riscvGccGlobs[0],
    .libDirs = riscvLibDirs,
    .libDirCount = sizeof riscvLibDirs / sizeof riscvLibDirs[0],

    .plt0Size = 32,
    .pltEntrySize = 16,
    .pltAlign = 16,
    .gotPltReserved = 2,

    .relocNone = R_RISCV_NONE,
    .relocAbs64 = R_RISCV_64,
    .relocRelative = R_RISCV_RELATIVE,
    .relocJumpSlot = R_RISCV_JUMP_SLOT,
    .relocCopy = R_RISCV_COPY,
    // RISC-V has no GLOB_DAT of its own: a GOT slot the loader has to fill in
    // carries an ordinary absolute relocation naming the symbol.
    .relocGlobDat = R_RISCV_64,

    .role = riscvRole,
    .relocName = riscvRelocName,
    .fieldWidth = riscvFieldWidth,
    .writePlt = riscvWritePlt,
  },
};

const LinkTarget *linkTargetFor(uint16_t machine) {
  unsigned i;
  for (i = 0; i < sizeof targets / sizeof targets[0]; ++i) {
    if (targets[i].machine == machine) return &targets[i];
  }
  return NULL;
}
