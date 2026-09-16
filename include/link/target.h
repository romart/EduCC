#ifndef __LINK_TARGET_H__
#define __LINK_TARGET_H__ 1

#include <elf.h>
#include <stdint.h>
#include <stddef.h>

#include "common.h"

// What the linker has to know about a machine, which turns out to be less than
// the twenty switch arms it was spelled as. Two targets exist and the second
// one is what proved the seam: everything the link *decides* depends on a
// relocation's role rather than its number, and only the bytes it finally
// writes are per-target.

// A relocation's role: the whole of what a decision before layout can depend
// on. The numbers themselves stay in the tables below.
enum RelocRole {
  // Nothing to arrange. A branch that reaches its own function, a difference
  // between two labels, and the annotations (RELAX, ALIGN) this linker is
  // entitled to ignore because it never relaxes.
  RELOC_ROLE_OTHER = 0,

  RELOC_ROLE_ABS64, // an absolute pointer-wide field
  RELOC_ROLE_ABS32, // an absolute 32-bit field, which no dynamic entry fits in
  RELOC_ROLE_PCREL, // PC-relative, which a preemptible name cannot be reached by
  RELOC_ROLE_CALL,  // a call, which may go through a stub
  RELOC_ROLE_GOT,   // the operand is a GOT entry rather than the symbol

  // Names the *label* of another relocation rather than a symbol of its own -
  // RISC-V's way of spelling the low half of a PC-relative pair. Resolving one
  // means finding the instruction it points at and asking what that resolved
  // to, so it is the one role the linker cannot answer from S and A alone.
  RELOC_ROLE_PAIRED_LO,

  // Understood well enough to name in a diagnostic and not well enough to
  // apply: TLS, mostly.
  RELOC_ROLE_UNSUPPORTED
};

typedef struct _LinkTarget {
  uint16_t machine; // EM_X86_64 or EM_RISCV
  const char *name; // what a diagnostic calls it

  const char *interpreter;

  // Where the target's own files live when it is not the machine this compiler
  // is running on. An absolute path inside a linker script is looked up here
  // first, the way ld's '--sysroot' does it: a cross glibc's libc.so says
  // 'GROUP ( /lib/libc.so.6 ... )' and means its own, not the host's.
  const char *sysroot;

  const char *const *crtDirs;  size_t crtDirCount;  // crt1.o, crti.o, crtn.o
  const char *const *gccGlobs; size_t gccGlobCount; // crtbegin.o, by glob
  const char *const *libDirs;  size_t libDirCount;  // the default '-L' list

  // PLT0 is not always an entry's size: x86-64's is 16 bytes like the rest,
  // RV64's is 32.
  uint32_t plt0Size;
  uint32_t pltEntrySize;
  uint32_t pltAlign;

  // .got.plt[0] is .dynamic on both. x86-64 reserves two further slots for
  // ld.so's resolver and link map; RV64 reserves one.
  uint32_t gotPltReserved;

  // The types this linker writes rather than reads.
  uint32_t relocNone;
  uint32_t relocAbs64;
  uint32_t relocRelative;
  uint32_t relocJumpSlot;
  uint32_t relocCopy;
  uint32_t relocGlobDat;

  enum RelocRole (*role)(uint32_t type);
  const char *(*relocName)(uint32_t type);

  // How many bytes of the section a relocation rewrites, for the bounds check.
  uint32_t (*fieldWidth)(uint32_t type);

  // PLT0 and every entry, plus the .got.plt slots that point back into them.
  void (*writePlt)(uint8_t *plt, uint64_t pltAddress, uint8_t *gotPlt,
                   uint64_t gotPltAddress, size_t count);
} LinkTarget;

// NULL for a machine this linker does not have a table for.
const LinkTarget *linkTargetFor(uint16_t machine);

#endif // __LINK_TARGET_H__
