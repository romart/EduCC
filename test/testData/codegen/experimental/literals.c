// String literals through the IR backend's constant pool.
//
// A literal is the one constant with no immediate form: its value is the
// address of bytes that exist only because the backend put them somewhere.
// Selection therefore builds a pool entry and a rip-relative 'lea' against it,
// and emission places the bytes in .rodata and resolves the index. Every step
// of that is invisible in a dump - the dump shows an index, not an address -
// so this fixture is the one that says the bytes are really there, really
// contiguous, and really where the instruction points.
//
// It guards the whole path at once: a wrong pool index, a relocation against
// the wrong section, a dropped displacement or an off-by-one offset all show
// up here as a wrong character rather than as a crash.
//
// The address comparisons are deliberate and EduCC-specific. Whether identical
// literals share storage is the implementation's business, and this one pools
// them by content in a single per-file cache, so 'same content, same address'
// holds across functions and across the two backends - one .rodata copy is
// shared by every use in the file. If pooling is ever changed to be per
// function, these are the checks that will say so.

#include <stdio.h>

static const char *hello(void) {
  return "hello";
}

static const char *helloAgain(void) {
  // The same content from a different function, and so from a different
  // constant pool: one copy in .rodata, reached by both.
  return "hello";
}

static int lengthOf(const char *s) {
  int n = 0;
  while (*s++) {
    ++n;
  }
  return n;
}

static int sameBytes(const char *a, const char *b, int n) {
  int i;
  for (i = 0; i < n; ++i) {
    if (a[i] != b[i]) {
      return 0;
    }
  }
  return 1;
}

int main(void) {
  const char *h = hello();

  if (h[0] != 'h' || h[4] != 'o' || h[5] != '\0') return 1;
  if (lengthOf(h) != 5) return 2;

  // Indexing a literal directly, where the address never reaches a variable.
  if ("abc"[1] != 'b') return 3;

  // Two entries in one pool, which is what says the index is per constant.
  const char *a = "first";
  const char *b = "second";
  if (a[0] != 'f' || b[0] != 's') return 4;
  if (a == b) return 5;

  // One entry, asked for twice in one function.
  const char *c = "first";
  if (c != a) return 6;

  // One entry, asked for from two functions.
  if (hello() != helloAgain()) return 7;

  // Escapes, and a byte no dump can print literally. The array is sized by the
  // initializer so the interior NUL is not what decides the length.
  const char esc[] = "a\n\"b\\c\x80";
  if (esc[0] != 'a' || esc[1] != '\n' || esc[2] != '"' || esc[3] != 'b') return 8;
  if (esc[4] != '\\' || esc[5] != 'c' || (unsigned char)esc[6] != 0x80) return 9;
  if (sizeof(esc) != 8) return 10;

  // A literal containing a NUL: the bytes after it are still stored, even
  // though every string function stops at it.
  const char nul[] = "ab\0cd";
  if (sizeof(nul) != 6) return 11;
  if (!sameBytes(nul, "ab\0cd", 6)) return 12;
  if (lengthOf(nul) != 2) return 13;

  // The ordinary use, and the one that reads the bytes back out of .rodata
  // through someone else's code.
  printf("literals ok: %s %s\n", h, b);

  return 0;
}
