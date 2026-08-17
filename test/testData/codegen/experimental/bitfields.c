// Assigning to a bit field, which the IR builds by hand out of shifts and
// masks: '(old & ~mask) | ((value << offset) & mask)'.
//
// encodeBitField (src/ir/ast2ir.c) shifted the wrong operand - it fed the
// *cleared storage* into the shift instead of the value being assigned, so the
// value never reached memory at all and every field read back as zero. The
// masks and the store were right, which is why the shape of the generated code
// looked correct in a dump; only running it says otherwise.
//
// It went unnoticed because the fixture that covers bit fields, codegen/my,
// only reached this backend once string literals stopped forcing a fallback -
// its main() calls printf. This one stays inside what the IR backend emits, so
// it is checked on every run of the experimental suite.
//
// Signed fields are the reason the reads matter as much as the writes: a 2-bit
// field holding 3 is -1, so a field that is written correctly but extracted
// without sign extension is still wrong, and the two failures are told apart
// by which value comes back.

struct packed {
  short a;
  char b;
  int c : 2;
  int d : 3;
  int e : 3;
};

struct wide {
  unsigned lo : 12;
  unsigned mid : 8;
  unsigned hi : 12;
};

static int readC(struct packed *p) {
  return p->c;
}

int main(void) {
  // Initialization, which goes through the same encode path field by field.
  struct packed p = {1, 2, 3, 4, 5};

  if (p.a != 1) return 1;
  if (p.b != 2) return 2;
  if (p.c != -1) return 3;  // 2 bits: 3 sign-extends to -1
  if (p.d != -4) return 4;  // 3 bits: 4 sign-extends to -4
  if (p.e != -3) return 5;  // 3 bits: 5 sign-extends to -3

  // The same storage read through a pointer, i.e. by a different function, so
  // a wrong write is not cancelled out by a matching wrong read.
  if (readC(&p) != -1) return 6;

  // Ordinary assignment after the fact, one field at a time. Each has to leave
  // its neighbours alone, which is what the mask is for.
  p.c = 1;
  if (p.c != 1) return 7;
  if (p.d != -4 || p.e != -3) return 8;
  if (p.a != 1 || p.b != 2) return 9;

  p.d = -1;
  if (p.d != -1) return 10;
  if (p.c != 1 || p.e != -3) return 11;

  // Fields wider than a byte and crossing byte boundaries, unsigned this time
  // so the full range is representable.
  struct wide w = {0xabc, 0xde, 0xf01};

  if (w.lo != 0xabc) return 12;
  if (w.mid != 0xde) return 13;
  if (w.hi != 0xf01) return 14;

  w.mid = 0x5a;
  if (w.mid != 0x5a) return 15;
  if (w.lo != 0xabc || w.hi != 0xf01) return 16;

  // A value that does not fit is truncated to the field's width, not stored
  // past it into the neighbour.
  w.mid = 0x1ff;
  if (w.mid != 0xff) return 17;
  if (w.lo != 0xabc || w.hi != 0xf01) return 18;

  // Compound assignment and increment, which read, modify and write back
  // through both halves of the encoding at once.
  w.lo += 1;
  if (w.lo != 0xabd) return 19;

  w.hi++;
  if (w.hi != 0xf02) return 20;
  if (w.lo != 0xabd || w.mid != 0xff) return 21;

  return 0;
}
