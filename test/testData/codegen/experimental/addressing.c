// Addressing modes, run rather than read.
//
// isel_addressing.c in the IR fixtures says what shape selection folds a GEP
// chain into; this says the shape is also the right address. The two failures
// it is looking for are ones a dump cannot show: an index scaled by the wrong
// power of two - the SIB byte holds the scale as a shift amount, so a 4 that
// should be an 8 is one wrong line in one table - and an index that was
// widened with the wrong sign, which is invisible until the index is negative.
//
// A frame-anchored address is the other half. '[rbp - 32 + rax*4]' spends its
// base on the frame pointer, so the slot's offset and the addressing mode's
// own displacement have to be added together somewhere; getting that wrong
// reads a neighbouring local, which is a wrong value and not a crash.
//
// Every element is given a value that says where it came from, so a read
// through a wrong address returns a wrong number rather than a plausible one.

struct Point {
  int x;
  int y;
};

struct Wide {
  char pad[24];
  int tail;
};

// Three bytes, so the element size is not a power of two and no scale can
// express it: the frontend multiplies instead of shifting and the whole offset
// arrives as one index term.
struct Odd {
  char a;
  char b;
  char c;
};

static int sumPoints(struct Point *p, int n) {
  int total = 0;
  int i;

  for (i = 0; i < n; ++i) {
    total += p[i].x * 100 + p[i].y;
  }

  return total;
}

int main(void) {
  char bytes[8];
  short halves[8];
  int words[8];
  long longs[8];
  struct Point points[4];
  struct Wide wide[3];
  struct Odd odd[4];
  int i;
  long li;

  // Every scale x86 can encode, written through a variable index so the
  // addressing mode is what does the multiply.
  for (i = 0; i < 8; ++i) {
    bytes[i] = (char)(i + 1);
    halves[i] = (short)(i + 10);
    words[i] = i + 100;
    longs[i] = i + 1000;
  }

  for (i = 0; i < 8; ++i) {
    if (bytes[i] != i + 1) return 1;
    if (halves[i] != i + 10) return 2;
    if (words[i] != i + 100) return 3;
    if (longs[i] != i + 1000) return 4;
  }

  // The last element of each, which is where a scale one step too large runs
  // off the end of the object and into the next local.
  if (bytes[7] != 8 || halves[7] != 17 || words[7] != 107 || longs[7] != 1007) return 5;

  // A negative index, which is the case that says the index was sign-extended
  // and not zero-extended: as an unsigned 64-bit value, -1 is enormous.
  {
    int *middle = &words[4];
    int back = -1;

    if (middle[back] != 103) return 6;
    if (middle[-4] != 100) return 7;

    middle[back] = 555;
    if (words[3] != 555) return 8;
    words[3] = 103;
  }

  // An index already at pointer width, which needs no widening at all.
  for (li = 0; li < 8; ++li) {
    if (words[li] != li + 100) return 9;
  }

  // Scaled index and displacement in one address: the element size scales the
  // subscript, the field offset is the displacement.
  for (i = 0; i < 4; ++i) {
    points[i].x = i + 1;
    points[i].y = i + 20;
  }

  for (i = 0; i < 4; ++i) {
    if (points[i].x != i + 1) return 10;
    if (points[i].y != i + 20) return 11;
  }

  // Through a function, so the addresses are built from a parameter rather
  // than from a frame slot - the same fold with a register for its base.
  // (1*100+20) + (2*100+21) + (3*100+22) + (4*100+23) = 1086
  if (sumPoints(points, 4) != 1086) return 12;

  // A displacement past the first cache line's worth of struct, so a byte
  // displacement and a word one are told apart.
  for (i = 0; i < 3; ++i) {
    wide[i].tail = i + 70;
    wide[i].pad[0] = (char)i;
  }

  for (i = 0; i < 3; ++i) {
    if (wide[i].tail != i + 70) return 13;
    if (wide[i].pad[0] != i) return 14;
  }

  // An element size no scale can express, so the offset is a multiply and the
  // address folds it as an index times one.
  for (i = 0; i < 4; ++i) {
    odd[i].a = (char)(i + 1);
    odd[i].b = (char)(i + 5);
    odd[i].c = (char)(i + 9);
  }

  for (i = 0; i < 4; ++i) {
    if (odd[i].a != i + 1) return 15;
    if (odd[i].b != i + 5) return 16;
    if (odd[i].c != i + 9) return 17;
  }

  // A whole struct copied between two folded addresses, which is the one rule
  // that builds several addresses from one fold and walks a displacement
  // across them.
  points[0] = points[3];
  if (points[0].x != 4 || points[0].y != 23) return 18;

  wide[0] = wide[2];
  if (wide[0].tail != 72 || wide[0].pad[0] != 2) return 19;

  return 0;
}
