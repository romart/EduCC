// Struct assignments big enough that the unrolled copy is a long run of
// load/store pairs rather than a couple of them.
//
// This fixture used to pin down a cap: selectMemoryCopy refused any copy over
// MAX_UNROLLED_COPY bytes, so 'copyBig' and 'sumBigByValue' were handed to the
// legacy backend and named in a big_copy.fallback beside this file. The cap was
// never a correctness boundary - only a guess at where unrolling stops paying -
// and it is gone. Both functions are built here now, and the .fallback with
// them; a copy of any constant size is spelled out.
//
// What is left of the boundary is the reason the cap looked reasonable: under
// the spill-everything allocator each chunk costs six instructions and, until
// the chunks shared one register, eight bytes of frame apiece. That is the
// allocator's price and not the copy's, so it is fixed where the allocator is.
//
// The one shape still refused is a copy whose *size* is not known until run
// time, which needs a loop. It has no repro here on purpose: generateComposite-
// Copy (src/ir/ast2ir.c) always builds the count from computeTypeSize(), so no
// C program reaches it today. It is guarded because the instruction allows it,
// not because anything produces it.
//
// Sizes are chosen to cover the chunking: 256 bytes is whole eightbytes, 259
// forces the 2- and 1-byte tail, and 32 is the short case that always worked.
// The tail matters more than it looks - every chunk now borrows one 8-byte
// register, so a narrow load has to read back only the bytes it wrote.
//
// Correctness is what is checked, not which backend built it: both have to copy
// every byte, and the functions meet across ordinary calls, so a copy that
// stopped early shows up as a wrong element rather than as a crash.

struct Big {
  int a[64];
};

struct Small {
  int a[8];
};

// Chars, so that the size is 259 rather than rounded up to an alignment: the
// copy is thirty-two eightbytes, then a 2-byte chunk and a 1-byte one.
struct Odd {
  char b[259];
};

static void fillBig(struct Big *b, int seed) {
  int i;
  for (i = 0; i < 64; ++i) {
    b->a[i] = seed + i;
  }
}

static void copyBig(struct Big *dst, struct Big *src) {
  *dst = *src;
}

static void copySmall(struct Small *dst, struct Small *src) {
  *dst = *src;
}

static void copyOdd(struct Odd *dst, struct Odd *src) {
  *dst = *src;
}

static int sumBig(struct Big b) {
  int i;
  int total = 0;

  for (i = 0; i < 64; ++i) {
    total += b.a[i];
  }

  return total;
}

// Passing the whole struct by value, which is the *other* refusal a 256-byte
// aggregate runs into: the IR hands the callee an address where the ABI says
// bytes on the stack. Kept in its own function so that main stays on the IR
// backend and the checks below are run by the backend under test.
static int sumBigByValue(struct Big *b) {
  return sumBig(*b);
}

int main(void) {
  struct Big src;
  struct Big dst;
  struct Small s = {{1, 2, 3, 4, 5, 6, 7, 8}};
  struct Small t;
  int i;

  fillBig(&src, 100);

  // Every byte of the destination set to something the copy has to overwrite,
  // so a chunk that never ran is visible rather than accidentally right.
  for (i = 0; i < 64; ++i) {
    dst.a[i] = -1;
  }

  copyBig(&dst, &src);

  for (i = 0; i < 64; ++i) {
    if (dst.a[i] != 100 + i) return 1;
  }

  // The first and last elements again, on their own: an off-by-one at either
  // end of the unrolled run is the failure this shape is looking for.
  if (dst.a[0] != 100) return 2;
  if (dst.a[63] != 163) return 3;

  copySmall(&t, &s);

  for (i = 0; i < 8; ++i) {
    if (t.a[i] != i + 1) return 4;
  }

  // 100 + 101 + ... + 163
  if (sumBigByValue(&src) != 8416) return 5;

  // Self-assignment, where the source and the destination are the same bytes.
  copyBig(&dst, &dst);
  if (dst.a[7] != 107) return 6;

  {
    struct Odd os;
    struct Odd od;

    for (i = 0; i < 259; ++i) {
      os.b[i] = (char)(i + 1);
      od.b[i] = -1;
    }

    copyOdd(&od, &os);

    for (i = 0; i < 259; ++i) {
      if (od.b[i] != (char)(i + 1)) return 7;
    }

    // The last three bytes on their own: they are the 2- and 1-byte chunks, the
    // ones the shared register has to read back narrower than it is.
    if (od.b[256] != (char)257) return 8;
    if (od.b[257] != (char)258) return 9;
    if (od.b[258] != (char)259) return 10;
  }

  return 0;
}
