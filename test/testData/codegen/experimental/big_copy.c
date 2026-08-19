// A struct assignment larger than the IR backend will spell out one chunk at
// a time, which is the one thing about aggregate copying it still refuses.
//
// selectMemoryCopy unrolls a copy into load/store pairs, and caps that at
// MAX_UNROLLED_COPY bytes - past which the right shape is a loop, or a call to
// memcpy, and neither exists in this backend yet. So a 256-byte struct
// assignment hands the whole function back to the legacy one, which is why
// 'big' is listed in big_copy.fallback while everything else here is not.
//
// The fixture exists to pin that boundary down from both sides. 'copySmall'
// copies 32 bytes and must stay on the IR backend; 'copyBig' copies 256 and
// must not. If the cap is ever raised, or a loop is emitted instead, the
// .fallback file stops being needed and the runner says so rather than letting
// the exemption outlive what it excused.
//
// 'sumBigByValue' is the second gap the same struct runs into and the second
// name in that file: an aggregate too large for one register is passed by
// address by the IR and read as stack bytes by the callee, which is a
// disagreement between the two halves of one convention rather than anything
// missing from selection. See docs/ir-codegen-design.md section 10 for both.
//
// The other half of that refusal - a copy whose *size* is not known until run
// time - has no repro here on purpose: generateCompositeCopy (src/ir/ast2ir.c)
// always builds the count from computeTypeSize(), so no C program reaches it
// today. It is guarded because the instruction allows it, not because anything
// produces it.
//
// Correctness is what is checked, not which backend built it: both have to
// copy every byte, and the two functions meet across an ordinary call, so a
// copy that stopped early would show up as a wrong element rather than as a
// crash.

struct Big {
  int a[64];
};

struct Small {
  int a[8];
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

  return 0;
}
