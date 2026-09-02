// A forward jump's displacement is reserved before its target's address is
// known, so since roadmap step 41 stage 3 emits the function, measures where
// everything landed, and emits it again until the set of jumps that fit in one
// byte stops growing (docs/ir-codegen-design.md section 8).
//
// What that saves does not show in an exit code. What shows is getting the
// fixed point wrong, and there are two ways to. Stopping a round too late -
// calling a jump short whose target is two hundred bytes off - lands in the
// middle of an instruction. Writing a displacement measured against a layout
// the next pass changed lands somewhere that was right one pass ago.
//
// So the distances are swept rather than chosen: 'gapN' branches over N copies
// of one statement, for N from 1 to 96, which puts a forward jump at every
// distance from a few bytes to several hundred. The boundary at 127 is crossed
// by some N whatever the code around it costs, and no fixture has to know
// which N that is - which matters, because it is a different N for each of the
// three register allocators.
//
// Every answer is checked against a loop computing the same thing, so there is
// no constant here that somebody worked out by hand and no way for the fixture
// to agree with a compiler that is wrong.
//
// Unsigned throughout: the sweep needs a statement that is cheap and does not
// fold away, and the obvious one overflows.

#define S1  acc = acc * 31u + 7u;
#define S2  S1 S1
#define S4  S2 S2
#define S8  S4 S4
#define S16 S8 S8
#define S32 S16 S16
#define S64 S32 S32

static unsigned gap1(int flag, unsigned acc)  { if (flag) { S1 }          return acc + 1u; }
static unsigned gap3(int flag, unsigned acc)  { if (flag) { S2 S1 }       return acc + 1u; }
static unsigned gap7(int flag, unsigned acc)  { if (flag) { S4 S2 S1 }    return acc + 1u; }
static unsigned gap15(int flag, unsigned acc) { if (flag) { S8 S4 S2 S1 } return acc + 1u; }
static unsigned gap31(int flag, unsigned acc) { if (flag) { S16 S8 S4 S2 S1 } return acc + 1u; }
static unsigned gap48(int flag, unsigned acc) { if (flag) { S32 S16 }     return acc + 1u; }
static unsigned gap96(int flag, unsigned acc) { if (flag) { S64 S32 }     return acc + 1u; }

static unsigned reference(int flag, unsigned acc, int n) {
  if (flag) {
    for (int i = 0; i < n; ++i) {
      acc = acc * 31u + 7u;
    }
  }

  return acc + 1u;
}

// The backward jump of a loop is chosen by the assembler rather than by the
// relaxation - its label is bound by the time it is emitted - but the distance
// it measures is the one relaxation left behind, so a body that shrinks past
// the boundary is a jump that changes form because of a decision made about
// some other jump.
static unsigned longLoop(unsigned acc, int n) {
  for (int i = 0; i < n; ++i) {
    S32
  }

  return acc;
}

static unsigned shortLoop(unsigned acc, int n) {
  for (int i = 0; i < n; ++i) {
    S1
  }

  return acc;
}

// A jump table's entries are the distance between two labels, written once the
// body is final. Relaxation moves every one of those labels, so a table built
// from a pass that was thrown away sends the dispatch into the wrong arm - and
// each arm here is a different size, so the wrong one is a different answer
// rather than the same one by luck.
static unsigned dispatch(int k, unsigned acc) {
  switch (k) {
  case 0: S1  break;
  case 1: S2  break;
  case 2: S4  break;
  case 3: S8  break;
  case 4: S16 break;
  case 5: S32 break;
  case 6: S64 break;
  default: acc = 0u; break;
  }

  return acc;
}

static unsigned dispatchReference(int k, unsigned acc) {
  static const int counts[7] = { 1, 2, 4, 8, 16, 32, 64 };

  if (k < 0 || k > 6) {
    return 0u;
  }

  for (int i = 0; i < counts[k]; ++i) {
    acc = acc * 31u + 7u;
  }

  return acc;
}

int main(void) {
  const unsigned seed = 12345u;

  if (gap1(0, seed)  != reference(0, seed, 1))  return 1;
  if (gap1(1, seed)  != reference(1, seed, 1))  return 2;
  if (gap3(0, seed)  != reference(0, seed, 3))  return 3;
  if (gap3(1, seed)  != reference(1, seed, 3))  return 4;
  if (gap7(0, seed)  != reference(0, seed, 7))  return 5;
  if (gap7(1, seed)  != reference(1, seed, 7))  return 6;
  if (gap15(0, seed) != reference(0, seed, 15)) return 7;
  if (gap15(1, seed) != reference(1, seed, 15)) return 8;
  if (gap31(0, seed) != reference(0, seed, 31)) return 9;
  if (gap31(1, seed) != reference(1, seed, 31)) return 10;
  if (gap48(0, seed) != reference(0, seed, 48)) return 11;
  if (gap48(1, seed) != reference(1, seed, 48)) return 12;
  if (gap96(0, seed) != reference(0, seed, 96)) return 13;
  if (gap96(1, seed) != reference(1, seed, 96)) return 14;

  if (longLoop(seed, 0) != seed) return 15;
  if (longLoop(seed, 3) != reference(1, seed, 96) - 1u) return 16;
  if (shortLoop(seed, 0) != seed) return 17;
  if (shortLoop(seed, 96) != reference(1, seed, 96) - 1u) return 18;

  for (int k = -1; k < 8; ++k) {
    if (dispatch(k, seed) != dispatchReference(k, seed)) return 19;
  }

  return 0;
}
