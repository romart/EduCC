// A call with more aggregate arguments than a single word can name.
//
// An aggregate larger than an eightbyte is passed as *bytes* in the argument
// area, and nothing in the IR says so: the argument is an IR_PTR pointing at
// the temporary copy the call built, exactly like a genuine pointer argument
// would be. Which inputs are which is carried alongside the call instead, and
// used to be carried as a single uint64_t - one bit per input, with input 0
// meaning the callee.
//
// Past the sixty-fourth input there was no bit left. translateCall set bit 0
// as an overflow flag instead and selection refused the whole call, which sent
// the caller to the legacy backend; without the flag it would have passed the
// temporary's *address* where the callee reads sixteen bytes of struct. The
// mask is a bitmap sized to the input count now, so an argument's position
// means nothing at all - see setCallMemoryArg and section 6.21 of
// docs/ir-codegen-design.md.
//
// Seventy arguments is past the boundary with room to spare, and the two
// callees put their arguments at different input positions: sumFields has the
// callee at input 0 and its first argument at input 1, while pickLast returns
// a struct too large for a register and so has the hidden return buffer at
// input 1, moving every argument one position later. An off-by-one in the
// bitmap moves one of the two and not the other.
//
// Each field is distinct and every one is added in, so an argument read from
// the wrong place - an address rather than bytes, most of all - shows up as a
// wildly wrong total rather than as a near miss.
//
// The sum is accumulated one statement at a time rather than written as one
// long '+' chain. That used to be forced: canonicalization walked an additive
// chain 2^depth times, so twenty-four terms took sixteen seconds and the
// natural way to write this callee did not compile in any reasonable time.
// Fixed since (see long_expression_chains.c); the shape is left as it is
// because it is not what this fixture is about.

struct S { long lo, hi; };

long sumFields(struct S p0, struct S p1, struct S p2, struct S p3,
               struct S p4, struct S p5, struct S p6, struct S p7,
               struct S p8, struct S p9, struct S p10, struct S p11,
               struct S p12, struct S p13, struct S p14, struct S p15,
               struct S p16, struct S p17, struct S p18, struct S p19,
               struct S p20, struct S p21, struct S p22, struct S p23,
               struct S p24, struct S p25, struct S p26, struct S p27,
               struct S p28, struct S p29, struct S p30, struct S p31,
               struct S p32, struct S p33, struct S p34, struct S p35,
               struct S p36, struct S p37, struct S p38, struct S p39,
               struct S p40, struct S p41, struct S p42, struct S p43,
               struct S p44, struct S p45, struct S p46, struct S p47,
               struct S p48, struct S p49, struct S p50, struct S p51,
               struct S p52, struct S p53, struct S p54, struct S p55,
               struct S p56, struct S p57, struct S p58, struct S p59,
               struct S p60, struct S p61, struct S p62, struct S p63,
               struct S p64, struct S p65, struct S p66, struct S p67,
               struct S p68, struct S p69) {
  long r = 0;
  r += p0.lo + p0.hi; r += p1.lo + p1.hi;
  r += p2.lo + p2.hi; r += p3.lo + p3.hi;
  r += p4.lo + p4.hi; r += p5.lo + p5.hi;
  r += p6.lo + p6.hi; r += p7.lo + p7.hi;
  r += p8.lo + p8.hi; r += p9.lo + p9.hi;
  r += p10.lo + p10.hi; r += p11.lo + p11.hi;
  r += p12.lo + p12.hi; r += p13.lo + p13.hi;
  r += p14.lo + p14.hi; r += p15.lo + p15.hi;
  r += p16.lo + p16.hi; r += p17.lo + p17.hi;
  r += p18.lo + p18.hi; r += p19.lo + p19.hi;
  r += p20.lo + p20.hi; r += p21.lo + p21.hi;
  r += p22.lo + p22.hi; r += p23.lo + p23.hi;
  r += p24.lo + p24.hi; r += p25.lo + p25.hi;
  r += p26.lo + p26.hi; r += p27.lo + p27.hi;
  r += p28.lo + p28.hi; r += p29.lo + p29.hi;
  r += p30.lo + p30.hi; r += p31.lo + p31.hi;
  r += p32.lo + p32.hi; r += p33.lo + p33.hi;
  r += p34.lo + p34.hi; r += p35.lo + p35.hi;
  r += p36.lo + p36.hi; r += p37.lo + p37.hi;
  r += p38.lo + p38.hi; r += p39.lo + p39.hi;
  r += p40.lo + p40.hi; r += p41.lo + p41.hi;
  r += p42.lo + p42.hi; r += p43.lo + p43.hi;
  r += p44.lo + p44.hi; r += p45.lo + p45.hi;
  r += p46.lo + p46.hi; r += p47.lo + p47.hi;
  r += p48.lo + p48.hi; r += p49.lo + p49.hi;
  r += p50.lo + p50.hi; r += p51.lo + p51.hi;
  r += p52.lo + p52.hi; r += p53.lo + p53.hi;
  r += p54.lo + p54.hi; r += p55.lo + p55.hi;
  r += p56.lo + p56.hi; r += p57.lo + p57.hi;
  r += p58.lo + p58.hi; r += p59.lo + p59.hi;
  r += p60.lo + p60.hi; r += p61.lo + p61.hi;
  r += p62.lo + p62.hi; r += p63.lo + p63.hi;
  r += p64.lo + p64.hi; r += p65.lo + p65.hi;
  r += p66.lo + p66.hi; r += p67.lo + p67.hi;
  r += p68.lo + p68.hi; r += p69.lo + p69.hi;
  return r;
}

// The same argument list behind a hidden return buffer.
struct S pickLast(struct S p0, struct S p1, struct S p2, struct S p3,
                  struct S p4, struct S p5, struct S p6, struct S p7,
                  struct S p8, struct S p9, struct S p10, struct S p11,
                  struct S p12, struct S p13, struct S p14, struct S p15,
                  struct S p16, struct S p17, struct S p18, struct S p19,
                  struct S p20, struct S p21, struct S p22, struct S p23,
                  struct S p24, struct S p25, struct S p26, struct S p27,
                  struct S p28, struct S p29, struct S p30, struct S p31,
                  struct S p32, struct S p33, struct S p34, struct S p35,
                  struct S p36, struct S p37, struct S p38, struct S p39,
                  struct S p40, struct S p41, struct S p42, struct S p43,
                  struct S p44, struct S p45, struct S p46, struct S p47,
                  struct S p48, struct S p49, struct S p50, struct S p51,
                  struct S p52, struct S p53, struct S p54, struct S p55,
                  struct S p56, struct S p57, struct S p58, struct S p59,
                  struct S p60, struct S p61, struct S p62, struct S p63,
                  struct S p64, struct S p65, struct S p66, struct S p67,
                  struct S p68, struct S p69) {
  return p69;
}

int main(void) {
  struct S v[70];
  for (int i = 0; i < 70; ++i) {
    v[i].lo = i + 1;
    v[i].hi = (long)(i + 1) * 100;
  }

  // Sum over i in [0, 70) of (i+1) + (i+1)*100.
  if (sumFields(v[0], v[1], v[2], v[3], v[4],
                v[5], v[6], v[7], v[8], v[9],
                v[10], v[11], v[12], v[13], v[14],
                v[15], v[16], v[17], v[18], v[19],
                v[20], v[21], v[22], v[23], v[24],
                v[25], v[26], v[27], v[28], v[29],
                v[30], v[31], v[32], v[33], v[34],
                v[35], v[36], v[37], v[38], v[39],
                v[40], v[41], v[42], v[43], v[44],
                v[45], v[46], v[47], v[48], v[49],
                v[50], v[51], v[52], v[53], v[54],
                v[55], v[56], v[57], v[58], v[59],
                v[60], v[61], v[62], v[63], v[64],
                v[65], v[66], v[67], v[68], v[69]) != 250985L) {
    return 1;
  }

  struct S last = pickLast(v[0], v[1], v[2], v[3], v[4],
                           v[5], v[6], v[7], v[8], v[9],
                           v[10], v[11], v[12], v[13], v[14],
                           v[15], v[16], v[17], v[18], v[19],
                           v[20], v[21], v[22], v[23], v[24],
                           v[25], v[26], v[27], v[28], v[29],
                           v[30], v[31], v[32], v[33], v[34],
                           v[35], v[36], v[37], v[38], v[39],
                           v[40], v[41], v[42], v[43], v[44],
                           v[45], v[46], v[47], v[48], v[49],
                           v[50], v[51], v[52], v[53], v[54],
                           v[55], v[56], v[57], v[58], v[59],
                           v[60], v[61], v[62], v[63], v[64],
                           v[65], v[66], v[67], v[68], v[69]);
  if (last.lo != 70L) return 2;
  if (last.hi != 7000L) return 3;

  return 0;
}
