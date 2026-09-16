// long double across a toolchain boundary.
//
// LP64D passes one in a pair of integer registers, aligned to sixteen bytes on
// the stack, and returns it in a0:a1 - none of which is what SysV does with
// x87's, so every one of these rules is this target's alone and was written
// against the psABI rather than inherited. A fixture compiled entirely by
// EduCC cannot tell whether it got them right: both halves of a wrong answer
// agree. This one is compiled half by EduCC and half by the cross toolchain,
// which is the only thing in the tree that asks a third party.
//
// It found one: a sixteen-byte struct whose only member is a long double.
// classifyComposite is SysV's rule, where such an aggregate is class MEMORY,
// and EduCC passed it by reference where gcc passes it in a0:a1 - so p_one
// read an address as a number, and p_mkone wrote through a hidden pointer the
// caller never passed and took the process with it.
//
// Printed rather than compared here, because what the harness diffs is this
// program's output against the same program built entirely by gcc.

int printf(const char *, ...);

typedef long double LD;

struct One { LD a; };
struct Wide { LD a; LD b; };
struct Mixed { int i; LD v; };

LD p_add(LD, LD);
LD p_sub(LD, LD);
LD p_mul(LD, LD);
LD p_div(LD, LD);
LD p_neg(LD);
LD p_five(LD, LD, LD, LD, LD);
LD p_afterInts(int, int, int, LD);
LD p_afterFive(long, long, long, long, long, LD);
LD p_mixed(int, LD, double, LD, long, float);
LD p_one(struct One);
struct One p_mkone(LD);
LD p_wide(struct Wide);
struct Wide p_mkwide(LD, LD);
LD p_mixedStruct(struct Mixed);
int p_cmp(LD, LD);
int p_rel(LD, LD, int *);
double p_toDouble(LD);
float p_toFloat(LD);
long p_toLong(LD);
int p_toInt(LD);
unsigned long p_toULong(LD);
short p_toShort(LD);
signed char p_toChar(LD);
LD p_fromDouble(double);
LD p_fromFloat(float);
LD p_fromInt(int);
LD p_fromLong(long);
LD p_fromUInt(unsigned);
LD p_fromULong(unsigned long);

// Sixteen digits, not twenty. A literal is parsed by whichever compiler read
// this file, into whatever long double *it* has - so the last digits of a
// constant belong to the host's format and not to the ABI this is about.
#define SHOW(x) printf("%s = %.16Lg\n", #x, (LD)(x))

int main(void) {
  LD a = 1.5L, b = 2.25L;

  SHOW(p_add(a, b)); SHOW(p_sub(a, b)); SHOW(p_mul(a, b)); SHOW(p_div(a, b));
  SHOW(p_neg(a));
  SHOW(p_five(1.0L, 2.0L, 3.0L, 4.0L, 5.0L));
  SHOW(p_afterInts(1, 2, 3, 0.5L));
  SHOW(p_afterFive(1, 2, 3, 4, 5, 0.25L));
  SHOW(p_mixed(3, a, 0.5, b, 9L, 1.25f));

  { struct One s; s.a = 2.5L; SHOW(p_one(s)); }
  { struct One r = p_mkone(7.5L); SHOW(r.a); }
  { struct Wide w; w.a = 9.5L; w.b = 1.25L; SHOW(p_wide(w)); }
  { struct Wide w = p_mkwide(3.5L, 0.75L); SHOW(w.a); SHOW(w.b); }
  { struct Mixed m; m.i = 3; m.v = 2.5L; SHOW(p_mixedStruct(m)); }

  printf("cmp %d %d %d\n", p_cmp(1.0L, 2.0L), p_cmp(2.0L, 1.0L), p_cmp(1.0L, 1.0L));

  { int o[6];
    p_rel(a, b, o); printf("lt %d %d %d %d %d %d\n", o[0],o[1],o[2],o[3],o[4],o[5]);
    p_rel(b, a, o); printf("gt %d %d %d %d %d %d\n", o[0],o[1],o[2],o[3],o[4],o[5]);
    p_rel(a, a, o); printf("eq %d %d %d %d %d %d\n", o[0],o[1],o[2],o[3],o[4],o[5]); }

  printf("out %.15g %.7g %ld %d %lu %d %d\n",
         p_toDouble(0.125L), (double)p_toFloat(0.125L), p_toLong(-300.75L),
         p_toInt(-70000.5L), p_toULong(1e19L), (int)p_toShort(-300.75L),
         (int)p_toChar(-100.75L));

  SHOW(p_fromDouble(0.5)); SHOW(p_fromFloat(0.25f)); SHOW(p_fromInt(-70000));
  SHOW(p_fromLong(-1234567890123L)); SHOW(p_fromUInt(4000000000u));
  SHOW(p_fromULong(18446744073709551615UL));

  return 0;
}
