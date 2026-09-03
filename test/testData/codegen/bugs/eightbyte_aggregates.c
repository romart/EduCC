// SysV splits an aggregate of at most sixteen bytes into eightbytes, classifies
// each INTEGER or SSE by what lands in it, and passes and returns one per
// register. The IR backend passed everything over a word on the stack and
// returned it through a hidden pointer instead, which is a different ABI: a
// {double,double} that gcc hands back in xmm0:xmm1 came back through memory,
// and tinycc's abitest failed ret_2double_test and crashed in
// ret_6plus2longlong_test.
//
// What this pins down beyond "the numbers come out right":
//
//   * which register file each eightbyte travels in, including the two mixed
//     orders, which are not each other's mirror - the classes are counted
//     separately, so {double,long} is xmm0 and rax rather than xmm0 and rdx
//   * a trailing partial eightbyte (12 and 13 bytes), where the register holds
//     fewer than eight meaningful bytes
//   * all-or-nothing: an aggregate needing two integer registers with one left
//     goes on the stack entire, and the argument behind it may still find a
//     register. That is what ret_6plus2longlong_test and ret_8plus2double_test
//     are for, and getting it wrong puts half a struct in r9 and the rest in
//     the wrong place.
//   * that an aggregate over sixteen bytes, and one with a long double in it,
//     still go to memory
//
// Expected values confirmed against gcc. What a single-file fixture cannot say
// is whether the two sides agree with the *platform* rather than only with each
// other, so these shapes were also split across a link with gcc, both
// directions, by hand.

struct I8 { int a, b; };
struct I12 { int a, b, c; };
struct I16 { long a, b; };
struct D16 { double a, b; };
struct F8 { float a, b; };
struct F12 { float a, b, c; };
struct M16 { double a; long b; };
struct N16 { long a; double b; };
struct C13 { char c[13]; };
struct Big { long a, b, c, d; };
struct WithLd { long double a; };

static struct I8 mkI8(int a, int b) { struct I8 r; r.a = a; r.b = b; return r; }
static struct I12 mkI12(int n) { struct I12 r; r.a = n; r.b = n + 1; r.c = n + 2; return r; }
static struct I16 mkI16(long a, long b) { struct I16 r; r.a = a; r.b = b; return r; }
static struct D16 mkD16(double a, double b) { struct D16 r; r.a = a; r.b = b; return r; }
static struct F12 mkF12(float n) { struct F12 r; r.a = n; r.b = n + 1; r.c = n + 2; return r; }
static struct M16 mkM16(double a, long b) { struct M16 r; r.a = a; r.b = b; return r; }
static struct N16 mkN16(long a, double b) { struct N16 r; r.a = a; r.b = b; return r; }
static struct Big mkBig(long n) { struct Big r; r.a = n; r.b = n+1; r.c = n+2; r.d = n+3; return r; }

static struct C13 mkC13(int base) {
  struct C13 r;
  int i;
  for (i = 0; i < 13; ++i) r.c[i] = (char)(base + i);
  return r;
}

static long takeI8(struct I8 s) { return s.a * 10 + s.b; }
static long takeI12(struct I12 s) { return s.a * 100 + s.b * 10 + s.c; }
static long takeI16(struct I16 s) { return s.a * 10 + s.b; }
static double takeD16(struct D16 s) { return s.a * 10 + s.b; }
static double takeF8(struct F8 s) { return s.a * 10 + s.b; }
static double takeF12(struct F12 s) { return s.a * 100 + s.b * 10 + s.c; }
static double takeM16(struct M16 s) { return s.a * 10 + s.b; }
static double takeN16(struct N16 s) { return s.a * 10 + s.b; }
static long takeBig(struct Big s) { return s.a + s.b + s.c + s.d; }
static long takeWithLd(struct WithLd s) { return (long)s.a; }

static long takeC13(struct C13 s) {
  long acc = 0;
  int i;
  for (i = 0; i < 13; ++i) acc += s.c[i];
  return acc;
}

// Five integers first, so the two-eightbyte struct has one integer register
// left and needs two: it goes on the stack whole, and 'f' behind it still
// lands in r9.
static long afterFive(long a, long b, long c, long d, long e, struct I16 s, long f, long g) {
  return s.a * 1000 + s.b * 100 + f * 10 + g + a + b + c + d + e;
}

// The same on the SSE side: seven doubles, then a {double,double} that needs
// two xmm registers with one left, then two more doubles that fit.
static double afterSeven(double a, double b, double c, double d, double e, double f,
                         double g, struct D16 s, double h, double i) {
  return s.a * 1000 + s.b * 100 + h * 10 + i + a + b + c + d + e + f + g;
}

// A struct in front of the arguments it must not move, in the middle of the
// register sequence rather than only at its head.
static long interleaved(long a, struct I16 s, long b, struct D16 d, double e) {
  return a * 100000 + s.a * 10000 + s.b * 1000 + b * 100 + (long)d.a * 10 + (long)d.b + (long)e;
}

// A call whose argument is another call's register-returned result, which is
// where the result slot and the outgoing argument have to be told apart.
static long chained(long n) { return takeI16(mkI16(n, n + 1)); }

int main(void) {
  int rc = 0;

  struct I8 i8 = mkI8(1, 2);
  if (i8.a != 1 || i8.b != 2 || takeI8(i8) != 12) rc |= 1 << 0;

  struct I12 i12 = mkI12(1);
  if (i12.a != 1 || i12.b != 2 || i12.c != 3 || takeI12(i12) != 123) rc |= 1 << 1;

  struct I16 i16 = mkI16(3, 4);
  if (i16.a != 3 || i16.b != 4 || takeI16(i16) != 34) rc |= 1 << 2;

  struct D16 d16 = mkD16(3, 4);
  if (d16.a != 3 || d16.b != 4 || takeD16(d16) != 34) rc |= 1 << 3;

  struct F8 f8; f8.a = 3; f8.b = 4;
  if (takeF8(f8) != 34) rc |= 1 << 4;

  struct F12 f12 = mkF12(1);
  if (f12.a != 1 || f12.b != 2 || f12.c != 3 || takeF12(f12) != 123) rc |= 1 << 5;

  struct M16 m16 = mkM16(3, 4);
  if (m16.a != 3 || m16.b != 4 || takeM16(m16) != 34) rc |= 1 << 6;

  struct N16 n16 = mkN16(3, 4);
  if (n16.a != 3 || n16.b != 4 || takeN16(n16) != 34) rc |= 1 << 7;

  struct C13 c13 = mkC13(1);
  if (c13.c[0] != 1 || c13.c[12] != 13 || takeC13(c13) != 91) rc |= 1 << 8;

  struct Big big = mkBig(10);
  if (big.a != 10 || big.d != 13 || takeBig(big) != 46) rc |= 1 << 9;

  struct WithLd ld; ld.a = 7;
  if (takeWithLd(ld) != 7) rc |= 1 << 10;

  if (afterFive(0, 0, 0, 0, 0, i16, 7, 8) != 3478) rc |= 1 << 11;
  if (afterSeven(0, 0, 0, 0, 0, 0, 0, d16, 7, 8) != 3478) rc |= 1 << 12;
  if (interleaved(1, i16, 2, d16, 5) != 134234 + 5) rc |= 1 << 13;
  if (chained(3) != 34) rc |= 1 << 14;

  // Through a function pointer, where the prototype is all the call site has.
  {
    struct I16 (*mk)(long, long) = mkI16;
    long (*take)(struct I16) = takeI16;
    if (take(mk(5, 6)) != 56) rc |= 1 << 15;
  }

  return rc;
}
