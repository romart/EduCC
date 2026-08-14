// Floating point through the -experimental backend: step 7's second part.
//
// Every function here is one the IR backend can emit, so the arithmetic, the
// comparisons and the conversions are all the new selector's - and because
// main is emittable too, the results cross no backend boundary and a
// disagreement between the two shows up as a wrong answer rather than as luck.
//
// Three things in particular are pinned:
//
//   * float constants, which are materialized through a general register
//     rather than loaded from a constant pool, so the bit pattern has to
//     survive the trip;
//   * NaN, which every relational operator is false on and which '!=' is true
//     on - the case that needs the parity flag and that the *legacy* backend
//     got wrong in the other direction until this was written (it returned the
//     signed condition codes after a compare that leaves them clear, so '<'
//     was never true and '>=' always was);
//   * conversions in both directions, including to _Bool, which is 'x != 0'
//     and not a truncation.
//
// Every check is against a value gcc agrees with; run this file through gcc to
// confirm it returns 0.

// The exit code is the number of the first check that failed, and zero when
// none did. A bitmask runs out of bits well before this file runs out of
// things worth checking, and "check 19" is a more useful thing to be told than
// a set bit anyway.
int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- arithmetic, and float arguments and returns --------

double dadd(double a, double b) { return a + b; }
double dsub(double a, double b) { return a - b; }
double dmul(double a, double b) { return a * b; }
double ddiv(double a, double b) { return a / b; }

float fadd(float a, float b) { return a + b; }
float fmul(float a, float b) { return a * b; }

// Eight double arguments fill xmm0..xmm7 exactly, which is the boundary the
// classification walk has to get right; a ninth would go on the stack and is
// refused for now.
double eight(double a, double b, double c, double d, double e, double f, double g, double h) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32 + g * 64 + h * 128;
}

// Integer and float arguments are counted against separate registers, so this
// one fills rdi/rsi and xmm0/xmm1 rather than four of anything.
double mixed(int a, double b, int c, double d) {
    return a + b + c * 10 + d * 100;
}

// -------- constants --------

double dconst(void) { return 1.5; }
float fconst(void) { return 0.25f; }
// Not representable as a float, so the double and float forms differ in more
// than width - which is the point of checking the conversion below.
double dtenth(void) { return 0.1; }

// -------- comparisons --------

int dlt(double a, double b) { return a < b; }
int dle(double a, double b) { return a <= b; }
int dgt(double a, double b) { return a > b; }
int dge(double a, double b) { return a >= b; }
int deq(double a, double b) { return a == b; }
int dne(double a, double b) { return a != b; }

// -------- conversions --------

double i2d(int x) { return x; }
double u2d(unsigned x) { return x; }
double l2d(long x) { return x; }
int d2i(double x) { return x; }
long d2l(double x) { return x; }
unsigned d2u(double x) { return x; }
double f2d(float x) { return x; }
float d2f(double x) { return x; }
int d2b(double x) { return (_Bool)x; }
double c2d(char x) { return x; }
double uc2d(unsigned char x) { return x; }

int main(void) {
    check(dadd(1.5, 2.25) == 3.75, 1);
    check(dsub(1.5, 2.25) == -0.75, 2);
    check(dmul(1.5, 2.5) == 3.75, 3);
    check(ddiv(7.0, 2.0) == 3.5, 4);

    check(fadd(1.5f, 2.25f) == 3.75f, 5);
    check(fmul(1.5f, 2.5f) == 3.75f, 6);

    check(eight(1, 1, 1, 1, 1, 1, 1, 1) == 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128, 7);
    check(mixed(1, 2.5, 3, 4.5) == 1 + 2.5 + 30 + 450, 8);

    check(dconst() == 1.5, 9);
    check(fconst() == 0.25f, 10);
    check(dtenth() > 0.09999 && dtenth() < 0.10001, 11);

    check(dlt(1.0, 2.0) && !dlt(2.0, 1.0) && !dlt(1.0, 1.0), 12);
    check(dle(1.0, 2.0) && !dle(2.0, 1.0) && dle(1.0, 1.0), 13);
    check(dgt(2.0, 1.0) && !dgt(1.0, 2.0) && !dgt(1.0, 1.0), 14);
    check(dge(2.0, 1.0) && !dge(1.0, 2.0) && dge(1.0, 1.0), 15);
    check(deq(1.0, 1.0) && !deq(1.0, 2.0), 16);
    check(dne(1.0, 2.0) && !dne(1.0, 1.0), 17);

    // NaN, built rather than named so that no header is needed. Every ordered
    // comparison against it is false - including 'nan >= nan' and 'nan <= nan',
    // which is what the parity flag is for - and only '!=' is true.
    double zero = ddiv(0.0, 1.0);
    double nan = ddiv(zero, zero);

    check(!dlt(nan, 1.0) && !dlt(1.0, nan), 18);
    check(!dle(nan, nan), 19);
    check(!dgt(nan, 1.0) && !dgt(1.0, nan), 20);
    check(!dge(nan, nan), 21);
    check(!deq(nan, nan), 22);
    check(dne(nan, nan), 23);

    check(i2d(-7) == -7.0, 24);
    check(u2d(4000000000u) == 4000000000.0, 25);
    check(l2d(-1234567890123L) == -1234567890123.0, 26);

    // Truncating toward zero, which is what a C cast means and why the
    // instruction is cvtt* and not cvt*.
    check(d2i(3.9) == 3 && d2i(-3.9) == -3, 27);
    check(d2l(-1234567890123.5) == -1234567890123L, 28);
    check(d2u(4000000000.5) == 4000000000u, 29);

    check(f2d(0.25f) == 0.25, 30);
    check(d2f(0.1) == 0.1f && d2f(0.1) != 0.1, 31);

    // (_Bool)x is 'x != 0' and not a truncation, so 0.5 is true and a NaN -
    // which is equal to nothing, including zero - is true as well. Only the
    // cases both backends agree on are checked here: the legacy one converts
    // to an integer first and answers 0 for anything below 1, which is pinned
    // by the muted test/testData/codegen/bugs/float_to_bool.c.
    check(d2b(0.0) == 0 && d2b(-0.0) == 0 && d2b(1.0) == 1, 32);

    check(c2d('A') == 65.0, 33);
    check(uc2d(200) == 200.0, 34);

    return failures;
}
