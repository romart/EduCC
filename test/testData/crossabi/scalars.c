// Scalar arguments and return values across a link between the two backends.
//
// This half is the driver: it calls into scalars.partner.c and is called back
// from it, so one link exercises both directions. The test runner compiles the
// two halves with different backends and then swaps them, which is the point -
// every check here is a question about what the *other* compiler put in the
// register, not about what this one reads out of it.
//
// Narrow types dominate the list because that is where the two disagree by
// construction: the legacy backend moves a whole register per argument, the IR
// backend copies the argument's own width and leaves the bytes above it
// unspecified, exactly as SysV allows. Both are correct in isolation. What is
// being checked is that neither ever reads more of a register than the other
// wrote - see codegen/bugs/narrow_argument_width.c for the bug that says this
// is worth checking on purpose.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <stdarg.h>

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

static void checkD(double got, double want, int id) {
    if (got != want && failures == 0) failures = id;
}

// Defined in scalars.partner.c, i.e. by the other backend.
long xSumNarrow(signed char a, short b, int c, long d,
                unsigned char e, unsigned short f, unsigned int g, unsigned long h);
double xSumFloats(double a, float b, double c, float d, double e,
                  float f, double g, float h, double i, float j);
long xMixedBanks(int a, double b, signed char c, float d, long e, double f,
                 short g, double h, int i, double j, unsigned char k, double l);
long xStackOdd(int a, int b, int c, int d, int e, int f, int g);
long xStackEven(int a, int b, int c, int d, int e, int f, int g, int h);
signed char xRetSChar(int v);
unsigned char xRetUChar(int v);
short xRetShort(int v);
unsigned short xRetUShort(int v);
float xRetFloat(float v);
long double xLongDoubleMix(long double a, double b, long double c);
long xVariadicInts(int n, ...);
double xVariadicDoubles(int n, ...);
long xVariadicMixed(int n, ...);
long xCallBack(long (*fn)(signed char, unsigned short, double), signed char a,
               unsigned short b, double c);
int xRoundTrip(void);

// Called from the other half. Non-static so it links; the whole point is that
// the call is compiled by one backend and the body by the other.
long dNarrowArgs(signed char a, unsigned char b, short c, unsigned short d) {
    return (long)a + b + c + d;
}

double dFloatArgs(float a, double b, float c, double d) {
    return (double)a + b + c + d;
}

short dRetShort(int v) {
    return (short)v;
}

unsigned char dRetUChar(int v) {
    return (unsigned char)v;
}

long dVariadic(int n, ...) {
    va_list ap;
    long sum = 0;
    va_start(ap, n);
    for (int i = 0; i < n; ++i) sum += va_arg(ap, int);
    va_end(ap);
    return sum;
}

long dCallBackTarget(signed char a, unsigned short b, double c) {
    return (long)a + b + (long)c;
}

// Globals rather than literals so nothing is constant-folded into the call.
signed char sc;
unsigned char uc;
short sh;
unsigned short ush;
int i32;
unsigned u32;
long i64;
unsigned long u64;
float f32;
double f64;

int main(void) {
    // Eight integer arguments of six widths: two land on the stack, and every
    // one of the six is a width one backend may have written less of than the
    // other reads.
    sc = -1; sh = -2; i32 = -3; i64 = -4;
    uc = 250; ush = 60000; u32 = 4000000000u; u64 = 5;
    check(xSumNarrow(sc, sh, i32, i64, uc, ush, u32, u64),
          -1 - 2 - 3 - 4 + 250 + 60000 + 4000000000L + 5, 1);

    // Zero in every one of them, so a register left holding the previous
    // call's bits cannot make a check pass by accident.
    sc = 0; sh = 0; i32 = 0; i64 = 0; uc = 0; ush = 0; u32 = 0; u64 = 0;
    check(xSumNarrow(sc, sh, i32, i64, uc, ush, u32, u64), 0, 2);

    // And all-ones in the narrow ones, which is what a sign-extending caller
    // and a zero-extending callee disagree about.
    sc = -1; uc = 255; sh = -1; ush = 65535; i32 = -1; u32 = 0xFFFFFFFFu;
    i64 = 0; u64 = 0;
    check(xSumNarrow(sc, sh, i32, i64, uc, ush, u32, u64),
          -1L - 1 - 1 + 255 + 65535 + 4294967295L, 3);

    // Ten floating-point arguments: two past xmm7, at both widths.
    checkD(xSumFloats(1.5, 2.25f, 4.0, 8.125f, 16.0, 32.5f, 64.0, 128.25f,
                      256.0, 512.5f),
           1.5 + 2.25 + 4.0 + 8.125 + 16.0 + 32.5 + 64.0 + 128.25 + 256.0 + 512.5, 4);

    // Interleaved banks: the two counters run out at different points, and the
    // integer half is narrow throughout.
    check(xMixedBanks(1, 2.0, 3, 4.0f, 5L, 6.0, 7, 8.0, 9, 10.0, 11, 12.0),
          78, 5);

    // An odd and an even number of stack arguments - the padding either way.
    check(xStackOdd(1, 2, 3, 4, 5, 6, 7), 28, 6);
    check(xStackEven(1, 2, 3, 4, 5, 6, 7, 8), 36, 7);

    // Narrow return values, both signs, at the boundary where the sign matters.
    check(xRetSChar(-1), -1, 8);
    check(xRetSChar(200), -56, 9);
    check(xRetUChar(200), 200, 10);
    check(xRetUChar(-1), 255, 11);
    check(xRetShort(-1), -1, 12);
    check(xRetShort(40000), -25536, 13);
    check(xRetUShort(40000), 40000, 14);
    check(xRetUShort(-1), 65535, 15);
    checkD(xRetFloat(2.5f), 2.5, 16);

    // x87 arguments and return value, mixed with an SSE one so the two
    // classes' counters both advance.
    if (xLongDoubleMix(1.0L, 2.0, 4.0L) != 7.0L && failures == 0) failures = 17;

    // Variadic callees: the trailing arguments are promoted by this backend
    // and read by the other, and %al says how many vector registers were used.
    check(xVariadicInts(3, 1, 2, 3), 6, 18);
    sc = -1; uc = 255; sh = -2;
    check(xVariadicInts(3, sc, uc, sh), 252, 19);
    checkD(xVariadicDoubles(3, 1.5, 2.25, 4.125), 7.875, 20);
    f32 = 1.25f;
    checkD(xVariadicDoubles(2, (double)f32, 2.75), 4.0, 21);
    // The doubles are truncated by the callee, hence 1 + 2 + 2 + 4.
    check(xVariadicMixed(4, 1, 2.5, 2, 4.5), 9, 22);

    // A function pointer handed across: the call is compiled here, the
    // indirect call site by the other backend, the body here again.
    check(xCallBack(dCallBackTarget, -1, 65535, 2.75), -1 + 65535 + 2, 23);

    // Everything above, in the other direction: the other half calls back into
    // this one and reports the number of its own first failed check.
    check(xRoundTrip(), 0, 24);

    return failures;
}
