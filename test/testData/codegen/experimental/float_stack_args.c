// Floating-point arguments that run out of SSE registers and have to be pushed.
//
// SysV gives the first eight floating-point arguments xmm0-xmm7 and puts the
// rest in the stack argument area. Selection used to refuse the whole call at
// that point - "there is no 'push xmm'" - which is true of the instruction and
// not of the ABI: the bits come out into a general register with movd/movq and
// that register is what gets pushed, the same shape an aggregate argument
// already used.
//
// Both ends are this backend's, and only one of them was ever refused. A stack
// float *parameter* has always been read out of the frame like any other, so
// every callee below was already built here; it is the calls that were not, and
// they had never been checked against a callee that agrees with them.
//
// What is worth covering is what the two argument counters can get wrong once
// they stop advancing together. The integer and SSE registers run out
// independently, so a call can be past xmm7 with rdi still free and the other
// way round; a stack argument occupies a whole eightbyte however narrow it is;
// the pushes go in reverse so the first one ends up at [rsp]; and an odd number
// of them needs eight bytes of padding to leave rsp where the callee wants it.
//
// Weights rather than a plain sum throughout, so that two arguments arriving
// in each other's slots is a different answer instead of the same one.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <stdarg.h>

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- past xmm7, one width at a time --------

static double tenDoubles(double a, double b, double c, double d, double e,
                         double f, double g, double h, double i, double j) {
    return a * 1 + b * 2 + c * 3 + d * 4 + e * 5
         + f * 6 + g * 7 + h * 8 + i * 9 + j * 10;
}

// Nine, so the argument area is one eightbyte and the call needs the padding
// slot that keeps rsp 16-aligned.
static double nineDoubles(double a, double b, double c, double d, double e,
                          double f, double g, double h, double i) {
    return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9;
}

// A float is four bytes in an eight-byte slot. The bytes above it are nobody's,
// which is the whole of what makes this different from the double case.
static float tenFloats(float a, float b, float c, float d, float e,
                       float f, float g, float h, float i, float j) {
    return a * 1 + b * 2 + c * 3 + d * 4 + e * 5
         + f * 6 + g * 7 + h * 8 + i * 9 + j * 10;
}

// The ninth argument straight back out, for a bit-exact round trip through a
// stack slot: every value above is a small whole number, and a push that
// carried only half the bits would still get those right whenever the half it
// dropped was zero.
static double ninthOf(double a, double b, double c, double d, double e,
                      double f, double g, double h, double i, double j) {
    return i;
}

// Alternating widths past the eighth, so a slot laid out for the wrong one
// leaves the next argument reading half of it.
static double mixedWidths(double a, double b, double c, double d, double e,
                          double f, double g, double h,
                          float i, double j, float k, double l) {
    return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8
         + i * 9 + j * 10 + k * 11 + l * 12;
}

// -------- the two counters running out at different points --------

// Ten integers and ten doubles: the sixth integer is the last in a register and
// the eighth double is, so the stack area holds four integers and two doubles
// interleaved in declaration order rather than grouped by class.
static double interleaved(int i1, double d1, int i2, double d2,
                          int i3, double d3, int i4, double d4,
                          int i5, double d5, int i6, double d6,
                          int i7, double d7, int i8, double d8,
                          int i9, double d9, int i10, double d10) {
    double ints = i1 * 1 + i2 * 2 + i3 * 3 + i4 * 4 + i5 * 5
                + i6 * 6 + i7 * 7 + i8 * 8 + i9 * 9 + i10 * 10;
    double dbls = d1 * 1 + d2 * 2 + d3 * 3 + d4 * 4 + d5 * 5
                + d6 * 6 + d7 * 7 + d8 * 8 + d9 * 9 + d10 * 10;
    return ints * 100 + dbls;
}

typedef struct { long a, b, c; } Big;

// A struct too big for a register is pushed too, so this call builds its
// argument area out of both kinds of push. The struct is declared between the
// eighth and ninth double on purpose: get the order wrong and the doubles land
// inside it.
static double structAmongFloats(double a, double b, double c, double d,
                                double e, double f, double g, double h,
                                Big s, double i, double j) {
    return a * 1 + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8
         + i * 9 + j * 10 + s.a * 100 + s.b * 200 + s.c * 300;
}

// The hidden buffer pointer for a large return takes an integer register ahead
// of every declared parameter, which is a counter the float side must not see.
static Big bigFromFloats(double a, double b, double c, double d, double e,
                         double f, double g, double h, double i, double j) {
    Big r;
    r.a = (long)(a + b + c + d + e);
    r.b = (long)(f + g + h);
    r.c = (long)(i * 10 + j * 100);
    return r;
}

// -------- variadic, where the stack area is also the overflow area --------

static double vaDoubles(int n, ...) {
    va_list ap;
    double sum = 0;
    int idx;

    va_start(ap, n);
    for (idx = 1; idx <= n; ++idx) {
        sum += va_arg(ap, double) * idx;
    }
    va_end(ap);

    return sum;
}

// -------- the stack pointer afterwards --------

// Every call above adjusts rsp and puts it back. Nothing observes that inside
// one call, so this makes a call with an odd argument count - the padded case -
// and then reads a local declared before it and calls again.
static int stillStanding(void) {
    int before = 1234;
    double odd = nineDoubles(1, 2, 3, 4, 5, 6, 7, 8, 9);
    double even = tenDoubles(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    return before == 1234 && odd == 285 && even == 385;
}

int main(void) {
    Big s;
    Big r;

    check(tenDoubles(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385, 1);
    check(nineDoubles(1, 2, 3, 4, 5, 6, 7, 8, 9) == 285, 2);
    check(tenFloats(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385, 3);

    // The ninth and tenth carry the weight, so a dropped push is visible.
    check(tenDoubles(0, 0, 0, 0, 0, 0, 0, 0, 1, 0) == 9, 4);
    check(tenDoubles(0, 0, 0, 0, 0, 0, 0, 0, 0, 1) == 10, 5);

    // 1*1 + ... + 12*12
    check(mixedWidths(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) == 650, 6);

    check(interleaved(1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                      6, 6, 7, 7, 8, 8, 9, 9, 10, 10) == 385 * 100 + 385, 7);

    s.a = 1; s.b = 2; s.c = 3;
    check(structAmongFloats(1, 2, 3, 4, 5, 6, 7, 8, s, 9, 10)
          == 385 + 100 + 400 + 900, 8);

    r = bigFromFloats(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    check(r.a == 15, 9);
    check(r.b == 21, 10);
    check(r.c == 1090, 11);

    check(vaDoubles(10, 1.0, 2.0, 3.0, 4.0, 5.0,
                    6.0, 7.0, 8.0, 9.0, 10.0) == 385, 12);
    check(vaDoubles(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0) == 285, 13);

    check(stillStanding(), 14);

    // A negative in a stack slot: the sign is bit 63 of a double and bit 31 of
    // a float, so between them they cover both halves of what gets pushed.
    check(tenDoubles(1, 2, 3, 4, 5, 6, 7, 8, -1, -2) == 175, 15);
    check(tenFloats(1, 2, 3, 4, 5, 6, 7, 8, -1, -2) == 175, 16);

    // Bits set in both halves of the mantissa, returned rather than summed, so
    // nothing rounds on the way back.
    check(ninthOf(0, 0, 0, 0, 0, 0, 0, 0, 1.0000000000000002, 0)
          == 1.0000000000000002, 17);
    check(ninthOf(0, 0, 0, 0, 0, 0, 0, 0, -0.30000000000000004, 0)
          == -0.30000000000000004, 18);

    return failures;
}
