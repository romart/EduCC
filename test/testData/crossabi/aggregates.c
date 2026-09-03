// Structs by value and by return across a link between the two backends.
//
// The driver half; see scalars.c for what the pair is for and how it is run.
// One struct per shape real SysV classifies differently: a single eightbyte,
// two integer ones, one whose second is partial, two SSE, the two mixed
// orders, and one too large for either. The IR backend splits an aggregate of
// at most sixteen bytes into eightbytes and passes each in a register of its
// own class; the legacy backend passes anything over a word on the stack. That
// is a deliberate disagreement and this fixture is muted for it - see
// aggregates.muted, which the runner prints on every run and which the runner
// flags the day the two agree again.
//
// The disagreements this can find are the ones no single-backend fixture can:
// a hidden return pointer one side passes and the other reads out of a
// different register, a struct one side leaves on the stack and the other
// rounds into one, a copy whose tail one side does not write.
//
// The exit code is the number of the first check that failed. gcc returns 0.

struct I8  { int a, b; };
struct S12 { int a, b, c; };
struct I16 { long a, b; };
struct D16 { double a, b; };
struct M16 { long a; double b; };
struct N16 { double a; long b; };
struct Big { long a, b, c, d; };
struct Odd { char c[13]; };

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

static void checkD(double got, double want, int id) {
    if (got != want && failures == 0) failures = id;
}

// Defined in aggregates.partner.c, i.e. by the other backend.
long xTakeI8(struct I8 s);
long xTakeS12(struct S12 s);
long xTakeI16(struct I16 s);
double xTakeD16(struct D16 s);
double xTakeM16(struct M16 s);
double xTakeN16(struct N16 s);
long xTakeBig(struct Big s);
long xTakeOdd(struct Odd s);
long xTakeAfterFive(int a, int b, int c, int d, int e, struct I16 s);
long xTakeBeforeStack(int a, int b, int c, int d, struct I16 s, int f);
long xTakeSeveral(struct I8 a, struct D16 b, struct M16 c, struct Big d, int e);
struct I8 xRetI8(int a, int b);
struct S12 xRetS12(int base);
struct I16 xRetI16(long a, long b);
struct D16 xRetD16(double a, double b);
struct M16 xRetM16(long a, double b);
struct Big xRetBig(long base);
struct Odd xRetOdd(int base);
long xScribble(struct I16 s);
long xCallBackStruct(struct I16 (*fn)(long), long v);
int xRoundTripAgg(void);

// Called from the other half.
long dTakeI16(struct I16 s) { return s.a + s.b; }
double dTakeD16(struct D16 s) { return s.a + s.b; }
long dTakeBig(struct Big s) { return s.a + s.b + s.c + s.d; }
long dTakeOdd(struct Odd s) { return s.c[0] + s.c[6] + s.c[12]; }

struct I16 dRetI16(long a, long b) { struct I16 s; s.a = a; s.b = b; return s; }
struct D16 dRetD16(double a, double b) { struct D16 s; s.a = a; s.b = b; return s; }
struct M16 dRetM16(long a, double b) { struct M16 s; s.a = a; s.b = b; return s; }
struct I16 dCallBackTarget(long v) { struct I16 s; s.a = v; s.b = v + 1; return s; }

struct Big dRetBig(long base) {
    struct Big s;
    s.a = base; s.b = base + 1; s.c = base + 2; s.d = base + 3;
    return s;
}

int main(void) {
    struct I8 i8; i8.a = 3; i8.b = 4;
    check(xTakeI8(i8), 7, 1);

    struct S12 s12; s12.a = 1; s12.b = 2; s12.c = 3;
    check(xTakeS12(s12), 6, 2);

    struct I16 i16; i16.a = 100; i16.b = -1;
    check(xTakeI16(i16), 99, 3);

    struct D16 d16; d16.a = 1.5; d16.b = 2.25;
    checkD(xTakeD16(d16), 3.75, 4);

    struct M16 m16; m16.a = 7; m16.b = 0.5;
    checkD(xTakeM16(m16), 7.5, 5);

    struct N16 n16; n16.a = 0.25; n16.b = 9;
    checkD(xTakeN16(n16), 9.25, 6);

    struct Big big; big.a = 1; big.b = 2; big.c = 3; big.d = 4;
    check(xTakeBig(big), 10, 7);

    // 13 bytes: too large for registers and not a multiple of eight, so the
    // copy's tail is the part the two backends could disagree about.
    struct Odd odd;
    for (int i = 0; i < 13; ++i) odd.c[i] = (char)(i + 1);
    check(xTakeOdd(odd), 1 + 7 + 13, 8);

    // Five integer arguments first, so the two-eightbyte struct no longer fits
    // in what is left and goes to the stack whole.
    check(xTakeAfterFive(1, 2, 3, 4, 5, i16), 15 + 99, 9);
    // Four, so it fits exactly, and the trailing int is what spills instead.
    check(xTakeBeforeStack(1, 2, 3, 4, i16, 6), 10 + 99 + 6, 10);

    check(xTakeSeveral(i8, d16, m16, big, 5), 7 + 3 + 7 + 10 + 5, 11);

    // Returns, one per class.
    struct I8 r8 = xRetI8(5, 6);
    check(r8.a + r8.b, 11, 12);

    struct S12 r12 = xRetS12(10);
    check(r12.a + r12.b + r12.c, 33, 13);

    struct I16 r16 = xRetI16(-5, 8);
    check(r16.a + r16.b, 3, 14);

    struct D16 rd = xRetD16(0.5, 0.25);
    checkD(rd.a + rd.b, 0.75, 15);

    struct M16 rm = xRetM16(4, 2.5);
    checkD(rm.a + rm.b, 6.5, 16);

    // Through memory, so the hidden pointer is passed by one backend and
    // written through by the other.
    struct Big rb = xRetBig(10);
    check(rb.a + rb.b + rb.c + rb.d, 46, 17);

    struct Odd ro = xRetOdd(20);
    check(ro.c[0] + ro.c[6] + ro.c[12], 20 + 26 + 32, 18);

    // The callee writes to its own copy; this one has to be untouched.
    i16.a = 100; i16.b = -1;
    check(xScribble(i16), 99, 19);
    check(i16.a + i16.b, 99, 20);

    // A struct-returning function pointer handed across the link.
    check(xCallBackStruct(dCallBackTarget, 3), 3 + 4, 21);

    // And the whole thing in the other direction.
    check(xRoundTripAgg(), 0, 22);

    return failures;
}
