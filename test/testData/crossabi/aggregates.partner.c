// The other half of aggregates.c, compiled by the other backend. See that file.

struct I8  { int a, b; };
struct S12 { int a, b, c; };
struct I16 { long a, b; };
struct D16 { double a, b; };
struct M16 { long a; double b; };
struct N16 { double a; long b; };
struct Big { long a, b, c, d; };
struct Odd { char c[13]; };

// Defined in aggregates.c, i.e. by the backend this one is linked against.
long dTakeI16(struct I16 s);
double dTakeD16(struct D16 s);
long dTakeBig(struct Big s);
long dTakeOdd(struct Odd s);
struct I16 dRetI16(long a, long b);
struct D16 dRetD16(double a, double b);
struct M16 dRetM16(long a, double b);
struct Big dRetBig(long base);

long xTakeI8(struct I8 s) { return s.a + s.b; }
long xTakeS12(struct S12 s) { return s.a + s.b + s.c; }
long xTakeI16(struct I16 s) { return s.a + s.b; }
double xTakeD16(struct D16 s) { return s.a + s.b; }
double xTakeM16(struct M16 s) { return (double)s.a + s.b; }
double xTakeN16(struct N16 s) { return s.a + (double)s.b; }
long xTakeBig(struct Big s) { return s.a + s.b + s.c + s.d; }
long xTakeOdd(struct Odd s) { return s.c[0] + s.c[6] + s.c[12]; }

long xTakeAfterFive(int a, int b, int c, int d, int e, struct I16 s) {
    return (long)a + b + c + d + e + s.a + s.b;
}

long xTakeBeforeStack(int a, int b, int c, int d, struct I16 s, int f) {
    return (long)a + b + c + d + s.a + s.b + f;
}

long xTakeSeveral(struct I8 a, struct D16 b, struct M16 c, struct Big d, int e) {
    return (long)(a.a + a.b) + (long)(b.a + b.b) + (long)((double)c.a + c.b)
         + (d.a + d.b + d.c + d.d) + e;
}

struct I8 xRetI8(int a, int b) { struct I8 s; s.a = a; s.b = b; return s; }

struct S12 xRetS12(int base) {
    struct S12 s;
    s.a = base; s.b = base + 1; s.c = base + 2;
    return s;
}

struct I16 xRetI16(long a, long b) { struct I16 s; s.a = a; s.b = b; return s; }
struct D16 xRetD16(double a, double b) { struct D16 s; s.a = a; s.b = b; return s; }
struct M16 xRetM16(long a, double b) { struct M16 s; s.a = a; s.b = b; return s; }

struct Big xRetBig(long base) {
    struct Big s;
    s.a = base; s.b = base + 1; s.c = base + 2; s.d = base + 3;
    return s;
}

struct Odd xRetOdd(int base) {
    struct Odd s;
    for (int i = 0; i < 13; ++i) s.c[i] = (char)(base + i);
    return s;
}

// Writes to its own copy of the parameter; the caller's has to be untouched.
long xScribble(struct I16 s) {
    long sum = s.a + s.b;
    s.a = 0;
    s.b = 0;
    return sum;
}

long xCallBackStruct(struct I16 (*fn)(long), long v) {
    struct I16 s = fn(v);
    return s.a + s.b;
}

// The mirror image of aggregates.c's main. Returns the number of its first
// failed check, 0 if none.
int xRoundTripAgg(void) {
    struct I16 i16; i16.a = 100; i16.b = -1;
    if (dTakeI16(i16) != 99) return 1;

    struct D16 d16; d16.a = 1.5; d16.b = 2.25;
    if (dTakeD16(d16) != 3.75) return 2;

    struct Big big; big.a = 1; big.b = 2; big.c = 3; big.d = 4;
    if (dTakeBig(big) != 10) return 3;

    struct Odd odd;
    for (int i = 0; i < 13; ++i) odd.c[i] = (char)(i + 1);
    if (dTakeOdd(odd) != 1 + 7 + 13) return 4;

    struct I16 r16 = dRetI16(-5, 8);
    if (r16.a + r16.b != 3) return 5;

    struct D16 rd = dRetD16(0.5, 0.25);
    if (rd.a + rd.b != 0.75) return 6;

    struct M16 rm = dRetM16(4, 2.5);
    if ((double)rm.a + rm.b != 6.5) return 7;

    struct Big rb = dRetBig(10);
    if (rb.a + rb.b + rb.c + rb.d != 46) return 8;

    return 0;
}
