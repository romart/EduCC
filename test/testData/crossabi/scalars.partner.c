// The other half of scalars.c, compiled by the other backend. See that file.

#include <stdarg.h>

// Defined in scalars.c, i.e. by the backend this one is linked against.
long dNarrowArgs(signed char a, unsigned char b, short c, unsigned short d);
double dFloatArgs(float a, double b, float c, double d);
short dRetShort(int v);
unsigned char dRetUChar(int v);
long dVariadic(int n, ...);

long xSumNarrow(signed char a, short b, int c, long d,
                unsigned char e, unsigned short f, unsigned int g, unsigned long h) {
    return (long)a + b + c + d + e + f + g + h;
}

double xSumFloats(double a, float b, double c, float d, double e,
                  float f, double g, float h, double i, float j) {
    return a + b + c + d + e + f + g + h + i + j;
}

long xMixedBanks(int a, double b, signed char c, float d, long e, double f,
                 short g, double h, int i, double j, unsigned char k, double l) {
    return (long)a + (long)b + c + (long)d + e + (long)f + g + (long)h + i
         + (long)j + k + (long)l;
}

long xStackOdd(int a, int b, int c, int d, int e, int f, int g) {
    return (long)a + b + c + d + e + f + g;
}

long xStackEven(int a, int b, int c, int d, int e, int f, int g, int h) {
    return (long)a + b + c + d + e + f + g + h;
}

signed char xRetSChar(int v) { return (signed char)v; }
unsigned char xRetUChar(int v) { return (unsigned char)v; }
short xRetShort(int v) { return (short)v; }
unsigned short xRetUShort(int v) { return (unsigned short)v; }
float xRetFloat(float v) { return v; }

long double xLongDoubleMix(long double a, double b, long double c) {
    return a + (long double)b + c;
}

long xVariadicInts(int n, ...) {
    va_list ap;
    long sum = 0;
    va_start(ap, n);
    for (int i = 0; i < n; ++i) sum += va_arg(ap, int);
    va_end(ap);
    return sum;
}

double xVariadicDoubles(int n, ...) {
    va_list ap;
    double sum = 0.0;
    va_start(ap, n);
    for (int i = 0; i < n; ++i) sum += va_arg(ap, double);
    va_end(ap);
    return sum;
}

// Alternating banks, so the callee walks both save areas.
long xVariadicMixed(int n, ...) {
    va_list ap;
    long sum = 0;
    va_start(ap, n);
    for (int i = 0; i < n; i += 2) {
        sum += va_arg(ap, int);
        sum += (long)va_arg(ap, double);
    }
    va_end(ap);
    return sum;
}

long xCallBack(long (*fn)(signed char, unsigned short, double), signed char a,
               unsigned short b, double c) {
    return fn(a, b, c);
}

// The mirror image of scalars.c's main: the calls are compiled here and the
// bodies over there. Returns the number of its first failed check, 0 if none,
// which is what that main checks.
int xRoundTrip(void) {
    static signed char sc;
    static unsigned char uc;
    static short sh;
    static unsigned short ush;
    static float f32;

    sc = -1; uc = 255; sh = -2; ush = 65535;
    if (dNarrowArgs(sc, uc, sh, ush) != -1L + 255 - 2 + 65535) return 1;

    sc = 0; uc = 0; sh = 0; ush = 0;
    if (dNarrowArgs(sc, uc, sh, ush) != 0) return 2;

    f32 = 1.25f;
    if (dFloatArgs(f32, 2.5, 4.125f, 8.0) != 15.875) return 3;

    if (dRetShort(-1) != -1) return 4;
    if (dRetShort(40000) != -25536) return 5;
    if (dRetUChar(200) != 200) return 6;
    if (dRetUChar(-1) != 255) return 7;

    sc = -1; uc = 255;
    if (dVariadic(2, sc, uc) != 254) return 8;
    if (dVariadic(3, 1, 2, 3) != 6) return 9;

    return 0;
}
