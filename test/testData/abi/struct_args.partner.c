// The other half of struct_args.c. Each function reads every field of what it
// was passed, so a member that arrived in the wrong register is a wrong number
// rather than a number that happens to be right.

struct FF { float a, b; };
struct FI { float a; int b; };
struct IF { int a; float b; };
struct DD { double a, b; };
struct DI { double a; int b; };
struct ID { int a; double b; };
struct LD_ { long a; double b; };
struct FFF { float a, b, c; };
struct FA { float a[2]; };
struct Nest { struct { double a; } s; double b; };
struct D1 { double a; };
struct F1 { float a; };
struct LL { long a, b; };
struct Ch { char a; double b; };
struct BF { double a; int b:3; };
struct FB { float a; int b:3; };
struct U { union { float a; int b; } u; };
struct ZA { double a; int z[0]; };
struct Big { long a, b, c, d; };

float p_ff(struct FF v) { return v.a * 100.0f + v.b; }
float p_fi(struct FI v) { return v.a * 100.0f + (float)v.b; }
float p_if(struct IF v) { return (float)v.a * 100.0f + v.b; }
double p_dd(struct DD v) { return v.a * 100.0 + v.b; }
double p_di(struct DI v) { return v.a * 100.0 + (double)v.b; }
double p_id(struct ID v) { return (double)v.a * 100.0 + v.b; }
double p_ld(struct LD_ v) { return (double)v.a * 100.0 + v.b; }
float p_fff(struct FFF v) { return v.a * 10000.0f + v.b * 100.0f + v.c; }
float p_fa(struct FA v) { return v.a[0] * 100.0f + v.a[1]; }
double p_nest(struct Nest v) { return v.s.a * 100.0 + v.b; }
double p_d1(struct D1 v) { return v.a; }
float p_f1(struct F1 v) { return v.a; }
double p_ch(struct Ch v) { return (double)v.a * 100.0 + v.b; }
double p_bf(struct BF v) { return v.a * 100.0 + (double)v.b; }
float p_fb(struct FB v) { return v.a * 100.0f + (float)v.b; }
float p_u(struct U v) { return v.u.a; }
double p_za(struct ZA v) { return v.a; }

struct FF p_mkff(float a, float b) { struct FF r; r.a = a; r.b = b; return r; }
struct FI p_mkfi(float a, int b) { struct FI r; r.a = a; r.b = b; return r; }
struct DD p_mkdd(double a, double b) { struct DD r; r.a = a; r.b = b; return r; }
struct FFF p_mkfff(float a, float b, float c) {
  struct FFF r; r.a = a; r.b = b; r.c = c; return r;
}
struct FA p_mkfa(float a, float b) {
  struct FA r; r.a[0] = a; r.a[1] = b; return r;
}
struct Nest p_mknest(double a, double b) {
  struct Nest r; r.s.a = a; r.b = b; return r;
}

double p_ddLate(double a, double b, double c, double d, double e, double f,
                double g, double h, struct DD v) {
  return ((((((a + b) * 2 + c) * 2 + d) * 2 + e) * 2 + f) * 2 + g) * 2 + h +
         v.a * 1000.0 + v.b;
}

float p_ffLate(double a, double b, double c, double d, double e, double f,
               double g, double h, struct FF v) {
  return (float)(a + b + c + d + e + f + g + h) + v.a * 1000.0f + v.b;
}

long p_llSplit(long a, long b, long c, long d, long e, long f, long g,
               struct LL v) {
  return (a + b + c + d + e + f + g) ^ v.a ^ (v.b << 1);
}

double p_ddSplit(long a, long b, long c, long d, long e, long f, long g,
                 struct DD v) {
  return (double)(a + b + c + d + e + f + g) + v.a * 1000.0 + v.b;
}

long double p_ldSplit(long a, long b, long c, long d, long e, long f, long g,
                      long double v) {
  return (long double)(a + b + c + d + e + f + g) + v * 8.0L;
}

long p_llSplit6(long a, long b, long c, long d, long e, long f, struct LL v) {
  return (a + b + c + d + e + f) ^ v.a ^ (v.b << 1);
}

double p_ninth(double a, double b, double c, double d, double e, double f,
               double g, double h, double i, float j) {
  return a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9 +
         (double)j * 10;
}

#include <stdarg.h>

long p_vSplit(int n, ...) {
  va_list ap;
  long acc = 0;
  int i;

  va_start(ap, n);
  for (i = 0; i < 6; ++i) acc += va_arg(ap, long);
  { struct LL v = va_arg(ap, struct LL); acc ^= v.a ^ (v.b << 1); }
  acc += va_arg(ap, long) * 100;
  va_end(ap);

  return acc;
}

long double p_vLdSplit(int n, ...) {
  va_list ap;
  long double acc = 0;
  int i;

  va_start(ap, n);
  for (i = 0; i < 6; ++i) acc += (long double)va_arg(ap, long);
  acc += va_arg(ap, long double) * 8.0L;
  acc += (long double)va_arg(ap, long) * 100.0L;
  va_end(ap);

  return acc;
}

long p_bigLate(long a, long b, long c, long d, long e, long f, long g, long h,
               struct Big v) {
  return (a + b + c + d + e + f + g + h) * 10000 + v.a * 1000 + v.b * 100 +
         v.c * 10 + v.d;
}
