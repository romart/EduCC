#include <stdio.h>
#include <stdarg.h>

int ret3(void) {
  return 3;
  return 5;
}

int add2(int x, int y) {
  return x + y;
}

int sub2(int x, int y) {
  return x - y;
}

int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

int addx(int *x, int y) {
  return *x + y;
}

int sub_char(char a, char b, char c) {
  return a - b - c;
}

int fib(int x) {
  if (x<=1)
    return 1;
  return fib(x-1) + fib(x-2);
}

int sub_long(long a, long b, long c) {
  return a - b - c;
}

int sub_short(short a, short b, short c) {
  return a - b - c;
}

int g1;

int *g1_ptr(void) { return &g1; }
char int_to_char(int x) { return x; }

int div_long(long a, long b) {
  return a / b;
}

static int static_fn(void) { return 3; }

int param_decay(int x[]) { return x[0]; }

int counter() {
  static int i;
  static int j = 1+1;
  return i++ + j++;
}

void ret_none() {
  return;
}

char char_fn();
short short_fn();

unsigned char uchar_fn();
unsigned short ushort_fn();

int add_all(int n, ...) {
  va_list args;
  va_start(args, n);

  int i;
  int r = 0;
  for (i = 0; i < n; ++i) {
      r += va_arg(args, int);
  }
  return r;
}


int sprintf(char *buf, char *fmt, ...);
int vsprintf(char *buf, char *fmt, va_list ap);


//char *fmt(char *buf, char *fmt, ...) {

//  va_list ap;
//  va_start(ap, fmt);

//  vsprintf(buf, fmt, ap);
//}


float add_float3(float x, float y, float z) {
  return x + y + z;
}

double add_double3(double x, double y, double z) {
  return x + y + z;
}

int (*fnptr(int (*fn)(int n, ...)))(int, ...) {
  return fn;
}

int param_decay2(int x()) { return x(); }

int many_args1(int a, int b, int c, int d, int e, int f, int g, int h) {
  return g / h;
}

double many_args2(double a, double b, double c, double d, double e,
                  double f, double g, double h, double i, double j) {
  return i / j;
}

int many_args3(int a, double b, int c, int d, double e, int f,
               double g, int h, double i, double j, double k,
               double l, double m, int n, int o, double p) {
  return o / p;
}

typedef struct { int a,b; short c; char d; } Ty4;
typedef struct { int a; float b; double c; } Ty5;
typedef struct { unsigned char a[3]; } Ty6;
typedef struct { long a, b, c; } Ty7;

int struct_test4(Ty4 x, int n);
int struct_test5(Ty5 x, int n);
int struct_test6(Ty6 x, int n);
int struct_test7(Ty7 x, int n);

int struct_test14(Ty4 x, int n) {
  switch (n) {
  case 0: return x.a;
  case 1: return x.b;
  case 2: return x.c;
  default: return x.d;
  }
}

int struct_test15(Ty5 x, int n) {
  switch (n) {
  case 0: return x.a;
  case 1: return x.b;
  default: return x.c;
  }
}

typedef struct { unsigned char a[10]; } Ty20;
typedef struct { unsigned char a[20]; } Ty21;

Ty4 struct_test34(void) {
  Ty4 r = {10, 20, 30, 40};
  return r;
}

Ty5 struct_test35(void) {
  Ty5 r = {10, 20, 30};
  return r;
}

Ty6 struct_test36(void) {
  Ty6 r = {10, 20, 30};
  return r;
}

Ty20 struct_test37(void) {
  Ty20 r = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
  return r;
}

Ty21 struct_test38(void) {
  Ty21 r = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
  return r;
}

//double to_double(long double x) {
//  return x;
//}

//long double to_ldouble(int x) {
//  return x;
//}

int main() {
  if (3 != ret3()) return 1;
  if (8 != add2(3, 5)) return 2;
  if (2 != sub2(5, 3)) return 3;
  if (21 != add6(1,2,3,4,5,6)) return 4;
  if (66 != add6(1,2,add6(3,4,5,6,7,8),9,10,11)) return 5;
  if (136 != add6(1,2,add6(3,add6(4,5,6,7,8,9),10,11,12,13),14,15,16)) return 6;

  if (7 != add2(3,4)) return 7;
  if (1 != sub2(4,3)) return 8;
  if (55 != fib(9)) return 9;

  if (1 != sub_char(7, 3, 3)) return 10;

  if (1 != sub_long(7, 3, 3)) return 11;
  if (1 != sub_short(7, 3, 3)) return 12;

  g1 = 3;

  if (3 != *g1_ptr()) return 13;
  if (5 != int_to_char(261)) return int_to_char(261);
  if (5 != int_to_char(261)) return 15;
  if (-5 != div_long(-10, 2)) return 16;

  if (3 != static_fn()) return 17;

  int x[2]; x[0]=3;
  if (3 != param_decay(x)) return 18;

  if (2 != counter()) return 19;
  if (4 != counter()) return 20;
  if (6 != counter()) return 21;

  ret_none();

  if (7.5f != add_float3(2.5, 2.5, 2.5)) return 22;
  if (7.5 != add_double3(2.5, 2.5, 2.5)) return 23;


  if (5 != (add2)(2,3)) return 24;
  if (5 != (&add2)(2,3)) return 25;
  int (*fn)(int,int) = add2;
  if (7 != fn(2,5)) return 26;

  if (3 != param_decay2(ret3)) return 28;

  if (4 != many_args1(1,2,3,4,5,6,40,10)) return 29;
  if (4 != many_args2(1,2,3,4,5,6,7,8,40,10)) return 30;
  if (8 != many_args3(1,2,3,4,5,6,7,8,9,10,11,12,13,14,80,10)) return 31;

  Ty4 x4 ={10,20,30,40};

  if (10 != struct_test14(x4, 0)) return 32;
  if (20 != struct_test14(x4, 1)) return 33;
  if (30 != struct_test14(x4, 2)) return 34;
  if (40 != struct_test14(x4, 3)) return 35;

  Ty5 x5={10,20,30};
  if (10 != struct_test15(x5, 0)) return 36;
  if (20 != struct_test15(x5, 1)) return 37;
  if (30 != struct_test15(x5, 2)) return 38;


  if (10 != struct_test34().a) return 39;
  if (20 != struct_test34().b) return 40;
  if (30 != struct_test34().c) return 41;
  if (40 != struct_test34().d) return 42;

  if (10 != struct_test35().a) return 43;
  if (20 != struct_test35().b) return 44;
  if (30 != struct_test35().c) return 45;

  if (10 != struct_test36().a[0]) return 46;
  if (20 != struct_test36().a[1]) return 47;
  if (30 != struct_test36().a[2]) return 48;

  if (10 != struct_test37().a[0]) return 49;
  if (60 != struct_test37().a[5]) return 50;
  if (100 != struct_test37().a[9]) return 51;

  if (1 != struct_test38().a[0]) return 52;
  if (5 != struct_test38().a[4]) return 53;
  if (10 != struct_test38().a[9]) return 54;
  if (15 != struct_test38().a[14]) return 55;
  if (20 != struct_test38().a[19]) return 56;

  if (5 != (***add2)(2,3)) return 57;

//  if (1 != (to_double(3.5) == 3.5)) return 58;
//  if (0 != (to_double(3.5) == 3)) return 59;

  if (6 != add_all(3,1,2,3)) return 60;
  if (5 != add_all(4,1,2,3,-1)) return 61;

  if (306 != fnptr(add_all)(4, 1, 2, 3, 300)) return 62;

  printf("OK\n");

  return 0;
}

