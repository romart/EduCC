#include <stdio.h>



int main() {

  union { int a; char b[6]; } x;
  x.a = 515;

  if (8 != sizeof(x)) return 1;

  if (3 != x.b[0]) return 2;

  if (2 != x.b[1]) return 3;

  if (0 != x.b[2]) return 4;
  if (0 != x.b[3]) return 5;

  union {int a,b;} x2,y2;
  x2.a=3;
  y2.a=5;
  y2=x2;
  if (3 != y2.a) return 6;

  union {struct {int a,b;} c;} x3,y3;
  x3.c.b=3;
  y3.c.b=5;
  y3=x3;
  if (3 != y3.c.b) return 7;

  union { struct { unsigned char a,b,c,d; }; long e; } x4;
  x4.e=0xdeadbeefl;

  if (0xef != x4.a) return 8;
  if (0xbe != x4.b) return 9;
  if (0xad != x4.c) return 10;
  if (0xde != x4.d) return 11;

  struct { union { int a,b; }; union { int c,d; }; } x5;
  x5.a=3;
  if (3 != x5.b) return 12;
  x5.d=5;
  if (5 != x5.c) return 13;

  printf("OK\n");
  return 0;
}
