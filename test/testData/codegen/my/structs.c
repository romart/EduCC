#include <stdio.h>


int main() {
  struct {int a; int b;} x;
  x.a=1;
  x.b=2;
  if (1 != x.a) return 1;
  if (2 != x.b) return 2;

  struct {char a; int b; char c;} x2;
  x2.a=1;
  x2.b=2;
  x2.c=3;
  if (1 != x2.a) return 3;
  if (2 != x2.b) return 4;
  if (3 != x2.c) return 5;

  struct {char a; char b;} x3[3];
  char *p = (char*)x3;
  p[0]=0;
  if (0 != x3[0].a) return 6;
  p[1]=1;
  if (1 != x3[0].b) return 7;
  p[2]=2;
  if (2 != x3[1].a) return 8;
  p[3]=3;
  if (3 != x3[1].b) return 9;

  struct {char a[3]; char b[5];} x4;
  char *p2 = (char*)&x4;
  x4.a[0]=6;
  if (6 != p2[0]) return 10;
  x4.b[0]=7;
  if (7 != p2[3]) return 11;

  struct { struct { char b; } a; } x5;
  x5.a.b=6;
  if (6 != x5.a.b) return 12;

  struct {int a;} x6;
  if (4 != sizeof(x6)) return 13;

  struct {int a; int b;} x7;
  if (8 != sizeof(x7)) return 14;
  struct {int a, b;} x8;
  if (8 != sizeof(x8)) return 15;
  struct {int a[3];} x9;
  if (12 != sizeof(x9)) return 16;
  struct {int a;} x10[4];
  if (16 != sizeof(x10)) return 17;
  struct {int a[3];} x11[2];
  if (24 != sizeof(x11)) return 18;
  struct {char a; char b;} x12;
  if (2 != sizeof(x12)) return 19;
  struct {} x13; ;
  if (0 != sizeof(x13)) return 20;
  struct {char a; int b;} x14;
  if (8 != sizeof(x14)) return 21;
  struct {int a; char b;} x15;
  if (8 != sizeof(x15)) return 22;

  struct t {int a; int b;} x16; struct t y2;
  if (8 != sizeof(y2)) return 23;

  struct t2 {char a[2];}; { struct t {char a[4];}; } struct t2 y3;
  if (2 != sizeof(y3)) return 24;

  struct t3 {int x;};
  int t=1;
  struct t3 y4;
  y4.x=2;
  if (3 != t+y4.x) return 25;

  struct t4 {char a;} x17;
  struct t4 *y5 = &x17;

  x17.a=3;
  if (3 != y5->a) return 26;

  y5->a = 42;
  if (42 != x17.a) return 27;

  struct t5 {int a,b;};
  struct t5 x18,y6;
  x18.a=3;
  y6=x18;
  if (3 != y6.a) return y6.a;

  struct t5 x19;
  x19.a=7;
  struct t5 y7;
  struct t5 *z=&y7;
  *z=x19;
  if (7 != y7.a) return 29;

  struct t5 x20; x20.a=7; struct t5 y8, *p3=&x20, *q=&y8;
  *q=*p3;
  if (7 != y8.a) return 30;

  struct t5 x21, y9;
  x21.a=5;
  y9=x21;
  if (5 != y9.a) return 31;

  struct t5 x22,y10;
  x22.a=3;
  y10=x22;
  if (3 != y10.a) return 32;

  struct t5 x23;
  x23.a=7;
  struct t5 y11;
  struct t5 *z2=&y11;
  *z2=x23;
  if (7 != y11.a) return 33;

  struct t5 x24; x24.a=7;
  struct t5 y12, *p4=&x24, *q2=&(y12);
  *q2=*p4;
  if (7 != y12.a) return 34;

  struct t6 {char a, b;} x25, y13;
  x25.a=5;
  y13=x25;
  if (5 != y13.a) return 35;

  struct {char a; long b;} x26;
  if (16 != sizeof(x26)) return 36;

  struct {char a; short b;} x27;
  if (4 != sizeof(x27)) return 37;

  struct foo *bar;
  if (8 != sizeof(bar)) return 38;

  struct T3 *foo; struct T3 {int x;};
  if (4 != sizeof(struct T3)) return 39;

  struct T2 { struct T2 *next; int x; } a; struct T2 b;
  b.x=1;
  a.next=&b;
  if (1 != a.next->x) return 40;

  typedef struct T T; struct T { int x; };
  if (4 != sizeof(T)) return 41;

  struct {int a;} x28={1}, y14={2};
  if (2 != (x28=y14).a) return 42;

  struct {int a;} x29={1}, y15={2};
  if (1 != (1?x29:y15).a) return 43;

  struct {int a;} x30={1}, y16={2};
  if (2 != (0?x30:y16).a) return 44;

  printf("OK\n");
  return 0;
}
