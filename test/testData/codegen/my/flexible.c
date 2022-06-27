
struct SX {
  int a, b, c, d, e;
  int flex[];
};

struct SF {
  struct SX sx;
  int g;
};

struct S
{
  unsigned char a,b;
  unsigned char c[2];
};

struct T
{
  unsigned char s[16];
  unsigned char a;
};

struct V
{
  struct S s;
  struct T t;
  unsigned char a;
};

struct W
{
  struct V t;
  struct S s[];
};

extern int printf(const char *__format, ...);

struct W ggw = {{1,2,3,4}, {1,2,3,4,5}};
struct W ggw2 = {{8,9,10,11} {12,32}};

int main() {
  printf("sizeof SX = %d\n", sizeof(struct SX));
  printf("sizeof SF = %d\n", sizeof(struct SF));

  struct SX sx1 = { 1, 2, 3, 4, 5 };
  printf("sx1 = %d\n", sizeof(sx1));

  printf("sizeof W = %d\n", sizeof(struct S));

  struct W gw = {{1,2,3,4}};
  printf("gw = %d\n", sizeof(gw));
  printf("ggw = %d\n", sizeof(ggw));


  if (1 !=  ggw.s[0].a) return 1;
  if (2 != ggw.s[0].b) return 2;
  if (3 != ggw.s[0].c[0]) return 3;
  if (4 != ggw.s[0].c[1]) return 4;
  if (5 != ggw.s[1].a) return 6;
  if (0 != ggw.s[1].b) return 7;
  if (0 != ggw.s[1].c[0]) return 8;
  if (0 != ggw.s[1].c[1]) return 9;

  if (8 !=  ggw2.t.s.a) return 1;
  if (9 != ggw2.t.s.b) return 2;
  if (10 != ggw2.t.s.c[0]) return 3;
  if (11 != ggw2.t.s.c[1]) return 4;
  if (12 != ggw2.s[0].a) return 6;
  if (32 != ggw2.s[0].b) return 7;
  if (0 != ggw2.s[0].c[0]) return 8;
  if (0 != ggw2.s[0].c[1]) return 9;

  return 0;
}
