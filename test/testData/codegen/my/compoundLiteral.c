

struct S {
  int a, b, c;
};

static int z = 38;

struct S2 {
  int a;
  int d;
  int *ptr;
};


struct S s = (struct S) { 1, 2, 3 };

struct S2 s2[] = { { 1, 2, &z }, (struct S2) { .ptr = &z } };

int foo() {
  return (struct S) { .c = 10, .a = 42 }.b;
}

int test1() {
  int y = 42;
  struct S2 ls[] = { (struct S2) { .ptr = &y } };
  if (*ls[0].ptr != 42) return 1;
  y = 24;
  if (*ls[0].ptr != 24) return 2;

  return 0;
}

int main() {
  int t1 = test1();
  if (t1 != 0) return t1 + 10;

  int f = foo();

  if (f) return f + 20;

  if (s.c != 3) return s.c + 30;

  int st1 = *s2[0].ptr;

  if (st1 != 38) return 100 + st1;

  z = 55;

  int st2 = *s2[0].ptr;

  if (st2 != 55) return 200 + st2;

  return 0;
}
