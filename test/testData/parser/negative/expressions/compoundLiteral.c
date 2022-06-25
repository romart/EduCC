

struct S {
  int a, b, c;
};

static int z = 38;

struct S2 {
  int a;
  int d;
  int *ptr;
};


struct S s = (struct S) { 10, 20, 30 };

struct S2 s2[] = { { 1, 2, &z }, (struct S2) { .ptr = &z } };



int foo() {
  return (struct S) { .c = 10, .a = 42 }.b;
}

int bar() {
  int y = 0;
  static struct S2 ss[] = { (struct S2) { .ptr = &y } };
  struct S2 ls[] = { (struct S2) { .ptr = &y } };
}
