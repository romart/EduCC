#include <stdio.h>

static int x = 10;

static int *px = &x;


int foo(int x, int y) {
  return x + y;
}

static int (*fptr)(int, int) = &foo;

struct S {
  int a, b, c;
  char x, y, z;
  long f, p, q;
};

int diff = ((char*)&(((struct S *)0)->f) - (char*)0);

int main() {

  if (diff != 16) return 1;

  *px = 42;

  if (x != 42) return 2;


  if (fptr(42, 24) != 66) return 3;

  return 0;
}
