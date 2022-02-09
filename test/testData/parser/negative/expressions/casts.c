
struct S { int a; };
struct X { int b; };

union U { int c; struct S s; };


void test() {
  struct S s;
  struct X x;
  union U u;
  int a;
  int *pa = &a;
  float f;
  int arr[10];
  long long l;
  char c;

  (struct S)s;
  (struct X)s;

  (int)s;
  (int)x;

  (int*)x;
  (int*)s;

  (union U)a;
  (union U)s;
  (union U)x;
  (union U)f;
  (union U)l;
  (union U)c;

  (float)pa;
  (int*)f;

  (struct S)l;

  (float)l;
  (long)f;

  (int [20])pa;
}
