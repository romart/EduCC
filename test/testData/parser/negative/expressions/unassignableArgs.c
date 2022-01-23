int foo(int *pa, int *pb);

struct S1 {};
struct S2 {};

int fex(struct S1 s1, struct S2 s2);

int g = 10;

int bar(float f) {
  struct S1 s1;
  struct S2 s2;
  int arr[20];

  foo(g, g);
  foo(f, f);
  foo(&g, f);
  foo(&g, &g);
  foo(&f, &f);
  foo(s1, s2);
  foo(&g, s1);
  foo(&g, s2);
  foo(&s1, &s1);
  foo(&s2, &s2);
  foo(arr, arr);

  fex(f, f);
  fex(g, g);
  fex(&g, &g);
  fex(&f, &f);
  fex(s1, s1);
  fex(s2, s2);
  fex(s2, s1);
  fex(&s1, &s2);
  fex(&s1, &s1);
  fex(&s2, &s2);
  fex(&foo, &foo);
  fex(s1, s2);
  fex(arr, arr);
}
