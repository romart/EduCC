
struct S { int a; };

void test() {
  int a;
  int *pa;
  register int r;

  struct S s, *ps;

  &a;
  &pa;
  &*pa;

  &(a + a);

  &r;

  &pa[1];

  &(a = a);

  &s.a;
  &ps->a;

  &++a;
  &a--;

  &(float)a;
}
