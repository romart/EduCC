
struct S {
  int a;
  unsigned f:10;
  int f2: 20;
};

void test() {
  int a;
  int *pa;
  register int r;

  struct S s, *ps = &s;

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

  &s.f;
  &ps->f2;
}
