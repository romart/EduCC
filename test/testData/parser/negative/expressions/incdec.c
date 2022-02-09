
struct S { int a; };

void test() {
  int a; int b;

  const int c;

  int ar[10];
  int *pa;

  float f;

  struct S s, *ps;

  --a;
  ++b;
  a--;
  b++;

  f++;
  f--;
  --f;
  ++f;

  pa++;
  pa--;
  ++pa;
  --pa;

  ++s;
  --s;
  s++;
  s--;

  ar++;
  ar--;
  --ar;
  ++ar;

  c++;
  c--;
  ++c;
  --c;

  s.a++;
  s.a--;
  ++s.a;
  --s.a;

  ps->a++;
  ps->a--;
  ++ps->a;
  --ps->a;

  *pa++;
  *pa--;
  --*pa;
  ++*pa;
  *--pa;
  *++pa;

  (*pa)++;
  (*pa)--;
  --(*pa);
  ++(*pa);

  (a + b)++;
  --(a * b);

  &a++;
  --&a;
  (&a)++;
}
