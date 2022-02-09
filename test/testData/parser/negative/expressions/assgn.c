
int test() {
  int a;
  int *pa;
  const int c;
  const int *pc;
  a++ = a;
  a + a = a;

  &a = a;
  *pa = a;

  --a = a;

  0 = a;

  c = a;

  *pc = a;
}
