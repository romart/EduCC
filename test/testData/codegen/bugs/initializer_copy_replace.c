struct S { int a, b; };
union U { int a; struct S s; };

int main(void) {
  struct S x = {1, 2};
  struct S a[1] = {[0] = x, [0] = {3}};
  struct S b[1] = {[0] = {3}, [0] = x};
  union U u = {.s = {1, 2}};
  union U c[1] = {[0] = u, [0] = {.s = {4}}};
  if (a[0].a != 3 || a[0].b != 0) return 1;
  if (b[0].a != 1 || b[0].b != 2) return 2;
  if (c[0].s.a != 4 || c[0].s.b != 0) return 3;
  return 0;
}
