struct S { int a, b; };
union U { int a, b; };
struct S global_struct = 1;
union U global_union = 1;

void test(void) {
  struct S local_struct = 1;
  union U local_union = 1;
}
