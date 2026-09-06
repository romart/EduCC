union U { int a, b; };
struct S { union U u; int c; };
static struct S global = {1, 2};
static union U array[2] = {3, 4};

int main(void) {
  struct S local = {5, 6};
  union U a[2] = {7, 8};
  struct S designated = {.u.a = 9, 10};
  if (global.u.a != 1 || global.c != 2 || array[0].a != 3 || array[1].a != 4) return 1;
  if (local.u.a != 5 || local.c != 6 || a[0].a != 7 || a[1].a != 8) return 2;
  if (designated.u.a != 9 || designated.c != 10) return 3;
  return 0;
}
