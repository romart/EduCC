struct S { char a[8]; int tail; };
static struct S global = {.a = "abc", .a[1] = 'x', .tail = 7};

int main(void) {
  struct S local = {.a = "abc", .a[1] = 'x', .a[6] = 'y', .tail = 9};
  if (global.a[0] != 'a' || global.a[1] != 'x' || global.a[2] != 'c') return 1;
  if (local.a[0] != 'a' || local.a[1] != 'x' || local.a[2] != 'c') return 2;
  if (global.a[3] || global.a[7] || global.tail != 7) return 3;
  if (local.a[3] || local.a[5] || local.a[6] != 'y' || local.a[7] || local.tail != 9) return 4;
  return 0;
}
