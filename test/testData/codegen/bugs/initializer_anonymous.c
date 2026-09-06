struct S { union { int a, b; }; int c; };
struct T { struct { int x, y; }; int z; };
static struct S global = {1, 2};
static struct S designated = {.b = 3, 4};

int main(void) {
  struct S local = {5, 6};
  struct S braced = {{7}, 8};
  struct S changed = {.a = 1, .b = 9, .c = 10};
  struct T nested = {.x = 11, 12, 13};
  if (global.a != 1 || global.c != 2 || designated.b != 3 || designated.c != 4) return 1;
  if (local.a != 5 || local.c != 6 || braced.a != 7 || braced.c != 8) return 2;
  if (changed.b != 9 || changed.c != 10) return 3;
  if (nested.x != 11 || nested.y != 12 || nested.z != 13) return 4;
  return 0;
}
