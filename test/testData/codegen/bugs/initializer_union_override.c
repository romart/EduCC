union U { int a, b; struct { int x, y; } s; };
static union U global = {.a = 1, .b = 2};
static union U reverse = {.b = 1, .a = 3};

int main(void) {
  union U local = {.a = 1, .b = 2};
  union U back = {.s = {5, 6}, .a = 7, .s.y = 8};
  union U partial = {.s = {5, 6}, .s.y = 8};
  union U replaced = {.s = {5, 6}, .s = {8}};
  if (global.b != 2 || reverse.a != 3 || local.b != 2) return 1;
  if (back.s.x || back.s.y != 8) return 2;
  if (partial.s.x != 5 || partial.s.y != 8) return 3;
  if (replaced.s.x != 8 || replaced.s.y) return 4;
  return 0;
}
