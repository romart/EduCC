// 6.5.4p2 exempts a cast to void from the scalar-operand rule, so any type may
// be discarded with one. '(void)x' on a struct parameter - how a lemon-
// generated parser marks its arguments used - was rejected.

struct big { int a, b, c, d; };
union u { long l; double d; };

static int takes(struct big s, union u u2) {
  (void)s;
  (void)u2;
  return 42;
}

static void discards(void) {
  struct big s;
  s.a = 1; s.b = 2; s.c = 3; s.d = 4;
  (void)s;
}

int main(void) {
  struct big s;
  union u u2;

  s.a = 1; s.b = 2; s.c = 3; s.d = 4;
  u2.l = 9;

  discards();
  return takes(s, u2) == 42 ? 0 : 1;
}
