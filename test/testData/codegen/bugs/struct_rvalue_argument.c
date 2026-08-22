// A composite argument that is not an lvalue.
//
// generateCall assumed a struct argument too large for a register was always
// '*E' - canonicalization writes an ordinary variable that way - and asserted
// as much before taking E's address. Anything else aborted the compiler:
//
//   Assertion `arg->op == EU_DEREF' failed.
//
// The obvious way to write one is to pass a call's result straight on, which
// is what 'chained' does, and there is nothing exotic about it. The fix is the
// same route the small-struct case takes: evaluate the argument like any other
// expression, which leaves a composite's *address* in the accumulator, and
// copy from there.
//
// Found while writing codegen/experimental/aggregate_arguments.c for the IR
// backend, which had no such assumption and compiled all of this already.

struct Big { int a, b, c; };
struct Wide { long a, b, c; };

struct Big mk(int n) {
  struct Big r;
  r.a = n;
  r.b = n + 1;
  r.c = n + 2;
  return r;
}

struct Wide mkWide(long n) {
  struct Wide r;
  r.a = n;
  r.b = n * 2;
  r.c = n * 3;
  return r;
}

int sum(struct Big s) { return s.a * 100 + s.b * 10 + s.c; }
long sumWide(struct Wide s) { return s.a + s.b + s.c; }
int withArgs(int p, struct Big s, int q) { return p * 10000 + sum(s) + q; }

int chained(int n) { return sum(mk(n)); }
long chainedWide(long n) { return sumWide(mkWide(n)); }

// A comma expression and a ternary are rvalues too, and neither is a deref.
int comma(int n) {
  struct Big x = {9, 9, 9};
  return sum((x, mk(n)));
}

int ternary(int n, int pick) {
  struct Big x = {4, 5, 6};
  return sum(pick ? mk(n) : x);
}

int main(void) {
  int rc = 0;

  if (chained(1) != 123) rc |= 1 << 0;
  if (chained(7) != 789) rc |= 1 << 1;
  if (chainedWide(5) != 30) rc |= 1 << 2;
  if (withArgs(3, mk(1), 4) != 30127) rc |= 1 << 3;
  if (comma(1) != 123) rc |= 1 << 4;
  if (ternary(1, 1) != 123) rc |= 1 << 5;
  if (ternary(1, 0) != 456) rc |= 1 << 6;

  // Two in one expression, so the second cannot reuse whatever the first left
  // behind.
  if (sum(mk(1)) + sum(mk(4)) != 123 + 456) rc |= 1 << 7;

  return rc;
}
