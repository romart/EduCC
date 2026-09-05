// A va_list handed on to another function has decayed to a pointer, and
// va_arg has to take it. Rejecting the decayed form stopped every vfprintf
// shaped helper - SQLite's printf and Lua's luaO_pushvfstring among them.

#include <stdarg.h>

static int sumList(int n, va_list ap) {
  int total = 0;
  int i;

  for (i = 0; i < n; ++i)
    total += va_arg(ap, int);

  return total;
}

static double sumDoubles(int n, va_list ap) {
  double total = 0;
  int i;

  for (i = 0; i < n; ++i)
    total += va_arg(ap, double);

  return total;
}

static int sumInts(int n, ...) {
  va_list ap;
  int r;

  va_start(ap, n);
  r = sumList(n, ap);
  va_end(ap);

  return r;
}

static double sumReals(int n, ...) {
  va_list ap;
  double r;

  va_start(ap, n);
  r = sumDoubles(n, ap);
  va_end(ap);

  return r;
}

int main(void) {
  if (sumInts(4, 1, 2, 3, 4) != 10) return 1;
  if (sumReals(3, 0.5, 1.5, 2.0) != 4.0) return 2;
  return 0;
}
