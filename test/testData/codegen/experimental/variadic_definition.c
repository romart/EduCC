// Being a variadic function, on the IR backend.
//
// Calling one was always selected; defining one was refused for the whole
// function (step 14 of docs/ir-codegen-design.md). va_start hands out arguments
// from a register save area that the six integer and eight SSE argument
// registers have to be spilled into on entry, and the translator allocated that
// area - '__va_area__' is right there in the locals - without emitting a single
// store into it. The legacy backend writes them in its own prologue, which is
// why variadic functions worked at all.
//
// The area is now filled in the IR, in the entry block, which is the moment the
// prologue used to be. So what is worth pinning here is the boundary each of
// the two cursors crosses: gp_offset and fp_offset start past whatever the
// named parameters already took, count up through their own bank, and hand over
// to the overflow area - a separate pointer, into the *caller's* frame, that
// this backend had spelled as an offset into the save area itself.
//
// variadic_composite_return.c covers the same machinery interleaved with a
// hidden return-buffer pointer, which moves every named argument one register
// along. This is the ordinary shape without that, and it is here rather than
// with the legacy fixtures because codegen/experimental is what runs under
// -noFallback: a function that quietly went back to the legacy backend would
// pass anywhere else.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <stdarg.h>

static int failures = 0;

static void check(int ok, int id) {
  if (!ok && failures == 0) failures = id;
}

// 'n' takes rdi, so five integer varargs are in registers and the sixth is the
// first to come out of the overflow area.
static long sumInts(int n, ...) {
  va_list ap;
  long total = 0;
  int i;

  va_start(ap, n);
  for (i = 0; i < n; ++i) total += va_arg(ap, int);
  va_end(ap);

  // The named parameter shares its register with the save area, which is
  // written on the way in: reading it back afterwards is the check that one did
  // not land on the other.
  return total * 10 + n;
}

// No named float, so all eight SSE registers are available and the ninth double
// is the first past the bank.
static double sumDoubles(int n, ...) {
  va_list ap;
  double total = 0;
  int i;

  va_start(ap, n);
  for (i = 0; i < n; ++i) total += va_arg(ap, double);
  va_end(ap);

  return total;
}

// A named double moves fp_offset along the way a named int moves gp_offset.
static double afterNamedDouble(int n, double first, ...) {
  va_list ap;
  double total = first;
  int i;

  va_start(ap, first);
  for (i = 0; i < n; ++i) total += va_arg(ap, double);
  va_end(ap);

  return total;
}

// Both banks at once, and past the end of both: the two cursors advance
// independently, and once each has run out they share one overflow pointer -
// which is the case that catches an overflow area anchored to the wrong frame.
static double mixed(int n, ...) {
  va_list ap;
  double total = 0;
  int i;

  va_start(ap, n);
  for (i = 0; i < n; ++i) {
    total += va_arg(ap, int);
    total += va_arg(ap, double);
  }
  va_end(ap);

  return total;
}

// Six named integers exhaust the bank before the ellipsis starts, so gp_offset
// is already at its bound on entry and every integer vararg is in memory.
static long sixNamed(int a, int b, int c, int d, int e, int f, ...) {
  va_list ap;
  long total = a + b + c + d + e + f;
  int i;

  va_start(ap, f);
  for (i = 0; i < 4; ++i) total += va_arg(ap, int);
  va_end(ap);

  return total;
}

// va_copy resumes where the first list stands, not where it started.
static long copied(int n, ...) {
  va_list ap;
  va_list ap2;
  long total = 0;
  int i;

  va_start(ap, n);
  total += va_arg(ap, int);
  total += va_arg(ap, int);

  va_copy(ap2, ap);
  for (i = 2; i < n; ++i) total += va_arg(ap2, int);
  va_end(ap2);

  // The original is still usable and still where it was.
  total += va_arg(ap, int) * 1000;
  va_end(ap);

  return total;
}

// Two lists over the same arguments, started independently, walked at different
// rates - each carries its own cursors, and neither writes the other's.
static long twoLists(int n, ...) {
  va_list a;
  va_list b;
  long total = 0;
  int i;

  va_start(a, n);
  va_start(b, n);

  for (i = 0; i < n; ++i) total += va_arg(a, int);
  total += va_arg(b, int) * 100;

  va_end(a);
  va_end(b);

  return total;
}

// Types other than 'int' through the ellipsis: a pointer is an integer argument
// of the widest kind, and a long is what the save area's slots actually are.
static long widths(int n, ...) {
  va_list ap;
  long total = 0;
  int i;
  int *p;

  va_start(ap, n);
  for (i = 0; i < n; ++i) {
    p = va_arg(ap, int *);
    total += *p;
    total += va_arg(ap, long);
  }
  va_end(ap);

  return total;
}

// One variadic function calling another: the callee's save area is filled from
// registers the caller has just set up, and the caller's own is still live
// across the call.
static long forwardOne(int n, ...) {
  va_list ap;
  long total;
  int a, b, c;

  va_start(ap, n);
  // One at a time: several va_args in one argument list are unsequenced, so
  // which of them advances the cursor first is not the compiler's to decide.
  a = va_arg(ap, int);
  b = va_arg(ap, int);
  c = va_arg(ap, int);

  total = sumInts(3, a, b, c);
  total += va_arg(ap, int);
  va_end(ap);

  return total;
}

// Split from the float checks below so that this one stays on the IR backend:
// every call here is an IR-built caller reaching an IR-built variadic callee,
// which is the pairing the fixture is for.
static void intChecks(void) {
  int x = 11;
  int y = 22;

  check(sumInts(0) == 0, 1);
  check(sumInts(5, 1, 2, 3, 4, 5) == 155, 2);
  // The sixth is the first one in memory, and the tenth is well past it.
  check(sumInts(6, 1, 2, 3, 4, 5, 6) == 216, 3);
  check(sumInts(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 560, 4);

  check(sixNamed(1, 2, 3, 4, 5, 6, 10, 20, 30, 40) == 121, 5);
  check(copied(5, 1, 2, 3, 4, 5) == 3015, 6);
  check(twoLists(4, 7, 8, 9, 10) == 734, 7);
  check(widths(2, &x, 100L, &y, 200L) == 333, 8);
  check(forwardOne(4, 1, 2, 3, 4) == 67, 9);
}

// These calls hand a double to a variadic function once the eight SSE argument
// registers are gone, which needs a 'push xmm' the selector does not have - so
// this one function goes back to the legacy backend and is named in the
// .fallback sibling. That is not a gap in what is under test: the callees are
// still built here, and a legacy caller reaching an IR-built variadic callee is
// the seam the two backends have to agree on anyway.
static void floatChecks(void) {
  check(sumDoubles(8, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) == 36.0, 10);
  check(sumDoubles(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0) == 45.0, 11);
  check(sumDoubles(12, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                   12.0) == 78.0, 12);

  check(afterNamedDouble(0, 100.0) == 100.0, 13);
  check(afterNamedDouble(8, 100.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) ==
        136.0, 14);

  check(mixed(3, 1, 1.5, 2, 2.5, 3, 3.5) == 13.5, 15);
  check(mixed(9, 1, 1.0, 2, 2.0, 3, 3.0, 4, 4.0, 5, 5.0, 6, 6.0, 7, 7.0, 8, 8.0,
              9, 9.0) == 90.0, 16);
}

int main(void) {
  intChecks();
  floatChecks();

  return failures;
}
