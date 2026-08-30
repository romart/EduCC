// Long double across the ABI: how it is passed, and how it comes back.
//
// SysV gives 'long double' the X87 class, which means the argument area and
// never a register, and returns it in st(0) - so both halves are unlike every
// other scalar and neither is exercised by a fixture that only does
// arithmetic. The IR backend builds the argument area rather than pushing
// eightbyte by eightbyte precisely because of what is checked here.
//
// Check 6 is the one that pushing cannot get right and the reason
// callStackArea exists. A long double is sixteen-byte aligned in the argument
// area, so seven integer arguments in front of one - six in registers and a
// seventh on the stack - leave an eight-byte hole that the callee reads across.
// Pushing closes the hole and hands the callee its argument eight bytes low;
// laying the area out leaves it. Nothing else in the corpus has an odd number
// of eightbytes in front of an over-aligned argument.
//
// Check 10 is the other half of the same rule: a struct containing a long
// double is itself sixteen-aligned, which used to be refused outright.
//
// Every value here is exact in binary, so every comparison is exact.

// Checks 13 to 16 are the variadic half, and they found two bugs apiece worth
// of their own. A long double variadic argument is in the overflow area and
// never in the register save area - SysV's X87 class is passed in memory
// however few SSE arguments came before it - so va_arg has to skip the
// register path entirely and then round the overflow cursor up to sixteen.
// Both backends read it out of the SSE save area instead.
//
// Check 15 is the one that needs the lexer rather than either backend: '1.0L'
// was lexed as a double, the 'L' suffix having been recognised and then
// dropped, and a variadic argument is the one context where nothing converts
// the value afterwards and the mistake reaches the callee.

#include <stdarg.h>

struct big { long double v; int tag; };

long double identity(long double x) { return x; }

long double sum3(long double a, long double b, long double c) { return a + b + c; }

// One integer register used before the argument area starts.
long double afterInt(int n, long double x) { return x + (long double)n; }

// Every SSE argument register spent, so a long double cannot borrow one.
long double afterDoubles(double a, double b, double c, double d,
                         double e, double f, double g, double h,
                         long double x) {
  return x + (long double)(a + b + c + d + e + f + g + h);
}

// Six integer registers, then one integer *on the stack*, then the long
// double - which is what leaves an odd number of eightbytes below it.
long double oddStackGap(int a, int b, int c, int d, int e, int f, int g,
                        long double x) {
  return x + (long double)(a + b + c + d + e + f + g);
}

// A long double on the stack with an ordinary integer above it, so a mistake
// in either one's placement moves the other.
long double straddled(long double x, int n) { return x * (long double)n; }

long double fromStruct(struct big s) { return s.v + (long double)s.tag; }

// A return travelling through a second call: st(0) has to be balanced across
// the inner one before the outer one leaves its own value there.
long double relay(long double x) { return identity(x) + identity(x); }

long double vaSum(int n, ...) {
  va_list ap;
  va_start(ap, n);
  long double r = 0.0L;
  for (int i = 0; i < n; ++i) {
    r = r + va_arg(ap, long double);
  }
  va_end(ap);
  return r;
}

// A long double as a *named* parameter of a variadic function, which puts the
// overflow area's start past a sixteen-byte parameter rather than past an
// eightbyte one.
long double vaNamed(long double first, int n, ...) {
  va_list ap;
  va_start(ap, n);
  long double r = first;
  for (int i = 0; i < n; ++i) {
    r = r + va_arg(ap, long double);
  }
  va_end(ap);
  return r;
}

// Tagged triples, so one va_list interleaves all three classes for as long as
// the caller cares to. Each value is a distinct power of two and the answer is
// their sum, which makes any single argument read from the wrong place show up
// as a wrong total rather than cancelling out.
//
// Long enough to run *both* register save areas out: the tags alone spend the
// six integer registers, and nine doubles is one more than there are SSE ones.
// So every class ends up drawing from the overflow area as well as from its
// own save area, and the overflow cursor is shared - a long double read that
// advances it by the wrong amount, or that forgets to round it up to sixteen
// first, moves the next int and the next double too.
long double vaWeave(int n, ...) {
  va_list ap;
  va_start(ap, n);
  long double acc = 0.0L;

  for (int i = 0; i < n; ++i) {
    int kind = va_arg(ap, int);

    if (kind == 0) {
      acc = acc + (long double)va_arg(ap, int);
    } else if (kind == 1) {
      acc = acc + (long double)va_arg(ap, double);
    } else {
      acc = acc + va_arg(ap, long double);
    }
  }

  va_end(ap);
  return acc;
}

// Mixed widths through one va_list: the integer and SSE cursors advance
// independently of the overflow one, so reading a long double must not disturb
// where the next int or double comes from.
long double vaMixed(int n, ...) {
  va_list ap;
  va_start(ap, n);
  int i = va_arg(ap, int);
  long double a = va_arg(ap, long double);
  double d = va_arg(ap, double);
  long double b = va_arg(ap, long double);
  va_end(ap);
  return a + b + (long double)i + (long double)d;
}

int main(void) {
  long double one = 1.0L;
  long double half = 0.5L;
  long double quarter = 0.25L;

  if (identity(one) != 1.0L) return 1;
  if (identity(-half) != -0.5L) return 2;

  if (sum3(one, half, quarter) != 1.75L) return 3;

  if (afterInt(3, half) != 3.5L) return 4;

  if (afterDoubles(1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0, half) != 255.5L) return 5;

  // The alignment check. 1+2+4+8+16+32+64 is 127, so a long double read eight
  // bytes low - or an integer read from where the padding is - misses badly
  // rather than subtly.
  if (oddStackGap(1, 2, 4, 8, 16, 32, 64, quarter) != 127.25L) return 6;

  if (straddled(half, 6) != 3.0L) return 7;

  // A long double argument that is a constant rather than a variable: the
  // constant has to reach memory before it can reach the argument area.
  if (identity(2.5L) != 2.5L) return 8;

  if (relay(quarter) != 0.5L) return 9;

  struct big s;
  s.v = 1.5L;
  s.tag = 2;
  if (fromStruct(s) != 3.5L) return 10;

  // Returned straight into another call's argument, with no named variable in
  // between for the value to live in.
  if (identity(identity(identity(one))) != 1.0L) return 11;

  // In a loop, which is where an unbalanced x87 stack shows up: eight
  // unmatched pushes overflow it and the ninth returns a NaN.
  long double acc = 0.0L;
  for (int i = 0; i < 40; ++i) {
    acc = acc + identity(quarter);
  }
  if (acc != 10.0L) return 12;

  if (vaSum(3, one, half, quarter) != 1.75L) return 13;

  if (vaNamed(8.0L, 2, half, quarter) != 8.75L) return 14;

  // Constants rather than variables, which is what needs the 'L' suffix to
  // survive lexing: nothing converts a variadic argument, so a literal that
  // came out a double is passed as one and the callee reads sixteen bytes
  // where the caller wrote eight.
  if (vaSum(3, 1.0L, 0.5L, 0.25L) != 1.75L) return 15;

  if (vaMixed(4, 3, one, 4.0, half) != 8.5L) return 16;

  // Nine of each class, values 2^0 through 2^26, summing to 2^27 - 1. Both
  // save areas run out part way through, so the second half of every class
  // comes from the overflow area interleaved with the other two.
  if (vaWeave(27,
              0, 1,          1, 2.0,          2, 4.0L,
              0, 8,          1, 16.0,         2, 32.0L,
              0, 64,         1, 128.0,        2, 256.0L,
              0, 512,        1, 1024.0,       2, 2048.0L,
              0, 4096,       1, 8192.0,       2, 16384.0L,
              0, 32768,      1, 65536.0,      2, 131072.0L,
              0, 262144,     1, 524288.0,     2, 1048576.0L,
              0, 2097152,    1, 4194304.0,    2, 8388608.0L,
              0, 16777216,   1, 33554432.0,   2, 67108864.0L) != 134217727.0L) {
    return 17;
  }

  // The same three classes with the long doubles first, so the overflow area
  // starts with sixteen-byte entries and the integers that spill land above
  // them rather than below.
  if (vaWeave(9,
              2, 1.0L,   2, 2.0L,   2, 4.0L,
              0, 8,      0, 16,     0, 32,
              1, 64.0,   1, 128.0,  1, 256.0) != 511.0L) {
    return 18;
  }

  return 0;
}
