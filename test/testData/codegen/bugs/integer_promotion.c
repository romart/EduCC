// The integer promotions (C99 6.3.1.1p2).
//
// Arithmetic on operands narrower than int is arithmetic on ints: both
// operands widen first, the operation happens at int width, and only an
// assignment narrows the answer again. Sema used to skip that step, so
// 'a + b' on two chars was an addition *at char width* - the AST held
// '(signed int)(*a + *b)', with the widening after the addition rather than
// before it - and every operator whose result is read wider than its operands
// answered wrong: 127 + 1 was -128, 100 * 3 was 44, 1 << 9 was 0.
//
// The values arrive as parameters on purpose. Written as locals with constant
// initializers the whole expression is folded at full precision by the
// constant propagation pass, which computes the right answer for the wrong
// reason and hides the bug.
//
// Nothing goes wrong where the result is stored back at the same width, which
// is why 'bugs/byte_arith.c' passed throughout and why a corpus this size
// never caught this.

int add(char a, char b) { return a + b; }
int sub(char a, char b) { return a - b; }
int mul(char a, char b) { return a * b; }
int dv(char a, char b) { return a / b; }
int shl(char a, char b) { return a << b; }
int shr(short a, char b) { return a >> b; }
int band(char a, char b) { return a & b; }
int gt(char a, char b) { return (a + b) > 100; }

int sadd(short a, short b) { return a + b; }
int uadd(unsigned char a, unsigned char b) { return a + b; }
int usub(unsigned char a, unsigned char b) { return a - b; }
int umul(unsigned char a, unsigned char b) { return a * b; }

int neg(char a) { return -a; }
int pos(char a) { return +a; }
int tilda(unsigned char a) { return ~a; }

// The shift's result is the promoted *left* operand, not the common type of
// the two: 'x << c' with a long count is an int shift.
int shiftType(int x, long c) { return (int)sizeof(x << c); }

int main() {
  if (add(127, 1) != 128) return 1;
  if (sub(-128, 1) != -129) return 2;
  if (mul(100, 3) != 300) return 3;
  if (dv(-128, -1) != 128) return 4;
  if (shl(1, 9) != 512) return 5;
  if (shr(-256, 4) != -16) return 6;
  if (band(-1, 3) != 3) return 7;
  if (gt(127, 1) != 1) return 8;

  if (sadd(30000, 10000) != 40000) return 9;
  if (uadd(200, 100) != 300) return 10;
  if (usub(1, 2) != -1) return 11;
  if (umul(200, 200) != 40000) return 12;

  if (neg(-128) != 128) return 13;
  if (pos(-1) != -1) return 14;
  if (tilda(0) != -1) return 15;

  // The type, not just the value: a narrow operand's promoted type is what
  // sizeof reads, and getting the value right at a use site would leave this
  // answering 1.
  {
    char a = 1, b = 2;
    short s = 3;
    unsigned char u = 4;
    if (sizeof(a + b) != sizeof(int)) return 16;
    if (sizeof(-a) != sizeof(int)) return 17;
    if (sizeof(~a) != sizeof(int)) return 18;
    if (sizeof(s * s) != sizeof(int)) return 19;
    if (sizeof(u << 1) != sizeof(int)) return 20;
    if (sizeof(a ? b : s) != sizeof(int)) return 21;
  }

  if (shiftType(1, 40) != (int)sizeof(int)) return 22;

  return 0;
}
