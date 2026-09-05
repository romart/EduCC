// Unary minus on a float owes -0.0 for +0.0, which '0.0 - x' does not give.
// Lua's test suite catches it through string.format("%a", -0.0).

static int isNegative(double d) {
  union { double d; unsigned long long bits; } u;
  u.d = d;
  return (int)(u.bits >> 63);
}

static int isNegativeF(float f) {
  union { float f; unsigned int bits; } u;
  u.f = f;
  return (int)(u.bits >> 31);
}

static double negate(double x) { return -x; }

int main(void) {
  double zero = 0.0;
  float fzero = 0.0f;

  if (!isNegative(-0.0)) return 1;            /* the literal */
  if (!isNegative(negate(zero))) return 2;    /* at run time */
  if (!isNegativeF(-fzero)) return 3;         /* and for a float */

  if (isNegative(negate(negate(zero)))) return 4;
  if (isNegative(0.0)) return 5;

  /* everything else negates as it did */
  if (negate(1.5) != -1.5) return 6;
  if (negate(-2.25) != 2.25) return 7;
  return 0;
}
