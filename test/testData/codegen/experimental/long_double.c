// Long double arithmetic, comparison and conversion.
//
// The IR backend keeps an x87 value in memory and names it by its address, so
// every operation here is a balanced fld/fld/op/fstp sequence rather than
// anything the register allocator sees. What that has to get right is the
// operand order for the two non-commutative operations, the stack depth (an
// unbalanced sequence overflows x87's eight slots and starts answering NaN),
// and the fact that the compare pops one operand and has to drop the other.
//
// Several checks are here to prove the arithmetic is *80-bit* rather than
// quietly going through double. Check 3 is the sharpest: 2^-63 added to 1.0 is
// exactly representable in a 64-bit mantissa and rounds away in a 53-bit one,
// so a backend that spilled through a double answers "equal". Check 20 is the
// same point about integers - 2^63-1 needs 63 significant bits, which double
// cannot hold and long double can.
//
// Check 1 is about the lexer rather than the backend: an 'L' suffix used to be
// recognised and then dropped, so every long double literal in this file was a
// double until step 17.

int main(void) {
  // The suffix reaches the type. Without it 'x' is a double widened on
  // assignment, which is right for the value and wrong for everything below
  // that depends on the literal's own precision.
  if (sizeof(1.0L) != 16) return 1;
  if (sizeof(1.0) != 8) return 2;

  long double eps = 1.0L;
  long double one = 1.0L;

  // 2^-63, built by halving rather than written as a literal, so the check
  // does not depend on hexadecimal float literals being supported.
  for (int i = 0; i < 63; ++i) {
    eps = eps / 2.0L;
  }

  if (one + eps == one) return 3;
  if ((double)(one + eps) != 1.0) return 4;

  // The four operations, and the two that care about operand order.
  long double a = 7.0L;
  long double b = 2.0L;

  if (a + b != 9.0L) return 5;
  if (a - b != 5.0L) return 6;
  if (b - a != -5.0L) return 7;
  if (a * b != 14.0L) return 8;
  if (a / b != 3.5L) return 9;
  if (b / a == 3.5L) return 10;
  if (-a != -7.0L) return 11;

  // Comparison, including the equality pair that needs the parity flag folded
  // in and the relational four that do not.
  if (!(a > b)) return 12;
  if (!(b < a)) return 13;
  if (!(a >= a)) return 14;
  if (!(a <= a)) return 15;
  if (a == b) return 16;
  if (!(a != b)) return 17;

  // As a condition rather than as a value, which is a conversion to _Bool and
  // not a comparison.
  long double zero = 0.0L;
  if (a ? 0 : 1) return 18;
  if (zero ? 1 : 0) return 19;

  // Integer conversions, in both directions and at both signednesses. 2^63-1
  // is the one that proves the path does not go through a double.
  long long big = 9223372036854775807LL;
  if ((long long)(long double)big != big) return 20;

  unsigned long huge = 18446744073709551615UL;
  if ((unsigned long)(long double)huge != huge) return 21;

  // Just below and just above 2^63, which is where the unsigned conversion
  // changes arm.
  unsigned long belowHalf = 9223372036854775807UL;
  unsigned long aboveHalf = 9223372036854775809UL;
  if ((unsigned long)(long double)belowHalf != belowHalf) return 22;
  if ((unsigned long)(long double)aboveHalf != aboveHalf) return 23;

  if ((int)(long double)-42 != -42) return 24;
  if ((long double)-42 != -42.0L) return 25;
  if ((unsigned int)3000000000U != 3000000000U) return 26;
  if ((long double)3000000000U != 3000000000.0L) return 27;

  // Truncation toward zero, which is what a C cast means and what the control
  // word has to be set to before the store.
  if ((int)2.75L != 2) return 28;
  if ((int)-2.75L != -2) return 29;

  // Narrower integer destinations take the low bytes of the wide conversion.
  if ((char)258.0L != 2) return 30;
  if ((short)-1.0L != -1) return 31;

  // Float conversions both ways. 0.5 is exact in all three formats, so a lost
  // conversion shows as a wrong value rather than as rounding.
  double d = 0.5;
  float f = 0.25f;
  if ((long double)d != 0.5L) return 32;
  if ((long double)f != 0.25L) return 33;
  if ((double)(long double)0.5L != 0.5) return 34;
  if ((float)(long double)0.25L != 0.25f) return 35;

  // A value that a double can hold and a float cannot, to check the narrowing
  // is the float one rather than a copy of the double one.
  if ((float)(1.0L + eps) != 1.0f) return 36;

  // Compound assignment, which reads the destination and writes it back.
  long double acc = 1.0L;
  acc += 2.0L;
  acc *= 3.0L;
  acc -= 1.0L;
  acc /= 4.0L;
  if (acc != 2.0L) return 37;

  // A chain deep enough that an unbalanced x87 sequence would have overflowed
  // the stack: eight unmatched pushes wrap it and the ninth answers NaN.
  long double chain = 0.0L;
  for (int i = 0; i < 32; ++i) {
    chain = chain + one * 2.0L - one;
  }
  if (chain != 32.0L) return 38;

  // Through an array and a pointer, so the address the value is named by is
  // computed rather than a frame slot.
  long double arr[4];
  for (int i = 0; i < 4; ++i) {
    arr[i] = (long double)i / 4.0L;
  }
  long double *p = arr;
  if (p[3] != 0.75L) return 39;
  if (*(p + 1) != 0.25L) return 40;

  // Assignment copies rather than aliases: writing through one name must not
  // be visible through the other.
  long double src = 1.0L;
  long double dst = src;
  src = 2.0L;
  if (dst != 1.0L) return 41;

  return 0;
}
