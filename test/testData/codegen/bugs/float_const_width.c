// A float or double constant has to hold a value of its own type.
//
// Every constant in this IR is stored as a long double, so a 'double' one was
// whatever the *wider* type made of the literal until createFloatConstant
// rounded it. Nothing noticed until a conversion to a wider type was folded:
// emission narrows on the way into a section, so what a program read at run
// time was always right, but '(long double)d' folded to the long double the
// literal was parsed as rather than to the double the variable holds.
//
// So the compiler answered 0.1 where the program answers 0.10000000000000000555
// - the same expression disagreeing with itself depending on whether it was
// folded, which is the shape codegen/bugs/x87_widened_source.c has for casts
// the other way round. Found by compiling this file for riscv64 and running it
// against the cross toolchain's answer.
//
// Nothing here is about a particular format: each check asks only that a value
// rounded to a narrower type and then widened again is what the round trip
// through storage gives, which is true of x87's eighty bits and of binary128
// alike.

int main(void) {
  // Folded: both operands are constants, so this is the compiler's arithmetic.
  if ((long double)0.1 != (long double)(double)0.1) return 1;
  if ((long double)0.1f != (long double)(float)0.1f) return 2;

  // The same values through storage, which is the answer the folded one has to
  // agree with. 'volatile' is what keeps the compiler from folding these too.
  {
    volatile double d = 0.1;
    volatile float f = 0.1f;
    long double fromStore = (long double)d;
    long double folded = (long double)0.1;

    if (fromStore != folded) return 3;
    if ((long double)f != (long double)0.1f) return 4;

    // A float widened to double and then to long double is the same value as
    // the float widened straight to long double: widening is exact.
    if ((long double)(double)f != (long double)f) return 5;
  }

  // A double literal that is exactly representable is unaffected either way,
  // which is what makes the two checks above about rounding and not about
  // arithmetic going wrong in general.
  if ((long double)0.5 != 0.5L) return 6;
  if ((long double)0.25f != 0.25L) return 7;
  if ((long double)2.0 != 2.0L) return 8;

  // The narrowing direction: a long double constant narrowed to double and
  // widened back has lost the bits double cannot hold, and has to have lost
  // exactly those - so doing it twice changes nothing more.
  {
    long double once = (long double)(double)0.1L;
    long double twice = (long double)(double)once;

    if (once != twice) return 9;

    volatile double d = 0.1L;
    if ((long double)d != once) return 10;
  }

  return 0;
}
