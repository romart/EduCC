// Converting a float to _Bool.
//
// C says '(_Bool)x' is 'x != 0', so every nonzero value is 1 - including the
// ones between zero and one, and including a NaN, which compares equal to
// nothing at all.
//
// The legacy backend converts the float to an integer first and then tests
// that, so anything with magnitude below 1 truncates to zero and comes back
// false. The -experimental backend compares against zero directly (see
// selectBooleanConversion in src/x86_64/isel_x86_64.c) and gets all four
// right, which is why this file is muted rather than deleted: it passes under
// -experimental already and fails under the default backend.

double ddiv(double a, double b) { return a / b; }
int d2b(double x) { return (_Bool)x; }
float f2b(float x) { return (_Bool)x; }

int main(void) {
  double z = ddiv(0.0, 1.0);
  double nan = ddiv(z, z);

  if (d2b(0.0) != 0) return 1;
  if (d2b(1.0) != 1) return 2;
  if (d2b(0.5) != 1) return 3;
  if (d2b(-0.5) != 1) return 4;
  if (f2b(0.25f) != 1) return 5;
  if (d2b(nan) != 1) return 6;

  return 0;
}
