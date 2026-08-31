// Narrowing a long double to a signed char or short.
//
// 'x87 fistp' stores 2, 4 or 8 bytes, so an 8- or 16-bit destination is stored
// wider than it is and loaded back narrow. The legacy backend loads it back as
// the *unsigned* type of that width - generateF10toInt in
// src/x86_64/codegen_x86_64.c rewrites T_S1 to T_U1 and T_S2 to T_U2 before
// the load - so a negative value comes back zero-extended: (short)-1.0L is
// 65535 and (char)-1.0L is 255. 'double' and 'float' sources go through a
// different path and are right, which is why this is about long double alone.
//
// Every value below is in range for its destination: a floating value that
// does not fit the integer type it is converted to is undefined, so there is
// nothing to pin there.
//
// The -experimental backend gets every line here right, hence '.muted.legacy'.
//
// This one also miscompiles the compiler. src/evaluate.c folds a cast to a
// narrow signed type with exactly this C - 'arg->i = (int16_t)arg->f' - so a
// self-hosted EduCC folds (short)-1.0L to 65535 and bakes it into whatever it
// is compiling. That is how this was found: bootstrap.sh, then the corpus run
// with the result, where codegen/experimental/long_double.c fails its check 31
// under a compiler that a gcc-built EduCC compiles correctly.

int main(void) {
  long double ld = -1.0L;

  if ((int)(short)ld != -1) return 1;
  if ((int)(char)ld != -1) return 2;

  long double neg = -300.5L;
  if ((int)(short)neg != -300) return 3;

  long double small = -100.5L;
  if ((int)(char)small != -100) return 4;

  // Positive values are right today; they are here so that a fix does not go
  // wrong in the other direction.
  long double pos = 200.0L;
  if ((int)(short)pos != 200) return 5;
  if ((int)(unsigned char)pos != 200) return 6;

  // The unsigned destination the rewrite was aimed at, which has to stay right.
  long double wide = 65535.0L;
  if ((int)(unsigned short)wide != 65535) return 7;

  return 0;
}
