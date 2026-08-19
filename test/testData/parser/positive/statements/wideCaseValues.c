// Case labels that do not fit in 32 bits, which used to be truncated on the
// way into the AST in three separate places: parseAsIntConst() cast its
// int64_t result through 'int', createLabelStatement() took the value as an
// 'int', and the duplicate-case check kept its set of seen values in an
// int array.
//
// The first two labels agree in their low 32 bits, so a truncating parser does
// not merely record the wrong value - it records the same value twice and
// rejects the function with a duplicate-case error. That is what makes this a
// positive test: the dump shows the values, and the empty .err shows that the
// switch was accepted at all.
//
// Enum constants are deliberately not tested here. ISO C restricts an
// enumerator to the range of 'int' before C23, and EnumConstant.value is
// 32 bits to match, so a wide one is a documented limitation rather than this
// bug wearing another hat.

int testWideCases(long long v) {
  switch (v) {
  case 0x100000001LL: return 1;
  case 0x200000001LL: return 2;
  case 1: return 3;
  case -0x300000005LL: return 4;
  default: return 0;
  }
}

// The boundaries either side of the range that used to be the whole of it, so
// a fix that merely widened the type without widening the comparison would
// still be caught.
int testWideBoundaries(long long v) {
  switch (v) {
  case 2147483647LL: return 1;  /* INT32_MAX */
  case 2147483648LL: return 2;  /* one past it */
  case -2147483648LL: return 3; /* INT32_MIN */
  case -2147483649LL: return 4; /* one below it */
  default: return 0;
  }
}
