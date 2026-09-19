// Widening an 'unsigned int' has to put the zeroes back.
//
// On RV64 a 32-bit value lives in a register *sign*-extended whatever its own
// signedness - that is what lw and every 'w' instruction produce, and what the
// psABI says a narrow argument arrives as. The riscv64 selector had one rule
// for "give me this value in a full register", and it answered both questions
// with it: the register's canonical form, and C's conversion. They agree for
// every type but this one, where the canonical form is sign-extended and the
// conversion is not - so (long long)(unsigned)-1 came out -1 rather than
// 4294967295.
//
// It reached the compiler before it reached a fixture: nothing here converted
// a negative-looking unsigned int to a wider type, so the first thing to
// notice was a riscv64-hosted EduCC folding Zydis' own
// ((int)-1 >> 1) < (int)((unsigned)-1 >> 1) to false and refusing to build.

int one(void) { return 1; }

unsigned uret(void) { return 0xffffffffu; }

int main(void) {
  unsigned u = 0xffffffffu;
  int s = -1;

  // The bug, in its two directions.
  if ((long long)u != 4294967295LL) return 1;
  if ((unsigned long long)u != 4294967295ULL) return 2;

  // The cast is what converts, not the variable's declaration: a signed int
  // holding the same bits still widens to -1.
  if ((long long)s != -1LL) return 3;
  if ((long long)(unsigned)s != 4294967295LL) return 4;

  // Through a call's return value, which arrives in a register nothing here
  // wrote, and through a value the optimizer cannot fold.
  if ((long long)uret() != 4294967295LL) return 5;
  if ((long long)(unsigned)(-one()) != 4294967295LL) return 6;

  // Narrower unsigned types were never wrong - their canonical form *is* their
  // own signedness - but they are the reason the two rules were ever one.
  {
    unsigned char c = 0xffu;
    unsigned short h = 0xffffu;

    if ((long long)c != 255LL) return 7;
    if ((long long)h != 65535LL) return 8;
    if ((long long)(signed char)c != -1LL) return 9;
    if ((long long)(short)h != -1LL) return 10;
  }

  // Implicitly, which is where it would actually be met: the usual arithmetic
  // conversions widen u to long long before the addition.
  {
    long long sum = u + 1LL;
    if (sum != 4294967296LL) return 11;
  }

  // And the expression that found it.
  if (!((int)-1 >> 1 < (int)((unsigned)-1 >> 1))) return 12;

  return 0;
}
