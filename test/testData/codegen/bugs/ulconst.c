// Masking a 64-bit value with a constant that needs all 32 of its low bits.
//
// x86 has no 64-bit ALU form taking a 64-bit immediate - the widest is an
// imm32 sign-extended to the operand width - so 0xFFFFFFFF has to be
// materialized into a register first. The backend did that only when the
// operand was *unsigned* 64-bit, which is why test2 was right and test1 was
// not: 's & 0x0FFFFFFFF' came out as 's & -1', which is to say as 's'.
//
// This file asserted that wrong answer - 'test1() != s' - until the fix, so
// the checks below are what C says rather than what the compiler said.

long s = 0x12300000456;
unsigned long u = 0x78900000abc;


long test1() {
  return s & 0x0FFFFFFFF;
}

unsigned long test2() {
  return u & 0x0FFFFFFFF;
}

int main() {

  if (test1() != 0x456) return 1;

  if (test2() != 0xabc) return 2;

  return 0;
}
