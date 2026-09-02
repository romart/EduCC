// The value of an assignment to a bit-field.
//
// 'b.f = v' is an expression, and its value is what was stored - read back out
// of the field, so a value too wide for it is truncated and a signed field is
// sign-extended from its own width. Both backends used to write memory
// correctly and hand back something else: the legacy one returned the raw bits
// left in the accumulator, extended from neither the field's width nor the
// storage type's, and the IR one returned the whole storage unit before the
// field was shifted out of it.
//
// '/=' and '%=' on a bit-field are left out on purpose: they abort both
// backends outright ("cdq has no byte form") when the storage unit is a byte,
// which is a different bug and has no fixture yet.

struct B {
  unsigned u : 3;
  int s : 5;
};

int main(void) {
  struct B b;
  struct B *p = &b;

  if ((p->s = -3) != -3) return 1;
  if (b.s != -3) return 2;

  if ((p->s = 7) != 7) return 3;
  if (b.s != 7) return 4;

  if ((p->u = 5) != 5) return 5;
  if (b.u != 5) return 6;

  if ((b.s = -16) != -16) return 7;
  if ((b.u = 0) != 0) return 8;

  // Too wide for the field: the value is what the field holds afterwards.
  if ((p->s = 40) != 8) return 9;
  if (b.s != 8) return 10;
  if ((p->u = 9) != 1) return 11;
  if (b.u != 1) return 12;

  b.s = 3;
  if ((b.s += 20) != -9) return 13;
  if (b.s != -9) return 14;

  b.u = 1;
  if ((b.u *= 6) != 6) return 15;

  b.s = -1;
  if ((b.s <<= 2) != -4) return 16;

  return 0;
}
