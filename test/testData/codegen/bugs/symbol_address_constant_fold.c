// A link-time address run through the IR constant folder.
//
// The address of a symbol and the address of a string literal are constants -
// IR_DEF_CONST, foldable by scp like any other - but their payload is a
// Symbol * or a char * in the *compiler's* address space, not the value the
// program will see. The folder reads data.i off whatever constant it is given,
// so '(long)&g' folded to the bits of the Symbol node describing g:
//
//     printf("%lx %x\n", (long)&g, (int)(long)&g)   ->  7f94f7d0f8e8 f7d0f8e8
//
// where the real address is 0x404020. What used to keep the folder away from
// these was their IR type: a literal was IR_LITERAL and a symbol IR_REF, and
// nothing computed with either. That was never a guard - irTypeClass() mapped
// both to the pointer class and the cast folded anyway - and once step 22 made
// both IR_PTR, the type could not have been one. The kind is checked instead:
// only integer and float constants fold.
//
// Every check here dereferences the address it built, so a folded-in pointer
// to compiler memory is a wrong value or a fault rather than a number nobody
// compares. Casting a pointer to long and back is implementation-defined, not
// undefined (C99 6.3.2.3p5-6), and round-trips on every target this compiles
// for.

int g = 7;
int arr[3] = {10, 20, 30};

int main() {
  long a = (long)&g;
  if (*(int *)a != 7) return 1;

  // The same address narrowed and widened again. Nothing may be lost that the
  // full 64-bit value did not already lose.
  unsigned long full = (unsigned long)&g;
  unsigned int lo = (unsigned int)(unsigned long)&g;
  if (lo != (unsigned int)(full & 0xffffffffu)) return 2;

  // An offset computed from a folded address.
  long base = (long)arr;
  if (*(int *)(base + 2 * (long)sizeof(int)) != 30) return 3;

  // A literal's address takes the same path.
  const char *s = "abc";
  long ls = (long)s;
  if (*(const char *)ls != 'a') return 4;
  if (((const char *)ls)[2] != 'c') return 5;

  // Two uses of one constant are equal to each other; two different ones are
  // not. This is the one thing about an address the folder may still decide.
  const char *p = "same";
  const char *q = "same";
  if (p != q) return 6;
  if ((long)&g == (long)&arr[0]) return 7;
  if (&g == 0) return 8;

  return 0;
}
