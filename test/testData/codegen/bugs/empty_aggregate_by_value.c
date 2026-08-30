// An empty struct passed and returned by value, which the IR backend gets
// wrong. Split out of codegen/experimental/empty_aggregate.c, which covers
// copying one - that half works and is what step 18 fixed.
//
// SysV gives a zero-sized object no eightbytes at all, so gcc passes and
// returns one in nothing. Neither backend here does that: translateCall's
// small-aggregate path loads eight bytes through a GEP whatever the type's
// size, and classifyParametersGeneric gives the parameter an integer register.
// The two agree with each other, which is why the legacy backend gets the
// right answer out of eight bytes of nothing in particular.
//
// The IR backend does not, because the frame disagrees with the code that
// writes it: the parameter's slot is sized from computeTypeSize(), which is
// zero, so the eight-byte store of the incoming register goes to offset 0 of a
// zero-byte object and lands on the frame base itself. In the dump:
//
//   fi#0 : local 0/1 @ 0 'e'
//   mov.8 [fi#0], $r10
//
// The saved frame pointer is what gets eight bytes of struct written over it,
// and the function returns into nowhere.
//
// Fixing it means deciding which of the two the compiler wants to be. Matching
// gcc - passing nothing - is the right answer and is a change to both backends
// and to the parameter classifier; sizing the slot to what is actually stored
// into it is the small answer and keeps this compiler's own ABI. Either way it
// is an ABI question about zero-sized objects and not the copy rule that
// step 18 was about.

struct E { };

struct E identity(struct E e) { return e; }

struct E pick(int which, struct E a, struct E b) { return which ? a : b; }

int sideEffect(struct E e, int n) { return n + 1; }

int main(void) {
  struct E a, b;

  struct E r = identity(a);
  b = r;

  struct E p = pick(1, a, b);
  b = p;

  // The argument after an empty one still has to arrive: whatever the empty
  // struct is passed as, it must not move the integer behind it.
  if (sideEffect(a, 41) != 42) return 1;

  return 0;
}
