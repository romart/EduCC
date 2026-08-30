// An empty struct passed and returned by value. Split out of
// codegen/experimental/empty_aggregate.c, which covers copying one - that half
// works and is what step 18 fixed.
//
// SysV gives a zero-sized object no eightbytes, so gcc passes and returns one
// in nothing at all. Neither backend did: translateCall's small-aggregate path
// loaded eight bytes through a GEP whatever the type's size, and
// classifyParametersGeneric handed the parameter an integer register. The two
// agreed with each other, which is why the legacy backend got a right answer
// out of eight bytes of nothing in particular - but only until the frame
// disagreed with the code writing it. The IR backend sized the parameter's
// slot from computeTypeSize(), which is zero, so the eight-byte store of the
// incoming register went to offset 0 of a zero-byte object:
//
//   fi#0 : local 0/1 @ 0 'e'
//   mov.8 [fi#0], $r10
//
// which is the frame base, so the saved frame pointer took the store and the
// function returned into nowhere. The legacy backend had the same shape of bug
// on the *return* side: a function whose only composite return is zero-sized
// gets no struct buffer (returnStructBuffer stays 0 and reads as "none"), and
// the eight bytes of R_ACC parked there after the call landed on [rbp].
//
// Both now pass and return nothing, which is also what an argument register
// costs: none. So what this pins down is that a zero-sized argument moves no
// other argument - the value behind it must arrive where it would have with
// the empty one deleted, in a register and on the stack - and that it is still
// evaluated for its side effects.
//
// Expected values confirmed against gcc.

#include <stdarg.h>
#include <stdio.h>

struct E { };

struct E identity(struct E e) { return e; }

struct E pick(int which, struct E a, struct E b) { return which ? a : b; }

// The integer behind an empty struct: it must land in the first argument
// register, not the second.
int sideEffect(struct E e, int n) { return n + 1; }

// Empty structs interleaved with the arguments they must not move, in the
// middle of the register sequence rather than only in front of it.
int interleaved(struct E a, int x, struct E b, int y, struct E c, int z) {
  return x * 100 + y * 10 + z;
}

// Enough integers to fill the argument registers, with empty structs among
// them: 'last' is the first argument that has to go on the stack, and it stays
// there only if the empty ones took no register.
int spill(struct E a, int i1, int i2, int i3, struct E b, int i4, int i5,
          int i6, struct E c, int last) {
  return i1 + i2 + i3 + i4 + i5 + i6 + last;
}

// The variadic half. Passing nothing and consuming nothing have to move
// together: while the call site pushed an eightbyte and va_arg took one, the
// two errors cancelled and every integer behind an empty struct still read
// correctly. Now that the caller passes nothing, a va_arg that still consumes
// an eightbyte hands back the argument after the one asked for - so what this
// checks is the integer *behind* each empty struct, and enough of them to run
// past the six argument registers into the overflow area.
int variadic(int n, ...) {
  va_list ap;
  int i, acc = 0;
  va_start(ap, n);
  for (i = 0; i < n; i++) {
    struct E skipped = va_arg(ap, struct E);
    acc = acc * 10 + va_arg(ap, int);
  }
  va_end(ap);
  return acc;
}

struct E sink;

// The struct-buffer half, which only shows from inside a function that has to
// return: the eight bytes parked at [rbp] were the caller's frame pointer, and
// the caller went on using whatever came back in its place. main() survives it
// because nothing needs its frame after the epilogue.
int nested(struct E e) {
  int guard = 1234;
  struct E t = identity(e);
  sink = t;
  return guard;
}

int calls;

struct E bump(void) {
  struct E z;
  calls = calls + 1;
  return z;
}

int main(void) {
  struct E a, b;

  struct E r = identity(a);
  b = r;

  struct E p = pick(1, a, b);
  b = p;

  if (sideEffect(a, 41) != 42) return 1;
  if (interleaved(a, 1, b, 2, r, 3) != 123) return 2;
  if (spill(a, 1, 2, 4, b, 8, 16, 32, r, 64) != 127) return 3;

  // An argument passed nowhere is still an expression: 'not handed over' is
  // not 'not evaluated'.
  calls = 0;
  struct E s = identity(bump());
  b = s;
  if (calls != 1) return 4;

  // The same in return position, where the value goes nowhere either.
  calls = 0;
  b = pick(0, bump(), bump());
  if (calls != 2) return 5;

  if (nested(a) != 1234) return 6;

  if (variadic(8, a, 1, b, 2, r, 3, a, 4, b, 5, r, 6, a, 7, b, 8) != 12345678)
    return 7;

  // The one check that is not this compiler talking to itself: sprintf was
  // compiled by gcc, so an empty struct taking an argument register here would
  // move the integers behind it into the wrong ones. The format deliberately
  // names two conversions for three arguments - the empty one is not a value
  // printf could print, only an argument it must not be handed.
  {
    char buf[16];
    const char *want = "12 34";
    int i;

    sprintf(buf, "%d %d", a, 12, 34);
    for (i = 0; want[i] != 0; ++i) {
      if (buf[i] != want[i]) return 8;
    }
    if (buf[i] != 0) return 9;
  }

  return 0;
}
