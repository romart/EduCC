// The two shapes of a struct return, in the IR that decides between them.
//
// Nothing about a composite return is a selection rule: by the time the
// backend sees a 'ret' the translator has already settled which convention the
// function follows, and the difference is visible here rather than in the
// instructions. A struct of more than eight bytes is written through a pointer
// the caller passes, so the function's return slot holds *that pointer* - the
// entry block reads it out of the first argument register, each 'return'
// copies through it, and the exit returns it. A struct that fits comes back as
// bytes, so the slot holds the value and the exit loads an eightbyte out of it.
//
// What that looks like in the dumps is two different exit blocks. 'large'
// returns the $rdi the entry block read - the pointer slot is a scalar, so
// mem2reg promotes it away and no alloca for it survives - while 'small' keeps
// its slot as an IR_ALLOCA and the exit is an IR_M_LOAD of type AGG feeding
// the IR_RET.
//
// 'large' and 'small' are those two. 'shifted' is what the hidden pointer
// costs the arguments - it takes the first integer register, so 'a' starts one
// along and the sixth parameter has nowhere to go but the stack. 'twice' has
// two returns, which is where writing through the caller's buffer rather than
// into a local pays: the copy is at each 'return' and the exit block adds
// nothing but the pointer. 'plain' is the scalar case unchanged, so a
// baseline diff shows what the composite ones do differently.

struct Large { long a[5]; };
struct Small { int x, y; };

struct Large large(int n) {
  struct Large r;
  r.a[0] = n;
  return r;
}

struct Small small(int n) {
  struct Small r;
  r.x = n;
  r.y = n + 1;
  return r;
}

struct Large shifted(int a, int b, int c, int d, int e, int f) {
  struct Large r;
  r.a[0] = a + b + c;
  r.a[1] = d + e + f;
  return r;
}

struct Large twice(int n) {
  struct Large r;
  r.a[0] = n;
  if (n < 0) {
    r.a[1] = -1;
    return r;
  }
  r.a[1] = 1;
  return r;
}

long plain(int n) {
  return n + 1;
}
