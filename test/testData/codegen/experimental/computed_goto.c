// Computed goto, run rather than read.
//
// '&&label' is the address of a block of the function being compiled, which
// makes it the one pointer whose value only the emitter can know. It has to
// survive being stored in an array, read back out and jumped through, so the
// failure this looks for is an address that is right where it is taken and
// wrong once it has been through memory.
//
// The second thing here is the reason step 11 was where "nothing is
// miscompiled because nothing is emitted yet" expired. A computed goto's edges
// cannot be split, so a phi in one of its targets would need its copy in the
// shared 'goto *' block, where it would run on the way to the other targets
// too. buildSSA declines to promote such a variable instead - see
// hasUnsplittablePredecessor() in src/ir/ir.c - and 'acc' below is one: it is
// live across both dispatches and read differently by each target, so a lost
// or misplaced copy shows up as an arithmetic answer that is off by exactly
// one target's worth.
//
// Both dispatches reach both labels on purpose. One 'goto *' site could be
// given a private block per label and the problem would go away; two sharing
// their targets is the shape where it cannot, and so is the shape to test.

static int twoSites(int n, int k) {
  void *tab[2];
  int acc = 0;

  tab[0] = &&A;
  tab[1] = &&B;

  if (n > 0) {
    acc = n + 1;
    goto *tab[k];
  }

  acc = n - 1;
  goto *tab[k];

A:
  return acc + 10;
B:
  return acc + 20;
}

// A dispatch loop, which is what the feature is actually for: the address is
// recomputed each time round and the accumulator carries across every jump.
static int threaded(const int *ops, int n) {
  // Filled by assignment rather than by an initializer: a label address is not
  // a constant expression this compiler will place in a static initializer.
  void *table[4];
  int acc = 0;
  int pc = 0;

  table[0] = &&opAdd;
  table[1] = &&opMul;
  table[2] = &&opNeg;
  table[3] = &&opDone;

  goto *table[ops[pc]];

opAdd:
  acc = acc + 7;
  pc += 1;
  goto *table[ops[pc]];

opMul:
  acc = acc * 3;
  pc += 1;
  goto *table[ops[pc]];

opNeg:
  acc = -acc;
  pc += 1;
  goto *table[ops[pc]];

opDone:
  return acc + n;
}

// A label address leaving the function in the return register, which is the
// one direction the calls above do not cover.
//
// The two arms return different values on purpose. Nothing may jump to either
// label once this has returned - the frame is gone - so all a caller can do
// with the address is compare it, and comparing only means anything while the
// blocks are distinguishable: two identical ones may legitimately be merged
// into one, address and all.
static void *pick(int which, int jump) {
  void *chosen = which == 0 ? &&here : &&there;

  if (jump) {
    goto *chosen;
  }

  return chosen;

here:
  return (void *)1;
there:
  return (void *)2;
}

// A label address is an ordinary pointer, so it has to survive the ABI as one.
// Passing it through here and back is what stops the jump below from being
// fed the address it was taken from without ever leaving a register.
static void *launder(void *p) {
  return p;
}

int main(void) {
  int prog[8];
  void *target;
  int i;

  if (twoSites(5, 0) != 16) return 1;
  if (twoSites(5, 1) != 26) return 2;
  if (twoSites(-5, 0) != 4) return 3;
  if (twoSites(-5, 1) != 14) return 4;

  // (0 + 7) * 3 = 21, negated = -21, plus n
  prog[0] = 0;
  prog[1] = 1;
  prog[2] = 2;
  prog[3] = 3;
  if (threaded(prog, 100) != 79) return 5;

  // Every opcode as the very first one, so the entry dispatch is exercised
  // with each table entry rather than only the first.
  prog[0] = 3;
  if (threaded(prog, 1) != 1) return 6;

  prog[0] = 2;
  prog[1] = 3;
  if (threaded(prog, 5) != 5) return 7;

  // The address picks the label it was taken from, once jumped through.
  if (pick(0, 1) != (void *)1) return 8;
  if (pick(1, 1) != (void *)2) return 9;

  // And two labels are two addresses, both of which survive being returned.
  if (pick(0, 0) == pick(1, 0)) return 10;
  if (pick(0, 0) != pick(0, 0)) return 11;

  // Through a variable, a call and back, twice round: the jump reads what the
  // load produced rather than the label the address was taken from.
  for (i = 0; i < 2; ++i) {
    target = launder(i == 0 ? &&even : &&odd);
    goto *target;

  even:
    if (i != 0) return 12;
    continue;

  odd:
    if (i != 1) return 13;
    continue;
  }

  return 0;
}
