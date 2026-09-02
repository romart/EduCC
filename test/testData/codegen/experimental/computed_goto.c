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
// cannot be split, so a phi in one of its targets has its copy in the shared
// 'goto *' block, where it runs on the way to the other targets too. That is
// sound - the copies for the targets not taken write registers dead on the
// path taken, and phi destruction issues all of them as one parallel
// assignment (src/ir/codegen/prepare.c) - and 'acc' below is what says so: it
// is live across every dispatch and read differently by each target, so a lost
// or misplaced copy shows up as an arithmetic answer that is off by exactly
// one target's worth.
//
// Both dispatches in twoSites reach both labels on purpose, and 'threaded'
// dispatches from five places to four labels: the more edges share a
// predecessor, the more copies that predecessor ends up running for edges it
// is not taking.

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

// A label reached both ways: 'goto *' lands on X, and the path that does not
// dispatch falls into it. So X's phis have an operand from an unsplittable
// edge beside one from an ordinary edge, and the copies for the target not
// taken sit in the dispatch block next to copies that are.
static int mixedEntry(int n, int k) {
  void *tab[2];
  int a = n;
  int b = n * 2;

  tab[0] = &&X;
  tab[1] = &&Y;

  if (k >= 0) {
    a = n + 1;
    b = n + 2;
    goto *tab[k];
  }

  a = n + 3;
  b = n + 4;

X:
  return a * 10 + b;
Y:
  return a * 100 + b;
}

// The case the *parallel* form is for, rather than one target's copies after
// another's. 'loop' is a target of the dispatch inside it, so its own phi
// register is still live there - and 'old' carries that register to 'done',
// which makes one target's copy the source of what the other target's copy
// overwrites. Issued in edge order the reader loses; issued together it does
// not.
static int backEdge(int n, int k) {
  void *tab[2];
  int v = k;
  int old = -1;
  int steps = 0;

  tab[0] = &&loop;
  tab[1] = &&done;

  // An ordinary edge into 'done' as well, so that it has phis at all: a block
  // one dispatch reaches and nothing else does has a single predecessor.
  if (n == 0) {
    goto done;
  }

  goto *tab[0];

loop:
  old = v;
  v = v + n;
  steps += 1;
  goto *tab[steps < 3 ? 0 : 1];

done:
  return old * 100 + v * 10 + steps;
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

  // Dispatched into X, fallen into X, and dispatched into Y.
  if (mixedEntry(5, 0) != 67) return 14;
  if (mixedEntry(5, 1) != 607) return 15;
  if (mixedEntry(5, -1) != 89) return 16;

  // (k+2n)*100 + (k+3n)*10 + 3 once the loop has run three times.
  if (backEdge(1, 5) != 783) return 17;
  if (backEdge(2, 1) != 573) return 18;
  if (backEdge(0, 4) != -60) return 19;

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
