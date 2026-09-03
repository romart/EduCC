// Folding a comparison into the branch that reads it.
//
// A comparison on its own has to produce an int, so it is a zeroing move, a
// cmp and a setcc; a branch on that int is then a test and a jcc. When the
// only thing the boolean is for is the branch, all five collapse to a cmp and
// a jcc - which is worth having, and is also the one folding where getting the
// condition wrong produces a program that runs and answers backwards. That is
// what a dump catches and an exit code does not.
//
// What each function is for:
//
//   folded       the ordinary case. One cmp, one jcc, no boolean anywhere.
//   inverted     the same, with the taken arm laid out next, so the branch has
//                to be on the opposite condition - '||' is where that happens,
//                since it adds its two successors the other way round from
//                everything else. Every condition has an inverse and they are
//                not interchangeable: 'jle' where 'jl' belongs is a program
//                that is wrong only at equality.
//   unsigned_    the condition comes from the *operands'* type, so this is
//                'jb' and not 'jl' - the two differ exactly when the values
//                straddle the sign bit.
//   kept         the boolean is read as a value as well as branched on, so it
//                still has to exist: cmp, setcc, and then the test and jcc of
//                an ordinary branch on a register.
//   distant      the comparison is in a block of its own, one the branch does
//                not terminate. Flags are not modelled, so a cmp has to end up
//                adjacent to the jcc that reads it, and one from another block
//                would have to be moved to manage that. It is not folded.
//   notCompare   the condition is not a comparison at all. There is nothing to
//                fold, and the branch tests the value against itself.
//   floatOrder   an ordered float comparison is a single setcc, so it folds
//                like an integer one. comis leaves the answer in the *unsigned*
//                flags, which is why this is 'jae' and not 'jge', and NaN
//                makes it false in both directions - which is what lets the
//                inversion be a single jcc too.
//   floatEqual   float equality does not fold: it needs the ordered-ness
//                folded in with a second setcc and an 'and', which is two
//                flags to branch on rather than one.

int folded(int a, int b) {
  if (a < b) {
    return 1;
  }

  return 2;
}

int inverted(int a, int b) {
  if (a > b || a == 0) {
    return 1;
  }

  return 2;
}

int unsigned_(unsigned a, unsigned b) {
  if (a < b) {
    return 1;
  }

  return 2;
}

int kept(int a, int b) {
  int c = a == b;

  if (c) {
    return c + 10;
  }

  return c;
}

int distant(int a, int b, int c) {
  int t = a > b;

  // A branch of its own between the comparison and the one that reads it, so
  // that the two end up in different blocks with nothing else changed.
  if (c) {
    c = 0;
  }

  if (t) {
    return 1;
  }

  return 2;
}

int notCompare(int a) {
  if (a) {
    return 1;
  }

  return 2;
}

int floatOrder(double x, double y) {
  if (x >= y) {
    return 1;
  }

  return 2;
}

int floatEqual(double x, double y) {
  if (x == y) {
    return 1;
  }

  return 2;
}
