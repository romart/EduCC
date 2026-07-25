// A constant sub-expression inside a loop body, mixed into a value that is
// anything but constant: 'acc' is a loop-carried phi that settles at the
// bottom of the lattice, yet '2 * 3' beside it still folds to 6. The two
// have to be tracked independently - a pass that let the accumulator's
// bottom contaminate the operands feeding the same addition would leave the
// multiply in place.
//
// Nothing else about the loop folds: the trip count depends on 'n', so the
// header's phis and the comparison all stay, and the folded constant is the
// only difference from the ssa snapshot.
int scp_loop_const(int n) {
    int acc = 0;
    int i = 0;
    while (i < n) {
        acc = acc + 2 * 3;
        i = i + 1;
    }
    return acc;
}
