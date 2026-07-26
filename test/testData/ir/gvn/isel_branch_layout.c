// Which arm of a conditional branch gets the fallthrough, and what happens
// when it is not the one the branch was written for.
//
// Selection fixes the block order before it fills any block in, so by the time
// a branch is selected it already knows which of its two successors comes next
// and needs no jump. There are three ways that can land, and the whole point of
// this fixture is that the ordinary corpus only ever reaches the first:
//
//   not-taken is next   emit 'jne <taken>' and fall through. Every plain 'if'
//                       in every other fixture does this.
//   taken is next       the condition has to be inverted - 'je <notTaken>' -
//                       so that the fallthrough is used rather than jumped
//                       over. Reached here, and essentially only here.
//   neither is next     both a conditional and an unconditional jump. Not
//                       reachable at all, in any source this compiler accepts:
//                       a conditional branch's two successors are created at
//                       the moment the branch is and nothing merges them away,
//                       so the layout walk has never visited the second one
//                       when it reaches the branch, and it therefore always
//                       comes next. Verified over every fixture plus a set of
//                       adversarial CFGs - two-entry loops, an infinite loop,
//                       a do-while entered by a goto into its body - none of
//                       which produces it. See docs/ir-codegen-design.md
//                       section 10.
//
// The inversion comes from '||' rather than from anything about the source
// being unusual: ast2ir records a short-circuit '||' branch's successors in
// the opposite order to every other conditional, so reverse postorder places
// its arms the other way round and the branch has to invert. The '&&' above it
// is the contrast - same shape in C, ordinary fallthrough in the dump.
//
// Both operands are compared against a parameter rather than a constant so
// that neither condition folds, and the assignments differ so that the join
// blocks carry real phis and the fixture also covers copies landing on a
// split critical edge.
int isel_branch_layout(int a, int b, int c) {
    int r = 0;

    if (a > c && b > c) {
        r = a + b;
    }

    if (a > c || b > c) {
        r = r + c;
    }

    return r;
}
