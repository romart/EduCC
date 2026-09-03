// Plain 'goto <label>', which the -experimental pipeline cannot translate at
// all: it aborts before any pass runs. See goto_label.muted for the cause.
//
// The two functions are the two shapes worth having once it works, not two
// spellings of the same thing:
//
//   goto_label_forward      the error-cleanup idiom, and the minimal case the
//                           bug is actually about. Two gotos target one label,
//                           so the label block is a real merge point with more
//                           than one predecessor - the shape SSA construction
//                           has to place a phi for, if 'r' were live across it.
//
//   goto_label_irreducible  a jump *into* the middle of a loop body, giving the
//                           loop two entry points. This is the only way to
//                           build an irreducible CFG in this language - the
//                           structured statements cannot express one - and it
//                           is what makes two currently-unreachable pieces of
//                           the backend testable: dominator construction on a
//                           loop with no single header, and the arm of x86
//                           conditional-branch selection that emits both a
//                           conditional and an unconditional jump, which needs
//                           a branch whose two successors are *both* already
//                           placed by the block layout (see the third case in
//                           test/testData/ir/pipeline/isel_branch_layout.c, and
//                           docs/ir-codegen-design.md section 10).
//
// Both compile and run correctly through the legacy pipeline, so this is a gap
// in ast2ir specifically and not a parser or sema limitation. Their values are
// pinned here so a future codegen fixture does not have to rederive them:
// forward(3, 4) == 7, forward(-1, 4) == forward(3, -1) == -1,
// irreducible(1, 3) == 3, irreducible(0, 0) == 0.
int goto_label_forward(int a, int b) {
    int r = 0;

    if (a < 0) goto fail;
    r = r + a;

    if (b < 0) goto fail;
    r = r + b;

    return r;

fail:
    return -1;
}

int goto_label_irreducible(int a, int n) {
    int i = 0;

    if (a) goto middle;

    while (i < n) {
        i = i + a;
    middle:
        i = i + 1;
    }

    return i;
}
