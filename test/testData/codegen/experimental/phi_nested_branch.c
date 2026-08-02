// Short-circuit operators and the conditional operator are control flow, and
// the value each produces is a phi over the ways its result can be decided.
// ast2ir builds those phis itself, which means it has to name the block each
// incoming value arrives from - and the block a sub-expression *ends* in is
// not the block it started in whenever that sub-expression is itself a branch.
//
//     (m && !v) || (!m && v)
//
// The right operand of the '||' is another '&&', so translating it opens two
// more blocks and leaves translation in the '< &&-exit>' one. The edge into
// the '< ||-exit>' join therefore comes from that block, but the phi used to
// record the block the operand *began* in, which by then had two successors
// and was not a predecessor of the join at all. Stage 0 pairs a phi's incoming
// blocks against its block's predecessor list by position (destroyPhisOfBlock,
// src/ir/codegen/prepare.c) and asserted; had it not, phi destruction would
// have put the copy in the wrong predecessor and the function would have
// returned whichever value the other path computed.
//
// So every function here nests a branching operator on the right of '&&'/'||'
// or inside a ternary arm - the left operand and the condition never needed a
// fix, since translation is already sitting in their end block when their
// block is captured. They are all free of loads, stores and calls so that
// instruction selection takes them rather than handing them to the legacy
// backend (see integer_subset.c); main() is all calls and always falls back.
//
// Each result is checked against a branch-free reference expression rather
// than against a repeat of itself, so the check does not share the shape it is
// testing. Expected values confirmed against gcc.

// The shape from codegen/simple/if.c:testCascade, on parameters rather than a
// global so it stays selectable. Logical xor: exactly one of the two true.
int xor2(int a, int b) {
    return (a && !b) || (!a && b);
}

// The right operand of '&&' branches.
int and_or(int a, int b, int c) {
    return a && (b || c);
}

// ... and of '||'.
int or_and(int a, int b, int c) {
    return a || (b && c);
}

// Three levels deep, so the innermost join's phi is reached through two
// enclosing ones and every level's end block differs from its start block.
int deep(int a, int b, int c, int d) {
    return a || (b && (c || d));
}

// Both ternary arms branch, which is the same defect in translateTernary:
// there it is the arm's own end block that the phi has to name, twice over.
int tern_arms(int a, int b, int c) {
    return a ? (b && c) : (b || c);
}

// A ternary nested directly inside a ternary arm.
int tern_in_arm(int a, int b, int c, int d) {
    return a ? (b ? c : d) : (c ? d : b);
}

// Control: the branching sub-expression is the *left* operand and the
// condition, the two positions that were always captured correctly. It must
// keep working, so a fix that moved the capture rather than adding one shows
// up here.
int branchy_left(int a, int b, int c) {
    return (a ? b : c) && (b || c);
}

int main(void) {
    // 0 and 1 for the plain cases, plus values that are true or false without
    // being 1 or 0 - the operators yield 1/0 regardless, and the phi carries
    // the operand's own value on the short-circuit edge, so a lost conversion
    // to boolean shows up only on these.
    int v[5];
    int i, j, k, l;

    v[0] = 0;
    v[1] = 1;
    v[2] = -1;
    v[3] = 2;
    v[4] = 7;

    for (i = 0; i < 5; i++) {
        for (j = 0; j < 5; j++) {
            int a = v[i];
            int b = v[j];

            if (xor2(a, b) != ((!!a) != (!!b))) return 1;

            for (k = 0; k < 5; k++) {
                int c = v[k];

                if (and_or(a, b, c) != ((!!a) & ((!!b) | (!!c)))) return 2;
                if (or_and(a, b, c) != ((!!a) | ((!!b) & (!!c)))) return 3;
                if (tern_arms(a, b, c) != (a ? ((!!b) & (!!c)) : ((!!b) | (!!c)))) return 4;
                if (branchy_left(a, b, c) != ((a ? (!!b) : (!!c)) & ((!!b) | (!!c)))) return 5;

                for (l = 0; l < 5; l++) {
                    int d = v[l];

                    if (deep(a, b, c, d) != ((!!a) | ((!!b) & ((!!c) | (!!d))))) return 6;
                    if (tern_in_arm(a, b, c, d) != (a ? (b ? c : d) : (c ? d : b))) return 7;
                }
            }
        }
    }

    return 0;
}
