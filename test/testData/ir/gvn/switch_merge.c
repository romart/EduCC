// A switch gives PRE what an 'if' never can: a join with *four* incoming
// edges, two of which already carry 'a + b' (the 'case 0' and 'case 1' arms)
// while the other two do not. The value is therefore partially redundant
// across an arity the two-predecessor fixtures cannot reach, and PRE must
// clone the computation into exactly the two arms that miss it and build a
// single four-input phi over all of them.
//
// Each case block ends in an unconditional branch to the join, so none of
// these edges is critical and no splitter is needed - the clones land
// directly in the 'case 2' and 'default' blocks. That is the counterpart to
// switch_shared_case.c, where the same construct does need splitting.
int switch_merge(int a, int b, int sel) {
    int r = 0;
    switch (sel) {
    case 0:
        r = a + b;
        break;
    case 1:
        r = (a + b) + 1;
        break;
    case 2:
        r = a - b;
        break;
    default:
        r = 0;
        break;
    }
    return r + (a + b);
}
