// The statement after an unconditional 'return' becomes a block no edge
// reaches, so it is dead as a whole rather than instruction by instruction:
// its computations do have uses - each other - which is precisely why the
// use-count sweep alone cannot touch them and the unreachable-block walk has
// to. The baseline pins down that the block is gone entirely, not merely
// emptied, and that the phi merging the two live returns at the exit block
// is left intact beside it.
//
// As in dce_side_effects.c, this has already happened by the gvn phase:
// buildDominatorInfo calls cleanupUnreachableBlock during SSA construction,
// so both snapshots show the same five blocks. What is pinned down is the
// property, not which pass first established it - and in particular that
// neither gvn nor the dce pass afterwards resurrects or trips over the
// removal (gvn walks the block list to poison algoIdx and to split critical
// edges, so a half-unlinked block would surface right here).
int dce_unreachable(int a, int b) {
    if (a > b) {
        return a + b;
    }
    return a - b;
    return (a + b) * (a - b);
}
