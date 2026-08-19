// The one critical edge stage 0 cannot split, pinned as a CFG shape.
//
// Both computed gotos below reach both labels, so each 'goto *' block has two
// successors and each label block two predecessors - four critical edges. An
// edge is split by interposing a block and repointing the terminator at it,
// which works for a conditional or a switch because their targets are written
// down in the terminator. A computed goto's are not: it jumps through an
// address produced by '&&label' somewhere else entirely, so there is nothing
// local to repoint, and splitCriticalEdges() skips IR_IBRANCH blocks outright.
//
// That matters because 'acc' is live across both gotos and read by both
// labels, so promoting it would put a phi in each: phi destruction would then
// want a copy on each incoming edge, and here two of those edges share a
// predecessor with nowhere to put anything edge-specific.
//
// So it is not promoted. buildSSA asks hasUnsplittablePredecessor() before it
// counts an alloca as a candidate and leaves this one in memory, which is why
// 'acc' still has its stores and loads below and neither label has a phi. That
// costs this one variable a load and a store and nothing else - the function
// is still built by the IR backend, which is the point: the alternative was
// handing the whole thing back to the legacy one.
int computed_goto_phi(int n, int k) {
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
