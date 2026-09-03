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
// labels, so promoting it puts a phi in each, and phi destruction wants a copy
// on each incoming edge - while here two of those edges share a predecessor
// with nowhere to put anything edge-specific.
//
// It is promoted anyway, which is what this dump pins. Each 'goto *' block
// carries the copies for *both* labels, as one parallel assignment: a phi's
// register is read only in the block that phi heads and every edge into that
// block writes it, so on the way to A the copy belonging to B writes a
// register nothing will read before B is entered again and writes it afresh.
// So 'acc' has no stack slot below and each dispatch block holds two copies.
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
