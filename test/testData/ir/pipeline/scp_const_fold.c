// Straight-line constant folding through a chain: '2 + 3 * 4' collapses to
// 14, that feeds '* 10' to give 140, and '- 40' finally gives 100, so the
// function reduces to 'a + 100'. Each step only folds because the lattice
// value of the step before it settled on a constant, which is what makes
// this a chain rather than three independent folds - a propagation that
// stops early leaves half the arithmetic standing and shows up immediately.
//
// The intermediate results are visible as constants in the entry block
// (the cache holds every value the fold produced along the way, whether or
// not anything still reads it); only the final one is actually used.
int scp_const_fold(int a) {
    int x = 2 + 3 * 4;
    int y = x * 10 - 40;
    return a + y;
}
