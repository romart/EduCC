// PRE, partially redundant case: 'a + b' is available only along the
// then-edge at the merge. The critical edge from the branch header to the
// merge is split (the '<crit_splitter>' block), the computation is cloned
// into that splitter - executing exactly when the then-branch was skipped,
// never speculatively - and the recomputation at the merge becomes a phi
// over the branch result and the clone.
int partial_redundancy(int a, int b, int flag) {
    int x = 0;
    if (flag > 0) {
        x = a + b;
    }
    return x + (a + b);
}
