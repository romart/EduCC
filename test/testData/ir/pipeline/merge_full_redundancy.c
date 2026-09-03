// PRE, fully redundant case: 'a + b' is computed in both branches, so at
// the merge point it is available across every incoming edge - just not
// from any single dominating instruction, which is why the dominator walk
// alone cannot fold it. The recomputation after the join must be replaced
// by a phi over the two branch results; no clone is inserted anywhere.
int merge_full_redundancy(int a, int b, int flag) {
    int x;
    if (flag > 0) {
        x = a + b;
    } else {
        x = a + b;
    }
    return x + (a + b);
}
