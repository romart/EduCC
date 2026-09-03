// PRE inserts only at a *redundant* computation site: a value computed in
// one branch and never recomputed at or after the merge must stay where it
// is - no clones on the other edge, no extra phis beyond the one SSA
// construction made for 'x' itself. Guards against eager availability
// seeding spraying speculative code around merges.
int no_speculation(int a, int b, int flag) {
    int x = 0;
    if (flag > 0) {
        x = a + b;
    }
    return x;
}
