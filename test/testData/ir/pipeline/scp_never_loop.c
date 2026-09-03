// A loop whose body can never run. Removing it deletes a *back* edge, so the
// header's loop-carried phi loses the entry that referred to a value the body
// defined - and that value is only reachable from the phi, which is exactly
// the circular use that keeps a naive sweep from ever emptying the block.
// With one input left the phi collapses into it, so the return reads the
// parameter directly and no phi survives anywhere.
//
// One artifact to expect in the baseline: the header still reports itself in
// its domination frontier. That was true while the back edge existed, and
// dce does not recompute frontiers - it only detaches removed blocks from
// their dominator's child list. Nothing runs after dce today, so the stale
// entry is inert; it is recorded here rather than quietly tolerated.
int scp_never_loop(int a, int n) {
    int acc = a;
    while (0) {
        acc = acc + 1;
    }
    return acc;
}
