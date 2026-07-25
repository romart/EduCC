// The case where a phi loses an entry and must nonetheless remain a phi. An
// unreachable 'continue' gives the loop header a third predecessor that then
// dies, so both header phis go from three inputs to two rather than
// collapsing - the path that ends in a single input is already covered by
// scp_never_loop.c, and this is the other half of that branch.
//
// Both phis are awkward on purpose. The one for 'i' carries the *same*
// definition along two different edges, so an entry dropped by value rather
// than by position could unpair the operands from the edges they belong to.
// The one for 'sum' takes itself along the dead edge, so removing that entry
// walks a self-referential use. Whichever entry goes, the two survivors must
// still name the preheader and the surviving latch.
int scp_dead_backedge(int a, int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        i = i + 1;
        if (0) {
            continue;
        }
        sum = sum + a;
    }
    return sum;
}
