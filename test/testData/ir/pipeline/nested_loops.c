// Loop-invariant 'a * b' recomputed at three depths: inside the inner body,
// in the outer body after the inner loop, and after both loops. Every one of
// those blocks is reached from a header whose latch is still unprocessed in
// PRE's reverse-postorder walk, so nothing is available on entry to any of
// them and all three computations correctly survive - the conservative
// back-edge rule of loop_backedge_guard.c, now at two nesting levels.
//
// The point of the nesting is the inner header: it sits inside the outer
// loop, so it is reached both from an outer-loop block *and* from its own
// latch, and a walk that confused "predecessor already visited" with
// "predecessor on a back edge" would wrongly seed it from the outer body.
// Hoisting these genuinely needs LICM or phi-translating PRE; until then the
// baseline pins down that GVN leaves them alone rather than moving them
// somewhere that does not dominate the use.
int nested_loops(int a, int b, int n, int m) {
    int total = 0;
    int i = 0;
    while (i < n) {
        int j = 0;
        while (j < m) {
            total = total + (a * b);
            j = j + 1;
        }
        total = total + (a * b);
        i = i + 1;
    }
    return total + (a * b);
}
