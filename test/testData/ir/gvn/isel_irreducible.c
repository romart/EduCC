// A loop with two entries, which is a CFG shape nothing else in the corpus
// produces and which the backend had never been fed until 'goto <label>'
// started translating.
//
// Every loop the structured statements can build has a single header that
// dominates the whole body. Jumping into the middle of one breaks that: the
// loop below is entered either at 'again' (from the goto) or at the while test
// (by falling in), so neither block dominates the other and the loop has no
// header at all. Reducible-CFG assumptions are the classic silent failure in a
// backend, and this is the only way to write a CFG that violates them here.
//
// What it puts pressure on, in pipeline order:
//
//   dominators   'again' and the while head are both dominated by the block
//                that branches on 'seed', and by neither each other. Any code
//                that assumes a loop body is dominated by one entry is wrong
//                here.
//   SSA          both entry blocks need a phi for 'i', carrying a different
//                pair of incoming values - the two are not copies of one
//                another and gvn must not merge them.
//   stage 0      those phis put copies on the edges of a loop that is entered
//                twice, including the back edge.
//   stage 1      block layout is a reverse postorder walk of this, so the
//                order the blocks come out in is not the order they are
//                written in, and one of the two loop entries necessarily
//                reaches its target by a jump rather than by falling through.
//
// 'acc' exists so that a second value is live across both entry edges rather
// than just the induction variable, which is what makes the phi copies on
// those edges non-trivial. The multiply keeps gvn from folding the body away.
//
// Values, checked against both the legacy pipeline and gcc, for whoever turns
// this into an executable fixture at step 6: (1, 2, 3) == 5, (0, 2, 3) == 11,
// (0, 0, 3) == 1 - the last being the case where the loop body never runs but
// the goto entry still has to be laid out.
int isel_irreducible(int seed, int n, int step) {
    int i = 0;
    int acc = 1;

    if (seed) goto again;

    while (i < n) {
        acc = acc * step;
    again:
        i = i + 1;
    }

    return acc + i;
}
