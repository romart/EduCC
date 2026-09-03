// Loop headers are merge points whose back-edge predecessor (the latch) is
// visited after the header in PRE's reverse-postorder walk, so it is always
// "unprocessed" when the header itself is analyzed. 'a + b', computed once
// inside the loop body, is recomputed after the loop; since the preheader
// edge into the header never carries it, the header's availability for it
// stays unset and the post-loop recomputation is correctly left in place -
// loop-carried redundancy needs phi-translation, which PRE does not attempt
// (see the comment above pre() in gvn.c). Guards against a regression where
// an unprocessed back-edge predecessor is mistaken for one carrying no value.
int loop_backedge_guard(int a, int b, int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        sum = sum + (a + b);
        i = i + 1;
    }
    return sum + (a + b);
}
