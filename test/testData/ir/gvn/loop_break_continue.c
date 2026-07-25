// 'continue' and 'break' give the loop a second back edge and a second exit,
// so the header is a three-predecessor join (preheader plus two latches) and
// the exit block is a two-predecessor join. That is the stress case for the
// rule that a block is only seeded when *every* predecessor has been
// processed: two of the header's three edges come from below it in reverse
// postorder, and treating either as carrying a value would let PRE hoist
// into a path that has not been analyzed yet.
//
// The 'continue' also makes the header's phi for 'sum' take itself as an
// input along that edge - a self-referential phi that phi dedup must compare
// by identity without looping. 'a + b' stays computed once in the body and
// once after the loop, as the back-edge rule requires.
int loop_break_continue(int a, int b, int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        i = i + 1;
        if (i == 3) {
            continue;
        }
        if (i == 7) {
            break;
        }
        sum = sum + (a + b);
    }
    return sum + (a + b);
}
