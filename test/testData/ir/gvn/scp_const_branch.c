// The regression test for a hang. When scp proves a condition constant it
// rewrites the IR_CBRANCH into a goto and drops the edge into the arm that
// can never run - but the *arm itself* still branches to the join, and the
// join's phi still lists it. dce then finds the arm unreachable and used to
// unlink it by editing the successor's predecessor vector directly, leaving
// that phi entry in place: the arm's value kept a use forever, so the block
// never went empty and unlinkAndEraseInstructions() spun until killed.
// Detaching through removeSuccessor() drops the phi entry with the edge.
//
// The whole 'k > 10' arm must be gone here, the phi with it, and the branch
// must be an unconditional goto - the function is just 'a - b'.
int scp_const_branch(int a, int b) {
    int k = 5;
    if (k > 10) {
        return a + b;
    }
    return a - b;
}
