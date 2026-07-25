// '&&' is control flow, not an operator: the right operand gets its own
// block, joined back by a phi over the two ways the condition can be decided.
// So 'a + b' is computed inside the short-circuit block, on a path that is
// taken only when 'flag > 0' - and that block does not dominate the join
// below, which is exactly the shape dominance-based numbering cannot fold.
//
// Both edges out of the condition are critical here, so two splitters appear.
// PRE reaches the final join with 'a + b' available from the 'if' body and
// missing from the other edge, and puts its clone in that edge's splitter -
// which is the whole point of splitting: the clone must not run on the path
// where the '&&' short-circuited. Note that it clones rather than reusing the
// occurrence in the short-circuit block, since PRE only consults a block's
// immediate predecessors and that value does not survive the intervening
// join.
int short_circuit(int a, int b, int flag) {
    int r = 0;
    if (flag > 0 && (a + b) > 10) {
        r = a + b;
    }
    return r + (a + b);
}
