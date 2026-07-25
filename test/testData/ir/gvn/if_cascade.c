// An else-if chain lowers to *nested* diamonds, so the joins nest too: the
// outermost one merges the 'k == 1' arm with the join of everything else,
// which itself merges the 'k == 2' arm with the join below it. PRE walks
// them outermost-first in reverse postorder and only ever looks at a block's
// immediate predecessors, so at the top join 'a + b' is available from the
// 'k == 1' arm and missing from the inner join - which gets the clone, even
// though two of the paths feeding *it* already computed the value further
// up. Sinking the clone past that point would need a second iteration; this
// baseline records the single-pass result.
//
// The 'k == 3' arm writes 'b + a' rather than 'a + b' so the commutative
// normalization is exercised inside a nest of joins, and the innermost
// else edge is the one critical edge in the function, so its splitter block
// gets created even though nothing ends up being inserted there.
int if_cascade(int a, int b, int k) {
    int r = 0;
    if (k == 1) {
        r = a + b;
    } else if (k == 2) {
        r = (a + b) * 2;
    } else if (k == 3) {
        r = b + a;
    }
    return r + (a + b);
}
