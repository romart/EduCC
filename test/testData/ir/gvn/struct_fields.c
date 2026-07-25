// Struct member access lowers to an IR_GET_ELEMENT_PTR carrying the member's
// offset, so 'p->x' is a pure address computation and the two occurrences
// must fold to one - the member identity is part of the expression, so this
// also pins down that two different members of the same struct would not.
//
// The loads through that one shared address stay separate, for the same
// reason as everywhere else: nothing here proves no store landed on p->x in
// between, and GVN has no alias analysis. Only the addressing folds.
struct Point {
    int x;
    int y;
};

int struct_fields(struct Point *p, int k) {
    int a = p->x + k;
    int b = p->x + k;
    return a + b;
}
