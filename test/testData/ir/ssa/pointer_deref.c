// Multi-level indirection, the case translateDeReference() (src/ir/ast2ir.c)
// got wrong: each '*' must load the operand's value to get the address the
// next level works on, so '**pp = **pp + 1' needs two chained LOADs before
// the ADD, and the STORE has to go through '*pp', not through 'pp' itself.
// See pointer_array.c for the single-level version.
int pointer_deref(int **pp) {
    int *p = *pp;
    **pp = **pp + 1;
    return *p;
}
