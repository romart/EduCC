// Two loads through the same pointer must NOT be merged by GVN: with no
// alias analysis there is no proof memory didn't change between them (and
// here the store in between makes merging observably wrong - the result
// must be 1 + 2, not 1 + 1). Both IR_M_LOADs and the IR_M_STORE must
// survive; only the address computation is shared.
int loads_not_merged(int *p) {
    int a = *p;
    *p = a + 1;
    int b = *p;
    return a + b;
}
