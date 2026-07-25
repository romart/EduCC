// Straight-line pointer arithmetic CSE: ptr1 and ptr2 compute the same
// address, so the second MUL/ADD pair folds into the first and both loads
// go through one address computation. The loads themselves stay separate
// (no alias analysis).
int pointer_arith(int *arr, int index) {
    int *ptr1 = arr + index;
    int *ptr2 = arr + index;
    int result = *ptr1 + *ptr2;
    return result;
}
