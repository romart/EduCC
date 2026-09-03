// Regression test for scoping in the dominator-based GVN walk: the else
// branch used to be visited first in RPO, making its 'arr + index' the
// class leader, which doesn't dominate the then branch - so the duplicate
// INSIDE the then branch was silently missed. With the scoped dominator-
// tree walk each branch folds its own duplicate (ptr2's computation reuses
// ptr1's), while the two sibling branches must NOT be merged with each
// other - neither dominates the other (catching that partial redundancy
// is PRE's job, not plain GVN's).
int branch_dup(int *arr, int index, int flag) {
    int *ptr1, *ptr2;
    if (flag > 0) {
        ptr1 = arr + index;
        ptr2 = arr + index;
    } else {
        ptr1 = arr + index;
        ptr2 = arr + index;
    }
    return *ptr1 + *ptr2;
}
