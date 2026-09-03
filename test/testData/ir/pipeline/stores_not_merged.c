// Stores are side effects: two textually identical stores get distinct
// value numbers and both IR_M_STOREs must survive GVN (the second one is
// what a later observer sees; deleting either changes program behavior
// under aliasing). Only the shared address/value computations may fold.
void stores_not_merged(volatile int *p, int v) {
    *p = v;
    *p = v;
}
