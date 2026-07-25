// Real 'arr[3]' indexing syntax lowers to a GET_ELEMENT_PTR + load, same as
// raw pointer arithmetic (pointer_arith.c), so PRE folds its address
// computation the same way it folds plain arithmetic: the GEP for 'arr[3]'
// is fully computed only in the then-branch, so PRE splits the critical
// else-edge, clones the GEP into the splitter, and phis the two together.
// The IR_M_LOAD through that merged address stays a fresh instruction in
// each block regardless - loads are never merged, matching loads_not_merged.c.
int array_index_pre(int arr[], int k, int flag) {
    int x = 0;
    if (flag > 0) {
        x = arr[3] + k;
    }
    return x + (arr[3] + k);
}
