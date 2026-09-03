// GVN/PRE state (the VN table, availOut, algoIdx numbering) is entirely
// per-function - translateAstToIr() drives gvn() once per IrFunction, each
// with its own fresh VNTable. Two functions computing the identical
// expression 'a + b' must fold independently within each function (VN 0
// reused for x/y locally) and must never be treated as redundant with each
// other, and the second function's numbering must not pick up where the
// first one's left off.
int first_fn(int a, int b) {
    int x = a + b;
    int y = a + b;
    return x + y;
}

int second_fn(int a, int b) {
    int x = a + b;
    int y = a + b;
    return x + y;
}
