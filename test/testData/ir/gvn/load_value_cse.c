// A load itself gets a unique value number (no alias analysis, so loads
// never merge with each other), but that VN must be *stable*: pure
// expressions consuming the loaded value are still subject to CSE. Here
// 'x + k' is computed twice over the same load and must fold into one ADD.
// This used to be broken by a fresh memory-epoch counter being minted on
// every VN query of the same load.
int load_value_cse(int *p, int k) {
    int x = *p;
    int y = x + k;
    int z = x + k;
    return y + z;
}
