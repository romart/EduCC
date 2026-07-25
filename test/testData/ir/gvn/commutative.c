// GVN canonicalizes commutative operations by sorting input value numbers,
// so 'a + b' and 'b + a' share one value number: the second MUL's inputs
// both resolve to the first '(a + b)' and the second '(b + a)*(b + a)' MUL
// is replaced by the first one. The dump is taken right after gvn, before
// dce sweeps the dead duplicates.
int commutative(int a, int b) {
    return (a + b) * (a + b) + (b + a) * (b + a);
}
