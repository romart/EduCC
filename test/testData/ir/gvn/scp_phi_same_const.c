// A phi is only constant when *every* incoming edge carries the same
// constant, which is the one lattice rule that has nothing to do with an
// instruction's operands being constant individually. Both arms assign 7, so
// the phi meets to 7 and disappears entirely - the return reads the constant
// directly, even though the branch itself is not resolvable and both blocks
// survive.
//
// This is also the shape that stops gvn from being asked anything: by the
// time it runs there is no phi left to deduplicate and no expression left to
// number, so its baselines record a function scp already finished with.
int scp_phi_same_const(int flag) {
    int x;
    if (flag > 0) {
        x = 7;
    } else {
        x = 7;
    }
    return x;
}
