// The IR_TBRANCH counterpart of scp_const_branch.c. A constant selector
// leaves exactly one case executable, so processSwitchBranch() drops several
// edges in one go and the four-entry phi at the join loses them one at a
// time until a single input remains and it collapses into that value.
//
// Removing edges one after another is what makes this more than a bigger
// version of the two-way case: each removal has to leave the phi's operands
// paired with the right remaining edges, or the survivor collapses onto some
// other case's value - and with only 'a * b' left standing, picking up the
// wrong one is a silent miscompile rather than a crash.
int scp_const_switch(int a, int b) {
    int sel = 2;
    int r = 0;
    switch (sel) {
    case 0:
        r = a + b;
        break;
    case 1:
        r = a - b;
        break;
    case 2:
        r = a * b;
        break;
    default:
        r = 0;
        break;
    }
    return r;
}
