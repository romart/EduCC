// Value numbering skips phis - what a phi evaluates to depends on the edge
// control arrived by - so mem2reg's one-phi-per-variable output survives both
// stages untouched, however redundant it is. 'x' and 'y' are assigned the same
// value on either arm, so the first join holds two phis mapping every incoming
// edge to the same definition; they are interchangeable and must collapse.
//
// The second join is what pins down that the sweep repeats: 'p' and 'q' start
// out taking *different* phis (x's and y's), so they are only equal once the
// inner pair has been merged - a single pass would leave one of them behind.
// The merged-away phis are left dead for dce (see the .dce.txt baseline).
int phi_dedup(int a, int b, int flag, int flag2) {
    int x, y;
    if (flag > 0) {
        x = a;
        y = a;
    } else {
        x = b;
        y = b;
    }

    int p = x;
    int q = y;
    if (flag2 > 0) {
        p = a;
        q = a;
    }

    return p + q;
}
