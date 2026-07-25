// Two joins, one real and one dead, so the removal has to tell them apart.
// The first 'if' is genuinely conditional and its phi over 'a' and 'b' must
// survive with both entries intact. The second is 'if (0)': its join loses
// the dead entry, is left with a single input, and collapses into it - so
// the return ends up reading the *first* phi.
//
// A removal that reached too far would take the live phi with it and a
// removal that stopped too early would leave the second phi standing with a
// dangling entry, and the baseline separates the two.
int scp_dead_arm_live_phi(int a, int b, int flag) {
    int x;
    if (flag > 0) {
        x = a;
    } else {
        x = b;
    }
    if (0) {
        x = a + b;
    }
    return x;
}
