// The negative of phi_dedup.c: 'x' and 'y' are built from the very same two
// definitions, but each takes them from the opposite arm, so the join holds
// two phis with identical operand *sets* and opposite edge pairings. They
// evaluate differently on both paths and must never be merged - which is the
// regression a comparison that matches operands without checking which edge
// each one arrives on would introduce.
int phi_swapped_edges(int a, int b, int flag) {
    int x, y;
    if (flag > 0) {
        x = a;
        y = b;
    } else {
        x = b;
        y = a;
    }

    return x - y;
}
