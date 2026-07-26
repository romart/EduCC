// The swap problem, which is what makes phi destruction more than "emit a copy
// per edge".
//
// Both loops rotate their variables, so the phis in each loop header take each
// other's results along the back edge: 'a = phi(.., b), b = phi(.., a)' in the
// first, a three-way rotation in the second. The copies for one edge are a
// parallel assignment - they all take effect at once on entry to the header -
// and emitting them in the order they are written down instead would have the
// first copy destroy the value the second still needs, leaving both variables
// holding the same thing.
//
// The two sizes are both worth having: a 2-cycle needs one temporary and then
// nothing else is left, while a 3-cycle needs a temporary and then two further
// copies that only become emittable because it was taken. See
// sequentializeCopies() in src/ir/codegen/prepare.c.
int phi_swap_cycle(int a, int b, int c, int n) {
    while (n > 0) {
        int t = a;
        a = b;
        b = t;
        n = n - 1;
    }

    while (n < 0) {
        int t = a;
        a = b;
        b = c;
        c = t;
        n = n + 1;
    }

    return a * 100 + b * 10 + c;
}
