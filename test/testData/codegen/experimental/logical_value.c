// '&&' and '||' produce int 0 or 1, never the operand that decided the answer
// (C99 6.5.13p3, 6.5.14p3). ast2ir used to build the join phi straight out of
// the two operands - the left one on the short-circuit edge, the right one on
// the other - which is only ever right when both happen to already be 0 or 1.
//
//     int f(int x, int y) { return x || y; }   f(7, 0)  ->  7, not 1
//
// Nothing caught it because every fixture in the tree used the result as an if
// condition, where any non-zero value is as good as 1. So this file uses the
// results as *values*: compared against 0/1, summed, indexed with, returned.
// A companion defect came from the same line - the phi was typed IR_BOOL while
// every other int-valued expression is IR_I32, so a conditional with one
// logical arm ('c ? 0 : (a || b)') hit translateTernary's same-type assertion
// and never compiled at all. That assertion also fires for an unrelated
// reason - the constant cache ignoring the type asked of it, which is what
// tinyc/46_grep.c trips over - so this file pins down only the typing of a
// logical expression, not that assertion in general.
//
// Short-circuiting itself is checked too, since making the operands yield 0/1
// means evaluating something extra on the surviving edge: the right operand
// must still not run when the left one already settled it, and neither operand
// may be evaluated twice.
//
// Float operands are deliberately absent: 'x && y' on doubles is wrong in the
// legacy backend as well, so it is not this pipeline's bug and does not belong
// in a fixture that both configurations have to pass.
//
// Expected values confirmed against gcc.

// The core of it: value position, so the result must be exactly 0 or 1.
int and_v(int a, int b) {
    return a && b;
}

int or_v(int a, int b) {
    return a || b;
}

// Condition position, which is what always worked - it has to keep working.
int and_c(int a, int b) {
    if (a && b) return 111;
    return 222;
}

int or_c(int a, int b) {
    if (a || b) return 111;
    return 222;
}

// Feeding the result into arithmetic pins the value down to 0/1 without any
// comparison in the way: with the operands carried through raw this returns
// 'a + b' for suitable inputs rather than a count of how many were true.
int count_true(int a, int b) {
    return (a != 0) + (b != 0) - (a && b) + (a || b);
}

// The result as an operand of another comparison.
int is_one(int a, int b) {
    return (a || b) == 1;
}

// A logical expression as a ternary arm - int vs IR_BOOL, the mismatch that
// used to abort translation outright.
int tern_logical_arm(int a, int b, int c) {
    return c ? 0 : (a || b);
}

int tern_both_logical(int a, int b, int c) {
    return c ? (a && b) : (a || b);
}

// Negation of a logical expression, which reads the value rather than the flag
// that produced it.
int not_or(int a, int b) {
    return !(a || b);
}

// Pointer operands: null is false, anything else true, and the result is still
// an int. These were broken along with the rest until the operands started
// being compared against zero.
int p_and(int *p, int *q) {
    return p && q;
}

int p_or(int *p, int *q) {
    return p || q;
}

// Side-effect counters for the short-circuit checks. Touching a global makes
// these fall back to the legacy backend, which is the point - the count is
// observed from the other side of the boundary.
int calls;

int bump(int r) {
    calls = calls + 1;
    return r;
}

int g1;
int g2;

int main(void) {
    // 0 and 1, plus values true or false without being either - the raw-operand
    // bug is invisible unless an operand is something other than 0 or 1.
    int v[6];
    int i, j;

    v[0] = 0;
    v[1] = 1;
    v[2] = -1;
    v[3] = 2;
    v[4] = 7;
    v[5] = -8;

    for (i = 0; i < 6; i++) {
        for (j = 0; j < 6; j++) {
            int a = v[i];
            int b = v[j];
            int ea = (a != 0) && (b != 0) ? 1 : 0;
            int eo = (a != 0) || (b != 0) ? 1 : 0;

            if (and_v(a, b) != ea) return 1;
            if (or_v(a, b) != eo) return 2;
            if (and_c(a, b) != (ea ? 111 : 222)) return 3;
            if (or_c(a, b) != (eo ? 111 : 222)) return 4;
            if (count_true(a, b) != (a != 0) + (b != 0) - ea + eo) return 5;
            if (is_one(a, b) != eo) return 6;
            if (not_or(a, b) != (eo ? 0 : 1)) return 7;
            if (tern_logical_arm(a, b, 0) != eo) return 8;
            if (tern_logical_arm(a, b, 1) != 0) return 9;
            if (tern_both_logical(a, b, 1) != ea) return 10;
            if (tern_both_logical(a, b, 0) != eo) return 11;

            // The result is 0 or 1 and nothing else, so it is a valid index
            // into a two-element array. A leaked operand reads out of bounds.
            {
                int t[2];
                t[0] = 40;
                t[1] = 50;
                if (t[and_v(a, b)] != (ea ? 50 : 40)) return 12;
                if (t[or_v(a, b)] != (eo ? 50 : 40)) return 13;
            }
        }
    }

    if (p_and(&g1, &g2) != 1) return 14;
    if (p_and(&g1, 0) != 0) return 15;
    if (p_and(0, &g2) != 0) return 16;
    if (p_and(0, 0) != 0) return 17;
    if (p_or(&g1, &g2) != 1) return 18;
    if (p_or(&g1, 0) != 1) return 19;
    if (p_or(0, &g2) != 1) return 20;
    if (p_or(0, 0) != 0) return 21;

    // '&&' stops when the left operand is false, '||' when it is true; in the
    // other case both operands run, exactly once each.
    calls = 0;
    if (bump(0) && bump(1)) return 22;
    if (calls != 1) return 23;

    calls = 0;
    if (bump(1) && bump(0)) return 24;
    if (calls != 2) return 25;

    calls = 0;
    if (!(bump(1) || bump(1))) return 26;
    if (calls != 1) return 27;

    calls = 0;
    if (bump(0) || bump(0)) return 28;
    if (calls != 2) return 29;

    // The same, with the result taken as a value rather than branched on.
    calls = 0;
    if ((bump(0) && bump(1)) != 0) return 30;
    if (calls != 1) return 31;

    calls = 0;
    if ((bump(5) || bump(9)) != 1) return 32;
    if (calls != 1) return 33;

    return 0;
}
