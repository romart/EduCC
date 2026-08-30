// A floating-point operand of '&&', '||' or '!' is never actually tested
// against zero, so every one of them answers as though the operand were true.
//
// The truth test for a scalar is "compare against zero", and for a double that
// has to be a floating-point compare. The legacy backend emits an *integer*
// one. From its own output for 'a && b' on two doubles:
//
//     movsd  -0x10(%rbp),%xmm0    ; the operand goes into an SSE register
//     test   %rax,%rax            ; ...and an unrelated GP register is tested
//     je     <false>
//
// There is no comisd/ucomisd anywhere in the function. %rax holds whatever was
// left there, so the branch is decided by garbage - which in practice means
// 'and_dd(2.5, 0.0)' returns 1, and so does every other combination including
// 'and_dd(0.0, 0.0)'. That is where it still stands: fixing it belongs in
// src/x86_64/codegen_x86_64.c and is out of scope by decision, so this file is
// muted for the legacy backend alone.
//
// Under '-experimental' it passes, and what it took is the useful part of the
// record (design doc section 11 step 20). Three separate defects, only the
// first of which is about floats:
//
//   - Nothing checked an operand's register class on the way out of stage 3.
//     xmm8 and r8 are different ids in the flat physical namespace but the
//     same encoded register number, so an operand arriving in the wrong bank
//     did not fail to encode - it encoded as a different real register, and
//     the machine IR still read correctly while the bytes did not.
//
//   - '!' on a double selected an integer 'test' with an SSE operand
//     ('test.8 $xmm8, $xmm8', not an instruction that exists), which the
//     emitter then wrote as 'test %r8,%r8'. It is built as a float compare
//     against zero now, the same choice the '&&' / '||' translation makes.
//
//   - The *left* operand of '&&' / '||' was branched on as it stood. The
//     parser wraps every other controlling expression in a '!= 0' of its own -
//     an 'if', 'while' or ternary on a double arrives already compared, which
//     is why cond_d below was correct all along - and the operands of a
//     logical operator are the one place it does not.
//
// The first check was what found the third: it is the class of bug with reach
// beyond floats, and an undefined double (the placeholder SSA leaves where a
// promoted local is read on a path that never wrote it) was materializing
// through it too.
//
// Expected values below are gcc's.

// The core case: both operands floating-point.
int and_dd(double a, double b) {
    return a && b;
}

int or_dd(double a, double b) {
    return a || b;
}

// Mixed operands, which is the more common way to hit this in real code. Only
// the double operand is mistested, so the answer depends on which side it is.
int and_di(double a, int b) {
    return a && b;
}

int or_id(int a, double b) {
    return a || b;
}

// float rather than double, to show it is not about the operand's width.
int and_ff(float a, float b) {
    return a && b;
}

// Logical negation of a float, the same truth test in unary form.
int not_d(double a) {
    return !a;
}

// The control: a double as the controlling expression of an 'if' is correct
// today, and so is an explicit comparison. Whatever fixes the above must not
// disturb these.
int cond_d(double a) {
    if (a) return 1;
    return 0;
}

int ne_d(double a) {
    return a != 0.0;
}

int main(void) {
    double v[4];
    int i, j;

    v[0] = 0.0;
    v[1] = 1.0;
    v[2] = -1.5;
    v[3] = 2.5;

    // Negative and non-1 values matter here for the same reason they do in
    // codegen/experimental/logical_value.c: the result must be exactly 0 or 1,
    // not whichever operand happened to decide it.
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            double a = v[i];
            double b = v[j];
            int ea = (a != 0.0) && (b != 0.0) ? 1 : 0;
            int eo = (a != 0.0) || (b != 0.0) ? 1 : 0;

            if (and_dd(a, b) != ea) return 1;
            if (or_dd(a, b) != eo) return 2;
            if (and_ff((float)a, (float)b) != ea) return 3;

            if (and_di(a, (int)b) != ((a != 0.0) && ((int)b != 0) ? 1 : 0)) return 4;
            if (or_id((int)a, b) != (((int)a != 0) || (b != 0.0) ? 1 : 0)) return 5;

            if (not_d(a) != (a != 0.0 ? 0 : 1)) return 6;

            // The control cases.
            if (cond_d(a) != (a != 0.0 ? 1 : 0)) return 7;
            if (ne_d(a) != (a != 0.0 ? 1 : 0)) return 8;
        }
    }

    // Short-circuiting must survive whatever fix lands: with the left operand
    // false the right one is not evaluated, so a division by zero on the right
    // must not happen.
    {
        double zero = v[0];
        if (zero && (1.0 / zero) != 0.0) return 9;
        if (!(zero == 0.0 || (1.0 / zero) != 0.0)) return 10;
    }

    return 0;
}
