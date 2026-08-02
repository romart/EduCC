// A floating-point operand of '&&', '||' or '!' is never actually tested
// against zero, so every one of them answers as though the operand were true.
//
// The truth test for a scalar is "compare against zero", and for a double that
// has to be a floating-point compare. EduCC emits an *integer* one. From the
// legacy backend's own output for 'a && b' on two doubles:
//
//     movsd  -0x10(%rbp),%xmm0    ; the operand goes into an SSE register
//     test   %rax,%rax            ; ...and an unrelated GP register is tested
//     je     <false>
//
// There is no comisd/ucomisd anywhere in the function. %rax holds whatever was
// left there, so the branch is decided by garbage - which in practice means
// 'and_dd(2.5, 0.0)' returns 1, and so does every other combination including
// 'and_dd(0.0, 0.0)'. The second operand goes the same way: 'test %rax,%rax'
// followed by 'setne %al'.
//
// For the binary operators this is a legacy-backend bug
// (src/x86_64/codegen_x86_64.c), not an IR-pipeline one: instruction selection
// does not cover them, so canEmitMachineFunction() declines and
// generateCodeForFile() hands them back. A file containing only 'and_dd'
// compiles to a byte-identical object with and without '-experimental'.
//
// 'not_d' is the exception, and it goes wrong twice more. The new backend does
// emit it, selection picks 'test' with an *SSE* operand - 'test.8 $xmm8, $xmm8',
// an instruction that does not exist - and emission then encodes that xmm
// operand as the same-numbered general-purpose register:
//
//     movsd  -0x8(%rbp),%xmm8
//     test   %r8,%r8               ; xmm8 became r8
//     sete   %r10b
//
// so 'sete' reads flags left by something unrelated. The register class is
// never checked on the way out, which is the part with reach beyond floats:
// any operand that reaches the emitter in the wrong bank is silently encoded
// as a different real register rather than refused.
//
// Note what is *not* broken, because it narrows the fix considerably: 'if (a)'
// on a double is correct (cond_d below), as is a written-out 'a != 0.0'. So the
// machinery for branching on a float exists and works; what is missing is its
// use when the float is an operand of a logical operator rather than the
// controlling expression of an 'if'.
//
// Because the branch is decided by a register nobody set, *which* check fails
// first depends on what the caller happened to leave in %rax: this file returns
// 1 (and_dd) compiled normally and 6 (not_d) under '-experimental', where main
// itself is emitted by the new backend and so leaves different garbage behind.
// Both are stable for a given build. Treat a sudden pass as something to
// confirm in the disassembly - a comisd against zero appearing - rather than as
// the bug being fixed, since garbage lining up favourably would look the same.
//
// Muted - see the sibling .muted file. Expected values below are gcc's.

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
