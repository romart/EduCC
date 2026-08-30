// A floating value that reaches emission in the wrong register bank.
//
// Physical registers live in one flat namespace whose two halves collapse onto
// the same encoded number - xmm8 is id 24 and r8 is id 8, and both encode as 8
// - so an operand in the wrong bank does not fail to encode. It encodes as a
// different real register, every dump still reads correctly, and only the bytes
// are wrong. Stage 3 checks the class now (design doc section 8); this pins the
// producer that check found.
//
// 'IR_BAD' is the placeholder buildSSA leaves where a promoted local is read on
// a path that never wrote it. Selection materialized its zero with a plain
// 'mov $0, <dst>' whatever the type, so for a double it wrote the general
// register of the same number as the xmm the value lives in - clobbering that
// register and leaving the xmm holding whatever it held. The zero goes through
// a general register and across with movd now, exactly as a float constant
// does.
//
// Be clear about what this fixture catches and what it does not. **Today it
// fails only through stage 3's assertion**: revert the selection rule and the
// compiler aborts on this file, but revert the assertion as well and every
// check below still passes. Both halves of the damage are currently invisible
// at run time - the undefined value is never read (each function returns
// something that does not depend on it, so what the checks assert is defined
// behaviour), and the clobbered general register is dead because the allocator
// spills everything and reloads before each use. The clobber becomes a wrong
// answer the day an allocator keeps a value in a register across a call, which
// is why the guard below is checked even though it cannot fail yet.
//
// Every function below therefore exists to make selection build the phi, which
// is all it takes to reach the rule.

// The core case: a double local written on one path and a phi over both.
double undef_double(int c) {
    double d;

    if (c) {
        d = 1.5;
    }

    return c ? d : 0.25;
}

// float rather than double, since the two take different movd widths.
float undef_float(int c) {
    float f;

    if (c) {
        f = 2.5f;
    }

    return c ? f : 0.5f;
}

// A loop, so the phi is at a back edge rather than at a join. The value is
// undefined on the entry edge only.
double undef_in_loop(int n) {
    double acc;
    int i;

    for (i = 0; i < n; i++) {
        acc = (double)i;
    }

    return n > 0 ? acc : -1.0;
}

// A long double takes the other path through selection - an IR_F80 value is an
// address and allocates in a general register - so its undef must not go
// through movd.
long double undef_long_double(int c) {
    long double l;

    if (c) {
        l = 3.5L;
    }

    return c ? l : 0.75L;
}

int main(void) {
    int i;
    // What the wrong-bank 'mov $0' overwrote. Dead today - see the header -
    // and checked anyway, because the allocator is what makes it dead.
    long guard = 0x5eed;

    for (i = 0; i < 2; i++) {
        if (undef_double(i) != (i ? 1.5 : 0.25)) return 1;
        if (undef_float(i) != (i ? 2.5f : 0.5f)) return 2;
        if (undef_long_double(i) != (i ? 3.5L : 0.75L)) return 3;
        if (guard != 0x5eed) return 4;
    }

    if (undef_in_loop(0) != -1.0) return 5;
    if (undef_in_loop(3) != 2.0) return 6;
    if (guard != 0x5eed) return 7;

    return 0;
}
