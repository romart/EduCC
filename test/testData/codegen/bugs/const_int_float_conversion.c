// A negative integer constant converted to a floating type, folded by the IR.
//
// The IR constant evaluator holds every integer constant in an int64_const_t,
// which is unsigned. Converting one to a float without asking the source type's
// signedness therefore made every negative value a number just under 2^64:
// '(double)(-1)' folded to 18446744073709551616.0. Instruction selection had
// always asked - selectConversion widens by isUnsignedIrOperand and then
// converts signed - so the two disagreed about a value only one of them could
// see, which is the failure mode isUnsignedIrOperand's own comment in ir.h
// warns about for '-7 / 2'.
//
// Every check runs the value through a variable first. Written as one
// expression - 'if ((double)(-1) != -1.0)' - the whole comparison is a constant
// the *AST* evaluator folds, correctly, in src/evaluate.c, and the IR one never
// sees it. That is why a bug this broad went unnoticed: the obvious way to
// write the test does not reach the code that had it.
//
// And each is written twice, once on a value the evaluator can fold and once on
// the same value routed through 'opaque' so nothing can, because the property
// is that the two agree. Either half alone was self-consistent throughout.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// Nothing folds through a call, so these reach the instructions instead.
static double opaqueD(double v) { return v; }
static long opaqueL(long v) { return v; }
static unsigned int opaqueU(unsigned int v) { return v; }

static double takesDouble(double v) { return v; }
static double returnsNegative(void) { return -1; }

// A static initializer is evaluated somewhere else again (src/evaluate.c, at
// parse time), and was right throughout. Here to keep it that way.
double globalFromNegative = -1;

int main(void) {
    double init = -1;
    double assigned;
    double arr[2];
    double cast = (double)(-1);
    double scaled = -2;
    float narrow = -1;
    double smallest = -2147483647 - 1;
    double unsignedWide = 4294967295u;
    int back;

    assigned = -1;
    arr[0] = -1;

    // -------- the conversion, in each place one is inserted --------

    check(init == -1.0, 1);
    check(assigned == -1.0, 2);
    check(arr[0] == -1.0, 3);
    check(cast == -1.0, 4);
    check(takesDouble(-1) == -1.0, 5);
    check(returnsNegative() == -1.0, 6);
    check(globalFromNegative == -1.0, 7);

    // Not just the sign: the magnitude was wrong by 2^64, so anything computed
    // from it is wrong by a lot rather than by a little.
    check(scaled * 100.0 == -200.0, 8);
    check(narrow == -1.0f, 9);
    check(smallest == -2147483648.0, 10);

    // Unsigned sources must not change, which is what makes the fix a question
    // about the source type rather than a sign flip.
    check(unsignedWide == 4294967295.0, 11);
    check(opaqueD((double)opaqueU(4294967295u)) == 4294967295.0, 12);

    // -------- the same values with nothing left to fold --------

    check(opaqueD((double)opaqueL(-1)) == -1.0, 13);
    check(opaqueD((double)opaqueL(-2)) * 100.0 == -200.0, 14);
    check((float)opaqueL(-1) == -1.0f, 15);
    check(opaqueD((double)opaqueL(-2147483647 - 1)) == -2147483648.0, 16);

    // -------- floating to integer, truncating toward zero --------

    // Toward zero and not toward minus infinity, so this is -1 rather than -2.
    // These were not part of the bug - on x86-64 the unsigned cast the fold
    // used to make compiles to the signed conversion and lands on the same bits
    // - but the fold is written the other way now and this is what says so.
    back = (int)(-1.5);
    check(back == -1, 17);
    check((int)opaqueD(-1.5) == -1, 18);

    back = (int)(-2.9);
    check(back == -2, 19);
    check((int)opaqueD(-2.9) == -2, 20);

    back = (int)(-0.5);
    check(back == 0, 21);
    check((int)opaqueD(-0.5) == 0, 22);

    // -------- round trip --------

    back = (int)(double)(-7);
    check(back == -7, 23);
    check((int)opaqueD((double)opaqueL(-7)) == -7, 24);

    return failures;
}
