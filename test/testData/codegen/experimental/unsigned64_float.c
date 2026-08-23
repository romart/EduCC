// Conversions between a 64-bit unsigned integer and a floating type.
//
// SSE converts between a float and a *signed* 64-bit integer and nothing else.
// The half of the unsigned range at or above 2^63 therefore has to be handled
// separately, which needs a branch - and a branch is not something instruction
// selection can build here: it fills blocks that already exist and asserts that
// nothing invents one. So both directions were a refusal, and any function
// converting a 'size_t' or an 'unsigned long' to a double went to the legacy
// backend whole. They are expanded in ast2ir now, where blocks and phis are
// ordinary.
//
// Nothing narrower is affected: a 32-bit unsigned value is not negative as a
// signed 64-bit one, so selection already reaches those by widening. The checks
// on 'unsigned int' below are there to keep it that way.
//
// The interesting half is the rounding. Going up, a value at or above 2^63 is
// halved, converted, and doubled, and halving loses the low bit - so a value
// that was exactly halfway between two representable doubles would round the
// wrong way. Or-ing the lost bit back in ("round to odd") keeps it on the
// correct side of the midpoint; check 12 is the value that says so, and it
// fails on a plain shift.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// Nothing folds through a call, so the conversions actually run.
static unsigned long opaqueUL(unsigned long v) { return v; }
static unsigned int opaqueU(unsigned int v) { return v; }
static double opaqueD(double v) { return v; }
static float opaqueF(float v) { return v; }

int main(void) {
    // -------- unsigned 64 -> double, below 2^63 (the signed path) --------

    check((double)opaqueUL(0UL) == 0.0, 1);
    check((double)opaqueUL(5UL) == 5.0, 2);
    check((double)opaqueUL(4294967295UL) == 4294967295.0, 3);

    // 2^53 and its neighbours: the last integer every larger one is not.
    check((double)opaqueUL(9007199254740992UL) == 9007199254740992.0, 4);
    check((double)opaqueUL(9007199254740991UL) == 9007199254740991.0, 5);

    // 2^63 - 1, the largest value that still takes the signed path.
    check((double)opaqueUL(9223372036854775807UL) == 9223372036854775808.0, 6);

    // -------- unsigned 64 -> double, at or above 2^63 --------

    check((double)opaqueUL(9223372036854775808UL) == 9223372036854775808.0, 7);
    check((double)opaqueUL(18446744073709551615UL) == 18446744073709551616.0, 8);

    // 2^63 + 2^11, the first value above 2^63 that is exactly representable:
    // one ulp up there is 2048.
    check((double)opaqueUL(9223372036854777856UL) == 9223372036854777856.0, 9);

    // Exactly halfway between 2^63 and the next double. Ties go to even, and
    // 2^63's mantissa is the even one.
    check((double)opaqueUL(9223372036854776832UL) == 9223372036854775808.0, 10);

    // One below halfway, which rounds down for an ordinary reason.
    check((double)opaqueUL(9223372036854776831UL) == 9223372036854775808.0, 11);

    // One *above* halfway, which must round up - and is the case a plain shift
    // gets wrong. Halving drops the odd bit, leaving a value that is itself
    // exactly halfway one binade down, which then ties to even and doubles back
    // to 2^63 instead of to 2^63 + 2048.
    check((double)opaqueUL(9223372036854776833UL) == 9223372036854777856.0, 12);

    // -------- unsigned 64 -> float --------

    check((float)opaqueUL(0UL) == 0.0f, 13);
    check((float)opaqueUL(16777216UL) == 16777216.0f, 14);
    check((float)opaqueUL(9223372036854775808UL) == 9223372036854775808.0f, 15);
    check((float)opaqueUL(18446744073709551615UL) == 18446744073709551616.0f, 16);

    // -------- double -> unsigned 64, below 2^63 (the signed path) --------

    check((unsigned long)opaqueD(0.0) == 0UL, 17);
    check((unsigned long)opaqueD(5.9) == 5UL, 18);
    check((unsigned long)opaqueD(4294967295.0) == 4294967295UL, 19);
    check((unsigned long)opaqueD(9007199254740992.0) == 9007199254740992UL, 20);

    // -------- double -> unsigned 64, at or above 2^63 --------

    check((unsigned long)opaqueD(9223372036854775808.0) == 9223372036854775808UL, 21);
    check((unsigned long)opaqueD(9223372036854777856.0) == 9223372036854777856UL, 22);
    check((unsigned long)opaqueD(1e19) == 10000000000000000000UL, 23);

    // The largest double strictly below 2^64, which is 2^64 - 2048.
    check((unsigned long)opaqueD(18446744073709549568.0) == 18446744073709549568UL, 24);

    // -------- float -> unsigned 64 --------

    check((unsigned long)opaqueF(5.9f) == 5UL, 25);
    check((unsigned long)opaqueF(9223372036854775808.0f) == 9223372036854775808UL, 26);

    // -------- narrower unsigned sources, which never needed any of this --------

    check((double)opaqueU(0u) == 0.0, 27);
    check((double)opaqueU(4294967295u) == 4294967295.0, 28);
    check((unsigned int)opaqueD(4294967295.0) == 4294967295u, 29);

    // -------- round trips --------

    check((unsigned long)(double)opaqueUL(9007199254740992UL) == 9007199254740992UL, 30);
    check((unsigned long)(double)opaqueUL(9223372036854775808UL)
          == 9223372036854775808UL, 31);

    return failures;
}
