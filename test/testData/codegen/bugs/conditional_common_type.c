// The arms of '?:' converted to the type of the whole expression.
//
// C99 6.5.15p5-6 says the result type is the one the usual arithmetic
// conversions produce, or the composite pointer type; sema computed it and
// then never applied it, so each arm reached the backend at its own width.
// transformTernaryExpression asked '!typeEquality(...)' - and typeEquality
// answers a TypeEqualityKind whose TEK_EQUAL is 1 and whose zero is
// TEK_UNKNOWN, the one answer it never returns - so the guard was false every
// time and no cast was ever inserted.
//
// The IR backend asserted that both arms have the same IR type and aborted;
// every shape below was one of the 30 files of EduCC's own source that
// '-experimental' could not compile. But the legacy backend, which was
// supposed to survive this by materializing each arm at the destination width,
// got check 3 wrong: it moved the int arm into the destination without sign
// extending, so a negative arm of a long conditional came back with its top
// half whatever was already there. The missing cast was a live miscompile in
// the shipped backend, not only a gap in the new one.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// Narrower arm widened: 'a' is read as an int and used as a long.
long widen(int c, int a, long b) { return c ? a : b; }

// Signedness, not width: the common type of unsigned and int is unsigned, so
// a negative int arm comes back as its unsigned pattern.
unsigned signedness(int c, unsigned a, int b) { return c ? a : b; }

// Integer to floating: the int arm is converted, not reinterpreted.
double toDouble(int c, double a, int b) { return c ? a : b; }

// Array against pointer: the array arm decays, and the result is a pointer.
static char text[4] = "abc";
char *decay(int c, char *y) { return c ? text : y; }

// A null pointer constant against a pointer: the one pointer/integer pairing
// C99 6.5.15p6 allows, and the result is the pointer's type. It used to warn
// here, because computeTernaryType saw only the two types and an integer arm
// against a pointer is otherwise worth complaining about.
char *nullConst(int c, char *y) { return c ? y : 0; }

int main(void) {
    long big = 0x100000000L;

    check(widen(1, 7, big) == 7, 1);
    check(widen(0, 7, big) == big, 2);
    check(widen(1, -1, big) == -1L, 3);

    check(signedness(1, 4000000000u, -1) == 4000000000u, 4);
    check(signedness(0, 4000000000u, -1) == 4294967295u, 5);

    check(toDouble(1, 0.5, 3) == 0.5, 6);
    check(toDouble(0, 0.5, 3) == 3.0, 7);

    check(decay(1, 0) == text, 8);
    check(decay(1, 0)[1] == 'b', 9);
    check(decay(0, text + 2) == text + 2, 10);

    check(nullConst(1, text) == text, 11);
    check(nullConst(0, text) == 0, 12);

    return failures;
}
