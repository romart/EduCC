// An argument reaching a callee narrower than the callee reads it.
//
// SysV leaves the bytes above a narrow argument unspecified, so the IR backend
// copies one into its register at its own width and lets the callee's prototype
// say how much of it to read. That is only sound while the two agree about the
// type, and sema had two ways of letting them disagree:
//
//   an enum parameter   typeEquality lets an enum stand for any integer type,
//                       which is right for compatibility and wrong for width.
//                       'unsigned char' handed to an 'enum' parameter compared
//                       equal, no conversion was inserted, and the callee read
//                       four bytes of a register the caller wrote one of.
//                       EduCC's own 'Boolean' is such an enum, which is how
//                       '-oneline' stopped working in a self-compiled compiler:
//                       'printDiagnostics(&d, config->verbose)' passed one bit
//                       of a byte and the callee read a whole int of garbage.
//
//   a bit field         The default argument promotions skipped it entirely -
//                       a TR_BITFIELD is not a scalar type, so the loop that
//                       promotes trailing arguments left it alone and the
//                       storage unit's single byte went out as the argument.
//
// The legacy backend hides both: it moves a whole register per argument.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <stdarg.h>

typedef enum _Boolean { FALSE = 0, TRUE = 1 } Boolean;
typedef enum _Level { LOW = 0, MID = 5, HIGH = 7 } Level;

struct Flags {
    unsigned first : 1;
    unsigned level : 3;
    int signedLevel : 3;
};

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

// The parameter is an enum and every argument below is narrower than one.
static int asInt(Boolean v) { return (int)v; }
static int levelAsInt(Level v) { return (int)v; }

// Reads its arguments as 'int', which is what the default argument promotions
// are supposed to have made of every one of them.
static long sumInts(int n, ...) {
    va_list ap;
    va_start(ap, n);
    long sum = 0;
    for (int i = 0; i < n; ++i) sum += va_arg(ap, int);
    va_end(ap);
    return sum;
}

unsigned char uc;
signed char sc;
unsigned short us;
struct Flags flags;

int main(void) {
    // A narrow integer to an enum parameter, both values, so that a register
    // left holding the previous call's bits cannot pass by accident.
    uc = 1;  check(asInt(uc), 1, 1);
    uc = 0;  check(asInt(uc), 0, 2);
    sc = -1; check(asInt(sc), -1, 3);
    sc = 0;  check(asInt(sc), 0, 4);
    us = 300; check(levelAsInt(us), 300, 5);
    us = 0;   check(levelAsInt(us), 0, 6);

    // A bit field to an enum parameter: the same defect one type further in,
    // since a bit field is not equal to anything until its storage unit is.
    flags.first = 1;
    flags.level = 5;
    flags.signedLevel = -3;
    check(asInt(flags.first), 1, 7);
    check(levelAsInt(flags.level), 5, 8);
    flags.first = 0;
    check(asInt(flags.first), 0, 9);

    // And as trailing arguments of a variadic callee, which is the other path.
    check(sumInts(1, uc), 0, 10);
    uc = 200;
    check(sumInts(1, uc), 200, 11);
    check(sumInts(3, uc, sc, us), 200, 12);
    check(sumInts(2, flags.level, flags.signedLevel), 2, 13);
    flags.first = 1;
    check(sumInts(4, flags.first, flags.level, flags.signedLevel, uc), 203, 14);

    return failures;
}
