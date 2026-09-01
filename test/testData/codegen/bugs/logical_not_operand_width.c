// '!x' where x is not as wide as the int the operator answers.
//
// validateOperandWidths (src/ir/ir.c) counted IR_U_NOT among the operations
// whose operands are values of its own type, which an add or an xor is. This
// one is not: '!p' on a pointer and '!c' on a char both answer 'int', and
// selectLogicalNot has always tested at the operand's own width. So the
// assertion fired on every '!' of anything but an int - 10 of EduCC's own
// source files, once the conditional-operator fix let them get that far.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

static int notChar(char c) { return !c; }
static int notShort(short s) { return !s; }
static int notLong(long l) { return !l; }
static int notPtr(void *p) { return !p; }
static int notUChar(unsigned char c) { return !c; }

// The operand narrower than int and only its low byte nonzero: a test of the
// wrong width here reads bytes of the register nothing wrote.
static int notLowByte(int x) { return !(char)x; }

int main(void) {
    int local = 0;

    check(notChar(0) == 1, 1);
    check(notChar('a') == 0, 2);
    check(notShort(0) == 1, 3);
    check(notShort(-1) == 0, 4);
    check(notLong(0) == 1, 5);
    check(notLong(0x100000000L) == 0, 6);
    check(notPtr(0) == 1, 7);
    check(notPtr(&local) == 0, 8);
    check(notUChar(0) == 1, 9);
    check(notUChar(255) == 0, 10);

    check(notLowByte(0x100) == 1, 11);
    check(notLowByte(0x101) == 0, 12);

    // '!' of a double is a floating compare rather than a test, and shares the
    // same result width.
    check(!0.0 == 1, 13);
    check(!0.5 == 0, 14);

    return failures;
}
