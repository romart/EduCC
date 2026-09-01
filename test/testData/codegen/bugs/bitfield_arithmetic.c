// Arithmetic on a bit field, which sema used to reject for half the operators.
//
// computeBinaryType asked what kind of type each operand was, and a bit field
// is its own kind - TR_BITFIELD, not TR_VALUE. Only the shift and bitwise rules
// said so explicitly, so 'b.f << 2' and 'b.f + 2' (which falls through to
// commonPrimitiveType, and that unwraps) were accepted while 'b.f * 2',
// 'b.f / 2', 'b.f % 2' and 'p + b.f' were "invalid operands to binary
// expression ('unsigned char' and 'signed int')" - naming the storage unit the
// check had refused to look at.
//
// A frontend bug, so it was there in both backends.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

struct Bits {
    unsigned small : 3;
    unsigned wide : 12;
    int signedSmall : 4;
};

struct Bits b;
long table[64];

int main(void) {
    for (int i = 0; i < 64; ++i) table[i] = i * 3;

    b.small = 5;
    b.wide = 1000;
    b.signedSmall = -6;

    check(b.small * 7, 35, 1);
    check(b.wide / 8, 125, 2);
    check(b.wide % 7, 1000 % 7, 3);
    check(b.signedSmall * 3, -18, 4);
    check(b.signedSmall / 2, -3, 5);
    check(b.signedSmall % 4, -2, 6);

    // The operators that already worked, so that unwrapping the type earlier
    // has not changed what they compute.
    check(b.small + 2, 7, 7);
    check(b.small << 2, 20, 8);
    check(b.wide & 0xF0, 1000 & 0xF0, 9);
    check(b.signedSmall - 4, -10, 10);

    // A bit field as the integer half of pointer arithmetic.
    long *p = table;
    check(*(p + b.small), 15, 11);
    check(*(b.small + p), 15, 12);
    check(p[b.small], 15, 13);
    check((p + b.wide / 100) - p, 10, 14);

    // Mixed with a wider operand, where the common type is the wider one.
    unsigned long big = 1000000;
    check(b.small * big, 5000000, 15);
    check(big / b.small, 200000, 16);

    return failures;
}
