// Integer division and remainder where the divisor is an lvalue read out of
// memory - a local, a field, an array element - rather than a computed value.
//
// generateDiv() in src/x86_64/codegen_x86_64.c had a separate branch for that
// shape which divided straight off the address, and it was wrong twice over.
//
// A division pins two registers: rax holds the dividend and rdx its high half,
// zeroed for an unsigned divide and sign-extended into for a signed one. The
// address translateAddress() builds for the divisor is free to name either of
// them, and the branch set both up *after* building it - so 'hash % s->size'
// with s in a local emitted 'pop %rax; idivq (%rax)' and divided by whatever
// the dividend happened to point at. Usually a segfault, which is how it was
// found: it is the shape findEdge() in src/ir/cp.c is written in, so a
// self-hosted EduCC crashed in the sparse constant propagation pass.
//
// The same branch also spelled the instruction OP_SDIV outright rather than
// using the signed/unsigned opcode picked a few lines above, so an unsigned
// division by an lvalue ran as a signed one. That one is silent for small
// values and only shows up once a dividend has its top bit set - hence the
// deliberately large unsigned cases below, which trap on x86 rather than
// merely returning the wrong number.
//
// Both are fixed by loading the divisor into a scratch register first, which
// is what the non-lvalue branch always did. The exit code is the number of the
// first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Set { unsigned long size; int tag; };
struct Nums { int si; unsigned ui; long sl; unsigned long ul; };

// The original shape: a field of a pointed-to struct, so the address needs a
// register that the divide then wants for itself.
static int bucketOf(struct Set *s, int hash) { return hash % s->size; }

// A plain local divisor, and an array element, to cover the other two ways an
// address gets built.
static long divLocal(long a, long b) { long d = b; return a / d; }
static long divElem(long a, long *tab, int i) { return a / tab[i]; }
static long modElem(long a, long *tab, int i) { return a % tab[i]; }

// Unsigned by an lvalue, where signed and unsigned genuinely differ: both
// dividends below have their top bit set, so an idiv here does not merely
// answer wrongly, it raises #DE and takes the process with it.
static unsigned long udivField(unsigned long a, struct Nums *n) { return a / n->ul; }
static unsigned long umodField(unsigned long a, struct Nums *n) { return a % n->ul; }
static unsigned udivField32(unsigned a, struct Nums *n) { return a / n->ui; }

// Signed by an lvalue, including negatives, so the fix cannot have quietly
// turned every division unsigned instead.
static long sdivField(long a, struct Nums *n) { return a / n->sl; }
static long smodField(long a, struct Nums *n) { return a % n->sl; }

int main(void) {
    struct Set s;
    s.size = 10;
    s.tag = 0;

    check(bucketOf(&s, 21) == 1, 1);
    check(bucketOf(&s, 30) == 0, 2);

    check(divLocal(100, 7) == 14, 3);

    long tab[3];
    tab[0] = 5;
    tab[1] = -4;
    tab[2] = 3;

    check(divElem(100, tab, 0) == 20, 4);
    check(divElem(100, tab, 1) == -25, 5);
    check(modElem(100, tab, 2) == 1, 6);

    struct Nums n;
    n.si = -7;
    n.ui = 3u;
    n.sl = -6;
    n.ul = 3UL;

    // 2^63 + something: a signed divide reads this as negative.
    unsigned long big = 0x8000000000000000UL + 6UL;

    check(udivField(big, &n) == big / 3UL, 7);
    check(umodField(big, &n) == big % 3UL, 8);
    check(udivField32(0x80000006u, &n) == 0x80000006u / 3u, 9);

    check(sdivField(-100, &n) == 16, 10);
    check(smodField(-100, &n) == -4, 11);
    check(sdivField(100, &n) == -16, 12);

    return failures;
}
