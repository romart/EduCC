// A displacement between 128 and 255 in an address that also has an index.
//
// x86 sign-extends a one-byte displacement, so an offset of 136 encoded in one
// byte reaches the CPU as -120. The encoder in src/x86_64/instructions_x86_64.c
// picked the short form with an *unsigned* round-trip - '(uint32_t)(uint8_t)d
// == d' - but only in the base+index+disp branch; the plain base+disp branch
// beside it had always used the signed test. So every read of a member array
// through a variable index landed 256 bytes low whenever the member sat that
// far into its struct.
//
// It took self-compilation to find. The legacy backend builds an address for
// 'p[i]' by adding into a register rather than by filling in a SIB byte, so it
// never reaches this branch, and no fixture had a struct large enough until
// EduCC's own TargetDescriptor - whose scratchRegCount is at offset 136, read
// as 'target->scratchRegCount[rc]' with rc a loop variable, which is the assert
// that fired in regalloc.c and stopped the -experimental self-host.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

// counts is at offset 136, tail at 152, far at 408: one displacement in the
// range the short form gets wrong, one below it, one above.
struct Wide {
    char pad[136];
    int counts[4];
    char tail[256];
    int far[4];
};

struct Wide w;

static int readCount(struct Wide *p, int i) { return p->counts[i]; }
static int readFar(struct Wide *p, int i) { return p->far[i]; }
static int *addressOfCount(struct Wide *p, int i) { return &p->counts[i]; }

// A global array indexed past a constant offset gets the same address shape
// without a struct in sight: 'a[i + 20]' is 'a + i*8 + 160'.
long a[64];

static long readOffset(int i) { return a[i + 20]; }

int main(void) {
    for (int i = 0; i < 4; ++i) {
        w.counts[i] = 100 + i;
        w.far[i] = 900 + i;
    }
    for (int i = 0; i < 64; ++i) a[i] = i * 7;

    for (int i = 0; i < 4; ++i) {
        check(readCount(&w, i), 100 + i, 1);
        check(readFar(&w, i), 900 + i, 2);
        check(*addressOfCount(&w, i), 100 + i, 3);
        check(addressOfCount(&w, i) - &w.counts[0], i, 4);
    }

    for (int i = 0; i < 40; ++i) {
        check(readOffset(i), (long)(i + 20) * 7, 5);
    }

    // Written through the same address shape, not only read.
    for (int i = 0; i < 4; ++i) {
        int *slot = &w.counts[i];
        *slot = 500 + i;
    }
    for (int i = 0; i < 4; ++i) {
        check(w.counts[i], 500 + i, 6);
    }

    // The bytes on either side of the member are untouched: a store that went
    // 256 low would have landed in 'pad'.
    for (int i = 0; i < 136; ++i) {
        check(w.pad[i], 0, 7);
    }

    return failures;
}
