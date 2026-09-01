// Long operator chains, which canonicalization used to walk 2^depth times.
//
// Every cannonize* rule descended into both children and then handed the node
// to another rule that descended again, so an expression's subtree was visited
// once per path to it. 'a[0] + ... + a[k]' cost 0.02s at k=16, 0.40s at k=20,
// 16.05s at k=24 and did not finish at k=28; the three affected operators were
// '+', '-' and '<<' (which reassociates its counts through an add), plus every
// array index and struct field, since both desugar to a pointer add and both
// re-descended into a base that had just been canonicalized. The other binary
// operators were always linear - '*' reassociates too, but only when both of
// its constants fold on the spot, so it never handed on an unfolded node.
//
// The rules are split now: rewrite* reshapes a node whose children are already
// canonical, cannonize* descends once and then calls it. Every chain below is
// 40 terms, roughly a million times the work at the old growth rate; before the
// fix this file did not compile. gcc returns 0.
//
// The exit code is the number of the first check that failed.

struct S { long lo, hi; };

long a[64];
struct S s[8];
long one = 1;

int failures = 0;

static void check(long got, long want, int id) {
    if (got != want && failures == 0) failures = id;
}

static void setup(void) {
    int i;
    for (i = 0; i < 64; i = i + 1) a[i] = i;
    for (i = 0; i < 8; i = i + 1) { s[i].lo = i; s[i].hi = 100 + i; }
}

// A plain additive chain: the shape that took sixteen seconds at k=24.
static long addChain(void) { return a[0] + a[1] + a[2] + a[3] + a[4] + a[5] + a[6] + a[7] + a[8] + a[9] + a[10] + a[11] + a[12] + a[13] + a[14] + a[15] + a[16] + a[17] + a[18] + a[19] + a[20] + a[21] + a[22] + a[23] + a[24] + a[25] + a[26] + a[27] + a[28] + a[29] + a[30] + a[31] + a[32] + a[33] + a[34] + a[35] + a[36] + a[37] + a[38] + a[39]; }

// Subtraction reassociates through the same rules and blew up the same way.
static long subChain(void) { return a[39] - a[0] - a[1] - a[2] - a[3] - a[4] - a[5] - a[6] - a[7]; }

// '(x << y) << z -> x << (y + z)' builds an add and used to re-canonicalize it
// from the top; this was the worst of the three.
static long shlChain(void) { return one << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1 << 1; }

// Struct fields and array indices both desugar to a pointer add, so a chain of
// them re-descended into a base that was already canonical.
static long fieldChain(void) { return s[0].hi + s[1].lo + s[2].hi + s[3].lo + s[4].hi + s[5].lo + s[6].hi + s[7].lo + s[0].hi + s[1].lo + s[2].hi + s[3].lo + s[4].hi + s[5].lo + s[6].hi + s[7].lo + s[0].hi + s[1].lo + s[2].hi + s[3].lo + s[4].hi + s[5].lo + s[6].hi + s[7].lo + s[0].hi + s[1].lo + s[2].hi + s[3].lo + s[4].hi + s[5].lo + s[6].hi + s[7].lo + s[0].hi + s[1].lo + s[2].hi + s[3].lo + s[4].hi + s[5].lo + s[6].hi + s[7].lo; }

// Mixed '+' and '-', which alternate between the two rule sets.
static long mixedChain(void) { return a[0] + a[1] + a[2] - a[3] + a[4] + a[5] - a[6] + a[7] + a[8] - a[9] + a[10] + a[11] - a[12] + a[13] + a[14] - a[15] + a[16] + a[17] - a[18] + a[19] + a[20] - a[21] + a[22] + a[23] - a[24] + a[25] + a[26] - a[27] + a[28] + a[29] - a[30] + a[31] + a[32] - a[33] + a[34] + a[35] - a[36] + a[37] + a[38] - a[39]; }

// Nested indexing was 3^depth rather than 2^depth: cannonizeArrayAccess
// transformed its base and then transformed the sum it built from it.
long m[3][3][3][3][3][3][3][3];

static long deepIndex(int i) { return m[i][i][i][i][i][i][i][i]; }

int main(void) {
    setup();
    m[1][1][1][1][1][1][1][1] = 77;
    m[2][2][2][2][2][2][2][2] = 88;

    check(addChain(), 780, 1);
    check(subChain(), 11, 2);
    check(shlChain(), 1099511627776L, 3);
    check(fieldChain(), 2140, 4);
    check(mixedChain(), 234, 5);
    check(deepIndex(1), 77, 6);
    check(deepIndex(2), 88, 7);
    check(deepIndex(0), 0, 8);

    return failures;
}
