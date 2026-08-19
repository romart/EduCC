// 'return <a struct in static storage>;', which the x86_64 backend miscompiled
// into a segfault.
//
// A struct return copies the value eightbyte by eightbyte, and copyStructTo()
// walked the source by advancing the Address it was handed. For a local that
// is a displacement off rbp and advancing it is arithmetic; for a global it is
// a rip-relative address carrying the Relocation that names the site the
// linker patches, and two things went wrong at once:
//
//   - encodeAR() wrote 'reloc->applySectionOffset' every time it encoded the
//     address, so one Relocation ended up naming only the last of the copy's
//     five instructions. The other four kept the 0x7EADBEFF placeholder and
//     were executed as an address.
//   - it also ignored Address.imm, which for a rip-relative operand has
//     nowhere to go but the addend - so even the one instruction that did get
//     relocated read the front of the struct rather than its own chunk.
//
// Nothing else builds a rip-relative address it then uses more than once, or
// at a nonzero offset into the symbol, which is why only this shape found it.
// Both sizes are here because they take different routes: a struct of more
// than eight bytes is copied through the caller's buffer and a smaller one is
// loaded whole into rax, so only the large one goes through copyStructTo.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Big { long a[5]; };
struct Odd { char c[13]; };       // not a multiple of eight: 8+4+1 chunks
struct Pair { int x, y; };

struct Big gBig = { { 10, 20, 30, 40, 50 } };
struct Odd gOdd = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 } };
struct Pair gPair = { 7, 8 };
static struct Big sBig = { { 1, 2, 3, 4, 5 } };

struct Big retGlobal(void) { return gBig; }
struct Odd retOdd(void) { return gOdd; }
struct Pair retSmall(void) { return gPair; }
struct Big retStatic(void) { return sBig; }

// A second call site, so the relocations of two copies of the same global
// cannot be confused for each other.
struct Big retGlobalAgain(void) { return gBig; }

struct Big retLocalStatic(void) {
    static struct Big inner = { { 100, 200, 300, 400, 500 } };
    return inner;
}

int main(void) {
    struct Big b = retGlobal();
    check(b.a[0] == 10, 1);
    check(b.a[1] == 20, 2);
    check(b.a[2] == 30, 3);
    check(b.a[3] == 40, 4);
    check(b.a[4] == 50, 5);

    struct Odd o = retOdd();
    check(o.c[0] == 1, 6);
    check(o.c[8] == 9, 7);
    check(o.c[12] == 13, 8);

    struct Pair p = retSmall();
    check(p.x == 7 && p.y == 8, 9);

    struct Big s = retStatic();
    check(s.a[0] == 1 && s.a[4] == 5, 10);

    struct Big b2 = retGlobalAgain();
    check(b2.a[0] == 10 && b2.a[4] == 50, 11);

    struct Big ls = retLocalStatic();
    check(ls.a[0] == 100 && ls.a[4] == 500, 12);

    return failures;
}
