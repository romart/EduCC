// A struct small enough to travel in a register, passed as an argument that
// runs out of registers and goes on the stack. The x86_64 backend put its
// address there rather than its bytes.
//
// generateExpression leaves a composite result's *address* in R_ACC, which is
// the convention every composite case in that file reads it under. The
// register path of generateCall knew it and loaded the value first:
//
//     if (isCompositeType(argType)) {
//         Address addr = { R_ACC, R_BAD, 0, 0, NULL, NULL };
//         emitMoveAR(f, &addr, R_ACC, argSize);
//     }
//     emitPushReg(f, R_ACC);
//
// The stack path next to it did not, and stored R_ACC straight into the
// outgoing slot - so the callee, which reads its stack argument as a value,
// got a pointer and read the two halves of the struct out of whatever the
// pointer's own bits happened to name.
//
// Six scalar arguments are what it takes to reach: with fewer, every struct
// is a register argument and the correct path runs. That the IR backend
// selects this case correctly is what made it findable - the two backends
// disagreed on a fixture written for the other one.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Small { int x, y; };
struct Byte { char c; };
struct Big { long a[5]; };

int takeSeventh(int a, int b, int c, int d, int e, int f, struct Small s) {
    return a + b + c + d + e + f + s.x + s.y;
}

int takeTwo(int a, int b, int c, int d, int e, int f, struct Small s, struct Small t) {
    return s.x + s.y + t.x * 2 + t.y * 2;
}

// Narrower than a word, so the slot is written at a width the struct does not
// fill and the bytes above it must not be read as part of the value.
int takeNarrow(int a, int b, int c, int d, int e, int f, struct Byte s) {
    return a + s.c;
}

// A large struct in the same call, which travels by a different route again -
// its bytes are copied into the outgoing area rather than moved through a
// register - so the two must not interfere.
int takeMixed(int a, int b, int c, int d, int e, int f, struct Small s, struct Big b2) {
    return s.x + s.y + (int)(b2.a[0] + b2.a[4]);
}

// The hidden buffer pointer of a large return takes a register too, so a
// struct argument reaches the stack one argument earlier.
struct Big takeAndReturn(int a, int b, int c, int d, int e, struct Small s) {
    struct Big r;
    for (int i = 0; i < 5; ++i) r.a[i] = 0;
    r.a[0] = a + b + c + d + e;
    r.a[1] = s.x;
    r.a[2] = s.y;
    return r;
}

int main(void) {
    struct Small s, t;
    s.x = 100; s.y = 200;
    t.x = 1000; t.y = 2000;

    check(takeSeventh(1, 2, 3, 4, 5, 6, s) == 21 + 300, 1);
    check(takeTwo(1, 2, 3, 4, 5, 6, s, t) == 300 + 6000, 2);

    struct Byte one;
    one.c = 42;
    check(takeNarrow(1, 2, 3, 4, 5, 6, one) == 43, 3);

    struct Big b;
    for (int i = 0; i < 5; ++i) b.a[i] = (i + 1) * 10;
    check(takeMixed(1, 2, 3, 4, 5, 6, s, b) == 300 + 60, 4);

    struct Big r = takeAndReturn(1, 2, 3, 4, 5, s);
    check(r.a[0] == 15, 5);
    check(r.a[1] == 100, 6);
    check(r.a[2] == 200, 7);

    return failures;
}
