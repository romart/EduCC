// Six integer arguments to a function that also returns a large struct, which
// the x86_64 backend passed one register short - and read one register past
// the end of an array to do it.
//
// A struct too big for a register is written through a buffer whose address is
// the hidden first argument, so it takes rdi and every declared argument moves
// one register along: five still travel in registers, a sixth goes on the
// stack. Both halves of the call have to agree about that, and the caller did
// not. 'firstIntRegArg' was set to 1 for exactly this case and then never
// read, so the classification loop kept testing 'intRegArgs < R_PARAM_COUNT'
// and made all six arguments register arguments.
//
// The consequence was not a missing argument but an out-of-bounds read:
// emitting the call pops each register argument into 'intArgumentRegs[ir++]'
// and ir reached 6, one past the last argument register, so the sixth
// argument was popped into whatever register followed the array in memory and
// the callee read its stack slot from a frame that had nothing in it.
//
// The callee side had it right all along, which is why five arguments work and
// only the sixth is wrong - and why nothing noticed: no fixture had put a
// sixth argument on a function returning a struct.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Big { long a[6]; };
struct Small { int x, y; };

// Five, which fit even with the buffer pointer ahead of them.
struct Big five(int a, int b, int c, int d, int e) {
    struct Big r;
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e; r.a[5] = 0;
    return r;
}

// Six, where the last one has to go on the stack.
struct Big six(int a, int b, int c, int d, int e, int f) {
    struct Big r;
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e; r.a[5] = f;
    return r;
}

// Seven, so two are on the stack and their order matters as well as their
// presence.
struct Big seven(int a, int b, int c, int d, int e, int f, int g) {
    struct Big r;
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e; r.a[5] = f * 10 + g;
    return r;
}

// A small return takes no hidden pointer, so all six stay in registers. Here
// to pin down that the correction is conditional on the return type.
struct Small sixSmall(int a, int b, int c, int d, int e, int f) {
    struct Small r;
    r.x = a + b + c;
    r.y = d + e + f;
    return r;
}

int main(void) {
    struct Big r5 = five(1, 2, 3, 4, 5);
    check(r5.a[0] == 1 && r5.a[3] == 4 && r5.a[4] == 5, 1);

    struct Big r6 = six(1, 2, 3, 4, 5, 6);
    check(r6.a[0] == 1, 2);
    check(r6.a[4] == 5, 3);
    check(r6.a[5] == 6, 4);

    struct Big r7 = seven(1, 2, 3, 4, 5, 6, 7);
    check(r7.a[4] == 5, 5);
    check(r7.a[5] == 67, 6);

    struct Small s = sixSmall(1, 2, 3, 4, 5, 6);
    check(s.x == 6 && s.y == 15, 7);

    return failures;
}
