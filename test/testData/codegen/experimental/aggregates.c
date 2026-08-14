// Structs through the -experimental backend: step 7's third part, and the
// first thing the memory rules made possible - every struct function is
// allocas, GEPs, loads, stores and block copies, none of which stage 1 could
// select before.
//
// What is covered here is what the IR can actually express:
//
//   * a struct small enough to travel in one register, passed and read back;
//   * struct assignment and initialization, which is IR_M_COPY;
//   * field and array-of-struct access, which is GEP plus a load or a store;
//   * *calling* a function that returns a struct, both sizes. A large one the
//     caller allocates the buffer for and passes the address of as a hidden
//     first argument; a small one comes back in rax as bytes, and since the IR
//     has a composite value be an address, selection is what has to find those
//     bytes a home - see selectRegisterReturnedStruct.
//
// What is deliberately absent is the mirror of that last one - *being* a
// function that returns a struct by value - along with passing a struct too
// big for one register. Both are refused by the selector and fall back to the
// legacy backend, because the IR's two sides do not agree: a large argument is
// handed over as an address by the caller and read as bytes on the stack by
// the callee, and a struct return hands back the address of a slot local to
// the callee rather than filling the caller's buffer. See section 6.10 of
// docs/ir-codegen-design.md.
//
// mkBig below is exactly such a function, kept on purpose: it is what makes
// useBig's call a real test of the hidden-buffer path, with the two backends
// meeting through the ABI.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Point { int x, y; };          // 8 bytes: one register
struct Pair { long a; long b; };     // 16 bytes: too big, by memory
struct Big { long a[5]; };           // 40 bytes: by memory, returned by buffer
struct Nested { struct Point p; int z; };

// -------- passed in a register, read back --------

int takePoint(struct Point p) { return p.x * 10 + p.y; }

// The same struct arriving and being handed straight to something else, so the
// eightbyte makes a round trip through a frame slot and back into rdi.
int forwardPoint(struct Point p) { return takePoint(p); }

// -------- assignment and initialization --------

int copyPoint(int x, int y) {
    struct Point a;
    a.x = x;
    a.y = y;
    struct Point b = a;      // IR_M_COPY of eight bytes
    b.x += 1;
    return b.x * 100 + b.y + a.x;
}

int copyPair(long a, long b) {
    struct Pair p;
    p.a = a;
    p.b = b;
    struct Pair q = p;       // IR_M_COPY of sixteen
    q.a *= 2;
    return (int)(q.a + q.b + p.a);
}

int copyBig(int n) {
    struct Big b;
    for (int i = 0; i < 5; ++i) b.a[i] = n + i;
    struct Big c = b;        // IR_M_COPY of forty
    c.a[0] = 100;
    return (int)(c.a[0] + c.a[4] + b.a[0]);
}

// -------- fields, arrays of structs, pointers to structs --------

int nested(int x, int y, int z) {
    struct Nested n;
    n.p.x = x;
    n.p.y = y;
    n.z = z;
    return n.p.x * 100 + n.p.y * 10 + n.z;
}

int viaPointer(struct Point *p) { return p->x - p->y; }

int arrayOfStructs(int n) {
    struct Point ps[3];
    int sum = 0;

    for (int i = 0; i < 3; ++i) {
        ps[i].x = n + i;
        ps[i].y = n - i;
    }

    for (int i = 0; i < 3; ++i) {
        sum += ps[i].x * ps[i].y;
    }

    return sum;
}

// -------- large struct returned by a callee, through a hidden buffer --------

// Falls back to the legacy backend - see the header comment - which is what
// makes the call below a genuine crossing.
struct Big mkBig(int n) {
    struct Big b;
    for (int i = 0; i < 5; ++i) b.a[i] = n * (i + 1);
    return b;
}

int useBig(int n) {
    struct Big b = mkBig(n);
    return (int)(b.a[0] + b.a[4]);
}

// A returned struct used without being named, so the buffer is a temporary
// rather than a declared local.
int useBigDirect(int n) { return (int)mkBig(n).a[2]; }

// -------- small struct returned in a register --------

// Also a fallback, for the same reason mkBig is.
struct Point mkPoint(int n) {
    struct Point p;
    p.x = n;
    p.y = n + 1;
    return p;
}

int usePoint(int n) {
    struct Point p = mkPoint(n);
    return p.x * 10 + p.y;
}

int addPoints(struct Point a, struct Point b) { return a.x + a.y + b.x + b.y; }

// Two register-returned structs in one expression, so the function gets two
// call-result slots rather than one. This does not prove they have to be
// separate - translation loads each result before emitting the next call, so
// one shared buffer would survive too - it just keeps the two-slot case
// compiled and run rather than only reasoned about.
int twoAtOnce(int n) { return addPoints(mkPoint(n), mkPoint(n + 10)); }

int main(void) {
    struct Point p;
    p.x = 3;
    p.y = 4;

    check(takePoint(p) == 34, 1);
    check(forwardPoint(p) == 34, 2);
    check(viaPointer(&p) == -1, 3);

    check(copyPoint(3, 4) == 407, 4);
    check(copyPair(5, 7) == 22, 5);
    check(copyBig(10) == 100 + 14 + 10, 6);

    check(nested(1, 2, 3) == 123, 7);
    check(arrayOfStructs(5) == 25 + 24 + 21, 8);

    check(useBig(3) == 3 + 15, 9);
    check(useBigDirect(3) == 9, 10);

    check(usePoint(4) == 45, 11);
    check(mkPoint(6).y == 7, 12);
    check(twoAtOnce(1) == 1 + 2 + 11 + 12, 13);

    return failures;
}
