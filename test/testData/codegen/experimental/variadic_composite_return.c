// Variadic functions that return a struct by value, over the argument mixes
// that make the two conventions interact.
//
// The two features meet in the register save area. A large struct return takes
// the first integer register for its hidden buffer pointer, so every named
// argument moves one along - and va_start has to hand out the *remaining*
// registers, from the right offset into the area the prologue spilled them to.
// Get either half wrong and it is the first variadic argument that comes back
// as the buffer pointer, which is a plausible-looking number rather than a
// crash.
//
// This fixture was written one step before the backend could build any of it.
// The IR translator allocated '__va_area__' and never emitted the stores that
// fill it, so selection turned every function here away and the fixture was the
// two backends meeting across the ABI. Step 14 put those stores in the IR, and
// all thirteen are built here now - which makes the interaction above a claim
// about one backend rather than about the seam between two. 'main' still falls
// back, for a reason of its own that the .fallback sibling names.
//
// The mixes are what matter, so each one is a separate function: varargs in
// registers and past them into the overflow area, integer and SSE classes on
// their own and interleaved, floats (which promote to double) and named float
// parameters (which do not), named parameters that exhaust the integer
// registers before the varargs even start, and structs travelling through the
// ellipsis at both sizes.
//
// Two things gcc accepts are deliberately absent. A va_list *parameter* decays
// to '__va_elem *' and this compiler's va_arg rejects that, so vprintf-style
// forwarding cannot be written here. And an enumerator wider than 'int' is a
// documented limitation elsewhere; nothing here needs one.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <stdarg.h>

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Big { long a[8]; };
struct Small { int x, y; };

static void zero(struct Big *r) {
    for (int i = 0; i < 8; ++i) r->a[i] = 0;
}

// -------- integer varargs, in registers and past them --------

// Five fit alongside the buffer pointer and 'n'; the rest are in the overflow
// area, which is the boundary the hidden pointer moved.
struct Big vaInts(int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    zero(&r);
    for (int i = 0; i < n; ++i) r.a[i % 8] += va_arg(ap, int);
    va_end(ap);
    return r;
}

// -------- SSE varargs, and floats promoting to double --------

struct Big vaDoubles(int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    zero(&r);
    for (int i = 0; i < n; ++i) r.a[i % 8] += (long)va_arg(ap, double);
    va_end(ap);
    return r;
}

// A float argument is promoted to double by the default argument promotions,
// so va_arg reads a double however the call site spelled it.
struct Small vaFloats(int n, ...) {
    struct Small r;
    va_list ap;
    va_start(ap, n);
    r.x = 0;
    r.y = n;
    for (int i = 0; i < n; ++i) r.x += (int)va_arg(ap, double);
    va_end(ap);
    return r;
}

// -------- both classes at once --------

struct Big vaMixed(int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    zero(&r);
    for (int i = 0; i < n; ++i) {
        if (i & 1) r.a[i % 8] = (long)va_arg(ap, double);
        else r.a[i % 8] = va_arg(ap, int);
    }
    va_end(ap);
    return r;
}

// Every integer register spent before the SSE ones start, so the two save
// areas are read at unrelated offsets.
struct Big vaManyBoth(int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    zero(&r);
    for (int i = 0; i < n; ++i) r.a[0] += va_arg(ap, int);
    for (int i = 0; i < n; ++i) r.a[1] += (long)va_arg(ap, double);
    va_end(ap);
    return r;
}

// -------- named parameters ahead of the varargs --------

// Named floats stay floats: only the varargs are promoted.
struct Big vaNamedFloats(float a, float b, double c, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, c);
    zero(&r);
    r.a[0] = (long)a;
    r.a[1] = (long)b;
    r.a[2] = (long)c;
    r.a[3] = (long)va_arg(ap, double);
    va_end(ap);
    return r;
}

// Five named integers plus the buffer pointer fill the integer registers, so
// every integer vararg is already in the overflow area.
struct Big vaFiveNamed(int a, int b, int c, int d, int e, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, e);
    zero(&r);
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e;
    r.a[5] = va_arg(ap, int);
    r.a[6] = (long)va_arg(ap, double);
    va_end(ap);
    return r;
}

// One more named integer than there are registers left, so the last named
// parameter is on the stack and the varargs follow it there.
struct Big vaSixNamed(int a, int b, int c, int d, int e, int f, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, f);
    zero(&r);
    r.a[0] = a; r.a[4] = e; r.a[5] = f;
    r.a[6] = va_arg(ap, int);
    r.a[7] = (long)va_arg(ap, double);
    va_end(ap);
    return r;
}

// A named struct parameter, which is passed as bytes on the stack and has to
// leave the register accounting alone.
struct Big vaNamedStruct(struct Big base, int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < 8; ++i) r.a[i] = base.a[i];
    for (int i = 0; i < n; ++i) r.a[i % 8] += va_arg(ap, int);
    va_end(ap);
    return r;
}

// -------- structs through the ellipsis --------

// Small enough to travel in an integer register, so va_arg has to read it out
// of the same save area an int comes from - it used to fall through to the
// overflow area because a struct is neither real nor scalar.
struct Small vaSmallArgs(int n, ...) {
    struct Small r;
    va_list ap;
    va_start(ap, n);
    r.x = 0;
    r.y = 0;
    for (int i = 0; i < n; ++i) {
        struct Small s = va_arg(ap, struct Small);
        r.x += s.x;
        r.y += s.y;
    }
    va_end(ap);
    return r;
}

// Too big for a register, so it is always in the overflow area.
struct Big vaBigArgs(int n, ...) {
    struct Big r;
    va_list ap;
    va_start(ap, n);
    zero(&r);
    for (int i = 0; i < n; ++i) {
        struct Big b = va_arg(ap, struct Big);
        for (int j = 0; j < 8; ++j) r.a[j] += b.a[j];
    }
    va_end(ap);
    return r;
}

// -------- va_copy, and one variadic call inside another --------

struct Big vaCopied(int n, ...) {
    struct Big r;
    va_list ap, ap2;
    va_start(ap, n);
    va_copy(ap2, ap);
    zero(&r);
    for (int i = 0; i < n; ++i) r.a[0] += va_arg(ap, int);
    for (int i = 0; i < n; ++i) r.a[1] += va_arg(ap2, int);
    va_end(ap2);
    va_end(ap);
    return r;
}

struct Big vaNested(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int first = va_arg(ap, int);
    va_end(ap);
    return vaSixNamed(first, 2, 3, 4, 5, 6, 70, 8.5);
}

int main(void) {
    struct Big i1 = vaInts(3, 10, 20, 30);
    check(i1.a[0] == 10 && i1.a[1] == 20 && i1.a[2] == 30, 1);

    // Eight, so three of them are past the five registers left over.
    struct Big i2 = vaInts(8, 1, 2, 3, 4, 5, 6, 7, 8);
    check(i2.a[0] == 1 && i2.a[4] == 5, 2);
    check(i2.a[5] == 6 && i2.a[7] == 8, 3);

    struct Big d1 = vaDoubles(3, 1.5, 2.5, 3.5);
    check(d1.a[0] == 1 && d1.a[1] == 2 && d1.a[2] == 3, 4);

    // Ten doubles: xmm0..xmm7 and then the overflow area.
    struct Big d2 = vaDoubles(10, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0);
    check(d2.a[0] == 1 + 9 && d2.a[1] == 2 + 10, 5);
    check(d2.a[7] == 8, 6);

    float f1 = 1.5f, f2 = 2.5f, f3 = 10.5f;
    struct Small fs = vaFloats(3, f1, f2, f3);
    check(fs.x == 13 && fs.y == 3, 7);

    struct Big m = vaMixed(4, 10, 2.5, 30, 4.5);
    check(m.a[0] == 10 && m.a[1] == 2, 8);
    check(m.a[2] == 30 && m.a[3] == 4, 9);

    struct Big mb = vaManyBoth(6, 1, 2, 3, 4, 5, 6, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    check(mb.a[0] == 21 && mb.a[1] == 21, 10);

    struct Big nf = vaNamedFloats(1.5f, 2.5f, 3.5, 4.5);
    check(nf.a[0] == 1 && nf.a[1] == 2 && nf.a[2] == 3 && nf.a[3] == 4, 11);

    struct Big fn = vaFiveNamed(1, 2, 3, 4, 5, 60, 7.5);
    check(fn.a[0] == 1 && fn.a[4] == 5, 12);
    check(fn.a[5] == 60 && fn.a[6] == 7, 13);

    struct Big sn = vaSixNamed(1, 2, 3, 4, 5, 6, 70, 8.5);
    check(sn.a[0] == 1 && sn.a[4] == 5 && sn.a[5] == 6, 14);
    check(sn.a[6] == 70 && sn.a[7] == 8, 15);

    struct Big base;
    for (int i = 0; i < 8; ++i) base.a[i] = i;
    struct Big ns = vaNamedStruct(base, 3, 100, 200, 300);
    check(ns.a[0] == 100 && ns.a[1] == 201 && ns.a[2] == 302, 16);
    check(ns.a[7] == 7, 17);

    struct Small s[8];
    for (int i = 0; i < 8; ++i) { s[i].x = i + 1; s[i].y = (i + 1) * 10; }
    struct Small sa = vaSmallArgs(5, s[0], s[1], s[2], s[3], s[4]);
    check(sa.x == 15 && sa.y == 150, 18);

    // Eight, so three of them are read from the overflow area instead.
    struct Small sa2 = vaSmallArgs(8, s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7]);
    check(sa2.x == 36 && sa2.y == 360, 19);

    struct Big b1, b2;
    for (int i = 0; i < 8; ++i) { b1.a[i] = i; b2.a[i] = i * 10; }
    struct Big ba = vaBigArgs(2, b1, b2);
    check(ba.a[1] == 11 && ba.a[7] == 77, 20);

    struct Big cp = vaCopied(3, 5, 6, 7);
    check(cp.a[0] == 18 && cp.a[1] == 18, 21);

    struct Big nn = vaNested(1, 42);
    check(nn.a[0] == 42 && nn.a[6] == 70 && nn.a[7] == 8, 22);

    return failures;
}
