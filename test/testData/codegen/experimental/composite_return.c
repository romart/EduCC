// Returning a struct by value, from the IR backend.
//
// Until step 12 of docs/ir-codegen-design.md every function here was handed
// back to the legacy backend at its 'ret'. The IR gave the callee a return
// slot of its own and returned that slot's *address*, which is neither half of
// what SysV asks for: a struct too big for a register has to be written
// through a pointer the caller passes, and a small one has to come back as
// bytes in rax. The caller side had been right all along, so the two halves of
// one ABI disagreed inside the IR itself.
//
// What that means for this fixture is that both sizes need exercising and they
// take completely different routes. Anything of eight bytes or less travels in
// rax; anything larger is copied into the caller's buffer, whose address
// arrives in rdi ahead of the first declared parameter and is returned in rax
// as well. The eight-byte line is EduCC's own: real SysV splits a struct of up
// to sixteen bytes into two eightbytes and returns those in rax:rdx, and both
// of this compiler's backends approximate that by treating anything oversized
// as memory. They agree with each other, which is what lets a function from
// either backend call one from the other, and that is what this fixture
// checks - it is compiled whole by one compiler, so it says nothing about
// linking against gcc.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- small: one register, whatever the width --------

struct Byte { char c; };                    // 1
struct Half { short s; };                   // 2
struct Three { char a, b, c; };             // 3, and not a power of two
struct Word { int w; };                     // 4
struct Pair { int x, y; };                  // 8, exactly the boundary
struct FloatPair { float u, v; };           // 8, and gcc would use xmm0

struct Byte mkByte(int n) { struct Byte r; r.c = (char)n; return r; }
struct Half mkHalf(int n) { struct Half r; r.s = (short)n; return r; }
struct Three mkThree(int n) { struct Three r; r.a = (char)n; r.b = (char)(n + 1); r.c = (char)(n + 2); return r; }
struct Word mkWord(int n) { struct Word r; r.w = n; return r; }
struct Pair mkPair(int n) { struct Pair r; r.x = n; r.y = n + 1; return r; }
struct FloatPair mkFloatPair(float n) { struct FloatPair r; r.u = n; r.v = n + 0.5f; return r; }

// -------- large: through the caller's buffer --------

struct Twelve { int a, b, c; };             // 12, just past the boundary
struct Sixteen { long a, b; };              // 16, where real SysV still uses registers
struct Doubles { double d1, d2; };          // 16, and SSE class in real SysV
struct Big { long a[5]; };                  // 40

struct Twelve mkTwelve(int n) { struct Twelve r; r.a = n; r.b = n + 1; r.c = n + 2; return r; }
struct Sixteen mkSixteen(long n) { struct Sixteen r; r.a = n; r.b = n * 2; return r; }
struct Doubles mkDoubles(double d) { struct Doubles r; r.d1 = d; r.d2 = d * 2; return r; }
struct Big mkBig(int n) { struct Big r; for (int i = 0; i < 5; ++i) r.a[i] = n + i; return r; }

// -------- a union, which is composite too --------

union Word4 { int i; char b[4]; };
union Wide { long l; double d; char b[16]; };   // 16, so it goes through a buffer

union Word4 mkWord4(int n) { union Word4 u; u.i = n; return u; }
union Wide mkWide(long n) { union Wide u; u.l = n; return u; }

// -------- the hidden pointer against the argument registers --------
//
// It arrives in rdi and pushes every declared parameter one register along, so
// a function with five of them still passes them all in registers and one with
// six spills the last onto the stack. Both sides have to agree about that, and
// they are classified by different code - the caller by callArgLocation in
// isel_x86_64.c, the callee by classifyParametersGeneric in ir/target.c.

struct Big five(int a, int b, int c, int d, int e) {
    struct Big r;
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e;
    return r;
}

struct Big six(int a, int b, int c, int d, int e, int f) {
    struct Big r;
    r.a[0] = a; r.a[1] = b; r.a[2] = c; r.a[3] = d; r.a[4] = e + f;
    return r;
}

// -------- more than one 'return', and a value that outlives a call --------

struct Big classify(int n) {
    struct Big r;
    for (int i = 0; i < 5; ++i) r.a[i] = 0;
    if (n < 0) { r.a[0] = -1; return r; }
    if (n == 0) { r.a[0] = 0; return r; }
    r.a[0] = 1;
    return r;
}

// The buffer pointer has to survive the inner call, which clobbers rdi.
struct Big through(int n) {
    struct Big inner = mkBig(n);
    struct Big r;
    for (int i = 0; i < 5; ++i) r.a[i] = inner.a[i] * 2;
    return r;
}

// And the same thing recursively, so several buffers are live at once.
struct Big countdown(int n) {
    if (n <= 0) return mkBig(0);
    struct Big prev = countdown(n - 1);
    struct Big r;
    for (int i = 0; i < 5; ++i) r.a[i] = prev.a[i] + 1;
    return r;
}

// -------- returning something that is not a local --------

struct Big global = { { 100, 200, 300, 400, 500 } };

struct Big fromGlobal(void) { return global; }
struct Big deref(struct Big *p) { return *p; }
struct Pair derefSmall(struct Pair *p) { return *p; }

int main(void) {
    check(mkByte(7).c == 7, 1);
    check(mkHalf(300).s == 300, 2);

    struct Three t = mkThree(1);
    check(t.a == 1 && t.b == 2 && t.c == 3, 3);

    check(mkWord(1234).w == 1234, 4);
    check(mkPair(5).x == 5 && mkPair(5).y == 6, 5);

    struct FloatPair fp = mkFloatPair(1.5f);
    check(fp.u == 1.5f && fp.v == 2.0f, 6);

    struct Twelve tw = mkTwelve(10);
    check(tw.a == 10 && tw.b == 11 && tw.c == 12, 7);

    struct Sixteen sx = mkSixteen(21);
    check(sx.a == 21 && sx.b == 42, 8);

    struct Doubles db = mkDoubles(2.5);
    check(db.d1 == 2.5 && db.d2 == 5.0, 9);

    struct Big b = mkBig(3);
    check(b.a[0] == 3 && b.a[4] == 7, 10);

    check(mkWord4(0x01020304).i == 0x01020304, 11);
    check(mkWide(0x0102030405060708L).l == 0x0102030405060708L, 12);

    struct Big f5 = five(1, 2, 3, 4, 5);
    check(f5.a[0] == 1 && f5.a[4] == 5, 13);

    struct Big f6 = six(1, 2, 3, 4, 5, 6);
    check(f6.a[0] == 1 && f6.a[3] == 4 && f6.a[4] == 11, 14);

    check(classify(-9).a[0] == -1, 15);
    check(classify(0).a[0] == 0, 16);
    check(classify(9).a[0] == 1, 17);

    struct Big th = through(4);
    check(th.a[0] == 8 && th.a[4] == 16, 18);

    struct Big cd = countdown(3);
    check(cd.a[0] == 3 && cd.a[4] == 7, 19);

    struct Big g = fromGlobal();
    check(g.a[0] == 100 && g.a[4] == 500, 20);

    struct Big d = deref(&global);
    check(d.a[2] == 300, 21);

    struct Pair p = mkPair(40);
    check(derefSmall(&p).y == 41, 22);

    // Assigning over a variable that already has a value, which is where the
    // copy out of the buffer and the copy into the destination meet.
    b = mkBig(50);
    check(b.a[0] == 50 && b.a[4] == 54, 23);

    // A return value nobody looks at still needs its buffer allocated.
    mkBig(0);
    check(b.a[0] == 50, 24);

    return failures;
}
