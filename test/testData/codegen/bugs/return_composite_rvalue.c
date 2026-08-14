// 'return <expression>;' where the expression has struct type and is not an
// lvalue.
//
// The x86_64 backend read every composite return as an lvalue and asserted so:
//
//   assert(retExpr->op == EU_DEREF);
//   translateAddress(f, retExpr->unaryExpr.argument, &src);
//
// which is right for 'return s;' and for nothing else. A call, a ternary and a
// comma all have struct type and none of them is a dereference - the canonical
// AST for the three functions below is 'RETURN mk...(n)', 'RETURN *c ? *a : *b'
// and 'RETURN *c, *a' - so each one aborted the compiler rather than producing
// a diagnostic or wrong code.
//
// Nothing in the corpus had written any of the three, so this went unnoticed
// until the compiler's own sources did: emit_x86_64.c's addressOperand() ends
// in 'return frameAddress(e, op->info.frameIdx);', and bootstrap.sh stopped
// dead compiling it.
//
// The fix is to fall back on the ordinary evaluation path, under the same
// convention the composite cases of assignment and initializer emission
// already use: generateExpression leaves the *address* of a composite result
// in R_ACC. Both sizes matter and take different routes - a struct wider than
// eightbyte is copied into the caller's hidden buffer, a narrower one is
// loaded into rax - so each is exercised here.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

struct Small { int x, y; };        // 8 bytes: returned in rax
struct Large { long a[5]; };       // 40 bytes: returned through a buffer

struct Small mkSmall(int n) {
    struct Small s;
    s.x = n;
    s.y = n + 1;
    return s;                       // the lvalue case, which always worked
}

struct Large mkLarge(int n) {
    struct Large l;
    for (int i = 0; i < 5; ++i) l.a[i] = n + i;
    return l;
}

// -------- returning a call's result --------

struct Small fwdSmall(int n) { return mkSmall(n); }
struct Large fwdLarge(int n) { return mkLarge(n); }

// Two calls deep, so the value passes through a hidden buffer twice.
struct Large fwdLarge2(int n) { return fwdLarge(n); }

// -------- returning a ternary and a comma --------

struct Small pickSmall(int c) {
    struct Small a = mkSmall(1), b = mkSmall(9);
    return c ? a : b;
}

struct Large pickLarge(int c) {
    struct Large a = mkLarge(10), b = mkLarge(20);
    return c ? a : b;
}

struct Small commaSmall(int c) {
    struct Small a = mkSmall(7);
    return (c, a);
}

int main(void) {
    check(fwdSmall(3).x == 3, 1);
    check(fwdSmall(3).y == 4, 2);

    check(fwdLarge(5).a[0] == 5, 3);
    check(fwdLarge(5).a[4] == 9, 4);
    check(fwdLarge2(5).a[4] == 9, 5);

    check(pickSmall(1).x == 1, 6);
    check(pickSmall(0).x == 9, 7);
    check(pickLarge(1).a[0] == 10, 8);
    check(pickLarge(0).a[0] == 20, 9);

    check(commaSmall(0).y == 8, 10);

    return failures;
}
