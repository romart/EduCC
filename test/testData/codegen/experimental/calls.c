// Calls and returns - step 7's first half - run for their answers rather than
// diffed against a dump. Everything here is scalar: aggregate arguments,
// aggregate returns and 'long double' still have no selection rule and still
// leave the function to the legacy backend, which is the second half.
//
// The shape follows integer_subset.c, and for the same reason. Every function
// below is free of loads and stores, because those have no rule yet and a
// function containing one is left to the legacy backend and would test nothing
// here. What is new is that a *call* no longer costs that: before step 7 an
// IR_CALL was a placeholder, so every one of these would have fallen back.
//
// The callees are deliberately a mixture. Some are emitted by the new backend
// and some - anything reading a stack parameter, which is a load - by the
// legacy one, so most of these results cross the boundary between the two
// through the ordinary SysV ABI in one direction or the other. That the two
// agree about argument registers, the stack argument area, the return register
// and 16-byte alignment is exactly what a golden dump cannot check.
//
// Values are the arithmetic's own, checked against gcc.

// Six integer arguments is the last one that fits in registers: rdi, rsi, rdx,
// rcx, r8, r9 in that order. Getting the order wrong is invisible when the
// arguments are equal, so no two of these weights are.
int six(int a, int b, int c, int d, int e, int f) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32;
}

// The seventh argument onwards goes on the stack, and the count decides the
// alignment: seven arguments is one stack slot, so the stack pointer needs
// eight bytes of padding under it to stay 16-byte aligned at the call, and
// eight arguments is two slots and needs none. Both are here because only the
// padded one can go wrong, and only the unpadded one shows it was not applied
// unconditionally.
int seven(int a, int b, int c, int d, int e, int f, int g) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32 + g * 64;
}

int eight(int a, int b, int c, int d, int e, int f, int g, int h) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32 + g * 64 + h * 128;
}

// Nine, so that the stack area is more than the one or two slots above and a
// wrong push order shows up as a rotation rather than as a swap.
int nine(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32 + g * 64 + h * 128 + i * 256;
}

// A callee whose arguments are all constants at the call site, so every one of
// them is folded into an immediate rather than materialized - the argument-side
// half of the fold-or-materialize decision, which until step 7 only ALU
// operands could reach.
int constants_only(void) {
    return six(1, 2, 3, 4, 5, 6);
}

// A 64-bit argument outside the sign-extended imm32 range, which has to be
// moved with movabs rather than the short form. The same constant folded into
// an 'add' would be refused and materialized first; into an argument register
// it is not, because the destination is a register either way.
long wide_argument(long v) {
    return v;
}

// A value returned as an immediate: no register, no spill slot, no reload -
// which is what makes the return the mirror of an argument rather than a
// special case that goes through a virtual register on the way.
int answer(void) {
    return 42;
}

long wide_answer(void) {
    return 0x7ff0000000000001L;
}

// Two returns in one function, so the epilogue is emitted twice and the frame
// has to be undone identically by both.
int pick(int c) {
    if (c) {
        return -1;
    }
    return 1;
}

// A void call has no result to copy out of rax, so the call instruction has no
// def at all - the one shape where the operand list is uses only.
int gSum;

void accumulate(int v);
int total(void);

void accumulate(int v) { gSum = gSum + v; }
int total(void) { return gSum; }

void accumulate_twice(int a, int b) {
    accumulate(a);
    accumulate(b);
}

// A call through a function pointer, which encodes the callee as a register
// rather than as a relocation. The pointer arrives as a parameter so that
// reading it is not a load - taking a function's address any other way needs a
// materializing 'lea', which has no rule yet.
int apply(int (*fn)(int), int v) {
    return fn(v);
}

int negate(int v) { return -v; }

// A call whose result feeds straight into another call's argument, so the
// result copy out of rax and the argument copy into rdi meet with nothing in
// between to separate them.
int nested_calls(int v) {
    return negate(negate(v));
}

// The result of a call used more than once. It has to survive being spilled
// across the second call, which is the case a clobbered caller-saved register
// would break.
int reused_result(int v) {
    int r = negate(v);
    return r + negate(r);
}

// A call in a loop, so the stack adjustment around it happens on every
// iteration and any drift in the stack pointer accumulates into a crash rather
// than into a wrong number.
int call_in_loop(int n) {
    int acc = 0;
    for (int i = 0; i < n; ++i) {
        acc += six(i, 1, 2, 3, 4, 5);
    }
    return acc;
}

// Stack arguments in a loop, for the same reason and against the push path -
// this is what catches an 'add rsp' that does not match the pushes above it.
int stack_args_in_loop(int n) {
    int acc = 0;
    for (int i = 0; i < n; ++i) {
        acc += nine(i, 1, 2, 3, 4, 5, 6, 7, 8);
    }
    return acc;
}

// A variadic callee, which needs al set to the number of SSE registers the
// call passes arguments in - zero for all of these, since a float argument
// has no rule yet. printf is not used because its output would have to be
// checked; this is the same ABI question with an answer the test can see.
int sum_varargs(int n, ...);

int call_varargs(void) {
    return sum_varargs(3, 100, 20, 3);
}

int main(void) {
    int failures = 0;

    if (six(1, 1, 1, 1, 1, 1) != 1 + 2 + 4 + 8 + 16 + 32) failures += 1;
    if (six(6, 5, 4, 3, 2, 1) != 6 + 10 + 16 + 24 + 32 + 32) failures += 2;
    if (seven(1, 1, 1, 1, 1, 1, 1) != 1 + 2 + 4 + 8 + 16 + 32 + 64) failures += 4;
    if (eight(1, 1, 1, 1, 1, 1, 1, 1) != 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128) failures += 8;
    if (nine(1, 1, 1, 1, 1, 1, 1, 1, 1) != 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256) failures += 16;
    if (nine(9, 8, 7, 6, 5, 4, 3, 2, 1) != 9 + 16 + 28 + 48 + 80 + 128 + 192 + 256 + 256) failures += 32;
    if (constants_only() != 1 + 4 + 12 + 32 + 80 + 192) failures += 64;

    // Both wide values are checked against a *variable* holding the same
    // literal, not against the literal itself. Comparing against one directly
    // is miscompiled - see test/testData/codegen/bugs/wide_immediate_compare.c,
    // which is where that bug is pinned - and this fixture is about calls, so
    // it has no business failing for a reason in the comparison. Passing the
    // literal as the argument is the half that matters here: it is folded into
    // the argument register as a movabs, which is the wide end of the
    // fold-or-materialize decision.
    long wideArg = 0x0123456789abcdefL;
    long wideRet = 0x7ff0000000000001L;

    if (wide_argument(0x0123456789abcdefL) != wideArg) failures += 128;
    if (answer() != 42) failures += 256;
    if (wide_answer() != wideRet) failures += 512;
    if (pick(1) != -1) failures += 1024;
    if (pick(0) != 1) failures += 2048;

    gSum = 0;
    accumulate_twice(20, 22);
    if (total() != 42) failures += 4096;

    if (apply(negate, 7) != -7) failures += 8192;
    if (nested_calls(5) != 5) failures += 16384;
    if (reused_result(3) != -3 + 3) failures += 32768;
    if (call_in_loop(4) != 0 + 1 + 2 + 3 + 4 * (2 + 8 + 24 + 64 + 160)) failures += 65536;
    if (stack_args_in_loop(3) != 0 + 1 + 2 + 3 * (2 + 8 + 24 + 64 + 160 + 384 + 896 + 2048))
        failures += 131072;
    if (call_varargs() != 123) failures += 262144;

    return failures;
}

// Defined after main on purpose: a variadic callee reads its arguments through
// the register save area va_start builds, which is a whole sequence of stores,
// so this function always falls back to the legacy backend. That is the point -
// the caller is the new backend's and the callee is not.
#include <stdarg.h>

int sum_varargs(int n, ...) {
    va_list ap;
    int sum = 0;

    va_start(ap, n);
    for (int i = 0; i < n; ++i) {
        sum += va_arg(ap, int);
    }
    va_end(ap);

    return sum;
}
