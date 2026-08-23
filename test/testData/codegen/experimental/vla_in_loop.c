// A variable-length array declared inside a loop, which has to give its
// storage back on the way round.
//
// C99 6.2.4p6 ends an automatic object's lifetime with the block it was
// declared in, so a VLA in a loop body is dead once the iteration is over. A
// VLA is allocated by moving the stack pointer, and nothing here moves it back
// at the end of a block - so before step 15 every iteration took another slice
// and the frame grew until it ran off the end of the stack. gcc runs the
// programs below; this compiler crashed on them, and the legacy backend still
// does.
//
// What puts it back is a save taken before the loop and a restore at the head
// of the two blocks every path out of the body lands in - the continue target,
// where the next iteration starts, and the exit. That is why 'continue' is
// here as its own case: it leaves the body from somewhere the bottom of the
// body never reaches, and an implementation that restored at the end of the
// body instead would pass every other check on this page and still grow.
//
// A call to alloca() is the deliberate exception, at the bottom: its storage
// lives until the *function* returns, not until the end of the block, so
// reclaiming it per iteration would take memory the program can still read.
// gcc grows the frame for that one too, and the check is that the first block
// is still there at the end rather than anything about how much it grew.
//
// The iteration counts are what make this a test rather than a formality: at
// 50000 rounds of 256 bytes a leak is 12MB against a default 8MB stack, so a
// regression is a crash and not a slow program. The exit code is the number of
// the first check that failed. gcc returns 0.

#include <alloca.h>

#define ROUNDS 50000

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- the three loop forms --------

// The continue target is the modifier block, so the restore goes at its head.
static int forLoop(int n, int iters) {
    int acc = 0;

    for (int i = 0; i < iters; ++i) {
        int v[n];
        v[0] = i;
        v[n - 1] = 1;
        acc += v[0] + v[n - 1];
    }

    return acc;
}

// No modifier, so the continue target is the condition block instead.
static int whileLoop(int n, int iters) {
    int acc = 0;
    int i = 0;

    while (i < iters) {
        int v[n];
        v[0] = i;
        acc += v[0];
        ++i;
    }

    return acc;
}

// The condition is at the bottom, so the restore lands between the body and
// the test rather than before it.
static int doLoop(int n, int iters) {
    int acc = 0;
    int i = 0;

    do {
        int v[n];
        v[0] = i;
        acc += v[0];
        ++i;
    } while (i < iters);

    return acc;
}

// -------- leaving the body somewhere other than the bottom --------

static int withContinue(int n, int iters) {
    int acc = 0;

    for (int i = 0; i < iters; ++i) {
        int v[n];
        v[0] = i;

        // Half the iterations never reach the end of the body.
        if (i & 1) continue;

        acc += v[0];
    }

    return acc;
}

// 'break' leaves through the exit block, which is the other place the restore
// sits. The allocation before it has to be gone once the loop is over, which
// the caller checks by looping again afterwards.
static int withBreak(int n, int iters) {
    int acc = 0;

    for (int i = 0; i < iters; ++i) {
        int v[n];
        v[0] = i;
        acc += v[0];
        if (i == iters / 2) break;
    }

    return acc;
}

// -------- the allocation somewhere below the body's own block --------

// A nested block: the loop's restore has to cover what any block under it
// allocated, not only what the body declares directly.
static int innerBlock(int n, int iters) {
    int acc = 0;

    for (int i = 0; i < iters; ++i) {
        {
            int v[n];
            v[0] = i;
            acc += v[0];
        }
    }

    return acc;
}

// Under an 'if', so it is allocated on some iterations and not others and the
// restore has to be right either way.
static int conditional(int n, int iters) {
    int acc = 0;

    for (int i = 0; i < iters; ++i) {
        if (i & 1) {
            int v[n];
            v[0] = i;
            acc += v[0];
        } else {
            acc += 1;
        }
    }

    return acc;
}

// Two loops, one inside the other, each with its own allocation: the inner
// one's restore brings the stack back to where the inner loop started, which
// is a point the outer one's restore is still above.
static int nested(int n, int outer, int inner) {
    int acc = 0;

    for (int i = 0; i < outer; ++i) {
        int a[n];
        a[0] = i;

        for (int j = 0; j < inner; ++j) {
            int b[n + j];
            b[0] = j;
            acc += a[0] + b[0];
        }
    }

    return acc;
}

// -------- alloca(), which is not reclaimed --------

// Its lifetime is the function, so every iteration takes another block and the
// first one is still readable at the end. Kept small, because this one really
// does grow.
static int allocaSurvives(int iters) {
    char *first = 0;

    for (int i = 0; i < iters; ++i) {
        char *p = alloca(64);
        p[0] = (char)(i + 1);
        if (first == 0) first = p;
    }

    return first[0];
}

int main(void) {
    // Sum of 0..ROUNDS-1, computed the same way in both places so the check is
    // about the loop and not about the arithmetic.
    int sum = 0;
    for (int i = 0; i < ROUNDS; ++i) sum += i;

    check(forLoop(64, ROUNDS) == sum + ROUNDS, 1);
    check(whileLoop(64, ROUNDS) == sum, 2);
    check(doLoop(64, ROUNDS) == sum, 3);

    int oddSum = 0;
    for (int i = 0; i < ROUNDS; i += 2) oddSum += i;
    check(withContinue(64, ROUNDS) == oddSum, 4);

    int half = ROUNDS / 2;
    int halfSum = 0;
    for (int i = 0; i <= half; ++i) halfSum += i;
    check(withBreak(64, ROUNDS) == halfSum, 5);

    // Again after the one that broke out, so a stack the break left standing
    // would have to be given back by something.
    check(withBreak(64, ROUNDS) == halfSum, 6);

    check(innerBlock(64, ROUNDS) == sum, 7);

    int condSum = 0;
    for (int i = 0; i < ROUNDS; ++i) condSum += (i & 1) ? i : 1;
    check(conditional(64, ROUNDS) == condSum, 8);

    int nestedSum = 0;
    for (int i = 0; i < 2000; ++i)
        for (int j = 0; j < 8; ++j)
            nestedSum += i + j;
    check(nested(64, 2000, 8) == nestedSum, 9);

    check(allocaSurvives(1000) == 1, 10);

    return failures;
}
