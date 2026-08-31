// The edges out of a VLA's scope that are not "fall off the end".
//
// vla_scope_exit.c covers the three shapes step 19 was written for; this one
// covers the rest of the rule, which is where a scope-based implementation can
// go wrong in ways a loop-based one never could. A 'break', a 'continue' and a
// 'goto' each leave a block from somewhere in the middle of it, and a single
// one of them can leave several scopes at once - so each has to know how far
// out it is going, and one restore has to undo however many it crossed.
//
// Read off the addresses, as vla_scope_exit.c is and for the same reason:
// reclaimed storage means the next allocation starts where the last one ended.
// The exit code is the number of the first check that failed. gcc returns 0.

#include <alloca.h>

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- 'break' out of a block that allocates --------

// The 'break' leaves the body block from the middle of it, so it carries the
// restore itself. Two runs of the loop that both break have to hand back the
// same address, which they only do if the first one gave its slice up.
static int breakOut(int n) {
    char *seen[2];

    for (int r = 0; r < 2; ++r) {
        int v[n];
        seen[r] = (char *)v;
        break;
    }

    // One round, so nothing to compare yet - re-enter and check the second
    // entry starts where the first did.
    for (int r = 0; r < 2; ++r) {
        int w[n];
        seen[1] = (char *)w;
        break;
    }

    return seen[0] == seen[1];
}

// -------- 'continue' out of a block that allocates --------

// The path a loop rule gets right by accident and a scope rule has to get
// right on purpose: 'continue' jumps to the increment clause, which is neither
// the end of the body nor the loop exit.
static int continueOut(int n) {
    char *first = 0;
    int ok = 1;

    for (int r = 0; r < 4; ++r) {
        int v[n];
        char *a = (char *)v;

        if (r == 0) {
            first = a;
        } else if (a != first) {
            ok = 0;
        }

        continue;
    }

    return ok;
}

// -------- one jump leaving two scopes at once --------

// The inner block sits inside the outer one, so the 'goto' crosses both. One
// restore covers them: the outer save was taken before the inner one, so
// putting the stack pointer back to it undoes the inner allocation too.
static int nestedScopes(int n) {
    char *first = 0;
    int ok = 1;
    int round = 0;

top:
    {
        int outer[n];
        char *a = (char *)outer;

        if (round == 0) {
            first = a;
        } else if (a != first) {
            ok = 0;
        }

        {
            int inner[n];
            inner[0] = round;
            if (inner[0] >= 0) goto next;
        }
    }

next:
    if (++round < 4) goto top;

    return ok;
}

// -------- 'break' out of a switch whose case block allocates --------

// A switch's 'break' lands at the switch exit rather than at a loop's, and it
// has to take its depth from the switch: the enclosing loop is one scope
// further out, and restoring that far would hand back the body's own array
// while it is still live. That is what the check reads - 'u' is allocated
// after the switch and must land below 'v', not on top of it.
static int switchBreak(int n, int which) {
    char *first = 0;
    int ok = 1;

    for (int r = 0; r < 4; ++r) {
        int v[n];
        char *a = (char *)v;

        if (r == 0) {
            first = a;
        } else if (a != first) {
            ok = 0;
        }

        v[0] = 5;

        switch (which) {
        case 1: {
            int w[n];
            w[0] = r;
            if (w[0] >= 0) break;
        }
        default:
            break;
        }

        { int u[n]; u[0] = 99; }

        if (v[0] != 5) ok = 0;
    }

    return ok;
}

// -------- a VLA in a 'for' initializer --------

// Scoped to the whole statement, not to an iteration of it: it is allocated
// once, before the mark the loop's own jumps restore to, so the body's rounds
// must not reclaim it - and the end of the 'for' must. Reading v[0] after the
// loop is what would catch the first mistake; the address check catches the
// second.
static int forInitVla(int n) {
    char *a = 0;
    char *b = 0;
    int live = 0;

    for (int v[n], i = 0; i < 3; ++i) {
        int w[n];
        v[0] = 7;
        a = (char *)v;
        w[0] = 0;
        live = v[0];
    }

    {
        int later[n];
        b = (char *)later;
    }

    return live == 7 && a == b;
}

// -------- alloca() is not reclaimed by a scope --------

// The exclusion the rule turns on. alloca()'s storage lives until the
// *function* returns, so a block that ends must not take it back: the block
// below is left twice and the second allocation has to sit below the first,
// not on top of it, and the first has to still be readable afterwards.
static int allocaSurvivesScope(int n) {
    char *a;
    char *b;

    { a = (char *)alloca(n); a[0] = 11; }
    { b = (char *)alloca(n); b[0] = 22; }

    return a != b && a[0] == 11 && b[0] == 22;
}

// -------- a label above the declaration, jumped to from another scope --------

// Legal C, and the shape that decides where the mark is taken. 'resume' is
// above 'b', so it is not in b's scope and the jump into the block is allowed
// (gcc compiles this; vla_jump_into_scope.c is the negative side of the same
// rule). A mark taken at the head of the block would be skipped by exactly
// this jump and the restore below it would read a register nothing wrote -
// which is why the mark is taken at the declaration instead, where every path
// to the restore crosses it.
static int labelAboveDeclaration(int n) {
    char *a = 0;
    char *b = 0;

    {
        int v[n];
        a = (char *)v;
        goto resume;
    }

    {
    resume:
        ;
        int w[n];
        b = (char *)w;
    }

    return a == b;
}

// -------- a backward jump over the declaration --------

// The same label, jumped to from below instead of from beside: 'resume' is
// above 'w', so the loop this builds re-enters the block at a point outside
// w's scope and crosses the declaration again on every round. Two things have
// to be true at once for the address to repeat - the jump has to carry the
// restore, and the mark has to be re-taken each round rather than held from
// the first, which is what taking it at the declaration and not at the head of
// the block gets for free. A jump that carried no restore would climb by one
// array per round and only give the last one back.
static int backwardOverDeclaration(int n) {
    char *first = 0;
    int ok = 1;
    int round = 0;

    {
    resume:
        ;
        int w[n];
        char *a = (char *)w;

        if (round == 0) {
            first = a;
        } else if (a != first) {
            ok = 0;
        }

        w[0] = round;
        if (++round < 4) goto resume;
    }

    return ok;
}

int main(void) {
    check(breakOut(64), 1);
    check(continueOut(64), 2);
    check(nestedScopes(64), 3);
    check(switchBreak(64, 1), 4);
    check(forInitVla(64), 5);
    check(allocaSurvivesScope(64), 6);
    check(labelAboveDeclaration(64), 7);
    check(backwardOverDeclaration(64), 8);

    return failures;
}
