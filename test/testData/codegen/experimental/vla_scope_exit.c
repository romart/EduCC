// A variable-length array's storage, given back when its scope ends.
//
// C99 6.2.4p6 ends an automatic object's lifetime with the block it was
// declared in. A VLA is allocated by moving the stack pointer, so ending its
// lifetime means moving the stack pointer back. Step 15 taught the IR pipeline
// to do it on every path round a 'for', 'while' or 'do', which is what stops a
// loop's frame growing without bound (codegen/experimental/vla_in_loop.c);
// step 19 replaced that with the general scope rule - a save where a block
// that declares one is entered, and a restore on every edge leaving it - and
// this fixture is what it was written against. See section 6.22 of
// docs/ir-codegen-design.md.
//
// The legacy backend gives none of it back and is skipped here rather than
// muted: it addresses its temporaries from rsp rather than from the frame
// pointer, so putting rsp back is not the one instruction it is on the other
// side, and it is not going to be taught to - the same call vla_in_loop makes.
//
// The checks read the property off the *addresses*, because that is what the
// standard's lifetime rule is observable as here and what makes each case a
// number rather than a crash: when the storage is reclaimed, the next
// allocation starts where the last one ended and the two have the same address.
// gcc reuses the address in all three; nothing about the addresses of distinct
// live objects is being assumed.
//
// Not a crash on purpose. 'gotoLoop' below is a loop like any other, and was
// the one shape that could still take a program down - it has no loop
// statement for step 15's save and restore to hang off - so before step 19 it
// grew every round. Four rounds is enough to see the drift and few enough that
// the rest of the file still runs and reports.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- two sibling blocks, one after the other --------

// The first block's array is dead before the second one is declared, so both
// start at the same place. This is the case a loop rule cannot reach at all:
// there is no loop here, and before step 19 the second was allocated below the
// first and both stood until the function returned.
static int siblingBlocks(int n) {
    char *a;
    char *b;

    { int v[n]; a = (char *)v; }
    { int w[n]; b = (char *)w; }

    return a == b;
}

// -------- a loop built out of 'goto' rather than out of 'for' --------

// The same program as a 'for' loop, and the shape that used to be able to take
// a program down: step 15 hung the save and the restore off the loop
// *statement*, and there is no loop statement here to hang them off, so every
// round took another slice and nothing gave it back. Scoping the pair to the
// block instead makes the two spellings of this loop behave alike - the label
// is outside the block, so every round enters and leaves it once.
static int gotoLoop(int n) {
    int round = 0;
    char *first = 0;
    int ok = 1;

top:
    {
        int v[n];
        char *a = (char *)v;

        if (round == 0) {
            first = a;
        } else if (a != first) {
            ok = 0;
        }
    }

    if (++round < 4) goto top;

    return ok;
}

// -------- 'goto' out of a loop whose body allocates --------

// The jump misses both places step 15's restore sat - the continue target and
// the loop exit - so that iteration's array was held. It was bounded rather
// than unbounded, the enclosing 'for' taking the stack back at the end of every
// outer round, which is why this check passed before the other two did. Under
// the scope rule the 'goto' carries the restore itself: it leaves the inner
// body's scope and lands at a label outside it.
static int gotoOutOfLoop(int n) {
    char *first = 0;
    int ok = 1;

    for (int r = 0; r < 4; ++r) {
        for (int i = 0; i < 3; ++i) {
            int v[n];
            char *a = (char *)v;

            if (r == 0) {
                first = a;
            } else if (a != first) {
                ok = 0;
            }

            goto next;
        }
    next: ;
    }

    return ok;
}

int main(void) {
    check(siblingBlocks(64), 1);
    check(gotoLoop(64), 2);
    check(gotoOutOfLoop(64), 3);

    return failures;
}
