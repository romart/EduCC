// A variable-length array's storage is not given back when its scope ends.
//
// C99 6.2.4p6 ends an automatic object's lifetime with the block it was
// declared in. A VLA is allocated by moving the stack pointer, so ending its
// lifetime means moving the stack pointer back - and neither backend does that
// at the end of an ordinary block. Step 15 taught the IR pipeline to do it on
// every path round a 'for', 'while' or 'do', which is what stops a loop's frame
// growing without bound (codegen/experimental/vla_in_loop.c); what is left is
// the general scope rule, and that is roadmap step 17 in
// docs/ir-codegen-design.md.
//
// The checks read the property off the *addresses*, because that is what the
// standard's lifetime rule is observable as here and what makes each case a
// number rather than a crash: when the storage is reclaimed, the next
// allocation starts where the last one ended and the two have the same address.
// gcc reuses the address in all three; nothing about the addresses of distinct
// live objects is being assumed.
//
// Not a crash on purpose. 'gotoLoop' below is a loop like any other and does
// grow every round, so it can exhaust the stack given enough rounds - four is
// enough to see the drift and few enough that the rest of the file still runs
// and reports.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- two sibling blocks, one after the other --------

// The first block's array is dead before the second one is declared, so both
// should start at the same place. Today the second is allocated below the
// first, and both stand until the function returns.
static int siblingBlocks(int n) {
    char *a;
    char *b;

    { int v[n]; a = (char *)v; }
    { int w[n]; b = (char *)w; }

    return a == b;
}

// -------- a loop built out of 'goto' rather than out of 'for' --------

// The same program as a 'for' loop, and the one shape that can still take a
// program down: step 15 hangs the save and the restore off the loop
// *statement*, and there is no loop statement here to hang them off, so every
// round takes another slice and nothing gives it back.
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

// The jump misses both places the restore sits - the continue target and the
// loop exit - so that iteration's array is held. It is bounded rather than
// unbounded here: the enclosing 'for' allocates too, so its own restore takes
// the stack back at the end of every outer round, which is why this one
// already passes under -experimental and fails only under the legacy backend.
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
