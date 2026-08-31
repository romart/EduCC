// 'goto *' out of a VLA's scope.
//
// vla_scope_jumps.c covers the edges out of a scope that carry a name.  This
// one covers the edge that carries a value instead, which is the reason it was
// left out: the restore in front of a 'goto' is chosen by looking up the
// label's scope depth, and a computed goto has no label to look up.
//
// It does have a set of them.  Only a label whose address was taken can be
// reached, and the deepest of those is a depth that is honest for every one:
// putting the stack pointer back that far is exact for the deepest target and
// hands back too little for the shallower ones.  Going by any shallower target
// would be the mistake worth being careful about - it would free a scope that
// a deeper target is still inside of, and that is not a leak, it is a live
// frame handed to the next allocation.
//
// Read off the addresses, as the other two VLA fixtures are: reclaimed storage
// means the next allocation starts where the last one ended.  The exit code is
// the number of the first check that failed.
//
// gcc does *not* return 0 here, and that is deliberate.  gcc reclaims nothing
// on a computed goto - the array below climbs by its own size every round -
// which is allowed, the storage being dead either way, just not free.  So this
// is the one VLA fixture that is not a gcc-parity test, and the checks say
// what this compiler does rather than what both do.

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- a dispatch loop over a declaration --------

// The shape the leak was recorded against: every round enters the block, takes
// a slice and jumps back out through a value.  Four rounds at the same address
// is the whole property - one round per slice is what a missing restore looks
// like.
static int dispatchLoop(int n) {
    void *tab[2];
    char *first = 0;
    int r = 0;
    int ok = 1;

    tab[0] = &&again;
    tab[1] = &&done;

again:
    {
        int v[n];

        v[0] = r;
        if (first == 0) {
            first = (char *)v;
        } else if (first != (char *)v) {
            ok = 0;
        }

        ++r;
        goto *tab[r < 4 ? 0 : 1];
    }

done:
    return ok;
}

// -------- a target inside a scope of its own --------

// Both halves of the rule in one function.  'deep' sits below 'outer's
// declaration and above 'mid's, so the deepest reachable target is one scope
// in: the jump has to give back 'mid' and keep 'outer'.  Reading outer[0]
// after the loop is what catches a restore that went one scope too far -
// 'mid' is allocated on top of it, so freeing 'outer' too hands 'mid' the
// bytes 'outer' is still using.
static int targetInsideScope(int n) {
    void *tab[1];
    char *first = 0;
    int r = 0;
    int ok = 1;

    {
        int outer[n];

        outer[0] = 0x5a;

    deep:
        tab[0] = &&deep;

        {
            int mid[n];

            mid[0] = r;
            if (first == 0) {
                first = (char *)mid;
            } else if (first != (char *)mid) {
                ok = 0;
            }

            if (++r < 4) goto *tab[0];
        }

        if (outer[0] != 0x5a) ok = 0;
    }

    return ok;
}

// -------- the target read out of the storage being given back --------

// The table is itself a VLA in the block the jump leaves, so the restore has
// to come after the address has been computed and not before.  A restore
// placed first leaves the load reading below the stack pointer, which is a
// read of storage the next call is entitled to write - it survives a quiet run
// and is exactly the kind of thing that stops surviving under a signal
// handler, so the ordering is pinned here rather than left to luck.
static int targetFromScopedStorage(int n) {
    char *first = 0;
    int r = 0;
    int ok = 1;

again:
    {
        void *tab[n];

        tab[0] = &&again;
        tab[1] = &&done;

        if (first == 0) {
            first = (char *)tab;
        } else if (first != (char *)tab) {
            ok = 0;
        }

        ++r;
        goto *tab[r < 4 ? 0 : 1];
    }

done:
    return ok;
}

int main(void) {
    check(dispatchLoop(64), 1);
    check(targetInsideScope(64), 2);
    check(targetFromScopedStorage(64), 3);

    return failures;
}
