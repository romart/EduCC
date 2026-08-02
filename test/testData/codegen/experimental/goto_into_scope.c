// Jumping into the middle of a block, which is the one place where a local's
// storage stops coinciding with the point its declaration is written at.
//
// C99 6.2.4p6 gives an automatic object a lifetime running over the whole
// block it is declared in, not just the part after the declaration, and 6.8.6.1
// only forbids jumping into the scope of a variably modified type - so every
// function below is well defined, and 'b' has storage at the label even though
// control never passed its declaration. What is skipped is the initializer,
// not the allocation.
//
// That distinction is invisible to the legacy backend, which lays the frame out
// from the declarations, and was wrong in the IR pipeline, which used to emit
// the alloca where the declaration sat. The block holding it is unreachable -
// nothing branches to it, the goto jumps past - while the stores and loads
// after the label are not, so the alloca ended up as a value defined in dead
// code and read from live code. buildSSA cannot place phis for a definition
// that does not dominate its uses, and cleanupUnreachableBlock waits for such
// a block to go empty, which it never does: the compiler hung. Locals are
// allocated in the entry block now (createLocalSlot, src/ir/ast2ir.c).
//
// So these are checked for their answers, but the first thing they check is
// that the compiler terminates at all. Values verified against gcc.

// The plain case: a declaration jumped over, written and read after the label.
int skip_declaration(int n) {
    int total = 0;
    goto inner;
    {
        int b;
inner:
        b = n * 2;
        total = b + 1;
    }
    return total;
}

// The same, but the jumped-over declaration has an initializer. Only the
// initializer is skipped, so 999 must never be observed - if the alloca and
// the store were treated as one unit, this would return 999.
int skip_initializer(int n) {
    int r = 0;
    goto inner;
    {
        int b = 999;
inner:
        b = n;
        r = b;
    }
    return r;
}

// Taking the address keeps the slot out of mem2reg's hands, so the alloca
// survives all the way to the frame layout rather than being promoted away.
int skip_addr_taken(int n) {
    int r;
    goto inner;
    {
        int b;
        int *p;
inner:
        b = n + 5;
        p = &b;
        r = *p;
    }
    return r;
}

// Two levels, with the label in the inner one: both blocks are jumped into and
// both hold a declaration that control never reaches.
int skip_nested(int n) {
    int r = 0;
    goto deep;
    {
        int a;
        {
            int b;
deep:
            b = n;
            a = b * 3;
            r = a - 1;
        }
    }
    return r;
}

// A jumped-over array, so what is skipped is a slot several registers wide
// rather than something mem2reg could have promoted either way.
int skip_array(int n) {
    int r = 0;
    goto fill;
    {
        int v[4];
fill:
        v[0] = n;
        v[1] = n + 1;
        v[2] = n + 2;
        v[3] = n + 3;
        r = v[0] + v[1] + v[2] + v[3];
    }
    return r;
}

// The control: here the label is outside the block, so the declarations and
// every use of them are unreachable together. This is the case that always
// worked - dead code that is dead all the way through - and it is here so a
// fix that simply stopped deleting unreachable blocks would not pass.
int dead_between(int n) {
    int r = 1;
    goto done;
    {
        int a = n * 3;
        int b = a + n;
        r = a * b;
    }
done:
    return r;
}

// Jumping backwards into a block already left, so the block is reached the
// second time around but still not through its declaration.
int back_into_scope(int n) {
    int r = 0;
    int trips = 0;
    {
        int b;
again:
        b = n + trips;
        r += b;
    }
    ++trips;
    if (trips < 3)
        goto again;
    return r;
}

int main(void) {
    int failures = 0;

    if (skip_declaration(10) != 21) failures += 1;
    if (skip_initializer(7) != 7) failures += 2;
    if (skip_addr_taken(4) != 9) failures += 4;
    if (skip_nested(5) != 14) failures += 8;
    if (skip_array(1) != 1 + 2 + 3 + 4) failures += 16;
    if (dead_between(100) != 1) failures += 32;
    if (back_into_scope(6) != 6 + 7 + 8) failures += 64;

    return failures;
}
