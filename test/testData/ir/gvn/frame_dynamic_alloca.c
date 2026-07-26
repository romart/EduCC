// A frame whose size is not known when it is laid out.
//
// A variable-length array moves the stack pointer at run time, so it gets no
// fixed displacement from the frame pointer - the frame object exists purely
// as something selection can point at, and its address comes from the stack
// pointer as it stands after the allocation. What the layout does have to
// reserve is the slot the old stack pointer is parked in so the epilogue can
// put it back, and that slot is placed first, closest to the frame pointer,
// so it stays reachable however far the frame grows below it.
//
// 'fixed' is there so the two kinds are laid out side by side: a dynamic
// object must not consume any of the static frame, and a static one following
// it must not be given an offset that assumes it did.
int frame_dynamic_alloca(int n) {
    int fixed[2];
    int v[n];

    v[0] = n;
    fixed[0] = v[0];
    fixed[1] = n;

    return fixed[0] + fixed[1];
}
