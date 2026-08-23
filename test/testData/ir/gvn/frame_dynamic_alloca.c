// A frame whose size is not known when it is laid out.
//
// A variable-length array moves the stack pointer at run time, so it gets no
// fixed displacement from the frame pointer - the frame object exists purely
// as something selection can point at, and its address comes from the stack
// pointer as it stands after the allocation.
//
// Nothing puts that stack pointer back, and the layout reserves no slot to
// park it in. Everything the frame holds is addressed from the frame pointer
// and the epilogue's 'leave' restores rsp from there, so the allocation ends
// when the function does, which is exactly as long as C says it lasts. The
// slot the layout used to reserve for the old stack pointer was written by
// nobody and read by nobody, and step 15 deleted it.
//
// The declaration is two allocations here and one object in the source: the
// dynamically sized block, and a word holding its address, which is what a
// read of 'v' loads. Only the first is in the frame below - mem2reg promotes a
// pointer that is stored once and only ever loaded - which is what makes the
// carve ('add 15', 'and -16', 'sub rsp') feed a register directly.
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
