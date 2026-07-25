// Taking a local variable's address prevents SSA promotion for it (it must
// live in memory once a pointer to it escapes), so every read of 'x' through
// 'p' lowers to an IR_M_LOAD - and, exactly as for a pointer parameter, GVN
// must never merge those loads with no alias analysis to prove nothing wrote
// through 'p' in between. 'p' itself is just an alias for x's ALLOCA'd
// address and gets promoted away entirely (mem2reg), so both loads read
// directly from that same address - only the loads must stay unmerged.
int local_addr_taken(int a) {
    int x = a;
    int *p = &x;
    int y = *p;
    int z = *p;
    return y + z;
}
