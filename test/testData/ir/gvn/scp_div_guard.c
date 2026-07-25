// Both operands of '10 / z' and '1 % z' are compile-time constants, so a
// folder that evaluates whatever it can prove constant would divide by zero
// while compiling - trapping the compiler itself, or silently baking in a
// result for something the standard leaves undefined. Neither division may
// be folded, and the baseline records both surviving as real IR_E_DIV /
// IR_E_MOD instructions.
//
// The divisions sit behind a branch that is never resolved, so this also
// pins down that "unreachable-ish" reasoning does not quietly excuse the
// fold: the blocks are both executable as far as scp is concerned.
int scp_div_guard(int a) {
    int z = 0;
    if (a > 0) {
        return 10 / z;
    }
    return 1 % z;
}
