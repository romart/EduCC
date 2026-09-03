// A pointer parameter that must not be confused with the stack pointer.
//
// IR_P_REG ids used to be per-class ordinals rather than register numbers, so
// the fourth integer parameter and the stack pointer were both '$3'. GVN keys
// IR_P_REG on (kind, type, physReg), and a pointer parameter is IR_PTR just
// like the stack pointer is, so the two were one expression as far as value
// numbering could tell and the load below was rewritten to dereference the
// stack pointer instead of 'p3'.
//
// Both halves of that have to stay visible for this to keep testing anything.
// 'p3' has to land in the *fourth* integer argument register (the id that
// collided), and the stack pointer has to stay live - hence a6/a7, which
// overflow the six SysV integer argument registers and so are addressed off
// the stack. If either stops holding, this fixture passes for the wrong
// reason, so the baseline pins down the register names too.
int param_ptr_vs_stack(int a0, int a1, int a2, int *p3,
                       int a4, int a5, int a6, int a7) {
    return *p3 + a6 + a7;
}
