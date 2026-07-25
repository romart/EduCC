// The point where each SysV argument register bank runs out.
//
// The IR used to classify parameters against two invented constants (both 10)
// rather than against the target, so it put the seventh integer and the ninth
// floating parameter in registers that do not exist for that purpose. SysV
// AMD64 passes six integer arguments in rdi/rsi/rdx/rcx/r8/r9 and eight
// floating ones in xmm0..xmm7, and everything after that on the stack.
//
// Both banks overflow by exactly one here, and the parameters used are chosen
// to straddle each boundary: i5/f7 are the last that fit in registers and
// i6/f8 the first that do not. The two banks are counted independently, so
// the seven integer parameters must not push the floating ones onto the stack
// early - which is what a single shared counter would do.
double param_abi_boundary(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
                          double f0, double f1, double f2, double f3,
                          double f4, double f5, double f6, double f7,
                          double f8) {
    return f7 + f8 + (double)(i5 + i6);
}
