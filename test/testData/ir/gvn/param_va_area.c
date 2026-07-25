// The va_list register-save area, whose shape is derived from the same two
// argument-register counts as ordinary parameter classification.
//
// __va_elem's gp_offset/fp_offset are cursors into a save area holding one
// slot per argument register, and va_arg compares them against the point where
// the registers run out and the overflow area takes over. While those counts
// were invented (10 and 10, rather than SysV's 6 and 8) the area was sized for
// registers that do not exist and the thresholds sat past the end of the real
// ones, so va_arg would keep reading the save area after the arguments had
// actually started arriving on the stack.
//
// Both cursors have to appear for this to cover both banks, hence one integer
// and one floating va_arg. There is no IR-level coverage of varargs otherwise,
// which is why this exists at all - the codegen suite's varargs tests run the
// legacy pipeline and never reach this code.
//
// This fixture is also the only thing standing between the __va_elem member
// offsets and a silent disagreement. generateVaArea() writes the area using
// hardcoded offsets while translateVaArg() reads it back through
// findStructualMember() on the real struct from sdk/include/stdarg.h, so if
// the two drift apart va_arg quietly returns the wrong field rather than
// failing. The baseline pins the written offsets to 0/4/16 and the save area
// to 24.
#include <stdarg.h>

double param_va_area(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int i = va_arg(ap, int);
    double d = va_arg(ap, double);
    va_end(ap);
    return d + i;
}
