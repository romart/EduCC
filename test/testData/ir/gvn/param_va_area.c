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
// and one floating va_arg. This was the only coverage of any kind when it was
// written, since the codegen suite's varargs tests all ran the legacy pipeline;
// step 14 changed that, and codegen/experimental/variadic_definition.c now runs
// the same machinery. What is still only here is the *shape* - the baselines
// are where the save-area stores and the two cursors are legible instruction by
// instruction rather than as an exit code.
//
// This fixture is also what stands between the __va_elem member offsets and a
// silent disagreement. generateVaArea() writes the area using hardcoded offsets
// while translateVaArg() reads it back through findStructualMember() on the
// real struct from sdk/include/stdarg.h, so if the two drift apart va_arg
// quietly returns the wrong field rather than failing. The baseline pins the
// written offsets to 0/4/8/16, the save area to 24, and the fourteen argument
// registers spilled into it - six integer at 24 and eight SSE at 72, which is
// the layout the two cursors' bounds (48 and 112) are measured against.
#include <stdarg.h>

double param_va_area(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int i = va_arg(ap, int);
    double d = va_arg(ap, double);
    va_end(ap);
    return d + i;
}
