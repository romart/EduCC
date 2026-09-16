#ifndef __FLOAT_IMAGE_H__
#define __FLOAT_IMAGE_H__ 1

#include "common.h"

// What sixteen bytes of 'long double' mean, which is not the same question on
// both targets: x86-64's is x87's 80-bit extended format padded out to sixteen,
// and LP64D's is IEEE binary128. The compiler's own long double is one of the
// two and is very often not the target's, so a constant is *converted* on the
// way into a section rather than copied - see R7 in the riscv64 plan.
enum LongDoubleFormat {
  LDF_X87_EXTENDED,
  LDF_BINARY128
};

// Which of them the host this compiler is running on uses. Decided from the
// bytes of 1.0L rather than from a predefined macro, because the compiler that
// built this binary may be EduCC, which defines none of the __LDBL_* set.
enum LongDoubleFormat hostLongDoubleFormat(void);

// How many bytes of a host long double carry the value. The rest is padding,
// and hashing it would make two copies of one constant compare unequal.
size_t hostLongDoubleWidth(void);

// The target's sixteen bytes for 'v', little-endian.
void longDoubleImage(enum LongDoubleFormat format, long double v, uint8_t out[16]);

#endif // __FLOAT_IMAGE_H__
