#ifndef __STDARG_H
#define __STDARG_H

#include <stddef.h>

// Two shapes, and which one a target has is enum VaListKind in
// include/ir/target.h - generateVaArea writes this and __builtin_va_arg reads
// it back by member name, so the two have to agree.
#ifdef __riscv

// LP64D. A variadic float arrives in an integer register, so there is one
// register save area rather than two, and the callee places it immediately
// below the caller's stack arguments - which makes the whole argument list one
// array and a walk over it one pointer with no counter and no bound.
typedef struct __va_elem {
  void *overflow_arg_area;
} __va_elem;

#else

typedef struct __va_elem {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void *overflow_arg_area;
  const void *reg_save_area;
} __va_elem;

#endif

typedef __va_elem va_list[1];


#define va_start(ap, last) \
  do { *(ap) = *(__va_elem *)__va_area__; } while (0)

#define va_end(v)
#define va_arg(v,l)	__builtin_va_arg(v,l)

#define va_copy(dest, src) ((dest)[0] = (src)[0])


#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif // __STDARG_H
