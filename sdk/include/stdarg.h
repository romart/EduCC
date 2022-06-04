#ifndef __STDARG_H
#define __STDARG_H

#include <stddef.h>

typedef struct __va_elem {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void *overflow_arg_area;
  const void *reg_save_area;
} __va_elem;

typedef __va_elem va_list[1];


#define va_start(ap, last) \
  do { *(ap) = *(__va_elem *)__va_area__; } while (0)

#define va_end(v)
#define va_arg(v,l)	__builtin_va_arg(v,l)

#define va_copy(dest, src) ((dest)[0] = (src)[0])


#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif // __STDARG_H
