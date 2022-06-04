#ifndef __STDDEF_H
#define __STDDEF_H

#include <basic.h>

#define NULL ((void *)0)

typedef unsigned long size_t;
typedef long ptrdiff_t;
typedef unsigned int wchar_t;
typedef long max_align_t;

#define offsetof(type, member) ((size_t)&(((type *)0)->member))

// temporary
extern void *alloca (size_t __size);

#endif // __STDDEF_H
