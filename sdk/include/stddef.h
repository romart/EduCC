#ifndef __STDDEF_H__
#define __STDDEF_H__

#define NULL ((void *)0)

typedef unsigned long size_t;
typedef long ptrdiff_t;
typedef unsigned int wchar_t;
typedef long max_align_t;

#define offsetof(type, member) ((size_t)&(((type *)0)->member))

#endif // __STDDEF_H__
