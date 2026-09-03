/* A frozen excerpt of glibc's <string.h>, with the pieces of <sys/cdefs.h> and
   <features.h> it leans on written out in place. It is checked in rather than
   included from the host because what a real system header declares moves with
   the C library's version - glibc 2.43 added memset_explicit, which no Ubuntu
   image ships yet - and a golden -E baseline cannot track that. Every other
   fixture here (sys/cdefs1.c, inner.h, include_dubledef.h) is an excerpt for
   the same reason. */

#ifndef _STRDECLS_H
#define _STRDECLS_H 1

/* <sys/cdefs.h>. Without __GNUC__ every one of these collapses to nothing,
   which is why the declarations below come out of -E as plain C. */
#ifdef __GNUC__
# define __LEAF , __leaf__
# define __THROW __attribute__ ((__nothrow__ __LEAF))
# define __nonnull(params) __attribute__ ((__nonnull__ params))
# define __attribute_pure__ __attribute__ ((__pure__))
# define __attr_access(x) __attribute__ ((__access__ x))
# define __wur __attribute__ ((__warn_unused_result__))
#else
# define __THROW
# define __nonnull(params)
# define __attribute_pure__
# define __attr_access(x)
# define __wur
# define __restrict
#endif

#define __BEGIN_DECLS
#define __END_DECLS

/* <features.h>. __GLIBC_USE pastes its argument onto a prefix, so each #if
   below expands a function-like macro inside a conditional. */
#define __GLIBC_USE(F) __GLIBC_USE_ ## F
#define __GLIBC_USE_ISOC23 0
#define __GLIBC_USE_LIB_EXT2 1

#define __USE_MISC 1
#define __USE_XOPEN2K8 1

/* Get size_t and NULL from <stddef.h>.  */
#define __need_size_t
#define __need_NULL
#include <stddef.h>

__BEGIN_DECLS

/* Copy N bytes of SRC to DEST.  */
extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
		     size_t __n) __THROW __nonnull ((1, 2));
/* Copy N bytes of SRC to DEST, guaranteeing
   correct behavior for overlapping strings.  */
extern void *memmove (void *__dest, const void *__src, size_t __n)
     __THROW __nonnull ((1, 2));

/* Copy no more than N bytes of SRC to DEST, stopping when C is found.  */
#if defined __USE_MISC || defined __USE_XOPEN || __GLIBC_USE (ISOC23)
extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
		      int __c, size_t __n)
    __THROW __nonnull ((1, 2)) __attr_access ((__write_only__, 1, 4));
#endif /* Misc || X/Open.  */

/* Set N bytes of S to C.  */
extern void *memset (void *__s, int __c, size_t __n) __THROW __nonnull ((1));

/* Not declared here: its guard is off, unlike the one above it.  */
#if __GLIBC_USE (ISOC23)
extern void *memset_explicit (void *__s, int __c, size_t __n)
     __THROW __nonnull ((1));
#endif

/* Compare N bytes of S1 and S2.  */
extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __THROW __attribute_pure__ __nonnull ((1, 2));

/* Search N bytes of S for C.  */
extern void *memchr (const void *__s, int __c, size_t __n)
      __THROW __attribute_pure__ __nonnull ((1));

/* Copy SRC to DEST.  */
extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __THROW __nonnull ((1, 2));
/* Copy no more than N characters of SRC to DEST.  */
extern char *strncpy (char *__restrict __dest,
		      const char *__restrict __src, size_t __n)
     __THROW __nonnull ((1, 2));

/* Append SRC onto DEST.  */
extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __THROW __nonnull ((1, 2));

/* Compare S1 and S2.  */
extern int strcmp (const char *__s1, const char *__s2)
     __THROW __attribute_pure__ __nonnull ((1, 2));
extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __THROW __attribute_pure__ __nonnull ((1, 2));

/* <bits/types/__locale_t.h>, inlined: a struct in the middle of a run of
   declarations, and a typedef of a typedef.  */
struct __locale_struct
{
  struct __locale_data *__locales[13];
  const unsigned short int *__ctype_b;
  const char *__names[13];
};
typedef struct __locale_struct *__locale_t;
typedef __locale_t locale_t;

extern int strcoll_l (const char *__s1, const char *__s2, locale_t __l)
     __THROW __attribute_pure__ __nonnull ((1, 2, 3));

/* Duplicate S, returning an identical malloc'd string.  */
#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2)
extern char *strdup (const char *__s)
     __THROW __nonnull ((1)) __wur;
extern char *strndup (const char *__string, size_t __n)
     __THROW __nonnull ((1)) __wur;
#endif

/* Find the first occurrence of C in S.  */
extern char *strchr (const char *__s, int __c)
     __THROW __attribute_pure__ __nonnull ((1));
extern char *strrchr (const char *__s, int __c)
     __THROW __attribute_pure__ __nonnull ((1));

/* Return the length of S.  */
extern size_t strlen (const char *__s)
     __THROW __attribute_pure__ __nonnull ((1));

/* Return a string describing the meaning of the errno code ERRNUM.  */
extern char *strerror (int __errnum) __THROW;

__END_DECLS

#endif /* strdecls.h */
