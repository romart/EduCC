# define __USE_FORTIFY_LEVEL 0
# define __GNUC_PREREQ(maj, min) 0
# define __glibc_clang_prereq(maj, min) 0
#if __USE_FORTIFY_LEVEL == 3 && (__glibc_clang_prereq (9, 0)		      \
                                 || __GNUC_PREREQ (12, 0))

NOT OK

#else

OK

#endif
