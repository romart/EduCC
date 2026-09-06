// 6.10.1p4: '#if' arithmetic is done in intmax_t/uintmax_t, so '~0U' is
// sixty-four bits of ones rather than thirty-two. libpng's png.h guards
// PNG_TRANSFORM_SCALE_16 with '#if ~0U > 0xffffU'.

#if ~0U > 0xffffU
int wide = 1;
#endif

#if ~0U == -1
int tilde_is_minus_one = 1;
#endif

#if (0U - 1U) == -1
int wrap_is_minus_one = 1;
#endif

// A comparison's own type is int, so its signedness comes from the operands.
#if -1 > 0xffffU
int promoted = 1;
#endif

#if 0xffffffffU > 0
int plain = 1;
#endif

#if -1 < 1
int still_signed = 1;
#endif
