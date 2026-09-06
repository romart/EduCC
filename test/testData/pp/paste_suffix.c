// '##' pastes the last token of the left argument with the first of the right;
// everything before it stays. The whole left sequence was being replaced by the
// pasted token, so mbedtls' ripemd160.c got 'Ap' where it wrote 'local.A ## p'.

#define SUFFIX(a) a ## p
#define JOIN(a, b) a ## b

int f(struct S *local) { return SUFFIX(local->A); }
int g(struct S local) { return SUFFIX(local.A); }
int h(void) { return JOIN(x + y, z); }
int i(void) { return SUFFIX(A); }
