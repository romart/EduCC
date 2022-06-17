# define STACK_OF(type) struct stack_st_##type

# define SKM_DEFINE_STACK_OF(t1) STACK_OF(t1); int sk_##t1##_num

SKM_DEFINE_STACK_OF(OPENSSL_STRING)
