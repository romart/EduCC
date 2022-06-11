


#define X

#ifdef X

#ifdef Y
NOT OK
#else

#ifndef U
OK
#endif

OK
#endif

OK

#elif defined Y

NOT OK

#else xxx

NOT OK

#endif


