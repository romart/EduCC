

#include <stdio.h>

int fred(int p)
{
   printf("yo %d\n", p);
   return 42;
}


/* To test what this is supposed to test the destination function
   (fprint here) must not be called directly anywhere in the test.  */

int main()
{
   int (*f)(int) = &fred;
   int (*fprintfptr)(FILE *, const char *, ...) = &fprintf;
   fprintfptr(stdout, "%d\n", (*f)(24));

   return 0;
}
