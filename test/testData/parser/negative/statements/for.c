void foo(const char *, ...);

int test1()
{
   for (static int Count = 1; Count <= 10; Count++);

   return 0;
}


int test2()
{
   for (extern int Count = 1; Count <= 10; Count++);

   return 0;
}


int test3()
{
   for (typedef int Count; Count <= 10; Count++);

   return 0;
}
