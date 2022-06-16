void foo(const char *, ...);

int test1()
{
   for (int Count = 1; Count <= 10; Count++)
   {
      foo("%d\n", Count);
   }

   return 0;
}


int test1(int a)
{
   for (int a = 1; a <= 10; a++)
   {
      foo("%d\n", a);
   }

   return 0;
}
