extern int printf(const char *, ...);

int main()
{
   int Count;

   for (Count = 1; Count <= 10; Count++)
   {
      printf("%d\n", Count);
   }

   return 0;
}
