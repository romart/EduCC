extern int printf(const char *, ...);

enum fred { a, b, c };

int main()
{
   printf("a=%d\n", a);
   printf("b=%d\n", b);
   printf("c=%d\n", c);

   enum fred d;

   typedef enum { e, f, g } h;
   typedef enum { i, j, k } m;

   printf("e=%d\n", e);
   printf("f=%d\n", f);
   printf("g=%d\n", g);

   printf("i=%d\n", i);
   printf("j=%d\n", j);
   printf("k=%d\n", k);

   return 0;
}