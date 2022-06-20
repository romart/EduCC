#include <stdio.h>

static const char tcc_keywords[] =
 "int" "\0"
 "void" "\0"
 "char" "\0"
 "if" "\0" "\0";



int test() {
  const char *p = tcc_keywords;
  const char *r;
  char c;
  while (*p) {
      r = p;
      for(;;) {
          c = *r++;
          if (c == '\0')
              break;
      }
      printf("%s\n", p);
      p = r;
  }
}



int main() {
  test();
}
