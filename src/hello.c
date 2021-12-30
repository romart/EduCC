
#include <stdio.h>
#include "hello.h"
#include "lex.h"


int foo(int x) {
  yyin = fopen("test.txt", "r");
  yylex();
//  printf("%d %d %d %d\n", x, x * x, num_lines, num_chars);
  return 0;
}
