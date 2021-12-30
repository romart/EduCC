#include <stdio.h>


void yyerror(const char *msg) {
  printf("%s\n", msg);
}

int yywrap(){
  return 1;
}
