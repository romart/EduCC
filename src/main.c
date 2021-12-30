#include <stdio.h>
#include "hello.h"
#include "lex.h"

int num_lines = 0;
int num_chars = 0;

int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  for (int i = 0; i < argc; ++i) {
    num_lines = num_chars = 0;
    const char* inputFile = argv[i];
    yyin = fopen(inputFile, "r");
    yylex();
    printf("File %s contains %d lines and %d chars\n", inputFile, num_lines, num_chars);
  }
  return ;
}
