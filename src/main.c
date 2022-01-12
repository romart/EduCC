#include <stdio.h>
#include "hello.h"
#include "lex.h"
#include "tokens.h"

int num_lines = 0;
int num_chars = 0;


static void processFileTokens() {
      int token = -1;
      while (token = yylex()) {
        printf("Token %s:#%d (at line %d), its text '%s'\n", tokenName(token), token, num_lines + 1, yytext);
      }
      printf("File ended (%d lines, %d chars).\n", num_lines, num_chars); 
}

static void processInputFile(const char* inputFile) {

    FILE* opened = fopen(inputFile, "r");
    if (opened != NULL) {
      yyin = opened;
      printf("Scanning file %s...\n", inputFile);
      processFileTokens();
      // yylex();
      // printf("File %s contains %d lines and %d chars\n", inputFile, num_lines, num_chars);
      fclose(opened);
    } else {
      printf("Cannot open file %s\n", inputFile);
    }
}


int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  for (int i = 0; i < argc; ++i) {
    num_lines = num_chars = 0;
    const char* inputFile = argv[i];
    processInputFile(inputFile);
  }
  return 0;
}
