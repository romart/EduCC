#include <stdio.h>

#include "hello.h"
#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "parser.h"
#include "treeDump.h"

static void dumpFile(AstFile *file) {
//  char tmpb[1024] = { 0 };
  const char *dumpFile = "ast.dump";
//  sprintf(tmpb, "ast.dump", file->fileName);
  printf("dump ast into %s\n", dumpFile);
  remove(dumpFile);
  FILE* toDump = fopen(dumpFile, "w");
  dumpAstFile(toDump, file);
  fclose(toDump);
}

static void processInputFile(const char* inputFile) {

    FILE* opened = fopen(inputFile, "r");
    AstFile* firstFile = NULL;
    if (opened != NULL) {
      printf("Scanning file %s...\n", inputFile);
      AstFile *f = parseFile(opened, inputFile);
      dumpFile(f);
      f->next = firstFile;
      firstFile = f;
      fclose(opened);
    } else {
      printf("Cannot open file %s\n", inputFile);
    }
}


int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  int i;
  for (i = 0; i < argc; ++i) {
    const char* inputFile = argv[i];
    processInputFile(inputFile);
  }
  return 0;
}
