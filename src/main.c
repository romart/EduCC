#include <stdio.h>

#include "hello.h"
#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "parser.h"
#include "treeDump.h"

static void dumpFile(AstFile *file, const char* dumpFile) {
//  char tmpb[1024] = { 0 };
//  sprintf(tmpb, "ast.dump", file->fileName);
  printf("dump ast into %s\n", dumpFile);
  remove(dumpFile);
  FILE* toDump = fopen(dumpFile, "w");
  dumpAstFile(toDump, file);
  fclose(toDump);
}

static void processInputFile(const char* inputFile, const char *dumpFileName) {

    FILE* opened = fopen(inputFile, "r");
    AstFile* firstFile = NULL;
    if (opened != NULL) {
      printf("Scanning file %s...\n", inputFile);
      AstFile *f = parseFile(opened, inputFile);
      if (dumpFileName) {
        dumpFile(f, dumpFileName);
      }
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
  const char *dumpFileName = NULL;
  for (i = 0; i < argc; ++i) {
    const char *arg = argv[i];
    if (strcmp("-astDump", arg) == 0) {
      dumpFileName = argv[++i];
    } else{
      const char* inputFile = argv[i];
      processInputFile(inputFile, dumpFileName);
    }
  }
  return 0;
}
