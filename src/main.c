#include <stdio.h>

#include "hello.h"
#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "parser.h"
#include "treeDump.h"

static void dumpFile(AstFile *file, const char* dumpFile) {
  remove(dumpFile);
  FILE* toDump = fopen(dumpFile, "w");
  dumpAstFile(toDump, file);
  fclose(toDump);
}

static void processInputFile(const char* inputFile, const char *dumpFileName, Boolean verbose) {

    FILE* opened = fopen(inputFile, "r");
    AstFile* firstFile = NULL;
    if (opened != NULL) {
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
  Boolean verbose = TRUE;
  for (i = 0; i < argc; ++i) {
    const char *arg = argv[i];
    if (strcmp("-astDump", arg) == 0) {
      dumpFileName = argv[++i];
    } else if (strcmp("-oneline", arg) == 0) {
      verbose = FALSE;
    } else {
      const char* inputFile = argv[i];
      processInputFile(arg, dumpFileName, verbose);
    }
  }
  return 0;
}
