#include <string.h>

#include "parser.h"


int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  unsigned i;
  Configuration config = { 0 };
  config.verbose = 1;
  for (i = 0; i < argc; ++i) {
    const char *arg = argv[i];
    if (strcmp("-astDump", arg) == 0) {
      config.dumpFileName = argv[++i];
    } else if (strcmp("-astCanonDump", arg) == 0) {
      config.canonDumpFileName = argv[++i];
    } else if (strcmp("-oneline", arg) == 0) {
      config.verbose = 0;
    } else if (strcmp("-memstat", arg) == 0) {
      config.memoryStatistics = 1;
    } else if (strcmp("-logtokens", arg) == 0) {
      config.logTokens = 1;
    } else if (strcmp("-skipCodegen", arg) == 0) {
      config.skipCodegen = 1;
    } else {
      config.fileToCompile = argv[i];
      compileFile(&config);
    }
  }
  return 0;
}
