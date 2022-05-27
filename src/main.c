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
        unsigned idx = ++i;
        if (idx < argc) {
          config.dumpFileName = argv[idx];
        } else {
          fprintf(stderr, "file name expected after '-astDump' option");
        }
    } else if (strcmp("-astCanonDump", arg) == 0) {
        unsigned idx = ++i;
        if (idx < argc) {
          config.canonDumpFileName = argv[idx];
        } else {
          fprintf(stderr, "file name expected after '-astCanonDump' option");
        }
    } else if (strcmp("-objDir", arg) == 0) {
        unsigned idx = ++i;
        if (idx < argc) {
          config.objDirName = argv[idx];
        } else {
          fprintf(stderr, "directory name expected after '-objDir' option");
        }
    } else if (strcmp("-oneline", arg) == 0) {
      config.verbose = 0;
    } else if (strcmp("-memstat", arg) == 0) {
      config.memoryStatistics = 1;
    } else if (strcmp("-logtokens", arg) == 0) {
      config.logTokens = 1;
    } else if (strcmp("-skipCodegen", arg) == 0) {
      config.skipCodegen = 1;
    } else if (strcmp("-E", arg) == 0) {
      config.ppOutput = 1;
    } else if (strncmp("-I", arg, 2) == 0) {
        IncludePath *ip = heapAllocate(sizeof(IncludePath));
        if (arg[2]) {
          ip->path = &arg[2];
        } else {
          ip->path = argv[++i];
        }
        ip->next = config.includePath;
        config.includePath = ip;
    } else {
      config.fileToCompile = argv[i];
      compileFile(&config);
    }
  }
  return 0;
}
