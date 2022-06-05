#include <string.h>

#include "parser.h"


static IncludePath *allocIncludePath(const char *path, IncludePath *next) {
  IncludePath *ip = heapAllocate(sizeof(IncludePath));
  ip->path = path;
  ip->next = next;
  return ip;
}

int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  unsigned i;
  Configuration config = { 0 };

  config.includePath = allocIncludePath("/usr/include", NULL);
  config.includePath = allocIncludePath("/usr/local/include", config.includePath);
  config.includePath = allocIncludePath("sdk/include", config.includePath);

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
      config.includePath = allocIncludePath(arg[2] ? &arg[2] : argv[++i], config.includePath);
    } else if (strcmp("-S", arg) == 0) {
      config.asmDump = 1;
    } else {
      config.fileToCompile = argv[i];
      compileFile(&config);
    }
  }
  return 0;
}
