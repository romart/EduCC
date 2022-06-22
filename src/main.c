#include <string.h>

#include "parser.h"


static IncludePath *allocIncludePath(const char *path, IncludePath *next) {
  IncludePath *ip = heapAllocate(sizeof(IncludePath));
  ip->path = path;
  ip->next = next;
  return ip;
}

static StringList *newStringNode(const char *s) {
  StringList *fl = heapAllocate(sizeof(StringList));
  fl->s = s;
  return fl;
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

  StringList fhead = { 0 }, *fcur = &fhead;
  StringList mhead = { 0 }, *mcur = &mhead;

  unsigned inputCount = 0;

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
    } else if (strcmp("-c", arg) == 0) {
        // we do not support any other compilation beside .o file
        continue;
    } else if (strcmp("-o", arg) == 0) {
        config.outputFile = argv[++i];
    } else if (strcmp("-g", arg) == 0) {
        // we do not generate debug info yet
        continue;
    } else if (strncmp("-std", arg, 4) == 0) {
        // we only support strict ansi c for now
        continue;
    } else if (strncmp("-O", arg, 2) == 0) {
        // optimization? lol
        continue;
    } else if (strncmp("-f", arg, 2) == 0) {
        // we do not support any extra feature yet
        // it's default
    } else if (strncmp("-W", arg, 2) == 0) {
        // ignore
        // it's default
    } else if (strncmp("-M", arg, 2) == 0) {
        // ignore
        // it's default
    } else if (strncmp("-D", arg, 2) == 0) {
        const char *macro = NULL;
        if (arg[2]) {
            macro = &arg[2];
        } else if (i + 1 < argc) {
            macro = argv[++i];
        } else {
            fprintf(stderr, "error: macro name missing after ‘-D’\n");
            return 2;
        }
        // define macro
        mcur = mcur->next = newStringNode(macro);
    } else {
        inputCount++;
        fcur = fcur->next = newStringNode(argv[i]);
    }
  }

  if (config.outputFile && inputCount > 1) {
      fprintf(stderr, "error: cannot specify ‘-o’ with multiple files\n");
      return 1;
  }

  config.macroses = mhead.next;
  fcur = fhead.next;

  while (fcur) {
    void *m = fcur;
    config.fileToCompile = fcur->s;
    fcur = fcur->next;
    releaseHeap(m);
    compileFile(&config);
  }

  return 0;
}
