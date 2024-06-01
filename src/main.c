#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/wait.h>
#include <glob.h>
#include <assert.h>


#include "parser.h"

extern char *strdup(const char *s);
extern char *mkdtemp (char *__template);

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

static void runProcess(int argc, char **argv, int verbose) {
  if (verbose) {
      fprintf(stdout, "%s", argv[0]);
      int i = 1;
      for (; i < argc; ++i) {
          fprintf(stdout, " %s", argv[i]);
      }

      fprintf(stdout, "\n");
  }


  if (fork() == 0) {
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  // Wait for the child process to finish.
  int status;
  while (wait(&status) > 0);
  if (status != 0) {
//    fprintf(stderr, "process failed (%d): %s: errno(%d) - %s\n", status, argv[0], errno, strerror(errno));
    exit(1);
  }
}

static const char *libPath() {
  if (access("/usr/lib/x86_64-linux-gnu/crti.o", F_OK) == 0) {
      return "/usr/lib/x86_64-linux-gnu";
  }
  if (access("/usr/lib64/crti.o", F_OK) == 0) {
      return "/usr/lib64";
  }

  unreachable("No library path found");
}

static char *find_file(char *pattern) {
  char *path = NULL;
  glob_t buf = {};
  glob(pattern, 0, NULL, &buf);
  if (buf.gl_pathc > 0)
    path = strdup(buf.gl_pathv[buf.gl_pathc - 1]);
  globfree(&buf);
  return path;
}

static void gccLibPath(char *buffer) {
  char *paths[] = {
     "/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
     "/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o", // For Gentoo
     "/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o", // For Fedora
   };

   int i = 0;
   for (; i < sizeof(paths) / sizeof(*paths); i++) {
     char *path = find_file(paths[i]);
     if (path) {
       unsigned len = strlen(path);
       static const char suffix[] = "/crtbegin.o";
       unsigned dirLen = len - (sizeof suffix - 1);
       memcpy(buffer, path, dirLen);
       free(path);
       return;
     }
   }

   unreachable("No gcc library path found");
}

static void runLinker(const char *outputFile, StringList *compiledObjs, StringList *cliObjs, StringList *libs, StringList *libDirs) {
  unsigned argc = 1;

  // -o <output>
  argc += 2;

  // -m elf_x86_64
  argc += 2;

  // -z noexecstack
  argc += 2;

  // -dynamic-linker /lib64/ld-linux-x86-64.so.2
  argc += 2;

  // crt1.o, crti.o, crtbegin.o
  argc += 3;

  char gccLPath[256] = { 0 };

  const char *lPath = libPath();
  gccLibPath(gccLPath);

  const char *stdLibPaths[] = {
    gccLPath,
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib64",
    "/lib64",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib/x86_64-pc-linux-gnu",
    "/usr/lib/x86_64-redhat-linux",
    "/usr/lib",
    "/lib"
  };

  StringList *n;

  argc += sizeof stdLibPaths / sizeof stdLibPaths[0];

  for (n = libDirs; n; n = n->next) {
      ++argc;
  }

  for (n = compiledObjs; n; n = n->next) {
      ++argc;
  }

  for (n = cliObjs; n; n = n->next) {
      ++argc;
  }

  ++argc; // -lc

  for (n = libs; n; n = n->next) {
      ++argc;
  }

  argc += 5;

  argc += 2;

  char **argv = heapAllocate(sizeof(char *) * (argc + 1));

  char buffer[512]= { 0 };

  unsigned i = 0;
  argv[i++] = strdup("ld");

  argv[i++] = strdup("-o");
  argv[i++] = strdup(outputFile);

  argv[i++] = strdup("-m");
  argv[i++] = strdup("elf_x86_64");

  argv[i++] = strdup("-z");
  argv[i++] = strdup("noexecstack");

  argv[i++] = strdup("-dynamic-linker");
  argv[i++] = strdup("/lib64/ld-linux-x86-64.so.2");

  sprintf(buffer, "%s/crt1.o", lPath);
  argv[i++] = strdup(buffer);
  sprintf(buffer, "%s/crti.o", lPath);
  argv[i++] = strdup(buffer);
  sprintf(buffer, "%s/crtbegin.o", gccLPath);
  argv[i++] = strdup(buffer);

  unsigned j = 0;

  for (; j < sizeof(stdLibPaths) / sizeof(stdLibPaths[0]); ++j) {
      const char *p = stdLibPaths[j];
      unsigned len = strlen(p) + 3;
      char *arg = heapAllocate(len);
      sprintf(arg, "-L%s", p);
      argv[i++] = arg;
  }

  for (n = libDirs; n; n = n->next) {
      unsigned len = strlen(n->s) + 3;
      char *arg = heapAllocate(len);
      sprintf(arg, "-L%s", n->s);
      argv[i++] = arg;
  }

  for (n = compiledObjs; n; n = n->next) {
      argv[i++] = (char*)n->s;
  }

  for (n = cliObjs; n; n = n->next) {
      argv[i++] = strdup(n->s);
  }

  argv[i++] = strdup("-lc");

  for (n = libs; n; n = n->next) {
      unsigned len = strlen(n->s) + 3;
      char *arg = heapAllocate(len);
      sprintf(arg, "-l%s", n->s);
      argv[i++] = arg;
  }

  argv[i++] = strdup("-lc");
  argv[i++] = strdup("-lgcc");
  argv[i++] = strdup("--as-needed");
  argv[i++] = strdup("-lgcc_s");
  argv[i++] = strdup("--no-as-needed");

  sprintf(buffer, "%s/crtend.o", gccLPath);
  argv[i++] = strdup(buffer);
  sprintf(buffer, "%s/crtn.o", lPath);
  argv[i++] = strdup(buffer);

  assert(i == argc);

  runProcess(argc, argv, 0);

  for (i = 0; i < argc; ++i) {
      releaseHeap(argv[i]);
  }

  releaseHeap(argv);
}

static StringList* compileFiles(StringList *files, Configuration *config, const char *tmpDir) {
  StringList coHead = { 0 }, *coCur = &coHead;

  const char *outputFile = config->outputFile;

  while (files) {
    if (tmpDir) {
        const char *fileName = files->s;
        unsigned l = strlen(fileName);

        int j;
        for (j = l - 1; j >= 0; --j) {
            if (fileName[j] == '/') break;
        }
        if (j) ++j;
        unsigned l1 = strlen(tmpDir);
        unsigned l2 = (l - j);
        unsigned len = l1 + 1 + l2 + 1;
        char *b = heapAllocate(len);

        sprintf(b, "%s/%s", tmpDir, &fileName[j]);
        b[len - 2] = 'o';
        config->outputFile = b;
        coCur = coCur->next = newStringNode(b);
    }
    void *m = files;
    config->fileToCompile = files->s;
    compileFile(config);
    files = files->next;
    releaseHeap(m);
  }

  config->outputFile = outputFile;

  return coHead.next;
}

int main(int argc, char** argv) {
  if (argc < 2) return -1;
  argc--; argv++;
  unsigned i;
  Configuration config = { 0 };

  config.includePath = allocIncludePath("/usr/include", NULL);
  config.includePath = allocIncludePath("/usr/local/include", config.includePath);
  config.includePath = allocIncludePath("sdk/include", config.includePath);
  config.includePath = allocIncludePath("/usr/include/x86_64-linux-gnu", config.includePath);

  config.verbose = 1;
  config.arch = X86_64;

  StringList chead = { 0 }, *ccur = &chead;
  StringList ohead = { 0 }, *ocur = &ohead;
  StringList mhead = { 0 }, *mcur = &mhead;
  StringList lhead = { 0 }, *lcur = &lhead;
  StringList lDirHead = { 0 }, *lDirCur = &lDirHead;

  unsigned inputCountC = 0;
  unsigned inputCountO = 0;
  unsigned libCount = 0;
  unsigned libDirCount = 0;

  for (i = 0; i < argc; ++i) {
    const char *arg = argv[i];
    if (strcmp("-astDump", arg) == 0) {
        unsigned idx = ++i;
        if (idx < argc) {
          config.dumpFileName = argv[idx];
        } else {
          fprintf(stderr, "file name expected after '-astDump' option");
          return 2;
        }
    } else if (strcmp("-astCanonDump", arg) == 0) {
        unsigned idx = ++i;
        if (idx < argc) {
          config.canonDumpFileName = argv[idx];
        } else {
          fprintf(stderr, "file name expected after '-astCanonDump' option");
          return 2;
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
        config.objOutput = 1;
        continue;
    } else if (strcmp("-o", arg) == 0) {
        config.outputFile = argv[++i];
    } else if (strcmp("-march", arg) == 0) {
        const char *march = argv[++i];
        if (strcmp(march, "x86_64") == 0) {
            config.arch = X86_64;
        } else if (strcmp(march, "riscv64") == 0) {
            config.arch = RISCV64;
        } else {
            fprintf(stderr, "Unknown march kind '%s'", march);
            return 2;
        }
    } else if (strcmp("-help", arg) == 0) {
        // TODO: add help & version options
        continue;
    } else if (strcmp("-g", arg) == 0) {
        // we do not generate debug info yet
        continue;
    } else if (strcmp("-w", arg) == 0) {
        // ignore
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
    } else if (strncmp("-l", arg, 2) == 0) {
        const char *lib = NULL;
        if (arg[2]) {
            lib = &arg[2];
        } else if (i + 1 < argc) {
            lib = argv[++i];
        } else {
            fprintf(stderr, "error: missing argument to ‘-l’\n");
            return 2;
        }
        ++libCount;
        lcur = lcur->next = newStringNode(lib);
    } else if (strncmp("-L", arg, 2) == 0) {
        const char *lib = NULL;
        if (arg[2]) {
            lib = &arg[2];
        } else if (i + 1 < argc) {
            lib = argv[++i];
        } else {
            fprintf(stderr, "error: missing argument to ‘-L’\n");
            return 2;
        }
        ++libDirCount;
        lDirCur = lDirCur->next = newStringNode(lib);
    } else {
        unsigned l = strlen(arg);
        if (l > 2) {
           if (arg[l - 2] == '.') {
               if (arg[l - 1] == 'c') {
                  ccur = ccur->next = newStringNode(arg);
                  ++inputCountC;
                  continue;
               } else if (arg[l - 1] == 'o' || arg[l - 1] == 'a') {
                  ++inputCountO;
                  ocur = ocur->next = newStringNode(arg);
                  continue;
               }
           }
        }

        fprintf(stderr, "unknown file format '%s'\n", arg);
        return 2;
    }
  }

  if (config.objOutput && inputCountO > 0) {
      StringList *n = ohead.next;
      for (;n; n = n->next) {
          fprintf(stderr, "warning: %s: linker input file unused because linking not done\n", n->s);
      }
  }

  if (config.outputFile && inputCountC > 1 && (config.objOutput || config.ppOutput)) {
      fprintf(stderr, "fatal error: cannot specify ‘-o’ with ‘-c’ or ‘-E’ with multiple files\n");
      return 2;
  }

  char *tmpDir = NULL;
  char template[] = "/tmp/tmpdir.XXXXXX";
  if (!(config.objOutput || config.ppOutput)) {
      tmpDir = mkdtemp(template);
  }

  config.macroses = mhead.next;

  StringList *compiledObjFiles = compileFiles(chead.next, &config, tmpDir);

  if (config.skipCodegen) return 0;

  if (!config.objOutput && !config.ppOutput) {
    runLinker(config.outputFile ? config.outputFile : "a.out", compiledObjFiles, ohead.next, lhead.next, lDirHead.next);
  }

  if (tmpDir) {
      rmdir(tmpDir);
  }

  return 0;
}
