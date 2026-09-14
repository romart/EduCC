#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <glob.h>
#include <assert.h>


#include "parser.h"
#include "utils.h"
#include "link/elfread.h"
#include "link/link.h"

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

// Everything else on the default include path belongs to the system, but the
// stddef.h/stdarg.h shims under sdk/include ship with this binary, so they are
// looked up relative to the binary rather than to whatever directory it
// happens to be invoked from. Returns NULL if none of the candidates exist.
static const char *sdkIncludePath() {
  const char *env = getenv("EDUCC_SDK_DIR");
  if (env) return env;

  char prefix[512] = { 0 };
  int len = readlink("/proc/self/exe", prefix, sizeof prefix - 1);

  if (len > 0) {
    prefix[len] = '\0';
    char *slash = strrchr(prefix, '/');
    if (slash) {
      *slash = '\0';                       // strip the binary, leaving bin/
      slash = strrchr(prefix, '/');
    }

    if (slash) {
      *slash = '\0';                       // strip bin/, leaving the prefix

      // An installed tree, addressed relative to the prefix above bin/ so it
      // survives being moved or unpacked somewhere else.
      static const char *installed[] = {
        "/lib/educc/include",
        "/lib64/educc/include",
        "/share/educc/include",
      };

      char buffer[1024] = { 0 };
      unsigned i = 0;
      for (; i < sizeof installed / sizeof installed[0]; ++i) {
        snprintf(buffer, sizeof buffer, "%s%s/stddef.h", prefix, installed[i]);
        if (access(buffer, F_OK) == 0) {
          snprintf(buffer, sizeof buffer, "%s%s", prefix, installed[i]);
          return strdup(buffer);
        }
      }
    }
  }

#ifdef EDUCC_BUILD_SDK_DIR
  // Built but never installed. CMake bakes in the absolute source location,
  // which is exact for a build directory at any depth, where guessing a
  // relative hop up from bin/ is not - selfhost.sh builds two levels down.
  return EDUCC_BUILD_SDK_DIR;
#else
  return NULL;
#endif
}

// Where the CRT objects live. Probed in src/link/, because the in-house
// linker needs the same answer and two probes that could disagree is one
// more than this driver has any use for.
static const char *stdLibPaths[] = {
  "/usr/lib/x86_64-linux-gnu",
  "/usr/lib64",
  "/lib64",
  "/usr/lib/x86_64-pc-linux-gnu",
  "/usr/lib/x86_64-redhat-linux",
  "/usr/lib",
  "/lib"
};

#define DYNAMIC_LINKER "/lib64/ld-linux-x86-64.so.2"

// The link, in this process. The input list is the one ld used to be given,
// in the same order and with the same CRT objects around it, except that the
// objects this run compiled are handed over as bytes: nothing wrote them down,
// so there is nothing to name.
static Boolean linkInternal(const char *outputFile, Boolean shared,
                            ObjectImage *compiledObjs, StringList *cliObjs,
                            StringList *libs, StringList *libDirs,
                            StringList *ldArgs) {
  LinkOptions options;
  initLinkOptions(&options);
  options.outputFile = outputFile;
  options.shared = shared;
  if (!shared) options.interpreter = DYNAMIC_LINKER;

  StringList *n;
  unsigned count = 0;
  Boolean asNeeded = FALSE;

  for (n = ldArgs; n; n = n->next) {
    const char *arg = n->s;

    if (strcmp(arg, "-s") == 0) {
      options.stripSymbols = TRUE;
    } else if (strcmp(arg, "-rpath") == 0 || strcmp(arg, "--rpath") == 0) {
      if (n->next == NULL) {
        fprintf(stderr, "error: missing argument to ‘%s’\n", arg);
        return FALSE;
      }
      options.runpath = (n = n->next)->s;
    } else if (strncmp(arg, "-rpath=", 7) == 0) {
      options.runpath = &arg[7];
    } else if (strncmp(arg, "--rpath=", 8) == 0) {
      options.runpath = &arg[8];
    } else if (strcmp(arg, "-soname") == 0 || strcmp(arg, "--soname") == 0 ||
               strcmp(arg, "-h") == 0) {
      if (n->next == NULL) {
        fprintf(stderr, "error: missing argument to ‘%s’\n", arg);
        return FALSE;
      }
      options.soname = (n = n->next)->s;
    } else if (strncmp(arg, "-soname=", 8) == 0) {
      options.soname = &arg[8];
    } else if (strncmp(arg, "--soname=", 9) == 0) {
      options.soname = &arg[9];
    } else if (strcmp(arg, "-e") == 0 || strcmp(arg, "--entry") == 0) {
      if (n->next == NULL) {
        fprintf(stderr, "error: missing argument to ‘%s’\n", arg);
        return FALSE;
      }
      options.entrySymbol = (n = n->next)->s;
    } else if (strcmp(arg, "--as-needed") == 0 ||
               strcmp(arg, "--no-as-needed") == 0) {
      // Where it sits among the '-l's is lost by the time they reach here, so
      // it says what the libraries this command line names are, all of them.
      asNeeded = arg[2] == 'a';
    } else if (strcmp(arg, "-z") == 0 && n->next != NULL) {
      // '-z noexecstack' and its relatives describe a layout this linker
      // already writes; none of them can ask for a different one.
      n = n->next;
    } else {
      fprintf(stderr, "error: ‘%s’ is not understood by the linker\n", arg);
      return FALSE;
    }
  }

  ObjectImage *o;
  for (o = compiledObjs; o; o = o->next) ++count;
  for (n = cliObjs; n; n = n->next) ++count;
  for (n = libs; n; n = n->next) ++count;
  count += 13; // the CRT objects, the implicit libraries, the as-needed pairs

  LinkInput *inputs = heapAllocate(sizeof(LinkInput) * count);
  unsigned at = 0;
  char buffer[512];

  const char *crtPath = linkCrtPath();
  const char *gccPath = linkGccPath();

#define PATH(p)                                                                \
  do {                                                                         \
    inputs[at].name = (p);                                                     \
    inputs[at].image = NULL;                                                   \
    inputs[at++].size = 0;                                                     \
  } while (0)

#define INPUT(fmt, ...)                                                        \
  do {                                                                         \
    snprintf(buffer, sizeof buffer, fmt, __VA_ARGS__);                         \
    PATH(strdup(buffer));                                                      \
  } while (0)

  // crt1.o holds _start, which a library must not carry a second copy of, and
  // the 'S' pair is the position-independent crtbegin/crtend.
  if (!shared) INPUT("%s/crt1.o", crtPath);
  INPUT("%s/crti.o", crtPath);
  INPUT("%s/crtbegin%s.o", gccPath, shared ? "S" : "");

  for (o = compiledObjs; o; o = o->next) {
    inputs[at].name = o->sourceFile;
    inputs[at].image = o->image;
    inputs[at++].size = o->size;
  }
  for (n = cliObjs; n; n = n->next) PATH(strdup(n->s));

  PATH(strdup("-lc"));
  if (asNeeded) PATH(strdup("--as-needed"));
  for (n = libs; n; n = n->next) INPUT("-l%s", n->s);
  if (asNeeded) PATH(strdup("--no-as-needed"));
  PATH(strdup("-lc"));
  PATH(strdup("-lgcc"));
  PATH(strdup("--as-needed"));
  PATH(strdup("-lgcc_s"));
  PATH(strdup("--no-as-needed"));

  INPUT("%s/crtend%s.o", gccPath, shared ? "S" : "");
  INPUT("%s/crtn.o", crtPath);

#undef INPUT
#undef PATH

  assert(at <= count);

  unsigned dirCount = 0;
  for (n = libDirs; n; n = n->next) ++dirCount;
  dirCount += sizeof stdLibPaths / sizeof stdLibPaths[0] + 1;

  char **dirs = heapAllocate(sizeof(char *) * dirCount);
  unsigned dirAt = 0;

  // A directory the user named comes first, which is what makes a program link
  // against the library beside it rather than the one the system ships.
  for (n = libDirs; n; n = n->next) dirs[dirAt++] = (char *)n->s;
  dirs[dirAt++] = (char *)gccPath;

  unsigned k;
  for (k = 0; k < sizeof stdLibPaths / sizeof stdLibPaths[0]; ++k) {
    dirs[dirAt++] = (char *)stdLibPaths[k];
  }

  Boolean ok = linkObjects(&options, (int)at, inputs, (int)dirAt, dirs);

  // Every path was strdup'd; an in-memory object's name belongs to the AST.
  for (k = 0; k < at; ++k) {
    if (inputs[k].image == NULL) releaseHeap((void *)inputs[k].name);
  }
  releaseHeap(inputs);
  releaseHeap(dirs);

  return ok;
}

// Each object either lands on disk under its own name ('-c') or is collected
// in config->objects for the link below; '-o' names the final output in the
// second case, so it is not touched here.
static void compileFiles(StringList *files, Configuration *config) {
  while (files) {
    void *m = files;
    config->fileToCompile = files->s;
    compileFile(config);
    files = files->next;
    releaseHeap(m);
  }
}

// Parses a comma-separated phase list from '-irDump:phase[,phase...]' (spec
// points right after the ':') into a bitmask of enum IrDumpPhase. Returns 0
// (an empty/invalid mask, since no defined phase is bit value 0) if spec is
// empty or contains an unknown phase name.
static unsigned parseIrDumpPhases(const char *spec) {
  static const struct { const char *name; enum IrDumpPhase phase; } knownPhases[] = {
    { "initial", IR_DUMP_PHASE_INITIAL },
    { "ssa",     IR_DUMP_PHASE_SSA },
    { "scp",     IR_DUMP_PHASE_SCP },
    { "gvn",     IR_DUMP_PHASE_GVN },
    { "dce",     IR_DUMP_PHASE_DCE },
    { "mir",     IR_DUMP_PHASE_MIR },
    { "isel",    IR_DUMP_PHASE_ISEL },
    { "ra",      IR_DUMP_PHASE_RA },
  };

  unsigned mask = 0;
  const char *tokenStart = spec;

  for (;;) {
    const char *tokenEnd = strchr(tokenStart, ',');
    size_t tokenLen = tokenEnd ? (size_t)(tokenEnd - tokenStart) : strlen(tokenStart);

    unsigned matched = 0;
    for (size_t i = 0; i < sizeof(knownPhases) / sizeof(knownPhases[0]); ++i) {
      if (strlen(knownPhases[i].name) == tokenLen && strncmp(knownPhases[i].name, tokenStart, tokenLen) == 0) {
        mask |= knownPhases[i].phase;
        matched = 1;
        break;
      }
    }
    if (!matched) return 0;

    if (!tokenEnd) break;
    tokenStart = tokenEnd + 1;
  }

  return mask;
}

int main(int argc, char** argv) {
  if (argc < 2) return -1;

  // Reads its remaining arguments as link inputs and prints what the reader
  // made of each. Before the flag parser, because those arguments are paths of
  // any shape - a '.so' is not something the driver otherwise accepts.
  if (strcmp(argv[1], "-Xlink-read") == 0) {
      return dumpLinkInputs(argc - 2, argv + 2);
  }

  // Link exactly what is named, without the CRT objects the driver puts around
  // a compile. A mode of its own rather than something the driver reaches on
  // its way to a link, because its arguments are paths of shapes the flag
  // parser rejects - and because linking a bare object with its own '_start'
  // is what a test can assert the exit code of.
  if (strcmp(argv[1], "-Xlink-static") == 0) {
      return linkStaticCommand(argc - 2, argv + 2);
  }

  if (strcmp(argv[1], "-Xlink-dynamic") == 0) {
      return linkDynamicCommand(argc - 2, argv + 2);
  }

  argc--; argv++;
  unsigned i;
  Configuration config = { 0 };

  const char *sdkDir = sdkIncludePath();

  config.includePath = allocIncludePath("/usr/include", NULL);
  config.includePath = allocIncludePath("/usr/local/include", config.includePath);
  if (sdkDir) {
    config.includePath = allocIncludePath(sdkDir, config.includePath);
  }
  config.includePath = allocIncludePath("/usr/include/x86_64-linux-gnu", config.includePath);

  config.verbose = 1;
  config.arch = X86_64;
  config.irBackend = 1;
  config.regAlloc = RA_COLOUR;

  StringList chead = { 0 }, *ccur = &chead;
  StringList ohead = { 0 }, *ocur = &ohead;
  StringList mhead = { 0 }, *mcur = &mhead;
  StringList lhead = { 0 }, *lcur = &lhead;
  StringList lDirHead = { 0 }, *lDirCur = &lDirHead;
  StringList ldHead = { 0 }, *ldCur = &ldHead;

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
    } else if (strncmp("-irDump", arg, 7) == 0 && (arg[7] == '\0' || arg[7] == ':')) {
        if (arg[7] == ':') {
          config.irDumpPhases = parseIrDumpPhases(&arg[8]);
          if (config.irDumpPhases == 0) {
            fprintf(stderr, "unknown ir dump phase in '%s' (expected a comma-separated list of: initial, ssa, scp, gvn, dce, mir, isel, ra)\n", arg);
            return 2;
          }
        }
        unsigned idx = ++i;
        if (idx < argc) {
          config.irDumpFileName = argv[idx];
        } else {
          fprintf(stderr, "file name expected after '-irDump' option");
          return 2;
        }
    } else if (strcmp("-print-sdk-dir", arg) == 0) {
      // Which copy of the shipped headers this binary resolved to - the only
      // way to tell an installed tree from a build one without an strace.
      if (!sdkDir) {
        fprintf(stderr, "error: no SDK include directory found\n");
        return 1;
      }
      fprintf(stdout, "%s\n", sdkDir);
      return 0;
    } else if (strcmp("-oneline", arg) == 0) {
      config.verbose = 0;
    } else if (strcmp("-memstat", arg) == 0) {
      config.memoryStatistics = 1;
    } else if (strcmp("-logtokens", arg) == 0) {
      config.logTokens = 1;
    } else if (strcmp("-trace", arg) == 0) {
      traceEnabled = TRUE;
    } else if (strcmp("-skipCodegen", arg) == 0) {
      config.skipCodegen = 1;
    } else if (strcmp("-E", arg) == 0) {
      config.ppOutput = 1;
    } else if (strncmp("-I", arg, 2) == 0) {
      config.includePath = allocIncludePath(arg[2] ? &arg[2] : argv[++i], config.includePath);
    } else if (strcmp("-S", arg) == 0) {
      config.asmDump = 1;
    } else if (strcmp("-legacy", arg) == 0) {
      config.irBackend = 0;
    } else if (strncmp("-Xregalloc=", arg, 11) == 0) {
      // Which stage 2 runs. 'chaitin' is the default since step 37; the other
      // two are kept reachable as differential oracles - see
      // docs/ir-codegen-design.md section 7.
      const char *which = &arg[11];
      if (strcmp(which, "trivial") == 0) {
        config.regAlloc = RA_TRIVIAL;
      } else if (strcmp(which, "linear") == 0) {
        config.regAlloc = RA_LINEAR;
      } else if (strcmp(which, "chaitin") == 0) {
        config.regAlloc = RA_COLOUR;
      } else {
        fprintf(stderr,
                "unknown register allocator '%s', expected 'chaitin', 'linear' or 'trivial'\n",
                which);
        return 2;
      }
    } else if (strcmp("-experimental", arg) == 0) {
      // What it selected is the default now. Kept as a no-op rather than
      // removed: it is in every script, launch configuration and shell history
      // this compiler has, and rejecting it would break them to say nothing.
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
    } else if (strcmp("-s", arg) == 0) {
        // Strip. Collected with whatever '-Wl,' and '-Xlinker' carry, and read
        // by linkInternal below rather than understood here.
        ldCur = ldCur->next = newStringNode(arg);
    } else if (strcmp("-Xlinker", arg) == 0) {
        if (i + 1 >= argc) {
            fprintf(stderr, "error: missing argument to ‘-Xlinker’\n");
            return 2;
        }
        ldCur = ldCur->next = newStringNode(argv[++i]);
    } else if (strncmp("-Wl,", arg, 4) == 0) {
        // One linker argument per comma, gcc's spelling. strdup'd because the
        // separators are overwritten in place to terminate each piece.
        char *rest = strdup(&arg[4]);
        while (rest && *rest) {
            char *comma = strchr(rest, ',');
            if (comma) *comma = '\0';
            if (*rest) {
                ldCur = ldCur->next = newStringNode(rest);
            }
            rest = comma ? comma + 1 : NULL;
        }
    } else if (strcmp("-shared", arg) == 0) {
        // The one of the three below that the link now has a layout for: no
        // crt1.o, no dynamic linker, and the 'S' pair of crtbegin/crtend.
        config.sharedOutput = 1;
        continue;
    } else if (strcmp("-static", arg) == 0 || strcmp("-nostdlib", arg) == 0 ||
               strcmp("-pie", arg) == 0 || strncmp("-fuse-ld=", arg, 9) == 0) {
        // Refused rather than ignored: the first three each decide which CRT
        // objects and dynamic linker the link needs, and this driver spells
        // those out for one layout only. The fourth names another linker to
        // run, and there is only the one in src/link/ now.
        fprintf(stderr, "error: ‘%s’ is not supported\n", arg);
        return 2;
    } else if (strncmp("-std", arg, 4) == 0) {
        // The dialect itself is not implemented - what is read here is whether
        // the name asks for strict ISO C, since '-std=c99' and '-std=gnu99'
        // differ to glibc's headers even where they do not differ to the
        // parser. Anything but a 'c' after '=' (gnu99, iso9899:1999) is not
        // strict; so is a bare '-std' with no value.
        config.strictAnsi = arg[4] == '=' && arg[5] == 'c';
        continue;
    } else if (strncmp("-O", arg, 2) == 0) {
        // optimization? lol
        continue;
    } else if (strcmp("-fPIC", arg) == 0 || strcmp("-fpic", arg) == 0 ||
               strcmp("-fPIE", arg) == 0 || strcmp("-fpie", arg) == 0) {
        // One setting for all four. PIE is the weaker request - an executable's
        // symbols are not preemptible, so it could keep the PC-relative form -
        // but GOT-loading them is correct for it too, and one code path that
        // is always exercised beats a second that almost never is.
        config.pic = 1;
        continue;
    } else if (strcmp("-fno-pic", arg) == 0 || strcmp("-fno-PIC", arg) == 0 ||
               strcmp("-fno-pie", arg) == 0 || strcmp("-fno-PIE", arg) == 0) {
        config.pic = 0;
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

  // A second target is the IR backend's job, where a target is a selector and
  // a descriptor rather than a second copy of the whole code generator - so
  // riscv64 has one backend and not two, and asking for the other is refused
  // by name rather than silently answered with the one that exists. The
  // half-written AST walker that used to make '-legacy' mean something here
  // went with step 59; it emitted empty function bodies or asserted.
  if (config.arch != X86_64 && !config.irBackend) {
      fprintf(stderr, "error: '-legacy' has no %s backend\n",
              config.arch == RISCV64 ? "riscv64" : "non-x86_64");
      return 2;
  }

  // Refused for the same reason, and for one more: under medlow a global is
  // reached with an absolute lui/addi pair, and position-independent code
  // wants the auipc-relative form with its paired relocations instead. Neither
  // the selector nor the linker has those yet, so the honest answer is the
  // error - see section 13 of docs/ir-codegen-design.md.
  if (config.arch == RISCV64 && config.pic) {
      fprintf(stderr, "error: '-fPIC' is not supported on riscv64\n");
      return 2;
  }

  // Refused rather than ignored, for the same reason the four flags above are:
  // accepting it would produce an object that links into a shared library and
  // crashes when it is loaded. The legacy backend reaches a global through a
  // rip-relative memory operand it builds at the use site, so the GOT load has
  // nowhere to put its register there - see step 51 in docs/ir-codegen-design.md.
  if (!config.irBackend && config.pic) {
      fprintf(stderr, "error: ‘-fPIC’ is not supported under ‘-legacy’\n");
      return 2;
  }

  // Easy to ask for by accident now that the IR backend is the default and
  // '-legacy' is the opt-out: the legacy backend builds no IR, so the dump was
  // silently not written and the file left as it was.
  if (!config.irBackend && config.irDumpFileName) {
      fprintf(stderr, "warning: '-irDump' has nothing to dump under '-legacy'\n");
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

  // A link follows unless this run stops at an object or at preprocessed
  // source, so the objects are kept rather than written.
  config.keepObjects = !(config.objOutput || config.ppOutput);

  config.macroses = mhead.next;

  compileFiles(chead.next, &config);

  if (config.skipCodegen) return config.hadError ? 1 : 0;

  // A file that did not compile left no object behind, so linking would only
  // report the same input missing a second time and in the linker's words.
  if (!config.hadError && config.keepObjects) {
    const char *output = config.outputFile ? config.outputFile : "a.out";

    if (!linkInternal(output, config.sharedOutput, config.objects, ohead.next,
                      lhead.next, lDirHead.next, ldHead.next)) {
      config.hadError = 1;
    }
  }

  return config.hadError ? 1 : 0;
}
