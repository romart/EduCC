#ifndef __LINK_LINK_H__
#define __LINK_LINK_H__ 1

#include "link/elfread.h"

// Writing side of the linker: objects in, an executable out, in place of the
// 'ld' that runLinker() still shells out to. Two shapes of link exist: a
// static, freestanding one with no dynamic machinery at all, and a dynamic
// non-PIE executable that reaches the distribution's own libc through
// ld.so - which is what the driver produces.

typedef struct _LinkOptions {
  const char *outputFile;

  // The symbol e_entry is taken from, '_start' unless overridden.
  const char *entrySymbol;

  // Where the first PT_LOAD is mapped. Page-aligned, and above the mmap floor
  // so that nothing the kernel places can land on top of it.
  uint64_t imageBase;

  // The dynamic loader's path, or NULL for a link that has none - which is
  // what makes the difference between the two shapes above. Everything
  // dynamic is synthesized only when this is set.
  const char *interpreter;

  // DT_RUNPATH, from '-Wl,-rpath,<dir>'.
  const char *runpath;

  // '-s': leave .symtab and .strtab out. Nothing reads them at load time.
  Boolean stripSymbols;
} LinkOptions;

void initLinkOptions(LinkOptions *options);

// Inputs are paths, or '-lname' for a library to be looked for in
// 'searchDirs'; the order is ld's, and an archive only gives up the members
// that resolve something undefined at the point it is reached.
Boolean linkObjects(const LinkOptions *options, int inputCount, char **inputs,
                    int dirCount, char **searchDirs);

Boolean linkStaticExecutable(const LinkOptions *options, int inputCount,
                             char **inputs);

// '-Xlink-static' and '-Xlink-dynamic': link exactly what they are given and
// report, the second one against shared libraries and through a loader. Both
// take '-o', '-e', '-s', '-L', '-l' and '-rpath', and '-dynamic-linker' names
// the interpreter. Nonzero on any error, and no output file is left behind
// when one is reported.
int linkStaticCommand(int argc, char **argv);
int linkDynamicCommand(int argc, char **argv);

// Where the distribution keeps crt1.o/crti.o/crtn.o, and where gcc keeps
// crtbegin.o/crtend.o. Probed once; both abort if there is nothing to find,
// since a link cannot be attempted without them either way.
const char *linkCrtPath(void);
const char *linkGccPath(void);

#endif // __LINK_LINK_H__
