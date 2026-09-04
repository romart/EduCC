#ifndef __LINK_LINK_H__
#define __LINK_LINK_H__ 1

#include "link/elfread.h"

// Writing side of the linker: relocatable objects in, an executable out, in
// place of the 'ld' that runLinker() still shells out to. This much of it is
// static and freestanding - no archives, no shared objects, no dynamic
// section - which is what layout and relocation can be proven with before any
// of the dynamic machinery exists to get in the way.

typedef struct _LinkOptions {
  const char *outputFile;

  // The symbol e_entry is taken from, '_start' unless overridden.
  const char *entrySymbol;

  // Where the first PT_LOAD is mapped. Page-aligned, and above the mmap floor
  // so that nothing the kernel places can land on top of it.
  uint64_t imageBase;
} LinkOptions;

void initLinkOptions(LinkOptions *options);

Boolean linkStaticExecutable(const LinkOptions *options, int inputCount,
                             char **inputs);

// '-Xlink-static -o <output> <object>...': link and report. Nonzero on any
// error, and no output file is left behind when one is reported.
int linkStaticCommand(int argc, char **argv);

#endif // __LINK_LINK_H__
