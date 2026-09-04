#ifndef __LINK_ELFREAD_H__
#define __LINK_ELFREAD_H__ 1

#include <elf.h>
#include <stdint.h>
#include <stddef.h>

#include "common.h"
#include "mem.h"

// Reading side of the linker: everything here points straight into a mapped
// file and owns none of it. The writer in src/elf.c is a separate model and
// stays that way - converging the two would mean a section that is both a
// growable buffer and a window onto someone else's bytes.

struct _InputFile;

typedef struct _InputSection {
  const char *name;
  uint32_t type;
  uint64_t flags;
  uint64_t align;
  uint64_t size;

  // NULL for SHT_NOBITS, which occupies no file space to point at.
  const uint8_t *data;

  uint32_t link;
  uint32_t info;
  uint64_t entsize;

  // The RELA entries of the .rela section that names this one, gathered here
  // so a consumer never has to walk the section table looking for its partner.
  const Elf64_Rela *relocs;
  size_t relocCount;

  struct _InputFile *file;
  unsigned index;
} InputSection;

typedef struct _InputSymbol {
  const char *name;
  uint64_t value;
  uint64_t size;

  uint8_t bind;
  uint8_t type;
  uint8_t visibility;
  uint16_t shndx;

  struct _InputFile *file;
} InputSymbol;

typedef struct _InputFile {
  const char *path;

  // The archive member this came out of, or NULL for a file of its own. Both
  // are needed to name an input in a diagnostic: "libc.a(printf.o)".
  const char *member;

  const uint8_t *image;
  size_t imageSize;

  uint16_t kind; // ET_REL or ET_DYN

  // ET_DYN only: DT_SONAME if it has one, else the basename of the path. This
  // is what a DT_NEEDED entry has to record, and it is not always the filename.
  const char *soname;

  InputSection *sections;
  unsigned sectionCount;

  InputSymbol *symbols;
  unsigned symbolCount;
  // Index of the first non-local symbol (sh_info of the symbol table). Local
  // symbols are never candidates for cross-file resolution.
  unsigned firstGlobal;

  struct _InputFile *next;
} InputFile;

typedef struct _ArchiveMember {
  const char *name;
  const uint8_t *image;
  size_t size;

  // Offset of the member's header within the archive. The armap keys on this,
  // so it is the member's identity rather than its name - names repeat.
  size_t headerOffset;

  // Set once the member has been pulled in, so a second symbol resolving to
  // the same member does not read it twice.
  InputFile *loaded;
} ArchiveMember;

typedef struct _ArchiveSymbol {
  const char *name;
  unsigned member; // index into Archive.members
} ArchiveSymbol;

typedef struct _Archive {
  const char *path;
  const uint8_t *image;
  size_t imageSize;

  ArchiveMember *members;
  unsigned memberCount;

  // The armap, absent in an archive built without one - which is a link error
  // rather than a reason to scan every member.
  ArchiveSymbol *index;
  unsigned indexCount;
  Boolean hasIndex;
} Archive;

enum InputKind {
  INPUT_UNKNOWN = 0,
  INPUT_REL,     // ET_REL, a relocatable object
  INPUT_DYN,     // ET_DYN, a shared object
  INPUT_EXEC,    // ET_EXEC, which is not a valid input
  INPUT_ARCHIVE, // !<arch>
  INPUT_SCRIPT   // text; /usr/lib/libc.so is one of these
};

typedef struct _ElfReader {
  Arena *arena;
  Boolean hadError;
} ElfReader;

// Maps a file read-only. Returns NULL and leaves errno set on failure.
const uint8_t *mapInputFile(const char *path, size_t *size);
void unmapInputFile(const uint8_t *image, size_t size);

enum InputKind classifyImage(const uint8_t *image, size_t size);

// Both return NULL after reporting, so a caller checks the result rather than
// r->hadError - though the flag is there for a caller collecting several.
InputFile *readObject(ElfReader *r, const char *path, const char *member,
                      const uint8_t *image, size_t size);
Archive *readArchive(ElfReader *r, const char *path, const uint8_t *image,
                     size_t size);

// Reads one member as an object, caching the result on the member.
InputFile *readArchiveMember(ElfReader *r, Archive *archive, unsigned member);

void dumpInputFile(FILE *output, const InputFile *file);
void dumpArchive(FILE *output, const Archive *archive);

// '-Xlink-read <file>...': read each input and print what the reader saw.
// Nonzero if any of them could not be read. This is how the reader is
// exercised before there is a linker to put it in.
int dumpLinkInputs(int count, char **paths);

#endif // __LINK_ELFREAD_H__
