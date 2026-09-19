#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <glob.h>
#include <unistd.h>
#include <sys/stat.h>

#include "link/link.h"
#include "utils.h"

// The link runs in one pass over a fixed set of inputs: read them, decide what
// every global name means, group the input sections into output ones, invent
// the sections a loader needs that nobody supplied, give all of them
// addresses, then write the bytes and patch them. Nothing here reads a file
// twice, and nothing is decided before layout that layout could change - the
// addresses relocations need do not exist until then.

#define LINK_PAGE_SIZE 0x1000
#define DEFAULT_IMAGE_BASE 0x400000
#define DEFAULT_ENTRY_SYMBOL "_start"

#define GOT_ENTRY_SIZE 8
// .got.plt[0] is the address of .dynamic, and the rest of the reserved run is
// ld.so's - how many there are is the target's answer, not a constant.

// Spelled out rather than taken from <limits.h>: these are the widths of the
// relocation fields, not of any type this compiler happens to have.
#define RELOC_S32_MIN (-0x80000000LL)
#define RELOC_S32_MAX 0x7fffffffLL
#define RELOC_U32_MAX 0xffffffffLL

enum SectionClass {
  CLASS_RODATA = 0, // allocated, neither writable nor executable
  CLASS_TEXT,       // executable
  CLASS_DATA,       // writable
  CLASS_COUNT
};

// Order within a class. The dynamic sections come before anything a compiler
// produced because a loader reads them before it runs a byte of the program,
// and the writable ones are arranged so that everything a relocation touches
// sits in front of everything that stays writable - which is the whole of what
// PT_GNU_RELRO says.
enum SectionRank {
  RANK_INTERP = 0,
  RANK_NOTE,
  RANK_HASH,
  RANK_DYNSYM,
  RANK_DYNSTR,
  RANK_RELA_DYN,
  RANK_RELA_PLT,
  RANK_INIT,
  RANK_PLT,
  RANK_TEXT,
  RANK_FINI,
  RANK_RODATA,
  RANK_EH_FRAME,
  RANK_PREINIT_ARRAY,
  RANK_INIT_ARRAY,
  RANK_FINI_ARRAY,
  RANK_DATA_REL_RO,
  RANK_DYNAMIC,
  RANK_GOT,
  // Everything above this line is read-only once the loader has finished
  // relocating; everything below it is written while the program runs.
  RANK_GOT_PLT,
  RANK_DATA,
  RANK_OTHER,
  RANK_BSS,
  RANK_COUNT
};

struct _LinkFile;
struct _OutputSection;

// One input section's contribution to an output section - or, when 'input' is
// NULL, a block the link had to invent: tentative definitions, copied data, or
// one of the dynamic sections.
typedef struct _LinkPiece {
  InputSection *input;
  struct _LinkFile *owner;
  struct _OutputSection *output;

  const char *name;
  uint32_t type;
  uint64_t flags;
  uint64_t align;
  uint64_t size;
  uint64_t entsize;
  const uint8_t *data; // NULL for SHT_NOBITS

  uint64_t address;
  uint64_t offset; // in the output file; meaningless for SHT_NOBITS
} LinkPiece;

typedef struct _OutputSection {
  const char *name;
  uint32_t type;
  uint64_t flags;
  uint64_t align;
  uint64_t size;
  uint64_t entsize;
  uint64_t address;
  uint64_t offset;

  int rank;
  unsigned index;      // in the output section header table
  uint32_t link;       // sh_link, patched once indices exist
  uint32_t info;       // sh_info, likewise
  uint32_t nameOffset; // into .shstrtab

  Vector pieces;
} OutputSection;

typedef struct _LinkSymbol {
  const char *name;

  // The definition that won, and the file it came from. NULL while the symbol
  // is undefined or still only a tentative definition.
  const InputSymbol *definition;
  struct _LinkFile *owner;

  // A shared object's definition, which loses to any of the above and is only
  // consulted once the relocatable inputs have had their say.
  const InputSymbol *shared;
  struct _LinkFile *sharedOwner;

  uint64_t address;
  uint64_t size;

  // A tentative definition (SHN_COMMON) is not a definition until the link
  // ends without finding a real one, so its size and alignment are carried
  // separately - the largest request wins, which is what C requires. Every
  // reader of 'isCommon' asks about 'definition' first, so a real definition
  // turning up later does not have to clear it.
  Boolean isCommon;
  uint64_t commonSize;
  uint64_t commonAlign;
  uint64_t commonOffset;

  // What a reference to a shared definition costs. A function gets a stub, so
  // that its address is one the executable owns; a data object gets a copy in
  // .bss, so that nothing has to be relocated at a use site.
  Boolean needsPlt;
  Boolean needsGot;
  Boolean needsCopy;
  unsigned pltIndex;
  uint64_t gotOffset;
  uint64_t copyOffset;
  uint64_t copySize;
  uint64_t copyAlign;

  unsigned dynIndex; // in .dynsym; 0 when the symbol is not dynamic

  Boolean weakDefinition;
  Boolean strongReference;

  // A shared object left this name to whoever loads it. If this link defines
  // it, the definition has to be advertised, or the library gets its own idea
  // of what the name means and interposition never happens.
  Boolean sharedReference;
  Boolean linkerDefined;
  Boolean resolved;
} LinkSymbol;

typedef struct _LinkFile {
  InputFile *file;

  // Indexed by input section number; NULL for a section that contributes
  // nothing to the output, which is every non-allocated one.
  LinkPiece **pieces;

  // Indexed by symbol number, filled for every non-local symbol. Keeping it
  // here is what makes relocation a lookup rather than a hash of the name.
  LinkSymbol **globals;

  // GOT slot offsets for this file's local symbols, biased by one so that a
  // zeroed array means 'no slot yet'.
  uint64_t *localGot;

  Boolean shared;
  Boolean needed; // a shared object that actually defined something we use

  struct _LinkFile *next;
} LinkFile;

typedef struct _LinkArchive {
  Archive *archive;
  struct _LinkArchive *next;
} LinkArchive;

// Every file the link mapped, so that unmapping is one walk at the end rather
// than an ownership question - an archive member's bytes belong to the
// archive, and a name read out of any of them is live until then.
typedef struct _Mapping {
  const uint8_t *image;
  size_t size;
  struct _Mapping *next;
} Mapping;

typedef struct _StringTable {
  char *blob;
  size_t size;
  size_t capacity;
} StringTable;

// A GOT entry, which a global symbol has at most one of, and a local symbol
// has at most one of per file.
typedef struct _GotSlot {
  LinkSymbol *global;
  LinkFile *file;
  unsigned index; // symbol number within 'file', for a local
  uint64_t offset;
} GotSlot;

typedef struct _Linker {
  Arena *arena;
  ElfReader reader;
  const LinkOptions *options;
  const LinkTarget *target;

  LinkFile *files;
  LinkFile *lastFile;
  LinkArchive *archives;
  LinkArchive *lastArchive;
  Mapping *mappings;

  Vector searchDirs; // const char *, in '-L' order

  // '--no-as-needed' is ld's default and this linker's: a shared object named
  // on the command line is recorded as needed whether anything used it or not,
  // because what a program dlsyms out of it at run time is invisible here.
  Boolean asNeeded;

  HashMap *globalMap;
  Vector globals; // LinkSymbol *, in the order the names were first seen

  Vector outputs; // OutputSection *, in layout order
  LinkPiece *commonPiece;
  LinkPiece *copyPiece;

  // The dynamic half, all NULL for a static link.
  Boolean dynamic;
  Vector plt;     // LinkSymbol *
  Vector got;     // GotSlot *
  Vector copies;  // LinkSymbol *
  Vector dynsyms; // LinkSymbol *, in .dynsym order
  StringTable dynstr;
  // .dynstr is sized before it is filled, so every name has to come out at the
  // same offset both times it is asked for.
  HashMap *dynstrMap;

  OutputSection *interpSection;
  OutputSection *hashSection;
  OutputSection *dynsymSection;
  OutputSection *dynstrSection;
  OutputSection *relaDynSection;
  OutputSection *relaPltSection;
  OutputSection *pltSection;
  OutputSection *gotSection;
  OutputSection *gotPltSection;
  OutputSection *dynamicSection;

  LinkPiece *interpPiece;
  LinkPiece *hashPiece;
  LinkPiece *dynsymPiece;
  LinkPiece *dynstrPiece;
  LinkPiece *relaDynPiece;
  LinkPiece *relaPltPiece;
  LinkPiece *pltPiece;
  LinkPiece *gotPiece;
  LinkPiece *gotPltPiece;
  LinkPiece *dynamicPiece;

  // How many entries .rela.dyn needs, counted before layout because layout is
  // what sizes the section. Only a shared object has any beyond its copies.
  size_t dynRelocCount;
  size_t relativeCount; // of those, the leading RELATIVE run

  uint64_t relroStart;
  uint64_t relroEnd;

  uint64_t entry;
  Boolean hadError;
} Linker;

// What the loader has to be told about one place holding an address.
enum DynAction {
  DYN_NONE = 0,  // the link knows the answer and has written it
  DYN_RELATIVE,  // the answer is this object's own, once it has been placed
  DYN_SYMBOL     // the answer is a name, and may not be this object's at all
};

static void linkError(Linker *l, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);

  fprintf(stderr, "educc: error: ");
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);

  va_end(args);
  l->hadError = TRUE;
}

// The symbol map is keyed by name. Its own hash rather than the front end's,
// so that nothing in src/link/ needs the parser to exist.
static int nameHash(intptr_t key) {
  const char *name = (const char *)key;
  int hash = 0;

  while (*name) {
    hash = hash * 31 + *name++;
  }

  return hash;
}

static int nameCompare(intptr_t left, intptr_t right) {
  return strcmp((const char *)left, (const char *)right);
}

static uint64_t alignUp(uint64_t value, uint64_t align) {
  if (align <= 1) return value;
  return (value + align - 1) & ~(align - 1);
}

static void write32(uint8_t *where, uint32_t value) {
  where[0] = (uint8_t)value;
  where[1] = (uint8_t)(value >> 8);
  where[2] = (uint8_t)(value >> 16);
  where[3] = (uint8_t)(value >> 24);
}

static void write64(uint8_t *where, uint64_t value) {
  write32(where, (uint32_t)value);
  write32(where + 4, (uint32_t)(value >> 32));
}

static char *copyString(Linker *l, const char *string, size_t length) {
  char *copy = areanAllocate(l->arena, length + 1);
  memcpy(copy, string, length);
  copy[length] = '\0';
  return copy;
}

// A file is named the way a diagnostic has to name it, member and all. Two
// buffers because 'multiple definition' names two files in one call, and one
// static buffer would make both of them the second one.
static const char *fileName(const LinkFile *lf) {
  static char buffers[2][512];
  static unsigned next = 0;

  if (lf->file->member == NULL) return lf->file->path;

  char *buffer = buffers[next];
  next = (next + 1) % 2;

  snprintf(buffer, sizeof buffers[0], "%s(%s)", lf->file->path,
           lf->file->member);
  return buffer;
}

/* --------------------------------------------------------- string tables */

static void initStringTable(StringTable *table) {
  table->capacity = 256;
  table->blob = heapAllocate(table->capacity);
  table->blob[0] = '\0';
  table->size = 1;
}

static uint32_t addString(StringTable *table, const char *string) {
  if (string == NULL || string[0] == '\0') return 0;

  size_t length = strlen(string) + 1;
  if (table->size + length > table->capacity) {
    size_t capacity = table->capacity;
    while (table->size + length > capacity) capacity *= 2;

    table->blob = heapReallocate(table->blob, table->capacity, capacity);
    table->capacity = capacity;
  }

  uint32_t offset = (uint32_t)table->size;
  memcpy(table->blob + table->size, string, length);
  table->size += length;

  return offset;
}

static void releaseStringTable(StringTable *table) {
  if (table->blob != NULL) releaseHeap(table->blob);
  table->blob = NULL;
}

/* ------------------------------------------------------ toolchain probing */

static char *findFile(const char *pattern) {
  char *path = NULL;
  glob_t buffer = { 0 };

  glob(pattern, 0, NULL, &buffer);
  if (buffer.gl_pathc > 0) path = strdup(buffer.gl_pathv[buffer.gl_pathc - 1]);
  globfree(&buffer);

  return path;
}

// Both are cached per machine rather than once: a driver that compiled for one
// target and links for it asks only about that one, but nothing stops a
// process asking twice and getting the other's answer.
static const LinkTarget *pathTargetFor(uint16_t machine) {
  const LinkTarget *target = linkTargetFor(machine);
  if (target != NULL) return target;

  fprintf(stderr, "educc: error: no linker support for e_machine %u\n",
          (unsigned)machine);
  exit(1);
}

const char *linkCrtPath(uint16_t machine) {
  static const char *cached[2];
  const LinkTarget *target = pathTargetFor(machine);
  unsigned which = target->machine == EM_X86_64 ? 0 : 1;

  if (cached[which] != NULL) return cached[which];

  size_t i;
  for (i = 0; i < target->crtDirCount; ++i) {
    char probe[512];
    snprintf(probe, sizeof probe, "%s/crti.o", target->crtDirs[i]);
    if (access(probe, F_OK) == 0) {
      cached[which] = target->crtDirs[i];
      return cached[which];
    }
  }

  fprintf(stderr, "educc: error: no %s C runtime directory found\n",
          target->name);
  exit(1);
}

const char *linkGccPath(uint16_t machine) {
  static char cached[2][512];
  const LinkTarget *target = pathTargetFor(machine);
  unsigned which = target->machine == EM_X86_64 ? 0 : 1;

  if (cached[which][0] != '\0') return cached[which];

  size_t i;
  for (i = 0; i < target->gccGlobCount; ++i) {
    char *path = findFile(target->gccGlobs[i]);
    if (path == NULL) continue;

    static const char suffix[] = "/crtbegin.o";
    size_t length = strlen(path);
    size_t directory = length - (sizeof suffix - 1);

    if (directory < sizeof cached[which]) {
      memcpy(cached[which], path, directory);
      cached[which][directory] = '\0';
    }
    free(path);

    if (cached[which][0] != '\0') return cached[which];
  }

  fprintf(stderr, "educc: error: no %s gcc runtime directory found\n",
          target->name);
  exit(1);
}

const char *const *linkLibraryPaths(uint16_t machine, size_t *count) {
  const LinkTarget *target = pathTargetFor(machine);
  *count = target->libDirCount;
  return target->libDirs;
}

/* ---------------------------------------------------------------- inputs */

static void collectSymbols(Linker *l, LinkFile *lf);
static void collectSharedSymbols(Linker *l, LinkFile *lf);
static Boolean loadPath(Linker *l, const char *path);

static const uint8_t *mapAndRemember(Linker *l, const char *path,
                                     size_t *size) {
  const uint8_t *image = mapInputFile(path, size);
  if (image == NULL) return NULL;

  Mapping *mapping = areanAllocate(l->arena, sizeof(Mapping));
  mapping->image = image;
  mapping->size = *size;
  mapping->next = l->mappings;
  l->mappings = mapping;

  return image;
}

static LinkFile *addLinkFile(Linker *l, InputFile *file, Boolean shared) {
  LinkFile *lf = areanAllocate(l->arena, sizeof(LinkFile));
  lf->file = file;
  lf->shared = shared;
  unsigned symbolCount = file->symbolCount != 0 ? file->symbolCount : 1;

  lf->pieces = areanAllocate(l->arena, sizeof(LinkPiece *) * file->sectionCount);
  lf->globals = areanAllocate(l->arena, sizeof(LinkSymbol *) * symbolCount);
  if (!shared) {
    lf->localGot = areanAllocate(l->arena, sizeof(uint64_t) * symbolCount);
  }
  lf->next = NULL;

  if (l->lastFile != NULL) {
    l->lastFile->next = lf;
  } else {
    l->files = lf;
  }
  l->lastFile = lf;

  return lf;
}

// Whether pulling this member in would resolve something. A tentative
// definition counts as unresolved: C says a real one somewhere else wins, and
// an archive is one of the places it can be.
static Boolean wantsArchiveSymbol(Linker *l, const char *name) {
  LinkSymbol *symbol = (LinkSymbol *)getFromHashMap(l->globalMap, (intptr_t)name);
  if (symbol == NULL) return FALSE;
  if (symbol->definition != NULL) return FALSE;
  if (symbol->linkerDefined) return FALSE;

  // A shared object that already answers for the name counts as an answer:
  // libc.so.6 and libc_nonshared.a are reached through one '-lc', and it is
  // the .so that decides which of the two supplies 'atexit'.
  if (symbol->shared != NULL) return FALSE;

  return symbol->strongReference || symbol->isCommon;
}

// An archive gives up a member the moment it resolves something, and pulling
// one in can leave a further symbol undefined - so the scan repeats until a
// pass takes nothing.
static void resolveArchive(Linker *l, LinkArchive *la) {
  Archive *archive = la->archive;
  Boolean progress = TRUE;

  while (progress) {
    progress = FALSE;

    unsigned i;
    for (i = 0; i < archive->indexCount; ++i) {
      ArchiveSymbol *entry = &archive->index[i];
      if (archive->members[entry->member].loaded != NULL) continue;
      if (!wantsArchiveSymbol(l, entry->name)) continue;

      InputFile *file = readArchiveMember(&l->reader, archive, entry->member);
      if (file == NULL) {
        l->hadError = TRUE;
        return;
      }

      if (file->kind != ET_REL) {
        linkError(l, "%s(%s): an archive member must be a relocatable object",
                  archive->path, file->member);
        return;
      }

      collectSymbols(l, addLinkFile(l, file, FALSE));
      progress = TRUE;
    }
  }
}

static Boolean addArchive(Linker *l, const char *path, const uint8_t *image,
                          size_t size) {
  Archive *archive = readArchive(&l->reader, path, image, size);
  if (archive == NULL) {
    l->hadError = TRUE;
    return FALSE;
  }

  if (!archive->hasIndex && archive->memberCount != 0) {
    linkError(l, "%s: archive has no index; run ranlib on it", path);
    return FALSE;
  }

  LinkArchive *la = areanAllocate(l->arena, sizeof(LinkArchive));
  la->archive = archive;
  la->next = NULL;

  if (l->lastArchive != NULL) {
    l->lastArchive->next = la;
  } else {
    l->archives = la;
  }
  l->lastArchive = la;

  resolveArchive(l, la);
  return !l->hadError;
}

/* --------------------------------------------------------- linker scripts */

// '-lc' finds /usr/lib/libc.so, which is not an object at all but four lines
// of GNU ld script naming the files that really make up the library. Only the
// commands that name inputs are understood; everything else is a name followed
// by a parenthesised group, and skipping those is the whole of the rest.
#define SCRIPT_MAX_DEPTH 16

static Boolean scriptKeyword(const char *token) {
  return strcmp(token, "GROUP") == 0 || strcmp(token, "INPUT") == 0 ||
         strcmp(token, "AS_NEEDED") == 0;
}

static Boolean parseLinkerScript(Linker *l, const char *path,
                                 const uint8_t *image, size_t size) {
  const char *text = (const char *)image;
  size_t at = 0;

  Boolean inputContext[SCRIPT_MAX_DEPTH];
  // AS_NEEDED is a group, so what it asks for lasts exactly as far as its
  // closing parenthesis; everything else in the script inherits whatever the
  // command line was in the middle of saying.
  Boolean outerAsNeeded[SCRIPT_MAX_DEPTH];
  Boolean entryAsNeeded = l->asNeeded;
  unsigned depth = 0;
  inputContext[0] = FALSE;

  char pending[256];
  pending[0] = '\0';

  while (at < size) {
    char c = text[at];

    if (c == '/' && at + 1 < size && text[at + 1] == '*') {
      at += 2;
      while (at + 1 < size && !(text[at] == '*' && text[at + 1] == '/')) ++at;
      at = at + 2 < size ? at + 2 : size;
      continue;
    }

    if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' ||
        c == ';') {
      ++at;
      continue;
    }

    if (c == '(') {
      // Whatever name came before the parenthesis decides whether the group is
      // a list of inputs or something to skip over.
      if (depth + 1 >= SCRIPT_MAX_DEPTH) {
        linkError(l, "%s: linker script nests deeper than %d groups", path,
                  SCRIPT_MAX_DEPTH);
        return FALSE;
      }

      ++depth;
      inputContext[depth] = scriptKeyword(pending) ||
                            (pending[0] == '\0' && inputContext[depth - 1]);
      outerAsNeeded[depth] = l->asNeeded;
      if (strcmp(pending, "AS_NEEDED") == 0) l->asNeeded = TRUE;
      pending[0] = '\0';
      ++at;
      continue;
    }

    if (c == ')') {
      if (depth > 0) l->asNeeded = outerAsNeeded[depth--];
      pending[0] = '\0';
      ++at;
      continue;
    }

    size_t start = at;
    while (at < size && text[at] != ' ' && text[at] != '\t' &&
           text[at] != '\n' && text[at] != '\r' && text[at] != '(' &&
           text[at] != ')' && text[at] != ',' && text[at] != ';') {
      ++at;
    }

    size_t length = at - start;
    if (length == 0 || length >= sizeof pending) {
      pending[0] = '\0';
      continue;
    }

    memcpy(pending, text + start, length);
    pending[length] = '\0';

    if (!inputContext[depth]) continue;
    if (scriptKeyword(pending)) continue;

    // The names inside a group are inputs in their own right, and the script's
    // bytes go away with the mapping, so each one is copied out first.
    if (!loadPath(l, copyString(l, pending, length))) return FALSE;
    pending[0] = '\0';
  }

  l->asNeeded = entryAsNeeded;
  return TRUE;
}

/* --------------------------------------------------------- input dispatch */

static const char *findLibrary(Linker *l, const char *name) {
  size_t i;
  for (i = 0; i < l->searchDirs.size; ++i) {
    const char *dir = (const char *)getFromVector(&l->searchDirs, i);

    // A shared library first, then a static one, which is what decides whether
    // '-lc' means libc.so.6 or libc.a on a machine that has both.
    const char *suffixes[] = { ".so", ".a" };
    unsigned s;
    for (s = 0; s < 2; ++s) {
      char probe[1024];
      snprintf(probe, sizeof probe, "%s/lib%s%s", dir, name, suffixes[s]);
      if (access(probe, R_OK) == 0) return copyString(l, probe, strlen(probe));
    }
  }

  return NULL;
}

// A linker script may name a file without saying where it is - /usr/lib/
// libgcc_s.so is 'GROUP ( libgcc_s.so.1 -lgcc )' - and ld looks such a name up
// on the search path. Anything that opens where it stands is taken as written.
static const char *resolveInput(Linker *l, const char *path) {
  // A cross toolchain's own libc.so says 'GROUP ( /lib/libc.so.6 ... )' and
  // means its sysroot's, not this machine's - which is the whole of what ld's
  // '--sysroot' does, and getting it wrong links the host's libc into a
  // riscv64 program.
  if (path[0] == '/' && l->target->sysroot != NULL) {
    char probe[1024];
    snprintf(probe, sizeof probe, "%s%s", l->target->sysroot, path);
    if (access(probe, R_OK) == 0) return copyString(l, probe, strlen(probe));
  }

  if (path[0] == '/' || access(path, R_OK) == 0) return path;

  size_t i;
  for (i = 0; i < l->searchDirs.size; ++i) {
    const char *dir = (const char *)getFromVector(&l->searchDirs, i);

    char probe[1024];
    snprintf(probe, sizeof probe, "%s/%s", dir, path);
    if (access(probe, R_OK) == 0) return copyString(l, probe, strlen(probe));
  }

  return path; // report it by the name the caller used
}

// What every input becomes once its bytes are somewhere: a file that was
// mapped, or an object the compiler in this process has just built and never
// wrote down. 'path' is what a diagnostic calls it either way.
static Boolean loadImage(Linker *l, const char *path, const uint8_t *image,
                         size_t size) {
  enum InputKind kind = classifyImage(image, size);

  if (kind == INPUT_DYN && !l->dynamic) {
    // A shared object only means anything to a loader, and this link has said
    // it will not have one. Saying so beats leaving its symbols undefined.
    linkError(l, "%s: a shared object needs a dynamic linker", path);
    return FALSE;
  }

  switch (kind) {
    case INPUT_REL: {
      InputFile *file = readObject(&l->reader, path, NULL, image, size);
      if (file == NULL) {
        l->hadError = TRUE;
        return FALSE;
      }
      collectSymbols(l, addLinkFile(l, file, FALSE));
      return TRUE;
    }

    case INPUT_DYN: {
      InputFile *file = readObject(&l->reader, path, NULL, image, size);
      if (file == NULL) {
        l->hadError = TRUE;
        return FALSE;
      }
      // '-lc' is named twice, so the same library arrives twice; ld loads one
      // copy of a soname and so does this, or DT_NEEDED says it twice.
      LinkFile *lf;
      for (lf = l->files; lf != NULL; lf = lf->next) {
        if (lf->shared && strcmp(lf->file->soname, file->soname) == 0) break;
      }

      if (lf == NULL) {
        lf = addLinkFile(l, file, TRUE);
        collectSharedSymbols(l, lf);
      }
      if (!l->asNeeded) lf->needed = TRUE;
      return TRUE;
    }

    case INPUT_ARCHIVE:
      return addArchive(l, path, image, size);

    case INPUT_SCRIPT:
      return parseLinkerScript(l, path, image, size);

    case INPUT_EXEC:
      linkError(l, "%s: an executable is not a link input", path);
      return FALSE;

    default:
      linkError(l, "%s: unrecognized file format", path);
      return FALSE;
  }
}

static Boolean loadPath(Linker *l, const char *path) {
  if (strcmp(path, "--as-needed") == 0 ||
      strcmp(path, "--no-as-needed") == 0) {
    l->asNeeded = path[2] == 'a';
    return TRUE;
  }

  if (path[0] == '-' && path[1] == 'l') {
    const char *found = findLibrary(l, path + 2);
    if (found == NULL) {
      linkError(l, "cannot find -l%s", path + 2);
      return FALSE;
    }
    path = found;
  } else {
    path = resolveInput(l, path);
  }

  size_t size = 0;
  const uint8_t *image = mapAndRemember(l, path, &size);

  if (image == NULL) {
    linkError(l, "cannot open '%s': %s", path, strerror(errno));
    return FALSE;
  }

  return loadImage(l, path, image, size);
}

/* ------------------------------------------------------------- resolution */

static LinkSymbol *lookupGlobal(Linker *l, const char *name) {
  LinkSymbol *symbol = (LinkSymbol *)getFromHashMap(l->globalMap, (intptr_t)name);
  if (symbol != NULL) return symbol;

  symbol = areanAllocate(l->arena, sizeof(LinkSymbol));
  symbol->name = name;

  putToHashMap(l->globalMap, (intptr_t)name, (intptr_t)symbol);
  addToVector(&l->globals, (intptr_t)symbol);

  return symbol;
}

static Boolean isDefinition(const InputSymbol *symbol) {
  return symbol->shndx != SHN_UNDEF && symbol->shndx != SHN_COMMON;
}

// A name a shared object defines and no relocatable input does. Everything
// that costs a PLT stub, a copy or a .dynsym entry starts here.
static Boolean isSharedSymbol(const LinkSymbol *symbol) {
  return symbol->definition == NULL && !symbol->isCommon &&
         !symbol->linkerDefined && symbol->shared != NULL;
}

// Whether somebody other than this object may end up answering for the name.
// Only a shared object has the question at all: an executable's definitions
// are final, and its references to a library are already bound to a stub or a
// copy it owns. Hidden means the name is this object's own business, and a
// linker-defined one describes this object and nothing else.
static Boolean isPreemptible(const Linker *l, const LinkSymbol *symbol) {
  if (!l->options->shared) return FALSE;
  if (symbol->linkerDefined) return FALSE;
  if (symbol->definition == NULL) return TRUE;

  return symbol->definition->visibility == STV_DEFAULT;
}

static void collectSymbols(Linker *l, LinkFile *lf) {
  InputFile *file = lf->file;

  unsigned i;
  for (i = 0; i < file->symbolCount; ++i) {
    InputSymbol *symbol = &file->symbols[i];

    if (symbol->bind == STB_LOCAL) continue;
    if (symbol->name == NULL || symbol->name[0] == '\0') continue;

    LinkSymbol *global = lookupGlobal(l, symbol->name);
    lf->globals[i] = global;

    if (isDefinition(symbol)) {
      Boolean weak = symbol->bind == STB_WEAK;

      if (global->definition != NULL && !global->weakDefinition && !weak) {
        linkError(l, "multiple definition of '%s': %s and %s", symbol->name,
                  fileName(global->owner), fileName(lf));
        continue;
      }

      // A strong definition takes a weak one's place; a weak one never takes
      // a strong one's, and the common block below is abandoned either way.
      if (global->definition == NULL || (global->weakDefinition && !weak)) {
        global->definition = symbol;
        global->owner = lf;
        global->weakDefinition = weak;
        global->size = symbol->size;
      }
      continue;
    }

    if (symbol->shndx == SHN_COMMON) {
      if (global->definition != NULL) continue;

      // st_value of a common symbol is its alignment, not an address.
      uint64_t align = symbol->value != 0 ? symbol->value : 1;

      global->isCommon = TRUE;
      global->owner = lf;
      if (symbol->size > global->commonSize) global->commonSize = symbol->size;
      if (align > global->commonAlign) global->commonAlign = align;
      continue;
    }

    if (symbol->bind != STB_WEAK) global->strongReference = TRUE;
  }
}

static void collectSharedSymbols(Linker *l, LinkFile *lf) {
  InputFile *file = lf->file;

  unsigned i;
  for (i = 0; i < file->symbolCount; ++i) {
    InputSymbol *symbol = &file->symbols[i];

    if (symbol->bind == STB_LOCAL) continue;
    if (symbol->name == NULL || symbol->name[0] == '\0') continue;

    if (symbol->shndx == SHN_UNDEF) {
      lookupGlobal(l, symbol->name)->sharedReference = TRUE;
      continue;
    }

    // Hidden and internal names are the library's own business, and a
    // non-default version is not what an unversioned reference asks for.
    if (symbol->visibility == STV_HIDDEN ||
        symbol->visibility == STV_INTERNAL) {
      continue;
    }
    if (!symbol->defaultVersion) continue;

    LinkSymbol *global = lookupGlobal(l, symbol->name);
    lf->globals[i] = global;

    if (global->shared == NULL) {
      global->shared = symbol;
      global->sharedOwner = lf;
    }
  }
}

// Names the linker itself answers for. Each is defined only when nothing that
// was actually linked defines it, which is what keeps a program free to have
// its own '_end'.
static const char *linkerSymbols[] = {
  "__ehdr_start", "__executable_start", "_GLOBAL_OFFSET_TABLE_", "_DYNAMIC",
  "__preinit_array_start", "__preinit_array_end",
  "__init_array_start", "__init_array_end",
  "__fini_array_start", "__fini_array_end",
  "etext", "_etext", "edata", "_edata", "__bss_start", "end", "_end",
  // RISC-V's, and it is load-bearing: crt1.o's _start puts it in gp before
  // anything else runs. Claimed on every target, since a name nothing
  // referenced is never asked about.
  "__global_pointer$"
};

static void claimLinkerSymbols(Linker *l) {
  unsigned i;
  for (i = 0; i < sizeof linkerSymbols / sizeof linkerSymbols[0]; ++i) {
    LinkSymbol *symbol =
        (LinkSymbol *)getFromHashMap(l->globalMap, (intptr_t)linkerSymbols[i]);

    if (symbol == NULL) continue;
    if (symbol->definition != NULL || symbol->isCommon) continue;

    symbol->linkerDefined = TRUE;
  }
}

// Tentative definitions that nothing defined get one block of .bss between
// them, laid out here so that the block is a piece like any other.
static void allocateCommon(Linker *l) {
  uint64_t size = 0;
  uint64_t align = 1;

  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);
    if (!symbol->isCommon || symbol->definition != NULL) continue;

    size = alignUp(size, symbol->commonAlign);
    symbol->commonOffset = size;
    size += symbol->commonSize;
    symbol->size = symbol->commonSize;

    if (symbol->commonAlign > align) align = symbol->commonAlign;
  }

  if (size == 0) return;

  LinkPiece *piece = areanAllocate(l->arena, sizeof(LinkPiece));
  piece->name = ".bss";
  piece->type = SHT_NOBITS;
  piece->flags = SHF_ALLOC | SHF_WRITE;
  piece->align = align;
  piece->size = size;

  l->commonPiece = piece;
}

static Boolean checkUndefined(Linker *l) {
  // A shared object is entitled to leave a name to whoever loads it, and ld
  // allows the same thing for '-shared' without being asked.
  if (l->options->shared) return TRUE;

  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);
    if (symbol->definition != NULL || symbol->isCommon) continue;
    if (symbol->shared != NULL || symbol->linkerDefined) continue;

    // A weak reference that nothing defines is not an error: it resolves to
    // zero, which is what the code testing '&sym' is asking about.
    if (symbol->strongReference) {
      linkError(l, "undefined symbol '%s'", symbol->name);
    }
  }

  return !l->hadError;
}

/* ----------------------------------------------------------------- layout */

// Which output section an input one is merged into. GCC's '-ffunction-sections'
// names and the compiler's own '.rodata.str1.1' all fold into their parent,
// and anything unrecognised keeps the name it came with rather than being
// guessed at.
static const char *outputSectionName(const char *name) {
  static const char *prefixes[] = {
    ".text", ".rodata", ".data.rel.ro", ".data", ".bss",
    ".preinit_array", ".init_array", ".fini_array", ".init", ".fini",
    ".note", ".eh_frame", ".gcc_except_table"
  };

  unsigned i;
  for (i = 0; i < sizeof prefixes / sizeof prefixes[0]; ++i) {
    const char *prefix = prefixes[i];
    size_t length = strlen(prefix);

    if (strcmp(name, prefix) == 0) return prefix;
    if (strncmp(name, prefix, length) == 0 && name[length] == '.') {
      return prefix;
    }
  }

  return name;
}

static int sectionRank(const char *name) {
  static const struct { const char *name; int rank; } table[] = {
    { ".interp", RANK_INTERP },
    { ".note", RANK_NOTE },
    { ".hash", RANK_HASH },
    { ".dynsym", RANK_DYNSYM },
    { ".dynstr", RANK_DYNSTR },
    { ".rela.dyn", RANK_RELA_DYN },
    { ".rela.plt", RANK_RELA_PLT },
    { ".init", RANK_INIT },
    { ".plt", RANK_PLT },
    { ".text", RANK_TEXT },
    { ".fini", RANK_FINI },
    { ".rodata", RANK_RODATA },
    { ".eh_frame", RANK_EH_FRAME },
    { ".preinit_array", RANK_PREINIT_ARRAY },
    { ".init_array", RANK_INIT_ARRAY },
    { ".fini_array", RANK_FINI_ARRAY },
    { ".data.rel.ro", RANK_DATA_REL_RO },
    // What src/elf.c calls it. Const objects whose initializers needed a
    // relocation, so they belong in the region the loader seals afterwards.
    { "data.rel.ro.local", RANK_DATA_REL_RO },
    { ".dynamic", RANK_DYNAMIC },
    { ".got", RANK_GOT },
    { ".got.plt", RANK_GOT_PLT },
    { ".data", RANK_DATA },
    { ".bss", RANK_BSS }
  };

  unsigned i;
  for (i = 0; i < sizeof table / sizeof table[0]; ++i) {
    if (strcmp(name, table[i].name) == 0) return table[i].rank;
  }

  return RANK_OTHER;
}

static enum SectionClass sectionClass(uint64_t flags) {
  if (flags & SHF_EXECINSTR) return CLASS_TEXT;
  if (flags & SHF_WRITE) return CLASS_DATA;
  return CLASS_RODATA;
}

static OutputSection *outputSectionFor(Linker *l, const LinkPiece *piece) {
  const char *name = outputSectionName(piece->name);

  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    if (strcmp(output->name, name) == 0) return output;
  }

  OutputSection *output = areanAllocate(l->arena, sizeof(OutputSection));
  output->name = name;
  output->type = piece->type;
  output->flags = piece->flags;
  output->align = 1;
  output->rank = sectionRank(name);
  initVector(&output->pieces, 8);

  addToVector(&l->outputs, (intptr_t)output);
  return output;
}

static void addPiece(Linker *l, LinkPiece *piece) {
  OutputSection *output = outputSectionFor(l, piece);

  // One PROGBITS contribution makes the whole output section occupy file
  // space; the flags are the union, so a section written by anyone is
  // writable for everyone.
  if (piece->type != SHT_NOBITS) output->type = piece->type;
  output->flags |= piece->flags;
  if (piece->align > output->align) output->align = piece->align;
  if (piece->entsize != 0) output->entsize = piece->entsize;

  piece->output = output;
  addToVector(&output->pieces, (intptr_t)piece);
}

// A section the link invents. Its bytes are allocated now, when the size is
// finally known, and filled once layout has told it what to say.
static LinkPiece *addSynthetic(Linker *l, const char *name, uint32_t type,
                               uint64_t flags, uint64_t align, uint64_t entsize,
                               uint64_t size, OutputSection **section) {
  LinkPiece *piece = areanAllocate(l->arena, sizeof(LinkPiece));
  piece->name = name;
  piece->type = type;
  piece->flags = flags;
  piece->align = align;
  piece->entsize = entsize;
  piece->size = size;
  if (type != SHT_NOBITS && size != 0) {
    piece->data = areanAllocate(l->arena, (size_t)size);
  }

  addPiece(l, piece);
  if (section != NULL) *section = piece->output;

  return piece;
}

static void gatherPieces(Linker *l) {
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared) continue;

    unsigned i;
    for (i = 0; i < lf->file->sectionCount; ++i) {
      InputSection *section = &lf->file->sections[i];

      if ((section->flags & SHF_ALLOC) == 0) continue;
      if (section->type != SHT_PROGBITS && section->type != SHT_NOBITS &&
          section->type != SHT_INIT_ARRAY && section->type != SHT_FINI_ARRAY &&
          section->type != SHT_PREINIT_ARRAY && section->type != SHT_NOTE) {
        continue;
      }

      // The CET note is the one allocated section that means something only
      // when it is merged, and merging is the property's whole content. The
      // output claims no property rather than claiming every input's.
      if (strcmp(section->name, ".note.gnu.property") == 0) continue;

      LinkPiece *piece = areanAllocate(l->arena, sizeof(LinkPiece));
      piece->input = section;
      piece->owner = lf;
      piece->name = section->name;
      piece->type = section->type;
      piece->flags = section->flags;
      piece->align = section->align != 0 ? section->align : 1;
      piece->size = section->size;
      piece->data = section->type == SHT_NOBITS ? NULL : section->data;

      lf->pieces[i] = piece;
      addPiece(l, piece);
    }
  }

  if (l->commonPiece != NULL) addPiece(l, l->commonPiece);
}

// Output sections in the order they are written: read-only first, because the
// ELF header shares their segment, then code, then data - with the sections
// that occupy no file space last within their own segment, so that .bss can
// be dropped off the end of the file rather than written as zeros.
static void orderOutputs(Linker *l) {
  Vector ordered = { 0 };
  initVector(&ordered, l->outputs.size + 1);

  int class;
  for (class = 0; class < CLASS_COUNT; ++class) {
    int nobits;
    for (nobits = 0; nobits < 2; ++nobits) {
      int rank;
      for (rank = 0; rank < RANK_COUNT; ++rank) {
        size_t i;
        for (i = 0; i < l->outputs.size; ++i) {
          OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
          if ((int)sectionClass(output->flags) != class) continue;
          if ((output->type == SHT_NOBITS) != (nobits != 0)) continue;
          if (output->rank != rank) continue;

          addToVector(&ordered, (intptr_t)output);
        }
      }
    }
  }

  size_t i;
  for (i = 0; i < ordered.size; ++i) {
    putAtVector(&l->outputs, i, getFromVector(&ordered, i));
  }

  releaseVector(&ordered);
}

typedef struct _Segment {
  uint64_t offset;
  uint64_t address;
  uint64_t fileSize;
  uint64_t memSize;
  uint32_t flags;
  Boolean present;
} Segment;

typedef struct _Layout {
  Segment segments[CLASS_COUNT];
  uint64_t fileEnd; // first byte past everything laid out so far
  unsigned phnum;
} Layout;

static uint32_t segmentFlags(enum SectionClass class) {
  switch (class) {
    case CLASS_TEXT: return PF_R | PF_X;
    case CLASS_DATA: return PF_R | PF_W;
    default: return PF_R;
  }
}

static void layoutSections(Linker *l, Layout *layout) {
  // A shared object is mapped wherever the loader puts it, so it is written as
  // though that were address zero and every address in it is really an offset.
  uint64_t base = l->options->shared ? 0 : l->options->imageBase;
  uint64_t offset = 0;

  int class;
  for (class = 0; class < CLASS_COUNT; ++class) {
    Boolean anything = FALSE;

    size_t i;
    for (i = 0; i < l->outputs.size; ++i) {
      OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
      if ((int)sectionClass(output->flags) == class) anything = TRUE;
    }

    // The read-only segment exists even when nothing is read-only: the ELF
    // header and the program headers live in it, and a loader that cannot see
    // them is not going to be able to check anything they say.
    if (!anything && class != CLASS_RODATA) continue;

    Segment *segment = &layout->segments[class];
    segment->present = TRUE;
    segment->flags = segmentFlags((enum SectionClass)class);

    // Every segment starts on a page boundary in both the file and memory, so
    // p_offset and p_vaddr are congruent modulo the page size for free, and
    // the constant difference between them holds for every section inside.
    segment->offset = alignUp(offset, LINK_PAGE_SIZE);
    segment->address = alignUp(base, LINK_PAGE_SIZE) + segment->offset;

    uint64_t cursor = segment->offset;
    if (class == CLASS_RODATA) {
      cursor += sizeof(Elf64_Ehdr) + layout->phnum * sizeof(Elf64_Phdr);
    }

    uint64_t delta = segment->address - segment->offset;
    uint64_t memCursor = cursor;
    Boolean inNobits = FALSE;

    if (class == CLASS_DATA) l->relroStart = segment->address;

    for (i = 0; i < l->outputs.size; ++i) {
      OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
      if ((int)sectionClass(output->flags) != class) continue;

      if (output->type == SHT_NOBITS) inNobits = TRUE;

      // A section that occupies no file space still gets the offset it would
      // have had: sh_offset of .bss is where the next section would start,
      // which is what every tool expects to read there.
      memCursor = alignUp(memCursor, output->align);
      output->address = memCursor + delta;
      output->offset = memCursor;

      uint64_t pieceCursor = memCursor;
      size_t j;
      for (j = 0; j < output->pieces.size; ++j) {
        LinkPiece *piece = (LinkPiece *)getFromVector(&output->pieces, j);

        pieceCursor = alignUp(pieceCursor, piece->align);
        piece->address = pieceCursor + delta;
        piece->offset = pieceCursor;
        pieceCursor += piece->size;
      }

      output->size = pieceCursor - memCursor;
      memCursor = pieceCursor;
      if (!inNobits) cursor = memCursor;

      // Everything the loader relocates is ranked ahead of everything it does
      // not, so the read-only-after-relocation region is a prefix of the
      // writable segment and ends at the last such section.
      if (class == CLASS_DATA && output->rank < RANK_GOT_PLT) {
        l->relroEnd = output->address + output->size;
      }
    }

    segment->fileSize = cursor - segment->offset;
    segment->memSize = memCursor - segment->offset;

    offset = cursor;
  }

  layout->fileEnd = offset;
}

/* ---------------------------------------------------- dynamic bookkeeping */

static uint32_t addDynString(Linker *l, const char *name) {
  if (name == NULL || name[0] == '\0') return 0;

  intptr_t cached = getFromHashMap(l->dynstrMap, (intptr_t)name);
  if (cached != 0) return (uint32_t)(cached - 1);

  uint32_t offset = addString(&l->dynstr, name);
  putToHashMap(l->dynstrMap, (intptr_t)name, (intptr_t)offset + 1);

  return offset;
}

// Every symbol that has to survive to run time gets an index here, and the
// three tables that name symbols - .dynsym, .rela.plt and .rela.dyn - all use
// it. Index zero is the null entry, so it doubles as 'not dynamic'.
static void addDynamicSymbol(Linker *l, LinkSymbol *symbol) {
  if (symbol->dynIndex != 0) return;

  symbol->dynIndex = (unsigned)l->dynsyms.size + 1;
  addToVector(&l->dynsyms, (intptr_t)symbol);
  addDynString(l, symbol->name);

  if (symbol->sharedOwner != NULL) symbol->sharedOwner->needed = TRUE;
}

static void needPlt(Linker *l, LinkSymbol *symbol) {
  if (symbol->needsPlt) return;

  symbol->needsPlt = TRUE;
  symbol->pltIndex = (unsigned)l->plt.size;
  addToVector(&l->plt, (intptr_t)symbol);
  addDynamicSymbol(l, symbol);
}

// A data object a library owns is copied into this executable's .bss, and the
// symbol is then defined here: ld.so writes the library's initial value in
// once, and every use site is an ordinary absolute reference afterwards.
static void needCopy(Linker *l, LinkSymbol *symbol) {
  if (symbol->needsCopy) return;

  const InputSymbol *shared = symbol->shared;
  uint64_t align = 8;

  if (shared->shndx < symbol->sharedOwner->file->sectionCount) {
    uint64_t sectionAlign =
        symbol->sharedOwner->file->sections[shared->shndx].align;
    if (sectionAlign > 0 && sectionAlign <= 64) align = sectionAlign;
  }

  if (shared->size == 0) {
    linkError(l, "cannot copy '%s' out of %s: it has no size", symbol->name,
              symbol->sharedOwner->file->soname);
    return;
  }

  symbol->needsCopy = TRUE;
  symbol->copySize = shared->size;
  symbol->copyAlign = align;
  symbol->size = shared->size;
  addToVector(&l->copies, (intptr_t)symbol);
  addDynamicSymbol(l, symbol);
}

// What a reference to a library's symbol costs depends on what the symbol is:
// code is reached through a stub, data is copied. A name with neither a type
// nor a size is treated as code, since that is what an assembler-written
// entry point looks like and copying nothing would be worse.
static void needSharedSymbol(Linker *l, LinkSymbol *symbol) {
  const InputSymbol *shared = symbol->shared;

  if (shared->type == STT_FUNC || shared->type == STT_GNU_IFUNC ||
      (shared->type == STT_NOTYPE && shared->size == 0)) {
    needPlt(l, symbol);
  } else {
    needCopy(l, symbol);
  }
}

static void needGlobalGot(Linker *l, LinkSymbol *symbol) {
  if (symbol->needsGot) return;

  GotSlot *slot = areanAllocate(l->arena, sizeof(GotSlot));
  slot->global = symbol;
  slot->offset = l->got.size * GOT_ENTRY_SIZE;

  symbol->needsGot = TRUE;
  symbol->gotOffset = slot->offset;
  addToVector(&l->got, (intptr_t)slot);
}

static void needLocalGot(Linker *l, LinkFile *lf, unsigned index) {
  if (lf->localGot[index] != 0) return;

  GotSlot *slot = areanAllocate(l->arena, sizeof(GotSlot));
  slot->file = lf;
  slot->index = index;
  slot->offset = l->got.size * GOT_ENTRY_SIZE;

  lf->localGot[index] = slot->offset + 1;
  addToVector(&l->got, (intptr_t)slot);
}

static enum RelocRole roleOf(const Linker *l, uint32_t type) {
  return l->target->role(type);
}

// What an absolute 64-bit reference costs in a shared object. Asked twice -
// once to size .rela.dyn and once to fill it - so the two cannot disagree
// about how many entries there are.
static enum DynAction absoluteAction(Linker *l, LinkFile *lf, uint32_t index) {
  if (!l->options->shared) return DYN_NONE;
  if (index == 0 || index >= lf->file->symbolCount) return DYN_RELATIVE;

  const InputSymbol *symbol = &lf->file->symbols[index];
  if (symbol->bind == STB_LOCAL) return DYN_RELATIVE;

  LinkSymbol *global = lf->globals[index];
  if (global == NULL) return DYN_RELATIVE;

  return isPreemptible(l, global) ? DYN_SYMBOL : DYN_RELATIVE;
}

// A GOT slot in a shared object is never a constant: either the loader looks
// the name up, or it adds the load address to what the link wrote.
static enum DynAction gotSlotAction(Linker *l, const GotSlot *slot) {
  if (!l->options->shared) return DYN_NONE;
  if (slot->global != NULL && isPreemptible(l, slot->global)) return DYN_SYMBOL;
  return DYN_RELATIVE;
}

// Somewhere the loader has to write, which it can only do to a section that is
// still writable when it runs. Arranging otherwise means DT_TEXTREL, which
// this linker does not emit and ld would not either without being asked.
static void requireWritable(Linker *l, LinkFile *lf, const LinkPiece *piece) {
  if (piece->flags & SHF_WRITE) return;

  linkError(l, "%s: '%s' needs an address written into it at load time, which "
               "a read-only section cannot have", fileName(lf), piece->name);
}

// One walk over every relocation in the link, before anything has an address,
// asking only what each one will need to exist. Layout cannot start until the
// answer is known: the sections that hold those answers are sized by it.
static void scanRelocations(Linker *l) {
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared) continue;

    unsigned i;
    for (i = 0; i < lf->file->sectionCount; ++i) {
      LinkPiece *piece = lf->pieces[i];
      if (piece == NULL || piece->input == NULL) continue;

      size_t j;
      for (j = 0; j < piece->input->relocCount; ++j) {
        const Elf64_Rela *rela = &piece->input->relocs[j];
        uint32_t type = (uint32_t)ELF64_R_TYPE(rela->r_info);
        uint32_t index = (uint32_t)ELF64_R_SYM(rela->r_info);

        if (index == 0 || index >= lf->file->symbolCount) continue;

        const InputSymbol *symbol = &lf->file->symbols[index];
        enum RelocRole role = roleOf(l, type);

        // The low half of a PC-relative pair names the instruction holding the
        // high half rather than a symbol, so everything it needs was arranged
        // when that one was seen.
        if (role == RELOC_ROLE_PAIRED_LO) continue;

        // A 32-bit absolute field cannot hold an address that is not known
        // until load time, and no dynamic relocation would fit in it either.
        if (l->options->shared && role == RELOC_ROLE_ABS32 &&
            symbol->shndx != SHN_ABS) {
          linkError(l, "%s: a 32-bit absolute reference to '%s' in '%s' cannot "
                       "be used when making a shared object", fileName(lf),
                    symbol->name != NULL ? symbol->name : "", piece->name);
          continue;
        }

        if (symbol->bind == STB_LOCAL) {
          if (role == RELOC_ROLE_GOT) needLocalGot(l, lf, index);
          if (l->options->shared && role == RELOC_ROLE_ABS64) {
            requireWritable(l, lf, piece);
            ++l->dynRelocCount;
          }
          continue;
        }

        LinkSymbol *global = lf->globals[index];
        if (global == NULL) continue;

        if (role == RELOC_ROLE_GOT) needGlobalGot(l, global);

        if (l->options->shared) {
          Boolean preempt = isPreemptible(l, global);

          if (role == RELOC_ROLE_GOT && preempt) addDynamicSymbol(l, global);

          switch (role) {
            case RELOC_ROLE_CALL:
              // A call to a name somebody else may define goes through a stub
              // even when this object defines it too - that is the whole of
              // what makes an interposed definition take effect.
              if (preempt) needPlt(l, global);
              break;

            case RELOC_ROLE_ABS64:
              requireWritable(l, lf, piece);
              if (preempt) addDynamicSymbol(l, global);
              ++l->dynRelocCount;
              break;

            case RELOC_ROLE_PCREL:
              if (preempt) {
                linkError(l, "%s: a PC-relative reference to '%s' in '%s' "
                             "cannot be used when making a shared object; "
                             "recompile with '-fPIC'",
                          fileName(lf), global->name, piece->name);
              }
              break;

            default:
              break;
          }
          continue;
        }

        if (!isSharedSymbol(global)) continue;

        switch (role) {
          case RELOC_ROLE_CALL:
            needPlt(l, global);
            break;

          case RELOC_ROLE_ABS64:
          case RELOC_ROLE_ABS32:
          case RELOC_ROLE_PCREL:
          case RELOC_ROLE_GOT:
            needSharedSymbol(l, global);
            break;

          default:
            break;
        }
      }
    }
  }

  // Every GOT slot in a shared object is one more thing for the loader to do,
  // and the last of them was allocated by the walk that just finished.
  if (l->options->shared) l->dynRelocCount += l->got.size;
}

// A shared object is linked against by name, so it has to advertise every
// definition it is willing to answer for - not only the ones this link
// happened to need itself.
static void exportSymbols(Linker *l) {
  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);

    // An executable advertises only the names a library it loaded also knows
    // about - whether the library references them or defines them itself.
    // Either way the program's definition is the one that has to win, and it
    // can only win by being in a table ld.so searches first. A shared object
    // advertises everything it has.
    if (!l->options->shared && symbol->shared == NULL &&
        !symbol->sharedReference) {
      continue;
    }

    if (symbol->definition != NULL) {
      uint8_t visibility = symbol->definition->visibility;
      if (visibility == STV_HIDDEN || visibility == STV_INTERNAL) continue;
    } else if (!symbol->isCommon) {
      // Undefined here: it belongs in .dynsym only when something in this
      // object actually asks the loader about it, which the scan above said.
      // A linker-defined name describes this object's own layout and would be
      // a lie about anyone else's, so it is never exported either.
      continue;
    }

    addDynamicSymbol(l, symbol);
  }
}

static void allocateCopies(Linker *l) {
  if (l->copies.size == 0) return;

  uint64_t size = 0;
  uint64_t align = 1;

  size_t i;
  for (i = 0; i < l->copies.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->copies, i);

    size = alignUp(size, symbol->copyAlign);
    symbol->copyOffset = size;
    size += symbol->copySize;

    if (symbol->copyAlign > align) align = symbol->copyAlign;
  }

  LinkPiece *piece = areanAllocate(l->arena, sizeof(LinkPiece));
  piece->name = ".bss";
  piece->type = SHT_NOBITS;
  piece->flags = SHF_ALLOC | SHF_WRITE;
  piece->align = align;
  piece->size = size;

  l->copyPiece = piece;
}

static uint32_t elfHash(const char *name) {
  uint32_t hash = 0;

  while (*name) {
    hash = (hash << 4) + (unsigned char)*name++;
    uint32_t high = hash & 0xf0000000u;
    if (high != 0) hash ^= high >> 24;
    hash &= ~high;
  }

  return hash;
}

static unsigned hashBucketCount(unsigned chainCount) {
  if (chainCount < 4) return 1;
  return chainCount / 2 + 1;
}

// How many DT_ entries .dynamic can possibly want. Counting exactly would mean
// deciding the contents before layout and again after it; this is sized once,
// filled once, and padded with the DT_NULL that terminates it anyway.
static unsigned dynamicCapacity(Linker *l) {
  unsigned needed = 0;

  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared && lf->needed) ++needed;
  }

  return needed + 32;
}

// The sections nobody supplied. Everything above has decided how big each of
// them is; nothing here can be filled in yet, because all of it is addresses.
static void createDynamicSections(Linker *l) {
  const char *interpreter = l->options->interpreter;

  // A shared object has none: whoever loads it already has an interpreter.
  if (interpreter != NULL) {
    l->interpPiece = addSynthetic(l, ".interp", SHT_PROGBITS, SHF_ALLOC, 1, 0,
                                  strlen(interpreter) + 1, &l->interpSection);
  }

  unsigned chainCount = (unsigned)l->dynsyms.size + 1;
  unsigned bucketCount = hashBucketCount(chainCount);

  l->hashPiece = addSynthetic(l, ".hash", SHT_HASH, SHF_ALLOC, 8, 4,
                              (2 + bucketCount + chainCount) * sizeof(uint32_t),
                              &l->hashSection);

  l->dynsymPiece = addSynthetic(l, ".dynsym", SHT_DYNSYM, SHF_ALLOC, 8,
                                sizeof(Elf64_Sym),
                                chainCount * sizeof(Elf64_Sym),
                                &l->dynsymSection);

  if (l->options->runpath != NULL) addDynString(l, l->options->runpath);
  if (l->options->soname != NULL) addDynString(l, l->options->soname);

  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared && lf->needed) addDynString(l, lf->file->soname);
  }

  l->dynstrPiece = addSynthetic(l, ".dynstr", SHT_STRTAB, SHF_ALLOC, 1, 0,
                                l->dynstr.size, &l->dynstrSection);

  // An executable's only dynamic relocations are its copies: everything else
  // in it has an address the link already knows.
  size_t relaDynCount =
      l->options->shared ? l->dynRelocCount : l->copies.size;

  if (relaDynCount != 0) {
    l->relaDynPiece = addSynthetic(l, ".rela.dyn", SHT_RELA, SHF_ALLOC, 8,
                                   sizeof(Elf64_Rela),
                                   relaDynCount * sizeof(Elf64_Rela),
                                   &l->relaDynSection);
  }

  if (l->plt.size != 0) {
    l->relaPltPiece = addSynthetic(l, ".rela.plt", SHT_RELA,
                                   SHF_ALLOC | SHF_INFO_LINK, 8,
                                   sizeof(Elf64_Rela),
                                   l->plt.size * sizeof(Elf64_Rela),
                                   &l->relaPltSection);

    l->pltPiece = addSynthetic(l, ".plt", SHT_PROGBITS,
                               SHF_ALLOC | SHF_EXECINSTR, l->target->pltAlign,
                               l->target->pltEntrySize,
                               l->target->plt0Size +
                                   l->plt.size * l->target->pltEntrySize,
                               &l->pltSection);
  }

  if (l->got.size != 0) {
    l->gotPiece = addSynthetic(l, ".got", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                               8, GOT_ENTRY_SIZE,
                               l->got.size * GOT_ENTRY_SIZE, &l->gotSection);
  }

  // .got.plt exists even with no stubs in it: its first word is where the
  // loader finds .dynamic, and _GLOBAL_OFFSET_TABLE_ points at its start.
  l->gotPltPiece = addSynthetic(l, ".got.plt", SHT_PROGBITS,
                                SHF_ALLOC | SHF_WRITE, 8, GOT_ENTRY_SIZE,
                                (l->target->gotPltReserved + l->plt.size) *
                                    GOT_ENTRY_SIZE,
                                &l->gotPltSection);

  l->dynamicPiece = addSynthetic(l, ".dynamic", SHT_DYNAMIC,
                                 SHF_ALLOC | SHF_WRITE, 8, sizeof(Elf64_Dyn),
                                 dynamicCapacity(l) * sizeof(Elf64_Dyn),
                                 &l->dynamicSection);

  if (l->copyPiece != NULL) addPiece(l, l->copyPiece);
}

/* ------------------------------------------------------- symbol addresses */

static OutputSection *findOutput(Linker *l, const char *name) {
  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    if (strcmp(output->name, name) == 0) return output;
  }

  return NULL;
}

static void sectionBounds(Linker *l, const char *name, uint64_t fallback,
                          uint64_t *start, uint64_t *end) {
  OutputSection *output = findOutput(l, name);

  *start = output != NULL ? output->address : fallback;
  *end = output != NULL ? output->address + output->size : fallback;
}

static uint64_t linkerSymbolValue(Linker *l, const Layout *layout,
                                  const char *name) {
  uint64_t dataEnd = 0;   // past the last section with bytes in the file
  uint64_t memoryEnd = 0; // past everything mapped
  uint64_t textEnd = 0;
  uint64_t bssStart = 0;

  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    uint64_t end = output->address + output->size;

    if (end > memoryEnd) memoryEnd = end;
    if (output->type != SHT_NOBITS && end > dataEnd) dataEnd = end;
    else if (output->type == SHT_NOBITS && bssStart == 0) {
      bssStart = output->address;
    }
    if ((output->flags & SHF_EXECINSTR) && end > textEnd) textEnd = end;
  }

  // ld reads these two off the same point in its script and they come out
  // equal, because .data and .bss are adjacent there. They are not adjacent
  // here when nothing is initialized, so each is asked its own question.
  if (bssStart == 0) bssStart = dataEnd;

  if (strcmp(name, "__ehdr_start") == 0 ||
      strcmp(name, "__executable_start") == 0) {
    return layout->segments[CLASS_RODATA].address;
  }

  if (strcmp(name, "_GLOBAL_OFFSET_TABLE_") == 0) {
    if (l->gotPltSection != NULL) return l->gotPltSection->address;
    if (l->gotSection != NULL) return l->gotSection->address;
    return 0;
  }

  if (strcmp(name, "_DYNAMIC") == 0) {
    return l->dynamicSection != NULL ? l->dynamicSection->address : 0;
  }

  // ld's riscv64 script puts this 2KiB into the small-data area, which is the
  // middle of a signed 12-bit reach either way. Nothing this linker writes
  // addresses through gp - it never relaxes a reference into doing so - so
  // what matters is that the name resolves at all.
  if (strcmp(name, "__global_pointer$") == 0) {
    return layout->segments[CLASS_DATA].address + 0x800;
  }

  uint64_t start = 0, end = 0;

  if (strncmp(name, "__preinit_array_", 16) == 0) {
    sectionBounds(l, ".preinit_array", dataEnd, &start, &end);
    return name[16] == 's' ? start : end;
  }

  if (strncmp(name, "__init_array_", 13) == 0) {
    sectionBounds(l, ".init_array", dataEnd, &start, &end);
    return name[13] == 's' ? start : end;
  }

  if (strncmp(name, "__fini_array_", 13) == 0) {
    sectionBounds(l, ".fini_array", dataEnd, &start, &end);
    return name[13] == 's' ? start : end;
  }

  if (strcmp(name, "etext") == 0 || strcmp(name, "_etext") == 0) {
    return textEnd;
  }

  if (strcmp(name, "end") == 0 || strcmp(name, "_end") == 0) return memoryEnd;
  if (strcmp(name, "__bss_start") == 0) return bssStart;

  return dataEnd; // '_edata' and 'edata'
}

static uint64_t pltEntryAddress(Linker *l, const LinkSymbol *symbol) {
  return l->pltSection->address + l->target->plt0Size +
         (uint64_t)symbol->pltIndex * l->target->pltEntrySize;
}

static Boolean resolveSymbolAddresses(Linker *l, const Layout *layout) {
  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);

    if (symbol->definition != NULL) {
      const InputSymbol *definition = symbol->definition;

      if (definition->shndx == SHN_ABS) {
        symbol->address = definition->value;
        symbol->resolved = TRUE;
        continue;
      }

      if (definition->shndx >= symbol->owner->file->sectionCount) {
        linkError(l, "%s: '%s' is defined in section %u, which does not exist",
                  fileName(symbol->owner), symbol->name, definition->shndx);
        continue;
      }

      LinkPiece *piece = symbol->owner->pieces[definition->shndx];
      if (piece == NULL) {
        linkError(l, "%s: '%s' is defined in '%s', which is not allocated",
                  fileName(symbol->owner), symbol->name,
                  symbol->owner->file->sections[definition->shndx].name);
        continue;
      }

      symbol->address = piece->address + definition->value;
      symbol->resolved = TRUE;
      continue;
    }

    if (symbol->isCommon) {
      symbol->address = l->commonPiece->address + symbol->commonOffset;
      symbol->resolved = TRUE;
      continue;
    }

    if (symbol->linkerDefined) {
      symbol->address = linkerSymbolValue(l, layout, symbol->name);
      symbol->resolved = TRUE;
      continue;
    }

    // A library's symbol has an address here, not there: the stub this link
    // wrote, or the copy it made. Both are what every reference in this
    // executable has to agree on, including one that takes its address.
    if (symbol->needsCopy) {
      symbol->address = l->copyPiece->address + symbol->copyOffset;
      symbol->resolved = TRUE;
      continue;
    }

    if (symbol->needsPlt) {
      symbol->address = pltEntryAddress(l, symbol);
      symbol->resolved = TRUE;
      continue;
    }

    // Left undefined and weakly referenced: zero is the answer.
    symbol->address = 0;
    symbol->resolved = TRUE;
  }

  return !l->hadError;
}

static Boolean localSymbolAddress(Linker *l, LinkFile *lf,
                                  const InputSymbol *symbol, uint64_t *result) {
  if (symbol->shndx == SHN_ABS) {
    *result = symbol->value;
    return TRUE;
  }

  if (symbol->shndx == SHN_UNDEF || symbol->shndx >= lf->file->sectionCount) {
    linkError(l, "%s: local symbol '%s' is not defined here", fileName(lf),
              symbol->name != NULL ? symbol->name : "");
    return FALSE;
  }

  LinkPiece *piece = lf->pieces[symbol->shndx];
  if (piece == NULL) {
    linkError(l, "%s: '%s' is defined in '%s', which is not allocated",
              fileName(lf), symbol->name != NULL ? symbol->name : "",
              lf->file->sections[symbol->shndx].name);
    return FALSE;
  }

  *result = piece->address + symbol->value;
  return TRUE;
}

/* ------------------------------------------------------- dynamic contents */

static void fillPlt(Linker *l) {
  if (l->pltPiece == NULL) return;

  l->target->writePlt((uint8_t *)l->pltPiece->data, l->pltPiece->address,
                      (uint8_t *)l->gotPltPiece->data,
                      l->gotPltPiece->address, l->plt.size);
}

static void fillGot(Linker *l) {
  if (l->gotPltPiece != NULL) {
    write64((uint8_t *)l->gotPltPiece->data, l->dynamicPiece->address);
  }

  if (l->gotPiece == NULL) return;

  uint8_t *got = (uint8_t *)l->gotPiece->data;

  size_t i;
  for (i = 0; i < l->got.size; ++i) {
    GotSlot *slot = (GotSlot *)getFromVector(&l->got, i);
    uint64_t value = 0;

    if (slot->global != NULL) {
      value = slot->global->address;
    } else {
      const InputSymbol *symbol = &slot->file->file->symbols[slot->index];
      if (!localSymbolAddress(l, slot->file, symbol, &value)) continue;
    }

    // Every address in this executable is fixed at link time, so a GOT entry
    // is a constant and not something ld.so has to be asked about. What it
    // holds for a library's function is the stub, which is the address this
    // program uses for that function everywhere else.
    write64(got + slot->offset, value);
  }
}

static void fillDynamicSymbols(Linker *l) {
  Elf64_Sym *symbols = (Elf64_Sym *)l->dynsymPiece->data;
  memset(symbols, 0, (size_t)l->dynsymPiece->size);

  size_t i;
  for (i = 0; i < l->dynsyms.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->dynsyms, i);
    Elf64_Sym *entry = &symbols[symbol->dynIndex];

    uint8_t bind = STB_GLOBAL;
    uint8_t type = STT_NOTYPE;
    uint16_t shndx = SHN_UNDEF;
    uint64_t value = 0;
    uint64_t size = 0;

    if (symbol->definition != NULL) {
      const InputSymbol *definition = symbol->definition;

      bind = symbol->weakDefinition ? STB_WEAK : STB_GLOBAL;
      type = definition->type;
      value = symbol->address;
      size = symbol->size;
      shndx = SHN_ABS;

      if (definition->shndx != SHN_ABS) {
        LinkPiece *piece = symbol->owner->pieces[definition->shndx];
        if (piece != NULL) shndx = (uint16_t)piece->output->index;
      }
    } else if (symbol->isCommon) {
      type = STT_OBJECT;
      shndx = (uint16_t)l->commonPiece->output->index;
      value = symbol->address;
      size = symbol->size;
    } else if (symbol->needsCopy) {
      // A copied object is defined *here* now, and has to be advertised that
      // way: the library binds its own references to this address too.
      type = symbol->shared->type;
      shndx = (uint16_t)l->copyPiece->output->index;
      value = symbol->address;
      size = symbol->copySize;
    } else if (symbol->linkerDefined) {
      shndx = SHN_ABS;
      value = symbol->address;
    } else {
      // Left to the loader. A name only weakly referenced has to say so, or
      // ld.so refuses to start a program that never needed it.
      if (symbol->shared != NULL) type = symbol->shared->type;
      if (!symbol->strongReference) bind = STB_WEAK;
    }

    entry->st_name = addDynString(l, symbol->name);
    entry->st_info = (uint8_t)((bind << 4) | type);
    entry->st_other = STV_DEFAULT;
    entry->st_shndx = shndx;
    entry->st_value = value;
    entry->st_size = size;
  }

  uint32_t *hash = (uint32_t *)l->hashPiece->data;
  unsigned chainCount = (unsigned)l->dynsyms.size + 1;
  unsigned bucketCount = hashBucketCount(chainCount);

  hash[0] = bucketCount;
  hash[1] = chainCount;

  uint32_t *buckets = hash + 2;
  uint32_t *chains = buckets + bucketCount;

  unsigned j;
  for (j = 0; j < bucketCount; ++j) buckets[j] = STN_UNDEF;
  for (j = 0; j < chainCount; ++j) chains[j] = STN_UNDEF;

  for (i = 0; i < l->dynsyms.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->dynsyms, i);
    unsigned bucket = elfHash(symbol->name) % bucketCount;

    chains[symbol->dynIndex] = buckets[bucket];
    buckets[bucket] = symbol->dynIndex;
  }
}

static void addDynRela(Elf64_Rela *entries, size_t *at, uint64_t offset,
                       uint64_t info, int64_t addend) {
  entries[*at].r_offset = offset;
  entries[*at].r_info = info;
  entries[*at].r_addend = addend;
  ++*at;
}

// Everything a shared object leaves for the loader. Walked twice, RELATIVE
// first: DT_RELACOUNT says how many of those there are rather than which,
// so they are only skippable if they come first.
static void emitDynamicRelocations(Linker *l, Boolean relative,
                                   Elf64_Rela *entries, size_t *at) {
  size_t i;
  for (i = 0; i < l->got.size; ++i) {
    GotSlot *slot = (GotSlot *)getFromVector(&l->got, i);
    uint64_t where = l->gotPiece->address + slot->offset;

    if (gotSlotAction(l, slot) == DYN_SYMBOL) {
      if (!relative) {
        addDynRela(entries, at, where,
                   ELF64_R_INFO(slot->global->dynIndex, l->target->relocGlobDat),
                   0);
      }
      continue;
    }

    if (!relative) continue;

    uint64_t value = 0;
    if (slot->global != NULL) {
      value = slot->global->address;
    } else {
      const InputSymbol *symbol = &slot->file->file->symbols[slot->index];
      if (!localSymbolAddress(l, slot->file, symbol, &value)) continue;
    }

    addDynRela(entries, at, where, ELF64_R_INFO(0, l->target->relocRelative),
               (int64_t)value);
  }

  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared) continue;

    unsigned s;
    for (s = 0; s < lf->file->sectionCount; ++s) {
      LinkPiece *piece = lf->pieces[s];
      if (piece == NULL || piece->input == NULL) continue;

      size_t j;
      for (j = 0; j < piece->input->relocCount; ++j) {
        const Elf64_Rela *rela = &piece->input->relocs[j];
        if ((uint32_t)ELF64_R_TYPE(rela->r_info) != l->target->relocAbs64) continue;

        uint32_t index = (uint32_t)ELF64_R_SYM(rela->r_info);
        uint64_t where = piece->address + rela->r_offset;

        if (absoluteAction(l, lf, index) == DYN_SYMBOL) {
          if (relative) continue;

          LinkSymbol *global = lf->globals[index];
          addDynRela(entries, at, where,
                     ELF64_R_INFO(global->dynIndex, l->target->relocAbs64),
                     rela->r_addend);
          continue;
        }

        if (!relative) continue;

        // The loader adds where the object landed, so what goes in the addend
        // is the whole answer the link would have written into an executable.
        uint64_t S = 0;
        if (index != 0 && index < lf->file->symbolCount) {
          const InputSymbol *symbol = &lf->file->symbols[index];

          if (symbol->bind == STB_LOCAL) {
            if (!localSymbolAddress(l, lf, symbol, &S)) continue;
          } else if (lf->globals[index] != NULL) {
            S = lf->globals[index]->address;
          }
        }

        addDynRela(entries, at, where, ELF64_R_INFO(0, l->target->relocRelative),
                   (int64_t)(S + (uint64_t)rela->r_addend));
      }
    }
  }
}

static void fillDynamicRelocations(Linker *l) {
  if (l->relaPltPiece != NULL) {
    Elf64_Rela *entries = (Elf64_Rela *)l->relaPltPiece->data;

    size_t i;
    for (i = 0; i < l->plt.size; ++i) {
      LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->plt, i);

      entries[i].r_offset = l->gotPltPiece->address +
                            GOT_ENTRY_SIZE * (l->target->gotPltReserved + i);
      entries[i].r_info =
          ELF64_R_INFO(symbol->dynIndex, l->target->relocJumpSlot);
      entries[i].r_addend = 0;
    }
  }

  if (l->relaDynPiece == NULL) return;

  Elf64_Rela *entries = (Elf64_Rela *)l->relaDynPiece->data;
  size_t capacity = (size_t)(l->relaDynPiece->size / sizeof(Elf64_Rela));
  size_t at = 0;

  if (l->options->shared) {
    emitDynamicRelocations(l, TRUE, entries, &at);
    l->relativeCount = at;
    emitDynamicRelocations(l, FALSE, entries, &at);
  } else {
    size_t i;
    for (i = 0; i < l->copies.size; ++i) {
      LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->copies, i);
      addDynRela(entries, &at, symbol->address,
                 ELF64_R_INFO(symbol->dynIndex, l->target->relocCopy), 0);
    }
  }

  assert(at <= capacity);

  // Short only when something above already failed, and a link that reported
  // an error writes nothing - but a half-filled table must still parse.
  while (at < capacity) {
    addDynRela(entries, &at, 0, ELF64_R_INFO(0, l->target->relocNone), 0);
  }
}

static void fillDynamicSection(Linker *l) {
  Elf64_Dyn *entries = (Elf64_Dyn *)l->dynamicPiece->data;
  size_t capacity = (size_t)(l->dynamicPiece->size / sizeof(Elf64_Dyn));
  size_t count = 0;

#define DYN(tag, value)                                                        \
  do {                                                                         \
    assert(count < capacity);                                                  \
    entries[count].d_tag = (tag);                                              \
    entries[count].d_un.d_val = (value);                                       \
    ++count;                                                                   \
  } while (0)

  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared && lf->needed) {
      DYN(DT_NEEDED, addDynString(l, lf->file->soname));
    }
  }

  if (l->options->soname != NULL) {
    DYN(DT_SONAME, addDynString(l, l->options->soname));
  }

  if (l->options->runpath != NULL) {
    DYN(DT_RUNPATH, addDynString(l, l->options->runpath));
  }

  DYN(DT_HASH, l->hashPiece->address);
  DYN(DT_STRTAB, l->dynstrPiece->address);
  DYN(DT_SYMTAB, l->dynsymPiece->address);
  DYN(DT_STRSZ, l->dynstr.size);
  DYN(DT_SYMENT, sizeof(Elf64_Sym));

  LinkSymbol *init = (LinkSymbol *)getFromHashMap(l->globalMap, (intptr_t)"_init");
  LinkSymbol *fini = (LinkSymbol *)getFromHashMap(l->globalMap, (intptr_t)"_fini");
  if (init != NULL && init->definition != NULL) DYN(DT_INIT, init->address);
  if (fini != NULL && fini->definition != NULL) DYN(DT_FINI, fini->address);

  static const struct { const char *name; int arrayTag; int sizeTag; } arrays[] = {
    { ".preinit_array", DT_PREINIT_ARRAY, DT_PREINIT_ARRAYSZ },
    { ".init_array", DT_INIT_ARRAY, DT_INIT_ARRAYSZ },
    { ".fini_array", DT_FINI_ARRAY, DT_FINI_ARRAYSZ }
  };

  unsigned i;
  for (i = 0; i < sizeof arrays / sizeof arrays[0]; ++i) {
    OutputSection *output = findOutput(l, arrays[i].name);
    if (output == NULL || output->size == 0) continue;

    DYN(arrays[i].arrayTag, output->address);
    DYN(arrays[i].sizeTag, output->size);
  }

  if (l->relaDynPiece != NULL) {
    DYN(DT_RELA, l->relaDynPiece->address);
    DYN(DT_RELASZ, l->relaDynPiece->size);
    DYN(DT_RELAENT, sizeof(Elf64_Rela));

    // The leading run the loader can apply without looking at a symbol, which
    // is why they were sorted to the front.
    if (l->relativeCount != 0) DYN(DT_RELACOUNT, l->relativeCount);
  }

  if (l->relaPltPiece != NULL) {
    DYN(DT_PLTRELSZ, l->relaPltPiece->size);
    DYN(DT_PLTREL, DT_RELA);
    DYN(DT_JMPREL, l->relaPltPiece->address);
  }

  DYN(DT_PLTGOT, l->gotPltPiece->address);

  // The loader writes the address of its own link map here, which is the only
  // way a debugger attached to a running program finds the shared libraries.
  DYN(DT_DEBUG, 0);
  DYN(DT_NULL, 0);

#undef DYN

  while (count < capacity) {
    entries[count].d_tag = DT_NULL;
    entries[count].d_un.d_val = 0;
    ++count;
  }
}

static void fillDynamicContents(Linker *l) {
  fillPlt(l);
  fillGot(l);
  fillDynamicSymbols(l);
  fillDynamicRelocations(l);
  fillDynamicSection(l);

  if (l->interpPiece != NULL) {
    memcpy((void *)l->interpPiece->data, l->options->interpreter,
           (size_t)l->interpPiece->size);
  }

  // Every name above was interned before .dynstr was sized, so asking for one
  // again cannot have grown it - and the section reserved exactly this much.
  assert(l->dynstr.size == l->dynstrPiece->size);
  memcpy((void *)l->dynstrPiece->data, l->dynstr.blob, l->dynstr.size);
}

/* ------------------------------------------------------------ relocations */

// What a relocation names, once the link has answered for it. Resolved in one
// place because RISC-V's paired low half has to ask the same questions about a
// relocation that is not the one being applied.
typedef struct _RelocOperands {
  uint32_t type;
  const char *name;
  uint64_t S;        // the symbol
  int64_t A;         // the addend
  uint64_t P;        // where the field is
  uint64_t G;        // its GOT entry, when it has one
  Boolean haveGot;
  LinkSymbol *global; // NULL for a local symbol or no symbol at all
} RelocOperands;

static Boolean resolveRelocOperands(Linker *l, LinkPiece *piece,
                                    const Elf64_Rela *rela,
                                    RelocOperands *o) {
  LinkFile *lf = piece->owner;
  uint32_t index = (uint32_t)ELF64_R_SYM(rela->r_info);

  memset(o, 0, sizeof *o);
  o->type = (uint32_t)ELF64_R_TYPE(rela->r_info);
  o->name = "";
  o->A = rela->r_addend;
  o->P = piece->address + rela->r_offset;

  if (index == 0) return TRUE;

  if (index >= lf->file->symbolCount) {
    linkError(l, "%s: a relocation in '%s' names symbol %u, which does not "
                 "exist", fileName(lf), piece->name, index);
    return FALSE;
  }

  const InputSymbol *symbol = &lf->file->symbols[index];
  if (symbol->name != NULL) o->name = symbol->name;

  if (symbol->bind == STB_LOCAL) {
    if (!localSymbolAddress(l, lf, symbol, &o->S)) return FALSE;
    if (lf->localGot[index] != 0) {
      o->haveGot = TRUE;
      o->G = l->gotPiece->address + lf->localGot[index] - 1;
    }
    return TRUE;
  }

  o->global = lf->globals[index];
  if (o->global == NULL || !o->global->resolved) {
    linkError(l, "%s: '%s' is not resolved", fileName(lf), o->name);
    return FALSE;
  }

  o->S = o->global->address;
  if (o->global->needsGot) {
    o->haveGot = TRUE;
    o->G = l->gotPiece->address + o->global->gotOffset;
  }
  return TRUE;
}

// Where a call goes. In a shared object a name this object defines may still
// be answered for by somebody else, and the stub is where that gets decided.
static uint64_t callTarget(Linker *l, const RelocOperands *o) {
  if (o->global != NULL && o->global->needsPlt) return pltEntryAddress(l, o->global);
  return o->S;
}

/* ------------------------------------------------------ x86-64 relocations */

static void applyX86Relocation(Linker *l, LinkPiece *piece, uint8_t *where,
                               const RelocOperands *o) {
  LinkFile *lf = piece->owner;
  const char *name = o->name;
  int64_t A = o->A;
  uint64_t P = o->P;

  switch (o->type) {
    case R_X86_64_64:
      write64(where, o->S + (uint64_t)A);
      return;

    // A call to a symbol this link defines needs no stub, so the two are the
    // same arithmetic. When the symbol came from a library, S is already the
    // stub's address and they stay the same arithmetic.
    case R_X86_64_PC32:
    case R_X86_64_PLT32: {
      uint64_t target =
          o->type == R_X86_64_PLT32 ? callTarget(l, o) : o->S;

      int64_t value = (int64_t)(target + (uint64_t)A) - (int64_t)P;
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: '%s' is too far from '%s' for a 32-bit displacement",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    case R_X86_64_32: {
      uint64_t value = o->S + (uint64_t)A;
      if (value > (uint64_t)RELOC_U32_MAX) {
        linkError(l, "%s: '%s' does not fit in the 32 bits '%s' has for it",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)value);
      return;
    }

    case R_X86_64_32S: {
      int64_t value = (int64_t)(o->S + (uint64_t)A);
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: '%s' does not fit in the 32 bits '%s' has for it",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    // The operand is the GOT entry, not the symbol: the instruction loads an
    // address out of memory rather than computing one.
    case R_X86_64_GOTPCREL:
    case R_X86_64_GOTPCRELX:
    case R_X86_64_REX_GOTPCRELX: {
      if (!o->haveGot) {
        // A link with no loader has no GOT to build one in: only the dynamic
        // half of this file allocates slots, and it is not running.
        linkError(l, "%s: relocation type %u against '%s' is not supported yet",
                  fileName(lf), o->type, name);
        return;
      }

      int64_t value = (int64_t)(o->G + (uint64_t)A) - (int64_t)P;
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: the GOT entry for '%s' is too far from '%s'",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    case R_X86_64_GOTPC32: {
      uint64_t got = l->gotPltSection != NULL ? l->gotPltSection->address : 0;
      int64_t value = (int64_t)(got + (uint64_t)A) - (int64_t)P;
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: the GOT is too far from '%s'", fileName(lf),
                  piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    default:
      linkError(l, "%s: relocation type %u against '%s' is not supported yet",
                fileName(lf), o->type, name);
      return;
  }
}

/* ----------------------------------------------------- riscv64 relocations */

// RISC-V splices an immediate through the instruction rather than laying it
// out in one field, so every one of these rewrites bits and leaves the rest of
// the word alone.

static uint32_t read32(const uint8_t *where) {
  return (uint32_t)where[0] | ((uint32_t)where[1] << 8) |
         ((uint32_t)where[2] << 16) | ((uint32_t)where[3] << 24);
}

static uint64_t read64(const uint8_t *where) {
  return (uint64_t)read32(where) | ((uint64_t)read32(where + 4) << 32);
}

static uint16_t read16(const uint8_t *where) {
  return (uint16_t)((uint32_t)where[0] | ((uint32_t)where[1] << 8));
}

static void write16(uint8_t *where, uint16_t value) {
  where[0] = (uint8_t)value;
  where[1] = (uint8_t)(value >> 8);
}

// A 12-bit signed low half and the 20-bit high half that carries its sign.
static uint32_t riscvHigh20(int64_t value) {
  return (uint32_t)(((uint64_t)value + 0x800u) & 0xfffff000u);
}

static int32_t riscvLow12(int64_t value) {
  return (int32_t)(value - (int64_t)(int32_t)riscvHigh20(value));
}

static void patchU(uint8_t *where, int64_t value) {
  write32(where, (read32(where) & 0x00000fffu) | riscvHigh20(value));
}

static void patchI(uint8_t *where, int64_t value) {
  write32(where, (read32(where) & 0x000fffffu) |
                     (((uint32_t)(int32_t)value & 0xfffu) << 20));
}

static void patchS(uint8_t *where, int64_t value) {
  uint32_t imm = (uint32_t)(int32_t)value & 0xfffu;
  write32(where, (read32(where) & 0x01fff07fu) | ((imm >> 5) << 25) |
                     ((imm & 0x1fu) << 7));
}

static void patchB(uint8_t *where, int64_t offset) {
  uint32_t imm = (uint32_t)(int32_t)offset;
  uint32_t encoded = (((imm >> 12) & 1u) << 31) | (((imm >> 5) & 0x3fu) << 25) |
                     (((imm >> 1) & 0xfu) << 8) | (((imm >> 11) & 1u) << 7);
  write32(where, (read32(where) & 0x01fff07fu) | encoded);
}

static void patchJ(uint8_t *where, int64_t offset) {
  uint32_t imm = (uint32_t)(int32_t)offset;
  uint32_t encoded = (((imm >> 20) & 1u) << 31) | (((imm >> 1) & 0x3ffu) << 21) |
                     (((imm >> 11) & 1u) << 20) | (((imm >> 12) & 0xffu) << 12);
  write32(where, (read32(where) & 0x00000fffu) | encoded);
}

static void patchRvcBranch(uint8_t *where, int64_t offset) {
  uint32_t imm = (uint32_t)(int32_t)offset;
  uint32_t encoded = (((imm >> 8) & 1u) << 12) | (((imm >> 3) & 3u) << 10) |
                     (((imm >> 6) & 3u) << 5) | (((imm >> 1) & 3u) << 3) |
                     (((imm >> 5) & 1u) << 2);
  write16(where, (uint16_t)((read16(where) & 0xe383u) | encoded));
}

static void patchRvcJump(uint8_t *where, int64_t offset) {
  uint32_t imm = (uint32_t)(int32_t)offset;
  uint32_t encoded = (((imm >> 11) & 1u) << 12) | (((imm >> 4) & 1u) << 11) |
                     (((imm >> 8) & 3u) << 9) | (((imm >> 10) & 1u) << 8) |
                     (((imm >> 6) & 1u) << 7) | (((imm >> 7) & 1u) << 6) |
                     (((imm >> 1) & 7u) << 3) | (((imm >> 5) & 1u) << 2);
  write16(where, (uint16_t)((read16(where) & 0xe003u) | encoded));
}

static Boolean fitsSigned(int64_t value, unsigned bits) {
  int64_t limit = (int64_t)1 << (bits - 1);
  return value >= -limit && value < limit;
}

// The low half of a PC-relative pair names the auipc that carries the high
// half, not the symbol - so the answer is whatever *that* relocation resolved
// to, and it has to be worked out a second time here.
static Boolean riscvPairedOffset(Linker *l, LinkPiece *piece,
                                 uint64_t auipcAddress, int64_t *offset) {
  if (auipcAddress < piece->address) return FALSE;
  uint64_t at = auipcAddress - piece->address;

  size_t j;
  for (j = 0; j < piece->input->relocCount; ++j) {
    const Elf64_Rela *high = &piece->input->relocs[j];
    if (high->r_offset != at) continue;

    uint32_t type = (uint32_t)ELF64_R_TYPE(high->r_info);
    if (type != R_RISCV_PCREL_HI20 && type != R_RISCV_GOT_HI20) continue;

    RelocOperands o;
    if (!resolveRelocOperands(l, piece, high, &o)) return FALSE;

    if (type == R_RISCV_GOT_HI20) {
      if (!o.haveGot) return FALSE;
      *offset = (int64_t)o.G - (int64_t)auipcAddress;
    } else {
      *offset = (int64_t)(o.S + (uint64_t)o.A) - (int64_t)auipcAddress;
    }
    return TRUE;
  }

  return FALSE;
}

static void applyRiscvRelocation(Linker *l, LinkPiece *piece, uint8_t *where,
                                 const RelocOperands *o) {
  LinkFile *lf = piece->owner;
  const char *name = o->name;
  uint64_t V = o->S + (uint64_t)o->A; // the plain answer, for everything absolute

  switch (o->type) {
    // Annotations. This linker never relaxes, so nothing shrinks and the
    // padding an assembler already emitted stays exactly the right size.
    case R_RISCV_RELAX:
    case R_RISCV_ALIGN:
      return;

    case R_RISCV_64:
      write64(where, V);
      return;

    case R_RISCV_32: {
      if (!fitsSigned((int64_t)V, 32) && V > (uint64_t)RELOC_U32_MAX) {
        linkError(l, "%s: '%s' does not fit in the 32 bits '%s' has for it",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)V);
      return;
    }

    // medlow: the address is computed outright, in two halves, and each half
    // names the symbol itself. Both must round the same way, which is what
    // riscvHigh20 and riscvLow12 are for.
    case R_RISCV_HI20:
      patchU(where, (int64_t)V);
      return;

    case R_RISCV_LO12_I:
      patchI(where, riscvLow12((int64_t)V));
      return;

    case R_RISCV_LO12_S:
      patchS(where, riscvLow12((int64_t)V));
      return;

    // One entry covers the auipc and the jalr after it.
    case R_RISCV_CALL:
    case R_RISCV_CALL_PLT: {
      int64_t offset =
          (int64_t)(callTarget(l, o) + (uint64_t)o->A) - (int64_t)o->P;
      if (!fitsSigned(offset, 32)) {
        linkError(l, "%s: '%s' is too far from '%s' for a call",
                  fileName(lf), name, piece->name);
        return;
      }
      patchU(where, offset);
      patchI(where + 4, riscvLow12(offset));
      return;
    }

    case R_RISCV_JAL: {
      int64_t offset =
          (int64_t)(callTarget(l, o) + (uint64_t)o->A) - (int64_t)o->P;
      if (!fitsSigned(offset, 21)) {
        linkError(l, "%s: '%s' is more than a megabyte from '%s', which is as "
                     "far as a jal reaches", fileName(lf), name, piece->name);
        return;
      }
      patchJ(where, offset);
      return;
    }

    case R_RISCV_BRANCH: {
      int64_t offset = (int64_t)V - (int64_t)o->P;
      if (!fitsSigned(offset, 13)) {
        linkError(l, "%s: '%s' is too far from '%s' for a branch",
                  fileName(lf), name, piece->name);
        return;
      }
      patchB(where, offset);
      return;
    }

    case R_RISCV_RVC_BRANCH: {
      int64_t offset = (int64_t)V - (int64_t)o->P;
      if (!fitsSigned(offset, 9)) {
        linkError(l, "%s: '%s' is too far from '%s' for a compressed branch",
                  fileName(lf), name, piece->name);
        return;
      }
      patchRvcBranch(where, offset);
      return;
    }

    case R_RISCV_RVC_JUMP: {
      int64_t offset = (int64_t)V - (int64_t)o->P;
      if (!fitsSigned(offset, 12)) {
        linkError(l, "%s: '%s' is too far from '%s' for a compressed jump",
                  fileName(lf), name, piece->name);
        return;
      }
      patchRvcJump(where, offset);
      return;
    }

    case R_RISCV_PCREL_HI20: {
      int64_t offset = (int64_t)V - (int64_t)o->P;
      if (!fitsSigned(offset, 32)) {
        linkError(l, "%s: '%s' is too far from '%s' for a pc-relative "
                     "reference", fileName(lf), name, piece->name);
        return;
      }
      patchU(where, offset);
      return;
    }

    case R_RISCV_GOT_HI20: {
      if (!o->haveGot) {
        linkError(l, "%s: relocation type %u against '%s' is not supported yet",
                  fileName(lf), o->type, name);
        return;
      }
      int64_t offset = (int64_t)o->G - (int64_t)o->P;
      if (!fitsSigned(offset, 32)) {
        linkError(l, "%s: the GOT entry for '%s' is too far from '%s'",
                  fileName(lf), name, piece->name);
        return;
      }
      patchU(where, offset);
      return;
    }

    case R_RISCV_PCREL_LO12_I:
    case R_RISCV_PCREL_LO12_S: {
      int64_t offset = 0;
      if (!riscvPairedOffset(l, piece, o->S, &offset)) {
        linkError(l, "%s: a %s in '%s' has no auipc to pair with",
                  fileName(lf), l->target->relocName(o->type), piece->name);
        return;
      }

      if (o->type == R_RISCV_PCREL_LO12_I) {
        patchI(where, riscvLow12(offset));
      } else {
        patchS(where, riscvLow12(offset));
      }
      return;
    }

    case R_RISCV_32_PCREL: {
      int64_t offset = (int64_t)V - (int64_t)o->P;
      if (!fitsSigned(offset, 32)) {
        linkError(l, "%s: '%s' is too far from '%s' for a 32-bit displacement",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)offset);
      return;
    }

    // A difference between two labels, which RISC-V spells as an add of one
    // and a subtract of the other into the same field. .eh_frame is full of
    // them and every CRT object this links has some.
    case R_RISCV_ADD8:  where[0] = (uint8_t)(where[0] + V); return;
    case R_RISCV_SUB8:  where[0] = (uint8_t)(where[0] - V); return;
    case R_RISCV_SET8:  where[0] = (uint8_t)V; return;

    case R_RISCV_ADD16: write16(where, (uint16_t)(read16(where) + V)); return;
    case R_RISCV_SUB16: write16(where, (uint16_t)(read16(where) - V)); return;
    case R_RISCV_SET16: write16(where, (uint16_t)V); return;

    case R_RISCV_ADD32: write32(where, read32(where) + (uint32_t)V); return;
    case R_RISCV_SUB32: write32(where, read32(where) - (uint32_t)V); return;
    case R_RISCV_SET32: write32(where, (uint32_t)V); return;

    case R_RISCV_ADD64:
      write64(where, read64(where) + V);
      return;
    case R_RISCV_SUB64:
      write64(where, read64(where) - V);
      return;

    // Six bits of one byte, which is how a CFI advance is patched.
    case R_RISCV_SUB6:
      where[0] = (uint8_t)((where[0] & 0xc0u) |
                           (((uint32_t)where[0] - (uint32_t)V) & 0x3fu));
      return;
    case R_RISCV_SET6:
      where[0] = (uint8_t)((where[0] & 0xc0u) | ((uint32_t)V & 0x3fu));
      return;

    default:
      linkError(l, "%s: relocation type %u against '%s' is not supported yet",
                fileName(lf), o->type, name);
      return;
  }
}

static void applyRelocation(Linker *l, LinkPiece *piece,
                            const Elf64_Rela *rela, uint8_t *image) {
  uint32_t type = (uint32_t)ELF64_R_TYPE(rela->r_info);
  LinkFile *lf = piece->owner;

  if (type == l->target->relocNone) return;

  size_t width = l->target->fieldWidth(type);

  if (rela->r_offset > piece->size || piece->size - rela->r_offset < width) {
    linkError(l, "%s: a %s in '%s' points past the end of the section",
              fileName(lf), l->target->relocName(type), piece->name);
    return;
  }

  RelocOperands o;
  if (!resolveRelocOperands(l, piece, rela, &o)) return;

  uint8_t *where = image + piece->offset + rela->r_offset;

  if (l->target->machine == EM_RISCV) {
    applyRiscvRelocation(l, piece, where, &o);
  } else {
    applyX86Relocation(l, piece, where, &o);
  }
}

static void applyRelocations(Linker *l, uint8_t *image) {
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared) continue;

    unsigned i;
    for (i = 0; i < lf->file->sectionCount; ++i) {
      LinkPiece *piece = lf->pieces[i];
      if (piece == NULL || piece->input == NULL) continue;
      if (piece->input->relocCount == 0) continue;

      if (piece->type == SHT_NOBITS) {
        linkError(l, "%s: '%s' occupies no file space but is relocated",
                  fileName(lf), piece->name);
        continue;
      }

      size_t j;
      for (j = 0; j < piece->input->relocCount; ++j) {
        applyRelocation(l, piece, &piece->input->relocs[j], image);
      }
    }
  }
}

/* ---------------------------------------------------------------- writing */

typedef struct _SymbolTable {
  Elf64_Sym *entries;
  size_t count;
  size_t capacity;
  size_t firstGlobal;
} SymbolTable;

static void addOutputSymbol(SymbolTable *table, uint32_t nameOffset,
                            uint8_t info, uint16_t shndx, uint64_t value,
                            uint64_t size) {
  if (table->count == table->capacity) {
    size_t capacity = table->capacity * 2;
    table->entries = heapReallocate(table->entries,
                                    table->capacity * sizeof(Elf64_Sym),
                                    capacity * sizeof(Elf64_Sym));
    table->capacity = capacity;
  }

  Elf64_Sym *symbol = &table->entries[table->count++];
  memset(symbol, 0, sizeof *symbol);
  symbol->st_name = nameOffset;
  symbol->st_info = info;
  symbol->st_shndx = shndx;
  symbol->st_value = value;
  symbol->st_size = size;
}

// The output symbol table is not needed to run the program - nothing reads it
// at load time - but it is what nm, objdump and a debugger have to work with,
// and a linker whose output cannot be inspected cannot be debugged either.
static void buildSymbolTable(Linker *l, SymbolTable *table,
                             StringTable *strings) {
  table->capacity = 64;
  table->entries = heapAllocate(table->capacity * sizeof(Elf64_Sym));
  table->count = 0;

  addOutputSymbol(table, 0, 0, SHN_UNDEF, 0, 0);

  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    if (lf->shared) continue;

    unsigned i;
    for (i = 0; i < lf->file->symbolCount; ++i) {
      const InputSymbol *symbol = &lf->file->symbols[i];

      if (symbol->bind != STB_LOCAL) continue;
      if (symbol->type == STT_FILE || symbol->type == STT_SECTION) continue;
      if (symbol->name == NULL || symbol->name[0] == '\0') continue;
      if (symbol->shndx >= lf->file->sectionCount) continue;

      LinkPiece *piece = lf->pieces[symbol->shndx];
      if (piece == NULL) continue;

      addOutputSymbol(table, addString(strings, symbol->name),
                      (uint8_t)((STB_LOCAL << 4) | symbol->type),
                      (uint16_t)piece->output->index,
                      piece->address + symbol->value, symbol->size);
    }
  }

  table->firstGlobal = table->count;

  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);

    uint16_t shndx = SHN_UNDEF;
    uint8_t type = STT_NOTYPE;
    uint8_t bind = symbol->weakDefinition ? STB_WEAK : STB_GLOBAL;

    if (symbol->definition != NULL) {
      type = symbol->definition->type;
      if (symbol->definition->shndx == SHN_ABS) {
        shndx = SHN_ABS;
      } else {
        LinkPiece *piece = symbol->owner->pieces[symbol->definition->shndx];
        if (piece != NULL) shndx = (uint16_t)piece->output->index;
      }
    } else if (symbol->isCommon) {
      type = STT_OBJECT;
      shndx = (uint16_t)l->commonPiece->output->index;
    } else if (symbol->needsCopy) {
      type = symbol->shared->type;
      shndx = (uint16_t)l->copyPiece->output->index;
    } else if (symbol->linkerDefined) {
      shndx = SHN_ABS;
    } else {
      // A library's function has a stub here and nothing else; an unresolved
      // weak reference defines nothing at all.
      if (!symbol->needsPlt) continue;
      type = STT_FUNC;
      shndx = (uint16_t)l->pltSection->index;
    }

    addOutputSymbol(table, addString(strings, symbol->name),
                    (uint8_t)((bind << 4) | type), shndx, symbol->address,
                    symbol->size);
  }
}

static void writeSectionHeaders(Linker *l, Elf64_Shdr *shdrs) {
  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    Elf64_Shdr *shdr = &shdrs[output->index];

    shdr->sh_name = output->nameOffset;
    shdr->sh_type = output->type;
    shdr->sh_flags = output->flags;
    shdr->sh_addr = output->address;
    shdr->sh_offset = output->offset;
    shdr->sh_size = output->size;
    shdr->sh_link = output->link;
    shdr->sh_info = output->info;
    shdr->sh_addralign = output->align;
    shdr->sh_entsize = output->entsize;
  }
}

// sh_link and sh_info of the dynamic sections are section numbers, which do not
// exist until every output section has been given one.
static void linkDynamicSections(Linker *l) {
  if (!l->dynamic) return;

  l->hashSection->link = l->dynsymSection->index;
  l->dynsymSection->link = l->dynstrSection->index;
  l->dynsymSection->info = 1; // only the null entry is local
  l->dynamicSection->link = l->dynstrSection->index;

  if (l->relaDynSection != NULL) {
    l->relaDynSection->link = l->dynsymSection->index;
  }

  if (l->relaPltSection != NULL) {
    l->relaPltSection->link = l->dynsymSection->index;
    l->relaPltSection->info = l->pltSection->index;
  }
}

static unsigned writeProgramHeaders(Linker *l, const Layout *layout,
                                    Elf64_Phdr *phdrs) {
  unsigned count = 0;

  const Segment *first = &layout->segments[CLASS_RODATA];

  // Both of these describe how the program is started, which is a question
  // only an executable is asked: a shared object has neither.
  if (l->interpPiece != NULL) {
    // PT_PHDR has to come first and has to be inside a PT_LOAD; the read-only
    // segment starts at file offset zero for exactly that reason.
    phdrs[count].p_type = PT_PHDR;
    phdrs[count].p_flags = PF_R;
    phdrs[count].p_offset = sizeof(Elf64_Ehdr);
    phdrs[count].p_vaddr = first->address + sizeof(Elf64_Ehdr);
    phdrs[count].p_paddr = phdrs[count].p_vaddr;
    phdrs[count].p_filesz = layout->phnum * sizeof(Elf64_Phdr);
    phdrs[count].p_memsz = phdrs[count].p_filesz;
    phdrs[count].p_align = 8;
    ++count;

    phdrs[count].p_type = PT_INTERP;
    phdrs[count].p_flags = PF_R;
    phdrs[count].p_offset = l->interpPiece->offset;
    phdrs[count].p_vaddr = l->interpPiece->address;
    phdrs[count].p_paddr = l->interpPiece->address;
    phdrs[count].p_filesz = l->interpPiece->size;
    phdrs[count].p_memsz = l->interpPiece->size;
    phdrs[count].p_align = 1;
    ++count;
  }

  int class;
  for (class = 0; class < CLASS_COUNT; ++class) {
    const Segment *segment = &layout->segments[class];
    if (!segment->present) continue;

    phdrs[count].p_type = PT_LOAD;
    phdrs[count].p_flags = segment->flags;
    phdrs[count].p_offset = segment->offset;
    phdrs[count].p_vaddr = segment->address;
    phdrs[count].p_paddr = segment->address;
    phdrs[count].p_filesz = segment->fileSize;
    phdrs[count].p_memsz = segment->memSize;
    phdrs[count].p_align = LINK_PAGE_SIZE;
    ++count;
  }

  if (l->dynamic) {
    phdrs[count].p_type = PT_DYNAMIC;
    phdrs[count].p_flags = PF_R | PF_W;
    phdrs[count].p_offset = l->dynamicPiece->offset;
    phdrs[count].p_vaddr = l->dynamicPiece->address;
    phdrs[count].p_paddr = l->dynamicPiece->address;
    phdrs[count].p_filesz = l->dynamicPiece->size;
    phdrs[count].p_memsz = l->dynamicPiece->size;
    phdrs[count].p_align = 8;
    ++count;
  }

  // An empty PT_GNU_STACK is the whole of what it says: the stack is mapped
  // with these permissions, and PF_X is not among them.
  phdrs[count].p_type = PT_GNU_STACK;
  phdrs[count].p_flags = PF_R | PF_W;
  phdrs[count].p_align = 0x10;
  ++count;

  if (l->dynamic && l->relroEnd > l->relroStart) {
    phdrs[count].p_type = PT_GNU_RELRO;
    phdrs[count].p_flags = PF_R;
    phdrs[count].p_offset = layout->segments[CLASS_DATA].offset;
    phdrs[count].p_vaddr = l->relroStart;
    phdrs[count].p_paddr = l->relroStart;
    phdrs[count].p_filesz = l->relroEnd - l->relroStart;
    phdrs[count].p_memsz = phdrs[count].p_filesz;
    phdrs[count].p_align = 1;
    ++count;
  }

  return count;
}

static Boolean writeImage(Linker *l, const Layout *layout, Boolean *wrote) {
  StringTable shstrtab, strtab;
  SymbolTable symtab = { 0 };
  Boolean symbols = !l->options->stripSymbols;

  initStringTable(&shstrtab);
  initStringTable(&strtab);

  // Section header indices are settled before anything asks for one: the
  // symbol tables record them, and both are written into the same image.
  unsigned index = 1;
  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    output->index = index++;
    output->nameOffset = addString(&shstrtab, output->name);
  }

  unsigned symtabIndex = symbols ? index++ : 0;
  unsigned strtabIndex = symbols ? index++ : 0;
  unsigned shstrtabIndex = index++;
  unsigned shnum = index;

  uint32_t symtabName = symbols ? addString(&shstrtab, ".symtab") : 0;
  uint32_t strtabName = symbols ? addString(&shstrtab, ".strtab") : 0;
  uint32_t shstrtabName = addString(&shstrtab, ".shstrtab");

  linkDynamicSections(l);
  if (l->dynamic) fillDynamicContents(l);
  if (symbols) buildSymbolTable(l, &symtab, &strtab);

  uint64_t offset = alignUp(layout->fileEnd, 8);
  uint64_t symtabOffset = offset;
  if (symbols) offset += symtab.count * sizeof(Elf64_Sym);

  uint64_t strtabOffset = offset;
  if (symbols) offset += strtab.size;

  uint64_t shstrtabOffset = offset;
  offset += shstrtab.size;

  offset = alignUp(offset, 8);
  uint64_t shoff = offset;
  offset += shnum * sizeof(Elf64_Shdr);

  size_t imageSize = (size_t)offset;
  uint8_t *image = heapAllocate(imageSize);
  memset(image, 0, imageSize);

  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    if (output->type == SHT_NOBITS) continue;

    size_t j;
    for (j = 0; j < output->pieces.size; ++j) {
      LinkPiece *piece = (LinkPiece *)getFromVector(&output->pieces, j);
      if (piece->data == NULL || piece->size == 0) continue;

      memcpy(image + piece->offset, piece->data, piece->size);
    }
  }

  applyRelocations(l, image);

  if (symbols) {
    memcpy(image + symtabOffset, symtab.entries,
           symtab.count * sizeof(Elf64_Sym));
    memcpy(image + strtabOffset, strtab.blob, strtab.size);
  }
  memcpy(image + shstrtabOffset, shstrtab.blob, shstrtab.size);

  Elf64_Ehdr *header = (Elf64_Ehdr *)image;
  memcpy(header->e_ident, ELFMAG, SELFMAG);
  header->e_ident[EI_CLASS] = ELFCLASS64;
  header->e_ident[EI_DATA] = ELFDATA2LSB;
  header->e_ident[EI_VERSION] = EV_CURRENT;
  header->e_ident[EI_OSABI] = ELFOSABI_SYSV;
  header->e_type = l->options->shared ? ET_DYN : ET_EXEC;
  header->e_machine = l->target->machine;
  header->e_flags = l->target->flags;
  header->e_version = EV_CURRENT;
  header->e_entry = l->entry;
  header->e_phoff = sizeof(Elf64_Ehdr);
  header->e_shoff = shoff;
  header->e_ehsize = sizeof(Elf64_Ehdr);
  header->e_phentsize = sizeof(Elf64_Phdr);
  header->e_phnum = (uint16_t)layout->phnum;
  header->e_shentsize = sizeof(Elf64_Shdr);
  header->e_shnum = (uint16_t)shnum;
  header->e_shstrndx = (uint16_t)shstrtabIndex;

  unsigned phnum =
      writeProgramHeaders(l, layout, (Elf64_Phdr *)(image + sizeof(Elf64_Ehdr)));
  assert(phnum == layout->phnum);

  Elf64_Shdr *shdrs = (Elf64_Shdr *)(image + shoff);
  writeSectionHeaders(l, shdrs);

  if (symbols) {
    shdrs[symtabIndex].sh_name = symtabName;
    shdrs[symtabIndex].sh_type = SHT_SYMTAB;
    shdrs[symtabIndex].sh_offset = symtabOffset;
    shdrs[symtabIndex].sh_size = symtab.count * sizeof(Elf64_Sym);
    shdrs[symtabIndex].sh_link = strtabIndex;
    shdrs[symtabIndex].sh_info = (uint32_t)symtab.firstGlobal;
    shdrs[symtabIndex].sh_addralign = 8;
    shdrs[symtabIndex].sh_entsize = sizeof(Elf64_Sym);

    shdrs[strtabIndex].sh_name = strtabName;
    shdrs[strtabIndex].sh_type = SHT_STRTAB;
    shdrs[strtabIndex].sh_offset = strtabOffset;
    shdrs[strtabIndex].sh_size = strtab.size;
    shdrs[strtabIndex].sh_addralign = 1;
  }

  shdrs[shstrtabIndex].sh_name = shstrtabName;
  shdrs[shstrtabIndex].sh_type = SHT_STRTAB;
  shdrs[shstrtabIndex].sh_offset = shstrtabOffset;
  shdrs[shstrtabIndex].sh_size = shstrtab.size;
  shdrs[shstrtabIndex].sh_addralign = 1;

  Boolean ok = !l->hadError;

  if (ok) {
    FILE *output = fopen(l->options->outputFile, "wb");
    if (output == NULL) {
      linkError(l, "cannot write '%s': %s", l->options->outputFile,
                strerror(errno));
      ok = FALSE;
    } else {
      if (fwrite(image, 1, imageSize, output) != imageSize) {
        linkError(l, "cannot write '%s': %s", l->options->outputFile,
                  strerror(errno));
        ok = FALSE;
      }
      fclose(output);
      *wrote = TRUE;

      if (ok) chmod(l->options->outputFile, 0755);
    }
  }

  releaseHeap(image);
  if (symtab.entries != NULL) releaseHeap(symtab.entries);
  releaseStringTable(&strtab);
  releaseStringTable(&shstrtab);

  return ok;
}

/* ------------------------------------------------------------------ entry */

static void releaseLinker(Linker *l) {
  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    releaseVector(&output->pieces);
  }

  releaseVector(&l->outputs);
  releaseVector(&l->globals);
  releaseVector(&l->searchDirs);
  releaseVector(&l->plt);
  releaseVector(&l->got);
  releaseVector(&l->copies);
  releaseVector(&l->dynsyms);
  releaseStringTable(&l->dynstr);
  releaseHashMap(l->dynstrMap);
  releaseHashMap(l->globalMap);

  // The inputs stay mapped until here: every name and every byte the link
  // read points into one of them.
  Mapping *mapping;
  for (mapping = l->mappings; mapping != NULL; mapping = mapping->next) {
    unmapInputFile(mapping->image, mapping->size);
  }

  releaseArena(l->arena);
}

void initLinkOptions(LinkOptions *options) {
  options->outputFile = "a.out";
  options->machine = EM_X86_64;
  options->entrySymbol = DEFAULT_ENTRY_SYMBOL;
  options->imageBase = DEFAULT_IMAGE_BASE;
  options->interpreter = NULL;
  options->runpath = NULL;
  options->stripSymbols = FALSE;
  options->shared = FALSE;
  options->soname = NULL;
}

// Once the inputs are in order, the archives are asked one last time about
// everything still missing. Reaching an archive before the object that needs
// it is a mistake ld reports and this one repairs, which costs nothing and
// removes a whole class of command-line ordering bug.
static void sweepArchives(Linker *l) {
  LinkArchive *la;
  for (la = l->archives; la != NULL && !l->hadError; la = la->next) {
    resolveArchive(l, la);
  }
}

static unsigned countProgramHeaders(Linker *l) {
  unsigned phnum = 1; // the read-only segment, which always exists

  int class;
  for (class = CLASS_TEXT; class < CLASS_COUNT; ++class) {
    size_t i;
    for (i = 0; i < l->outputs.size; ++i) {
      OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
      if ((int)sectionClass(output->flags) == class) {
        ++phnum;
        break;
      }
    }
  }

  ++phnum; // PT_GNU_STACK

  if (l->dynamic) {
    phnum += 2; // PT_DYNAMIC and PT_GNU_RELRO
    if (l->interpPiece != NULL) phnum += 2; // PT_PHDR and PT_INTERP
  }

  return phnum;
}

Boolean linkObjects(const LinkOptions *options, int inputCount,
                    LinkInput *inputs, int dirCount, char **searchDirs) {
  Linker linker = { 0 };
  linker.arena = createArena("linker", DEFAULT_CHUNCK_SIZE);
  linker.reader.arena = linker.arena;
  linker.options = options;
  linker.target = linkTargetFor(options->machine);
  linker.dynamic = options->interpreter != NULL || options->shared;

  if (linker.target == NULL) {
    linkError(&linker, "no linker support for e_machine %u",
              (unsigned)options->machine);
    releaseArena(linker.arena);
    return FALSE;
  }

  // An input for another machine is a mistake worth naming rather than a pile
  // of relocations that make no sense.
  linker.reader.machine = linker.target->machine;
  linker.globalMap = createHashMap(DEFAULT_MAP_CAPACITY, &nameHash,
                                   &nameCompare);
  initVector(&linker.globals, 64);
  initVector(&linker.outputs, 16);
  initVector(&linker.searchDirs, dirCount > 0 ? dirCount : 1);
  initVector(&linker.plt, 16);
  initVector(&linker.got, 16);
  initVector(&linker.copies, 8);
  initVector(&linker.dynsyms, 16);
  initStringTable(&linker.dynstr);
  linker.dynstrMap = createHashMap(DEFAULT_MAP_CAPACITY, &nameHash,
                                   &nameCompare);

  Boolean wrote = FALSE;
  Boolean ok = FALSE;

  int i;
  for (i = 0; i < dirCount; ++i) {
    addToVector(&linker.searchDirs, (intptr_t)searchDirs[i]);
  }

  if (inputCount == 0) {
    linkError(&linker, "no input files");
    goto done;
  }

  for (i = 0; i < inputCount; ++i) {
    const LinkInput *input = &inputs[i];
    Boolean loaded =
        input->image != NULL
            ? loadImage(&linker, input->name, input->image, input->size)
            : loadPath(&linker, input->name);
    if (!loaded) break;
  }
  if (linker.hadError) goto done;

  sweepArchives(&linker);
  if (linker.hadError) goto done;

  claimLinkerSymbols(&linker);
  if (!checkUndefined(&linker)) goto done;

  allocateCommon(&linker);
  gatherPieces(&linker);

  if (linker.dynamic) {
    scanRelocations(&linker);
    if (linker.hadError) goto done;

    exportSymbols(&linker);
    allocateCopies(&linker);
    createDynamicSections(&linker);
  }

  orderOutputs(&linker);

  Layout layout = { 0 };
  layout.phnum = countProgramHeaders(&linker);
  layoutSections(&linker, &layout);

  if (!resolveSymbolAddresses(&linker, &layout)) goto done;

  // A shared object is not started, it is loaded, and ld leaves e_entry at
  // zero for one rather than inventing something to put there.
  if (!options->shared) {
    LinkSymbol *entry =
        (LinkSymbol *)getFromHashMap(linker.globalMap,
                                     (intptr_t)options->entrySymbol);
    if (entry == NULL || entry->definition == NULL) {
      linkError(&linker, "entry symbol '%s' is not defined",
                options->entrySymbol);
      goto done;
    }
    linker.entry = entry->address;
  }

  ok = writeImage(&linker, &layout, &wrote);

done:
  // A half-written executable is worse than none: it is runnable.
  if (!ok && wrote) unlink(options->outputFile);

  releaseLinker(&linker);
  return ok;
}

Boolean linkStaticExecutable(const LinkOptions *options, int inputCount,
                             LinkInput *inputs) {
  return linkObjects(options, inputCount, inputs, 0, NULL);
}

int linkCommand(int argc, char **argv, Boolean dynamic) {
  LinkOptions options;
  initLinkOptions(&options);
  if (dynamic) options.interpreter = linkTargetFor(options.machine)->interpreter;

  int room = argc > 0 ? argc : 1;
  LinkInput *inputs = heapAllocate(sizeof(LinkInput) * room);
  char **dirs = heapAllocate(sizeof(char *) * room);
  int inputCount = 0;
  int dirCount = 0;

  int i;
  for (i = 0; i < argc; ++i) {
    char *arg = argv[i];

    if (strcmp(arg, "-o") == 0 && i + 1 < argc) {
      options.outputFile = argv[++i];
    } else if (strcmp(arg, "-e") == 0 && i + 1 < argc) {
      options.entrySymbol = argv[++i];
    } else if (strcmp(arg, "-s") == 0) {
      options.stripSymbols = TRUE;
    } else if (strcmp(arg, "-shared") == 0) {
      // A shared object is loaded by whoever needs it, so it names no
      // interpreter even when '-Xlink-dynamic' has just supplied one.
      options.shared = TRUE;
      options.interpreter = NULL;
    } else if ((strcmp(arg, "-soname") == 0 || strcmp(arg, "-h") == 0) &&
               i + 1 < argc) {
      options.soname = argv[++i];
    } else if (strcmp(arg, "-rpath") == 0 && i + 1 < argc) {
      options.runpath = argv[++i];
    } else if (strcmp(arg, "-dynamic-linker") == 0 && i + 1 < argc) {
      options.interpreter = argv[++i];
    } else if (strncmp(arg, "-L", 2) == 0) {
      dirs[dirCount++] = arg[2] != '\0' ? arg + 2 : argv[++i];
    } else {
      // '-lname' included: the order libraries and objects are named in is
      // what decides which archive member answers for what.
      inputs[inputCount].name = arg;
      inputs[inputCount].image = NULL;
      inputs[inputCount].size = 0;
      ++inputCount;
    }
  }

  Boolean ok = linkObjects(&options, inputCount, inputs, dirCount, dirs);

  releaseHeap(dirs);
  releaseHeap(inputs);
  return ok ? 0 : 1;
}

int linkStaticCommand(int argc, char **argv) {
  return linkCommand(argc, argv, FALSE);
}

int linkDynamicCommand(int argc, char **argv) {
  return linkCommand(argc, argv, TRUE);
}
