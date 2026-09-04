#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#include "link/link.h"
#include "utils.h"

// The link runs in one pass over a fixed set of objects: read them, decide
// what every global name means, group the input sections into output ones,
// give those addresses, then write the bytes and patch them. Nothing here
// reads a file twice, and nothing is decided before layout that layout could
// change - the addresses relocations need do not exist until then.

#define LINK_PAGE_SIZE 0x1000
#define DEFAULT_IMAGE_BASE 0x400000
#define DEFAULT_ENTRY_SYMBOL "_start"

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

struct _LinkFile;
struct _OutputSection;

// One input section's contribution to an output section - or, when 'input' is
// NULL, the block of tentative definitions the link had to invent.
typedef struct _LinkPiece {
  InputSection *input;
  struct _LinkFile *owner;
  struct _OutputSection *output;

  const char *name;
  uint32_t type;
  uint64_t flags;
  uint64_t align;
  uint64_t size;
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
  uint64_t address;
  uint64_t offset;

  unsigned index;      // in the output section header table
  uint32_t nameOffset; // into .shstrtab

  Vector pieces;
} OutputSection;

typedef struct _LinkSymbol {
  const char *name;

  // The definition that won, and the file it came from. NULL while the symbol
  // is undefined or still only a tentative definition.
  const InputSymbol *definition;
  struct _LinkFile *owner;

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

  Boolean weakDefinition;
  Boolean strongReference;
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

  struct _LinkFile *next;
} LinkFile;

typedef struct _Linker {
  Arena *arena;
  ElfReader reader;
  const LinkOptions *options;

  LinkFile *files;
  LinkFile *lastFile;

  HashMap *globalMap;
  Vector globals; // LinkSymbol *, in the order the names were first seen

  Vector outputs; // OutputSection *, in layout order
  LinkPiece *commonPiece;

  uint64_t entry;
  Boolean hadError;
} Linker;

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

/* ---------------------------------------------------------------- inputs */

static Boolean loadInput(Linker *l, const char *path) {
  size_t size = 0;
  const uint8_t *image = mapInputFile(path, &size);

  if (image == NULL) {
    linkError(l, "cannot open '%s': %s", path, strerror(errno));
    return FALSE;
  }

  enum InputKind kind = classifyImage(image, size);
  if (kind != INPUT_REL) {
    // Archives and shared objects are the next milestone's problem; saying so
    // is better than a link that quietly leaves their symbols undefined.
    linkError(l, "%s: %s is not a relocatable object", path,
              kind == INPUT_ARCHIVE   ? "an archive"
              : kind == INPUT_DYN     ? "a shared object"
              : kind == INPUT_EXEC    ? "an executable"
              : kind == INPUT_SCRIPT  ? "a linker script"
                                      : "this");
    return FALSE;
  }

  InputFile *file = readObject(&l->reader, path, NULL, image, size);
  if (file == NULL) {
    l->hadError = TRUE;
    return FALSE;
  }

  LinkFile *lf = areanAllocate(l->arena, sizeof(LinkFile));
  lf->file = file;
  lf->pieces = areanAllocate(l->arena, sizeof(LinkPiece *) * file->sectionCount);
  lf->globals = areanAllocate(l->arena, sizeof(LinkSymbol *) * file->symbolCount);
  lf->next = NULL;

  if (l->lastFile != NULL) {
    l->lastFile->next = lf;
  } else {
    l->files = lf;
  }
  l->lastFile = lf;

  return TRUE;
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
  size_t i;
  for (i = 0; i < l->globals.size; ++i) {
    LinkSymbol *symbol = (LinkSymbol *)getFromVector(&l->globals, i);
    if (symbol->definition != NULL || symbol->isCommon) continue;

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

  piece->output = output;
  addToVector(&output->pieces, (intptr_t)piece);
}

static void gatherPieces(Linker *l) {
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    unsigned i;
    for (i = 0; i < lf->file->sectionCount; ++i) {
      InputSection *section = &lf->file->sections[i];

      if ((section->flags & SHF_ALLOC) == 0) continue;
      if (section->type != SHT_PROGBITS && section->type != SHT_NOBITS &&
          section->type != SHT_INIT_ARRAY && section->type != SHT_FINI_ARRAY &&
          section->type != SHT_PREINIT_ARRAY && section->type != SHT_NOTE) {
        continue;
      }

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
      size_t i;
      for (i = 0; i < l->outputs.size; ++i) {
        OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
        if ((int)sectionClass(output->flags) != class) continue;
        if ((output->type == SHT_NOBITS) != (nobits != 0)) continue;

        addToVector(&ordered, (intptr_t)output);
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
  uint64_t base = l->options->imageBase;
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
    }

    segment->fileSize = cursor - segment->offset;
    segment->memSize = memCursor - segment->offset;

    offset = cursor;
  }

  layout->fileEnd = offset;
}

/* ------------------------------------------------------- symbol addresses */

static Boolean resolveSymbolAddresses(Linker *l) {
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

/* ------------------------------------------------------------ relocations */

static const char *relocationName(uint32_t type) {
  switch (type) {
    case R_X86_64_64: return "R_X86_64_64";
    case R_X86_64_PC32: return "R_X86_64_PC32";
    case R_X86_64_PLT32: return "R_X86_64_PLT32";
    case R_X86_64_32: return "R_X86_64_32";
    case R_X86_64_32S: return "R_X86_64_32S";
    case R_X86_64_GOTPCREL: return "R_X86_64_GOTPCREL";
    default: return "relocation";
  }
}

static void applyRelocation(Linker *l, LinkPiece *piece,
                            const Elf64_Rela *rela, uint8_t *image) {
  uint32_t type = (uint32_t)ELF64_R_TYPE(rela->r_info);
  uint32_t index = (uint32_t)ELF64_R_SYM(rela->r_info);

  if (type == R_X86_64_NONE) return;

  LinkFile *lf = piece->owner;
  size_t width = type == R_X86_64_64 ? 8 : 4;

  if (rela->r_offset > piece->size || piece->size - rela->r_offset < width) {
    linkError(l, "%s: a %s in '%s' points past the end of the section",
              fileName(lf), relocationName(type), piece->name);
    return;
  }

  const char *name = "";
  uint64_t S = 0;

  if (index != 0) {
    if (index >= lf->file->symbolCount) {
      linkError(l, "%s: a relocation in '%s' names symbol %u, which does not "
                   "exist", fileName(lf), piece->name, index);
      return;
    }

    const InputSymbol *symbol = &lf->file->symbols[index];
    if (symbol->name != NULL) name = symbol->name;

    if (symbol->bind == STB_LOCAL) {
      if (!localSymbolAddress(l, lf, symbol, &S)) return;
    } else {
      LinkSymbol *global = lf->globals[index];
      if (global == NULL || !global->resolved) {
        linkError(l, "%s: '%s' is not resolved", fileName(lf), name);
        return;
      }
      S = global->address;
    }
  }

  int64_t A = rela->r_addend;
  uint64_t P = piece->address + rela->r_offset;
  uint8_t *where = image + piece->offset + rela->r_offset;

  switch (type) {
    case R_X86_64_64:
      write64(where, S + (uint64_t)A);
      return;

    // A call to a symbol this link defines needs no PLT, so the two are the
    // same arithmetic. The day one of them is a shared object's symbol, PLT32
    // stops being PC32 and starts being a stub address.
    case R_X86_64_PC32:
    case R_X86_64_PLT32: {
      int64_t value = (int64_t)(S + (uint64_t)A) - (int64_t)P;
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: '%s' is too far from '%s' for a 32-bit displacement",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    case R_X86_64_32: {
      uint64_t value = S + (uint64_t)A;
      if (value > (uint64_t)RELOC_U32_MAX) {
        linkError(l, "%s: '%s' does not fit in the 32 bits '%s' has for it",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)value);
      return;
    }

    case R_X86_64_32S: {
      int64_t value = (int64_t)(S + (uint64_t)A);
      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {
        linkError(l, "%s: '%s' does not fit in the 32 bits '%s' has for it",
                  fileName(lf), name, piece->name);
        return;
      }
      write32(where, (uint32_t)(int32_t)value);
      return;
    }

    default:
      linkError(l, "%s: relocation type %u against '%s' is not supported yet",
                fileName(lf), type, name);
      return;
  }
}

static void applyRelocations(Linker *l, uint8_t *image) {
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
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

typedef struct _StringTable {
  char *blob;
  size_t size;
  size_t capacity;
} StringTable;

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
  releaseHeap(table->blob);
}

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
    } else {
      continue; // an unresolved weak reference defines nothing
    }

    addOutputSymbol(table, addString(strings, symbol->name),
                    (uint8_t)((bind << 4) | type), shndx, symbol->address,
                    symbol->size);
  }
}

static Boolean writeImage(Linker *l, const Layout *layout, Boolean *wrote) {
  StringTable shstrtab, strtab;
  SymbolTable symtab;

  initStringTable(&shstrtab);
  initStringTable(&strtab);

  // Section header indices are settled before anything asks for one: the
  // symbol table records them, and it is written into the same image.
  unsigned index = 1;
  size_t i;
  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    output->index = index++;
    output->nameOffset = addString(&shstrtab, output->name);
  }

  unsigned symtabIndex = index++;
  unsigned strtabIndex = index++;
  unsigned shstrtabIndex = index++;
  unsigned shnum = index;

  uint32_t symtabName = addString(&shstrtab, ".symtab");
  uint32_t strtabName = addString(&shstrtab, ".strtab");
  uint32_t shstrtabName = addString(&shstrtab, ".shstrtab");

  buildSymbolTable(l, &symtab, &strtab);

  uint64_t offset = alignUp(layout->fileEnd, 8);
  uint64_t symtabOffset = offset;
  offset += symtab.count * sizeof(Elf64_Sym);

  uint64_t strtabOffset = offset;
  offset += strtab.size;

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

  memcpy(image + symtabOffset, symtab.entries,
         symtab.count * sizeof(Elf64_Sym));
  memcpy(image + strtabOffset, strtab.blob, strtab.size);
  memcpy(image + shstrtabOffset, shstrtab.blob, shstrtab.size);

  Elf64_Ehdr *header = (Elf64_Ehdr *)image;
  memcpy(header->e_ident, ELFMAG, SELFMAG);
  header->e_ident[EI_CLASS] = ELFCLASS64;
  header->e_ident[EI_DATA] = ELFDATA2LSB;
  header->e_ident[EI_VERSION] = EV_CURRENT;
  header->e_ident[EI_OSABI] = ELFOSABI_SYSV;
  header->e_type = ET_EXEC;
  header->e_machine = EM_X86_64;
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

  Elf64_Phdr *phdrs = (Elf64_Phdr *)(image + sizeof(Elf64_Ehdr));
  unsigned phdr = 0;

  int class;
  for (class = 0; class < CLASS_COUNT; ++class) {
    const Segment *segment = &layout->segments[class];
    if (!segment->present) continue;

    phdrs[phdr].p_type = PT_LOAD;
    phdrs[phdr].p_flags = segment->flags;
    phdrs[phdr].p_offset = segment->offset;
    phdrs[phdr].p_vaddr = segment->address;
    phdrs[phdr].p_paddr = segment->address;
    phdrs[phdr].p_filesz = segment->fileSize;
    phdrs[phdr].p_memsz = segment->memSize;
    phdrs[phdr].p_align = LINK_PAGE_SIZE;
    ++phdr;
  }

  // An empty PT_GNU_STACK is the whole of what it says: the stack is mapped
  // with these permissions, and PF_X is not among them.
  phdrs[phdr].p_type = PT_GNU_STACK;
  phdrs[phdr].p_flags = PF_R | PF_W;
  phdrs[phdr].p_align = 0x10;
  ++phdr;

  assert(phdr == layout->phnum);

  Elf64_Shdr *shdrs = (Elf64_Shdr *)(image + shoff);

  for (i = 0; i < l->outputs.size; ++i) {
    OutputSection *output = (OutputSection *)getFromVector(&l->outputs, i);
    Elf64_Shdr *shdr = &shdrs[output->index];

    shdr->sh_name = output->nameOffset;
    shdr->sh_type = output->type;
    shdr->sh_flags = output->flags;
    shdr->sh_addr = output->address;
    shdr->sh_offset = output->offset;
    shdr->sh_size = output->size;
    shdr->sh_addralign = output->align;
  }

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
  releaseHeap(symtab.entries);
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
  releaseHashMap(l->globalMap);

  // The inputs stay mapped until here: every name and every byte the link
  // read points into one of them.
  LinkFile *lf;
  for (lf = l->files; lf != NULL; lf = lf->next) {
    unmapInputFile(lf->file->image, lf->file->imageSize);
  }

  releaseArena(l->arena);
}

void initLinkOptions(LinkOptions *options) {
  options->outputFile = "a.out";
  options->entrySymbol = DEFAULT_ENTRY_SYMBOL;
  options->imageBase = DEFAULT_IMAGE_BASE;
}

Boolean linkStaticExecutable(const LinkOptions *options, int inputCount,
                             char **inputs) {
  Linker linker = { 0 };
  linker.arena = createArena("linker", DEFAULT_CHUNCK_SIZE);
  linker.reader.arena = linker.arena;
  linker.options = options;
  linker.globalMap = createHashMap(DEFAULT_MAP_CAPACITY, &nameHash,
                                   &nameCompare);
  initVector(&linker.globals, 64);
  initVector(&linker.outputs, 16);

  Boolean wrote = FALSE;
  Boolean ok = FALSE;

  if (inputCount == 0) {
    linkError(&linker, "no input files");
    goto done;
  }

  int i;
  for (i = 0; i < inputCount; ++i) {
    loadInput(&linker, inputs[i]);
  }
  if (linker.hadError) goto done;

  LinkFile *lf;
  for (lf = linker.files; lf != NULL; lf = lf->next) {
    collectSymbols(&linker, lf);
  }
  if (linker.hadError) goto done;

  if (!checkUndefined(&linker)) goto done;

  allocateCommon(&linker);
  gatherPieces(&linker);
  orderOutputs(&linker);

  Layout layout = { 0 };
  layout.phnum = 1; // the read-only segment, which always exists
  int class;
  for (class = CLASS_TEXT; class < CLASS_COUNT; ++class) {
    size_t j;
    for (j = 0; j < linker.outputs.size; ++j) {
      OutputSection *output =
          (OutputSection *)getFromVector(&linker.outputs, j);
      if ((int)sectionClass(output->flags) == class) {
        ++layout.phnum;
        break;
      }
    }
  }
  ++layout.phnum; // PT_GNU_STACK

  layoutSections(&linker, &layout);

  if (!resolveSymbolAddresses(&linker)) goto done;

  LinkSymbol *entry =
      (LinkSymbol *)getFromHashMap(linker.globalMap,
                                   (intptr_t)options->entrySymbol);
  if (entry == NULL || entry->definition == NULL) {
    linkError(&linker, "entry symbol '%s' is not defined",
              options->entrySymbol);
    goto done;
  }
  linker.entry = entry->address;

  ok = writeImage(&linker, &layout, &wrote);

done:
  // A half-written executable is worse than none: it is runnable.
  if (!ok && wrote) unlink(options->outputFile);

  releaseLinker(&linker);
  return ok;
}

int linkStaticCommand(int argc, char **argv) {
  LinkOptions options;
  initLinkOptions(&options);

  char **inputs = heapAllocate(sizeof(char *) * (argc > 0 ? argc : 1));
  int inputCount = 0;

  int i;
  for (i = 0; i < argc; ++i) {
    if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
      options.outputFile = argv[++i];
    } else if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
      options.entrySymbol = argv[++i];
    } else {
      inputs[inputCount++] = argv[i];
    }
  }

  Boolean ok = linkStaticExecutable(&options, inputCount, inputs);

  releaseHeap(inputs);
  return ok ? 0 : 1;
}
