#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "link/elfread.h"

// Everything an input file says about itself is a number read out of that same
// file, so every one of them is checked before it is used as an offset. A
// truncated or hostile object has to produce a diagnostic, not a segfault -
// and the asan sweep compiles this file too.

static void readError(ElfReader *r, const char *path, const char *member,
                      const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);

  if (member != NULL) {
    fprintf(stderr, "educc: error: %s(%s): ", path, member);
  } else {
    fprintf(stderr, "educc: error: %s: ", path);
  }
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);

  va_end(args);
  r->hadError = TRUE;
}

// A range check that cannot itself overflow: the addition is done in size_t
// after each half is known to fit.
static Boolean inBounds(size_t imageSize, size_t offset, size_t length) {
  if (offset > imageSize) return FALSE;
  if (length > imageSize - offset) return FALSE;
  return TRUE;
}

static Boolean inBoundsArray(size_t imageSize, size_t offset, size_t count,
                             size_t entrySize) {
  if (entrySize != 0 && count > imageSize / entrySize) return FALSE;
  return inBounds(imageSize, offset, count * entrySize);
}

// A name out of a string table, bounded by the table and by a NUL that has to
// be inside it. A table whose last byte is not NUL cannot terminate a name.
static const char *stringAt(const uint8_t *image, size_t imageSize,
                            size_t tableOffset, size_t tableSize,
                            uint32_t nameOffset) {
  if (!inBounds(imageSize, tableOffset, tableSize)) return NULL;
  if (nameOffset >= tableSize) return NULL;
  if (image[tableOffset + tableSize - 1] != '\0') return NULL;

  return (const char *)(image + tableOffset + nameOffset);
}

const uint8_t *mapInputFile(const char *path, size_t *size) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return NULL;

  struct stat st;
  if (fstat(fd, &st) != 0) {
    int saved = errno;
    close(fd);
    errno = saved;
    return NULL;
  }

  if (st.st_size <= 0) {
    close(fd);
    errno = ENOEXEC;
    return NULL;
  }

  void *image = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  int saved = errno;
  close(fd);

  if (image == MAP_FAILED) {
    errno = saved;
    return NULL;
  }

  *size = (size_t)st.st_size;
  return (const uint8_t *)image;
}

void unmapInputFile(const uint8_t *image, size_t size) {
  if (image != NULL) {
    munmap((void *)image, size);
  }
}

#define ARCHIVE_MAGIC "!<arch>\n"
#define ARCHIVE_MAGIC_SIZE 8

enum InputKind classifyImage(const uint8_t *image, size_t size) {
  if (size >= ARCHIVE_MAGIC_SIZE &&
      memcmp(image, ARCHIVE_MAGIC, ARCHIVE_MAGIC_SIZE) == 0) {
    return INPUT_ARCHIVE;
  }

  if (size >= EI_NIDENT && memcmp(image, ELFMAG, SELFMAG) == 0) {
    if (size < sizeof(Elf64_Ehdr)) return INPUT_UNKNOWN;

    const Elf64_Ehdr *header = (const Elf64_Ehdr *)image;
    switch (header->e_type) {
      case ET_REL: return INPUT_REL;
      case ET_DYN: return INPUT_DYN;
      case ET_EXEC: return INPUT_EXEC;
      default: return INPUT_UNKNOWN;
    }
  }

  // Not ELF and not an archive. /usr/lib/libc.so is a GNU ld script, and it is
  // reached by '-lc' on every link, so text is a kind rather than an error.
  return INPUT_SCRIPT;
}

static Boolean checkElfHeader(ElfReader *r, const char *path,
                              const char *member, const uint8_t *image,
                              size_t size) {
  if (size < sizeof(Elf64_Ehdr) || memcmp(image, ELFMAG, SELFMAG) != 0) {
    readError(r, path, member, "not an ELF file");
    return FALSE;
  }

  const Elf64_Ehdr *header = (const Elf64_Ehdr *)image;

  if (header->e_ident[EI_CLASS] != ELFCLASS64) {
    readError(r, path, member, "not a 64-bit object");
    return FALSE;
  }

  if (header->e_ident[EI_DATA] != ELFDATA2LSB) {
    readError(r, path, member, "not little-endian");
    return FALSE;
  }

  if (header->e_machine != EM_X86_64) {
    readError(r, path, member, "not an x86-64 object (e_machine %u)",
              (unsigned)header->e_machine);
    return FALSE;
  }

  if (header->e_type != ET_REL && header->e_type != ET_DYN) {
    readError(r, path, member, "not a relocatable or shared object");
    return FALSE;
  }

  if (header->e_shentsize != 0 &&
      header->e_shentsize != sizeof(Elf64_Shdr)) {
    readError(r, path, member, "unexpected section header size %u",
              (unsigned)header->e_shentsize);
    return FALSE;
  }

  return TRUE;
}

// e_shnum and e_shstrndx are escaped through section 0 when they do not fit in
// the header's 16 bits, which is how an object with 65280 sections says so.
static Boolean sectionTableExtent(ElfReader *r, const char *path,
                                  const char *member, const uint8_t *image,
                                  size_t size, unsigned *countOut,
                                  unsigned *shstrndxOut) {
  const Elf64_Ehdr *header = (const Elf64_Ehdr *)image;

  if (header->e_shoff == 0) {
    *countOut = 0;
    *shstrndxOut = SHN_UNDEF;
    return TRUE;
  }

  if (!inBounds(size, header->e_shoff, sizeof(Elf64_Shdr))) {
    readError(r, path, member, "section header table is out of bounds");
    return FALSE;
  }

  const Elf64_Shdr *sections = (const Elf64_Shdr *)(image + header->e_shoff);

  size_t count = header->e_shnum;
  if (count == 0) count = sections[0].sh_size;

  unsigned shstrndx = header->e_shstrndx;
  if (shstrndx == SHN_XINDEX) shstrndx = sections[0].sh_link;

  if (!inBoundsArray(size, header->e_shoff, count, sizeof(Elf64_Shdr))) {
    readError(r, path, member, "section header table is out of bounds");
    return FALSE;
  }

  if (count > UINT32_MAX) {
    readError(r, path, member, "implausible section count");
    return FALSE;
  }

  *countOut = (unsigned)count;
  *shstrndxOut = shstrndx;
  return TRUE;
}

static Boolean readSections(ElfReader *r, InputFile *file, const uint8_t *image,
                            size_t size, unsigned count, unsigned shstrndx) {
  const Elf64_Ehdr *header = (const Elf64_Ehdr *)image;
  const Elf64_Shdr *raw = (const Elf64_Shdr *)(image + header->e_shoff);

  size_t nameTableOffset = 0;
  size_t nameTableSize = 0;

  if (shstrndx != SHN_UNDEF && shstrndx < count) {
    nameTableOffset = raw[shstrndx].sh_offset;
    nameTableSize = raw[shstrndx].sh_size;
  }

  file->sections = areanAllocate(r->arena, sizeof(InputSection) * count);
  file->sectionCount = count;

  unsigned i;
  for (i = 0; i < count; ++i) {
    InputSection *s = &file->sections[i];
    const Elf64_Shdr *h = &raw[i];

    s->file = file;
    s->index = i;
    s->type = h->sh_type;
    s->flags = h->sh_flags;
    s->align = h->sh_addralign;
    s->size = h->sh_size;
    s->link = h->sh_link;
    s->info = h->sh_info;
    s->entsize = h->sh_entsize;
    s->relocs = NULL;
    s->relocCount = 0;

    s->name = stringAt(image, size, nameTableOffset, nameTableSize,
                       h->sh_name);
    if (s->name == NULL) s->name = "";

    if (h->sh_type == SHT_NOBITS) {
      // Occupies no file space, so there is nothing to point at - and .bss is
      // the section this matters for, since EduCC's own writer gives it a real
      // sh_offset it does not need.
      s->data = NULL;
      continue;
    }

    if (h->sh_type == SHT_NULL) {
      s->data = NULL;
      s->size = 0;
      continue;
    }

    if (!inBounds(size, h->sh_offset, h->sh_size)) {
      readError(r, file->path, file->member,
                "section '%s' extends past the end of the file", s->name);
      return FALSE;
    }

    s->data = image + h->sh_offset;
  }

  return TRUE;
}

// Attaches each SHT_RELA section's entries to the section it relocates. REL
// without addends is not read: nothing in this toolchain's world emits it for
// x86-64, and silently treating the field as the addend would be wrong.
static Boolean attachRelocations(ElfReader *r, InputFile *file) {
  unsigned i;
  for (i = 0; i < file->sectionCount; ++i) {
    InputSection *s = &file->sections[i];

    if (s->type == SHT_REL) {
      readError(r, file->path, file->member,
                "section '%s' uses REL relocations, which are not supported",
                s->name);
      return FALSE;
    }

    if (s->type != SHT_RELA) continue;

    if (s->info >= file->sectionCount) {
      readError(r, file->path, file->member,
                "relocation section '%s' names section %u, which does not exist",
                s->name, s->info);
      return FALSE;
    }

    if (s->entsize != 0 && s->entsize != sizeof(Elf64_Rela)) {
      readError(r, file->path, file->member,
                "relocation section '%s' has entry size %llu", s->name,
                (unsigned long long)s->entsize);
      return FALSE;
    }

    if (s->size % sizeof(Elf64_Rela) != 0) {
      readError(r, file->path, file->member,
                "relocation section '%s' is not a whole number of entries",
                s->name);
      return FALSE;
    }

    if (s->data == NULL && s->size != 0) {
      readError(r, file->path, file->member,
                "relocation section '%s' has no contents", s->name);
      return FALSE;
    }

    InputSection *target = &file->sections[s->info];
    target->relocs = (const Elf64_Rela *)s->data;
    target->relocCount = s->size / sizeof(Elf64_Rela);
  }

  return TRUE;
}

// SHT_GNU_versym is an array of 16-bit indices running parallel to .dynsym,
// whose top bit marks a definition the linker may not bind an unversioned
// reference to. Absent in an unversioned library, where every name is default.
static const uint16_t *findVersyms(const InputFile *file, unsigned symbolCount) {
  unsigned i;
  for (i = 0; i < file->sectionCount; ++i) {
    const InputSection *s = &file->sections[i];
    if (s->type != SHT_GNU_versym || s->data == NULL) continue;
    if (s->size / sizeof(uint16_t) < symbolCount) return NULL;
    return (const uint16_t *)s->data;
  }

  return NULL;
}

static Boolean readSymbols(ElfReader *r, InputFile *file, const uint8_t *image,
                           size_t size, uint32_t wantedType) {
  const InputSection *symtab = NULL;
  unsigned i;

  for (i = 0; i < file->sectionCount; ++i) {
    if (file->sections[i].type == wantedType) {
      symtab = &file->sections[i];
      break;
    }
  }

  if (symtab == NULL) {
    // An object with no symbols at all is legal and contributes nothing to
    // resolution; crtn.o is very nearly one.
    file->symbols = NULL;
    file->symbolCount = 0;
    file->firstGlobal = 0;
    return TRUE;
  }

  if (symtab->size % sizeof(Elf64_Sym) != 0) {
    readError(r, file->path, file->member,
              "symbol table is not a whole number of entries");
    return FALSE;
  }

  if (symtab->link >= file->sectionCount) {
    readError(r, file->path, file->member,
              "symbol table names string table %u, which does not exist",
              symtab->link);
    return FALSE;
  }

  const InputSection *strtab = &file->sections[symtab->link];
  if (strtab->data == NULL || symtab->data == NULL) {
    readError(r, file->path, file->member, "symbol table has no contents");
    return FALSE;
  }

  size_t stringOffset = (size_t)(strtab->data - image);

  unsigned count = (unsigned)(symtab->size / sizeof(Elf64_Sym));
  const Elf64_Sym *raw = (const Elf64_Sym *)symtab->data;

  file->symbols = areanAllocate(r->arena, sizeof(InputSymbol) * (count ? count : 1));
  file->symbolCount = count;
  file->firstGlobal = symtab->info <= count ? symtab->info : count;

  const uint16_t *versyms =
      wantedType == SHT_DYNSYM ? findVersyms(file, count) : NULL;

  for (i = 0; i < count; ++i) {
    InputSymbol *sym = &file->symbols[i];
    const Elf64_Sym *h = &raw[i];

    sym->defaultVersion = versyms == NULL || (versyms[i] & 0x8000) == 0;
    sym->file = file;
    sym->value = h->st_value;
    sym->size = h->st_size;
    sym->bind = ELF64_ST_BIND(h->st_info);
    sym->type = ELF64_ST_TYPE(h->st_info);
    sym->visibility = ELF64_ST_VISIBILITY(h->st_other);
    sym->shndx = h->st_shndx;

    sym->name = stringAt(image, size, stringOffset, strtab->size, h->st_name);
    if (sym->name == NULL) sym->name = "";
  }

  return TRUE;
}

// A shared object's SONAME is what a DT_NEEDED entry has to record, and it is
// not the filename: '-lc' opens libc.so.6 whose SONAME happens to match, but
// libgcc_s.so.1 is routinely reached through a libgcc_s.so symlink.
static void readSoname(InputFile *file, const uint8_t *image, size_t size) {
  const char *slash = strrchr(file->path, '/');
  file->soname = slash != NULL ? slash + 1 : file->path;

  unsigned i;
  const InputSection *dynamic = NULL;
  for (i = 0; i < file->sectionCount; ++i) {
    if (file->sections[i].type == SHT_DYNAMIC) {
      dynamic = &file->sections[i];
      break;
    }
  }

  if (dynamic == NULL || dynamic->data == NULL) return;
  if (dynamic->link >= file->sectionCount) return;

  const InputSection *strtab = &file->sections[dynamic->link];
  if (strtab->data == NULL) return;

  size_t stringOffset = (size_t)(strtab->data - image);
  size_t count = dynamic->size / sizeof(Elf64_Dyn);
  const Elf64_Dyn *entries = (const Elf64_Dyn *)dynamic->data;

  for (i = 0; i < count; ++i) {
    if (entries[i].d_tag == DT_NULL) break;
    if (entries[i].d_tag != DT_SONAME) continue;

    const char *soname = stringAt(image, size, stringOffset, strtab->size,
                                  (uint32_t)entries[i].d_un.d_val);
    if (soname != NULL && soname[0] != '\0') file->soname = soname;
    return;
  }
}

InputFile *readObject(ElfReader *r, const char *path, const char *member,
                      const uint8_t *image, size_t size) {
  if (!checkElfHeader(r, path, member, image, size)) return NULL;

  const Elf64_Ehdr *header = (const Elf64_Ehdr *)image;

  InputFile *file = areanAllocate(r->arena, sizeof(InputFile));
  file->path = path;
  file->member = member;
  file->image = image;
  file->imageSize = size;
  file->kind = header->e_type;
  file->soname = NULL;
  file->next = NULL;

  unsigned count = 0;
  unsigned shstrndx = SHN_UNDEF;
  if (!sectionTableExtent(r, path, member, image, size, &count, &shstrndx)) {
    return NULL;
  }

  if (count == 0) {
    readError(r, path, member, "has no section header table");
    return NULL;
  }

  if (!readSections(r, file, image, size, count, shstrndx)) return NULL;

  if (file->kind == ET_REL) {
    if (!attachRelocations(r, file)) return NULL;
    if (!readSymbols(r, file, image, size, SHT_SYMTAB)) return NULL;
  } else {
    // Only the dynamic symbol table of a shared object is binding-visible; its
    // .symtab, when it has one, describes internals nothing may resolve to.
    if (!readSymbols(r, file, image, size, SHT_DYNSYM)) return NULL;
    readSoname(file, image, size);
  }

  return file;
}

// Archive members are laid out as a 60-byte header and then the data, padded
// to an even offset. Every numeric field is left-justified ASCII, which is why
// each one is copied out and terminated before it is read.
#define AR_HEADER_SIZE 60
#define AR_NAME_SIZE 16
#define AR_SIZE_OFFSET 48
#define AR_SIZE_SIZE 10

static Boolean archiveField(const uint8_t *header, unsigned offset,
                            unsigned length, char *buffer) {
  memcpy(buffer, header + offset, length);
  buffer[length] = '\0';

  // Trailing blanks are the padding, not part of the value.
  int i;
  for (i = (int)length - 1; i >= 0 && buffer[i] == ' '; --i) {
    buffer[i] = '\0';
  }

  return buffer[0] != '\0';
}

// Where the next member's header begins: members start at an even offset, so
// an odd-sized one is followed by a pad byte. One function because the archive
// is walked twice and two copies of this rule could disagree - which desyncs
// every member after the first odd-sized one.
static size_t nextMemberOffset(size_t offset, size_t memberSize) {
  size_t next = offset + AR_HEADER_SIZE + memberSize;
  return next + (next & 1);
}

static const char *archiveMemberName(ElfReader *r, const uint8_t *header,
                                     const char *longNames,
                                     size_t longNamesSize) {
  char raw[AR_NAME_SIZE + 1];
  archiveField(header, 0, AR_NAME_SIZE, raw);

  if (raw[0] == '/' && raw[1] >= '0' && raw[1] <= '9') {
    // "/N" - an offset into the long-name table, where the name runs to a
    // '/' or a newline.
    size_t offset = (size_t)strtoul(raw + 1, NULL, 10);
    if (longNames == NULL || offset >= longNamesSize) return NULL;

    const char *start = longNames + offset;
    size_t remaining = longNamesSize - offset;
    size_t length = 0;
    while (length < remaining && start[length] != '/' && start[length] != '\n') {
      ++length;
    }

    char *name = areanAllocate(r->arena, length + 1);
    memcpy(name, start, length);
    name[length] = '\0';
    return name;
  }

  // A short name is terminated by the '/' GNU ar appends.
  size_t length = strlen(raw);
  if (length > 0 && raw[length - 1] == '/') --length;

  char *name = areanAllocate(r->arena, length + 1);
  memcpy(name, raw, length);
  name[length] = '\0';
  return name;
}

static uint32_t readBigEndian32(const uint8_t *p) {
  return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
         ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}

static uint64_t readBigEndian64(const uint8_t *p) {
  return ((uint64_t)readBigEndian32(p) << 32) | (uint64_t)readBigEndian32(p + 4);
}

// The armap: a count, that many offsets of member headers, and then that many
// NUL-terminated names in the same order. '/SYM64/' is the same thing with
// 64-bit offsets, which an archive over 4GB needs.
static Boolean readArchiveIndex(ElfReader *r, Archive *archive,
                                const uint8_t *data, size_t dataSize,
                                Boolean wide) {
  unsigned entrySize = wide ? 8 : 4;

  if (dataSize < entrySize) {
    readError(r, archive->path, NULL, "archive index is truncated");
    return FALSE;
  }

  uint64_t count = wide ? readBigEndian64(data) : readBigEndian32(data);
  size_t offsetsSize = (size_t)count * entrySize;

  if (count > dataSize / entrySize || offsetsSize > dataSize - entrySize) {
    readError(r, archive->path, NULL, "archive index is truncated");
    return FALSE;
  }

  const uint8_t *offsets = data + entrySize;
  const char *names = (const char *)(offsets + offsetsSize);
  size_t namesSize = dataSize - entrySize - offsetsSize;

  archive->index = areanAllocate(r->arena,
                                 sizeof(ArchiveSymbol) * (count ? count : 1));
  archive->indexCount = 0;
  archive->hasIndex = TRUE;

  size_t nameOffset = 0;
  uint64_t i;

  for (i = 0; i < count; ++i) {
    if (nameOffset >= namesSize) {
      readError(r, archive->path, NULL, "archive index has fewer names than symbols");
      return FALSE;
    }

    size_t length = strnlen(names + nameOffset, namesSize - nameOffset);
    if (nameOffset + length >= namesSize) {
      readError(r, archive->path, NULL, "archive index has an unterminated name");
      return FALSE;
    }

    uint64_t headerOffset = wide ? readBigEndian64(offsets + i * 8)
                                 : readBigEndian32(offsets + i * 4);

    // The armap points at member headers; a symbol naming one that was not
    // seen in the member walk means the two disagree.
    unsigned member;
    Boolean found = FALSE;
    for (member = 0; member < archive->memberCount; ++member) {
      if (archive->members[member].headerOffset == headerOffset) {
        found = TRUE;
        break;
      }
    }

    if (found) {
      archive->index[archive->indexCount].name = names + nameOffset;
      archive->index[archive->indexCount].member = member;
      ++archive->indexCount;
    }

    nameOffset += length + 1;
  }

  return TRUE;
}

Archive *readArchive(ElfReader *r, const char *path, const uint8_t *image,
                     size_t size) {
  if (size < ARCHIVE_MAGIC_SIZE ||
      memcmp(image, ARCHIVE_MAGIC, ARCHIVE_MAGIC_SIZE) != 0) {
    readError(r, path, NULL, "not an archive");
    return NULL;
  }

  Archive *archive = areanAllocate(r->arena, sizeof(Archive));
  archive->path = path;
  archive->image = image;
  archive->imageSize = size;
  archive->hasIndex = FALSE;

  // Two passes: the first counts members so the array can be allocated once,
  // the second fills it in. The index and long-name members are found on the
  // second pass, since the index needs the member offsets to resolve against.
  size_t offset;
  unsigned count = 0;

  for (offset = ARCHIVE_MAGIC_SIZE; offset + AR_HEADER_SIZE <= size;) {
    const uint8_t *header = image + offset;
    char sizeField[AR_SIZE_SIZE + 1];

    if (!archiveField(header, AR_SIZE_OFFSET, AR_SIZE_SIZE, sizeField)) {
      readError(r, path, NULL, "archive member at offset %zu has no size", offset);
      return NULL;
    }

    size_t memberSize = (size_t)strtoul(sizeField, NULL, 10);
    if (!inBounds(size, offset + AR_HEADER_SIZE, memberSize)) {
      readError(r, path, NULL, "archive member at offset %zu is truncated", offset);
      return NULL;
    }

    ++count;
    offset = nextMemberOffset(offset, memberSize);
  }

  archive->members = areanAllocate(r->arena,
                                   sizeof(ArchiveMember) * (count ? count : 1));
  archive->memberCount = 0;

  const char *longNames = NULL;
  size_t longNamesSize = 0;
  const uint8_t *indexData = NULL;
  size_t indexSize = 0;
  Boolean indexWide = FALSE;

  for (offset = ARCHIVE_MAGIC_SIZE; offset + AR_HEADER_SIZE <= size;) {
    const uint8_t *header = image + offset;
    char sizeField[AR_SIZE_SIZE + 1];
    char rawName[AR_NAME_SIZE + 1];

    archiveField(header, AR_SIZE_OFFSET, AR_SIZE_SIZE, sizeField);
    archiveField(header, 0, AR_NAME_SIZE, rawName);

    size_t memberSize = (size_t)strtoul(sizeField, NULL, 10);
    const uint8_t *data = image + offset + AR_HEADER_SIZE;

    if (strcmp(rawName, "//") == 0) {
      longNames = (const char *)data;
      longNamesSize = memberSize;
    } else if (strcmp(rawName, "/") == 0 || strcmp(rawName, "/SYM64/") == 0) {
      indexData = data;
      indexSize = memberSize;
      indexWide = strcmp(rawName, "/SYM64/") == 0;
    } else {
      ArchiveMember *member = &archive->members[archive->memberCount++];
      member->image = data;
      member->size = memberSize;
      member->headerOffset = offset;
      member->loaded = NULL;
      member->name = NULL; // named below, once the long-name table is known
    }

    offset = nextMemberOffset(offset, memberSize);
  }

  // Naming is a second walk because "//" may follow the members that use it -
  // GNU ar puts it first, but the format does not require that.
  unsigned i;
  for (i = 0; i < archive->memberCount; ++i) {
    ArchiveMember *member = &archive->members[i];
    const uint8_t *header = image + member->headerOffset;

    member->name = archiveMemberName(r, header, longNames, longNamesSize);
    if (member->name == NULL) {
      readError(r, path, NULL, "archive member at offset %zu has an unresolvable name",
                member->headerOffset);
      return NULL;
    }
  }

  if (indexData != NULL) {
    if (!readArchiveIndex(r, archive, indexData, indexSize, indexWide)) {
      return NULL;
    }
  }

  return archive;
}

InputFile *readArchiveMember(ElfReader *r, Archive *archive, unsigned member) {
  if (member >= archive->memberCount) return NULL;

  ArchiveMember *m = &archive->members[member];
  if (m->loaded != NULL) return m->loaded;

  m->loaded = readObject(r, archive->path, m->name, m->image, m->size);
  return m->loaded;
}

static const char *symbolBindName(uint8_t bind) {
  switch (bind) {
    case STB_LOCAL: return "LOCAL";
    case STB_GLOBAL: return "GLOBAL";
    case STB_WEAK: return "WEAK";
    default: return "?";
  }
}

static const char *symbolTypeName(uint8_t type) {
  switch (type) {
    case STT_NOTYPE: return "NOTYPE";
    case STT_OBJECT: return "OBJECT";
    case STT_FUNC: return "FUNC";
    case STT_SECTION: return "SECTION";
    case STT_FILE: return "FILE";
    case STT_COMMON: return "COMMON";
    case STT_TLS: return "TLS";
    case STT_GNU_IFUNC: return "IFUNC";
    default: return "?";
  }
}

void dumpInputFile(FILE *output, const InputFile *file) {
  if (file->member != NULL) {
    fprintf(output, "%s(%s): %s", file->path, file->member,
            file->kind == ET_REL ? "relocatable" : "shared");
  } else {
    fprintf(output, "%s: %s", file->path,
            file->kind == ET_REL ? "relocatable" : "shared");
    if (file->kind == ET_DYN) fprintf(output, ", soname %s", file->soname);
  }

  // The first-global index is the one thing here that is load-bearing rather
  // than informational: a local symbol is never a candidate for cross-file
  // resolution, and this is where that line is drawn.
  fprintf(output, ", symbols %u, first global %u\n", file->symbolCount,
          file->firstGlobal);

  unsigned i;
  for (i = 0; i < file->sectionCount; ++i) {
    const InputSection *s = &file->sections[i];
    if (s->type == SHT_NULL) continue;

    // A section may legally have no name (sh_name 0), and an empty column
    // would silently shift every field after it.
    fprintf(output, "  section %-24s type %-3u flags 0x%-4llx size %-8llu align %-4llu",
            s->name[0] != '\0' ? s->name : "(unnamed)", (unsigned)s->type,
            (unsigned long long)s->flags, (unsigned long long)s->size,
            (unsigned long long)s->align);
    if (s->relocCount != 0) {
      fprintf(output, " relocs %zu", s->relocCount);
    }
    // Whether the section has bytes in the file at all, which is otherwise
    // invisible - and is the difference between copying .bss's contents to the
    // output and copying whatever the file happens to hold at that offset.
    if (s->data == NULL) {
      fprintf(output, " nocontents");
    }
    fputc('\n', output);
  }

  for (i = 0; i < file->symbolCount; ++i) {
    const InputSymbol *sym = &file->symbols[i];
    if (sym->name[0] == '\0') continue;

    fprintf(output, "  symbol  %-32s %-6s %-7s ", sym->name,
            symbolBindName(sym->bind), symbolTypeName(sym->type));
    if (sym->shndx == SHN_UNDEF) {
      fprintf(output, "undefined\n");
    } else if (sym->shndx == SHN_ABS) {
      fprintf(output, "absolute 0x%llx\n", (unsigned long long)sym->value);
    } else if (sym->shndx == SHN_COMMON) {
      fprintf(output, "common size %llu\n", (unsigned long long)sym->size);
    } else {
      fprintf(output, "section %u + 0x%llx\n", (unsigned)sym->shndx,
              (unsigned long long)sym->value);
    }
  }
}

int dumpLinkInputs(int count, char **paths) {
  ElfReader reader = { 0 };
  reader.arena = createArena("link reader", DEFAULT_CHUNCK_SIZE);

  int i;
  for (i = 0; i < count; ++i) {
    const char *path = paths[i];
    size_t size = 0;
    const uint8_t *image = mapInputFile(path, &size);

    if (image == NULL) {
      fprintf(stderr, "educc: error: cannot open '%s': %s\n", path,
              strerror(errno));
      reader.hadError = TRUE;
      continue;
    }

    switch (classifyImage(image, size)) {
      case INPUT_REL:
      case INPUT_DYN: {
        InputFile *file = readObject(&reader, path, NULL, image, size);
        if (file != NULL) dumpInputFile(stdout, file);
        break;
      }
      case INPUT_ARCHIVE: {
        Archive *archive = readArchive(&reader, path, image, size);
        if (archive != NULL) dumpArchive(stdout, archive);
        break;
      }
      case INPUT_SCRIPT:
        fprintf(stdout, "%s: linker script\n", path);
        break;
      case INPUT_EXEC:
        readError(&reader, path, NULL, "is an executable, not a link input");
        break;
      default:
        readError(&reader, path, NULL, "unrecognized file format");
        break;
    }

    // The images stay mapped: everything read points into them, and this mode
    // exits straight afterwards.
  }

  releaseArena(reader.arena);
  return reader.hadError ? 1 : 0;
}

void dumpArchive(FILE *output, const Archive *archive) {
  fprintf(output, "%s: archive, %u members, %s\n", archive->path,
          archive->memberCount,
          archive->hasIndex ? "indexed" : "no index");

  unsigned i;
  for (i = 0; i < archive->memberCount; ++i) {
    fprintf(output, "  member  %-32s size %zu\n", archive->members[i].name,
            archive->members[i].size);
  }

  for (i = 0; i < archive->indexCount; ++i) {
    fprintf(output, "  defines %-32s in %s\n", archive->index[i].name,
            archive->members[archive->index[i].member].name);
  }
}
