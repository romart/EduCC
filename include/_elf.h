
#ifndef __ELF_H__
#define __ELF_H__ 1


#include <elf.h>
#include <stdint.h>
#include <stddef.h>

#include "common.h"
#include "mem.h"
#include "instructions.h"

struct _Relocation;

typedef struct _Section {
  const char *name;
  int32_t type;
  uint64_t flags;
  uint64_t align;

  ptrdiff_t offset;

  address start;
  address pc;
  size_t size;

  int32_t headerIndex;
  int32_t symbolIndex;

  Elf64_Shdr *header;

  struct _Section *relocatedSection;

  unsigned tableIdx;

  struct _Relocation *reloc;

  Elf64_Sym *symbol;
} Section;

struct ElfFileSection {
  Section *nullSection;
  Section *text;
  Section *reText;
  Section *data;
  Section *bss;
  Section *rodata;
  Section *rodataLocal;
  Section *reRodataLocal;
  Section *dataLocal;
  Section *reDataLocal;
  Section *symtab;
  Section *strtab;
  Section *shstrtab;
};

typedef struct _ElfFile {

  union {
    Section *asList[sizeof(struct ElfFileSection) / sizeof(Section *)];
    struct ElfFileSection asStruct;
  } sections;


} ElfFile;

struct _GeneratedFile;

uint8_t *generateElfFile(ElfFile *elfFile, struct _GeneratedFile *genFile, size_t *elfFileSize);

void emitSectionByte(Section *s, uint8_t b);
void alignSection(Section *s, int32_t align);

#endif // __ELF_H__
