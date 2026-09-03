

#include <assert.h>
#include <stdio.h>

#include "common.h"
#include "_elf.h"
#include "codegen.h"
#include "sema.h"

static size_t computeRelocs(Section *s) {
  Relocation *reloc = s->reloc;
  unsigned r = 0;

  while (reloc) {
      ++r;
      reloc = reloc->next;
  }
}

static unsigned serializeString(Section *section, const char *str) {
  unsigned idx = 0;

  ptrdiff_t diff = section->pc - section->start;

  while (str[idx]) {
      emitSectionByte(section, str[idx++]);
  }

  emitSectionByte(section, '\0');

  return diff;
}

static unsigned serializeSectionHeaders(ElfFile *elfFile, Elf64_Shdr *buffer) {
  unsigned sectionCount = sizeof(elfFile->sections.asList) / sizeof(elfFile->sections.asList[0]);

  unsigned idx;

  unsigned headerIndex = 0;

  for (idx = 0; idx < sectionCount; ++idx) {
    Section *s = elfFile->sections.asList[idx];

    if (!s) continue;

    Elf64_Shdr *header = &buffer[idx];
    s->header = header;

    header->sh_name = serializeString(elfFile->sections.asStruct.shstrtab, s->name);
    header->sh_flags = s->flags;
    header->sh_addralign = s->align;
    header->sh_type = s->type;

    s->headerIndex = headerIndex++;
  }

  return idx;
}

static void emitBuffer(Section *s, uint8_t *b, size_t size) {
  unsigned i;

  for (i = 0; i < size; ++i) {
      emitSectionByte(s, b[i]);
  }
}

static void serializeVariableSymbol(Section *symtab, Section *strtab, GeneratedVariable *v, uint8_t bind, unsigned idx) {
  Elf64_Sym objSym = { 0 };

  objSym.st_name = serializeString(strtab, v->name);
  objSym.st_info = ELF64_ST_INFO(bind, STT_OBJECT);
  objSym.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
  objSym.st_size = v->size;
  objSym.st_value = v->sectionOffset;
  objSym.st_shndx = v->section->headerIndex;

  v->symbol->symbolTableIndex = idx;

  emitBuffer(symtab, (uint8_t*)(&objSym), sizeof(Elf64_Sym));
}

static void serializeFunctionSymbol(Section *symtab, Section *strtab, GeneratedFunction *f, uint8_t bind, unsigned idx) {
  Elf64_Sym funcSym = { 0 };

  funcSym.st_name = serializeString(strtab, f->name);
  funcSym.st_info = ELF64_ST_INFO(bind, STT_FUNC);
  funcSym.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
  funcSym.st_size = f->bodySize;
  funcSym.st_value = f->sectionOffset;
  funcSym.st_shndx = f->section->headerIndex;

  f->symbol->symbolTableIndex = idx;

  emitBuffer(symtab, (uint8_t*)(&funcSym), sizeof(Elf64_Sym));
}

static void serializeExternalSymbol(Section *symtab, Section *strtab, const char *name) {
  Elf64_Sym extSym = { 0 };

  extSym.st_name = serializeString(strtab, name);
  extSym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
  extSym.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
  extSym.st_size = 0;
  extSym.st_value = 0;
  extSym.st_shndx = SHN_UNDEF;

  emitBuffer(symtab, (uint8_t*)(&extSym), sizeof(Elf64_Sym));
}

// Give every symbol this section's relocations name, but which is defined
// somewhere else, an UND entry to point at. Every *defined* symbol already has
// one by the time this runs, so whatever is left without an index is external.
//
// This has to see the relocations of the data sections too, not only .text's.
// A cross-TU symbol whose address appears solely in a static initializer is
// named by no instruction at all, and used to end up in no symbol table
// either - serializeDataReloc then read back the zero symbolTableIndex was
// born with and emitted the relocation against the null symbol, which ld
// resolves to NULL rather than diagnosing.
//
// Index zero belongs to the null symbol and can never be a real one, so it
// doubles as the "not yet emitted" mark: a symbol named by several
// relocations gets one UND entry rather than one per reference.
static unsigned serializeExternalSymbols(Section *symtab, Section *strtab, Relocation *reloc, unsigned idx) {

  while (reloc) {
      // RK_GOT names a symbol the same way RK_SYMBOL does and needs the same
      // entry: 'extern int x;' read through the GOT is still undefined here.
      if (reloc->kind == RK_SYMBOL || reloc->kind == RK_GOT) {
          Symbol *s = reloc->symbolData.symbol;
          if (s->symbolTableIndex == 0) {
              if (s->kind == ValueSymbol && s->variableDesc->gen == NULL) {
                  serializeExternalSymbol(symtab, strtab, reloc->symbolData.symbolName);
                  s->symbolTableIndex = idx++;
              } else if (s->kind == FunctionSymbol && (s->function == NULL || s->function->gen == NULL)) {
                  serializeExternalSymbol(symtab, strtab, reloc->symbolData.symbolName);
                  s->symbolTableIndex = idx++;
              }
          }
      }
      reloc = reloc->next;
  }

  return idx;
}

static unsigned serializeSymbolTable(ElfFile *elfFile, GeneratedFile *file, unsigned *localsEnd) {
    Section *symTableSection = elfFile->sections.asStruct.symtab;
    Section *strSym = elfFile->sections.asStruct.strtab;

    unsigned idx = 0;

    {
      Elf64_Sym nullSym = { 0 };

      nullSym.st_shndx = SHN_UNDEF;
      serializeString(strSym, "");

      emitBuffer(symTableSection, (uint8_t*)(&nullSym), sizeof(Elf64_Sym));
      ++idx;
    }

    {
      Elf64_Sym fileSym = { 0 };

      fileSym.st_name = serializeString(strSym, file->name);
      fileSym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE);
      fileSym.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
      fileSym.st_size = 0;
      fileSym.st_shndx = SHN_ABS;
      fileSym.st_value = 0x0;

      emitBuffer(symTableSection, (uint8_t*)(&fileSym), sizeof(Elf64_Sym));
      ++idx;
    }

    {
      Elf64_Sym rodataSym = { 0 };
      Section *rodata = elfFile->sections.asStruct.rodata;
      rodataSym.st_name = serializeString(strSym, rodata->name);
      rodataSym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
      rodataSym.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
      rodataSym.st_size = 0;
      rodataSym.st_value = 0x0;
      rodataSym.st_shndx = rodata->headerIndex;

      rodata->symbolIndex = idx++;

      emitBuffer(symTableSection, (uint8_t*)(&rodataSym), sizeof(Elf64_Sym));
    }


    GeneratedVariable *sv = file->staticVariables;

    while (sv) {
        serializeVariableSymbol(symTableSection, strSym, sv, STB_LOCAL, idx++);
        sv = sv->next;
    }

    GeneratedFunction *sf = file->staticFunctions;

    while (sf) {
        serializeFunctionSymbol(symTableSection, strSym, sf, STB_LOCAL, idx++);
        sf = sf->next;
    }

    *localsEnd = idx;

    GeneratedVariable *v = file->variables;

    while (v) {
        serializeVariableSymbol(symTableSection, strSym, v, STB_GLOBAL, idx++);
        v = v->next;
    }

    GeneratedFunction *f = file->functions;

    while (f) {
        serializeFunctionSymbol(symTableSection, strSym, f, STB_GLOBAL, idx++);
        f = f->next;
    }

    idx = serializeExternalSymbols(symTableSection, strSym, elfFile->sections.asStruct.text->reloc, idx);
    idx = serializeExternalSymbols(symTableSection, strSym, elfFile->sections.asStruct.dataLocal->reloc, idx);
    idx = serializeExternalSymbols(symTableSection, strSym, elfFile->sections.asStruct.rodataLocal->reloc, idx);

    return idx;
}


static unsigned serializeTextRelocs(Section *section, Relocation *relocs) {

  unsigned count = 0;

  while (relocs) {
      Elf64_Rela rela =  { 0 };

      if (relocs->kind == RK_SYMBOL) {
          const Symbol *s = relocs->symbolData.symbol;

          Elf64_Xword type = s->kind == ValueSymbol ? R_X86_64_PC32 : R_X86_64_PLT32;
          Elf64_Xword sym = s->symbolTableIndex;

          rela.r_info = ELF64_R_INFO(sym, type);
          rela.r_addend = relocs->addend;
          rela.r_offset = relocs->applySectionOffset;
      } else if (relocs->kind == RK_GOT) {
          // The four bytes hold the distance to the symbol's GOT slot, not to
          // the symbol - the linker makes the slot and points this at it. Only
          // '-fPIC' asks for one; see selectConstant in isel_x86_64.c.
          const Symbol *s = relocs->symbolData.symbol;

          rela.r_info = ELF64_R_INFO(s->symbolTableIndex, R_X86_64_GOTPCREL);
          rela.r_addend = relocs->addend;
          rela.r_offset = relocs->applySectionOffset;
      } else if (relocs->kind == RK_RIP) {

          Elf64_Xword type = R_X86_64_PC32;
          Elf64_Xword sym = relocs->sectionData.dataSection->symbolIndex;

          rela.r_info = ELF64_R_INFO(sym, type);
          rela.r_addend = relocs->addend + relocs->sectionData.dataSectionOffset;
          rela.r_offset = relocs->applySectionOffset;
      } else {
          unreachable("Unexpected relocation kind in text section");
      }

      emitBuffer(section, (uint8_t*)(&rela), sizeof(rela));

      ++count;
      relocs = relocs->next;
  }

  return count;
}

static unsigned serializeDataReloc(Section *section, Relocation *relocs) {
  unsigned count = 0;

  while (relocs) {

      Elf64_Rela rela = { 0 };
      Elf64_Xword type = R_X86_64_64;
      Elf64_Xword sym;

      if (relocs->kind == RK_REF) {
        sym = relocs->sectionData.dataSection->symbolIndex;
      } else if (relocs->kind == RK_SYMBOL) {
        sym = relocs->symbolData.symbol->symbolTableIndex;
      } else {
        unreachable("Unknown relocation type");
      }

      rela.r_info = ELF64_R_INFO(sym, type);
      rela.r_offset = relocs->applySectionOffset;
      rela.r_addend = relocs->addend;

      emitBuffer(section, (uint8_t*)(&rela), sizeof(rela));

      ++count;
      relocs = relocs->next;
  }

  return count;
}

size_t computeSectionsSize(ElfFile *elfFile, unsigned sectionCount, size_t offset) {
  unsigned idx;
  size_t size = offset;
  for (idx = 0; idx < sectionCount; ++idx) {
    Section *s = elfFile->sections.asList[idx];

    if (!s) continue;

    unsigned align = s->align;
    size_t aligned = align ? ALIGN_SIZE(size, align) : size;
    size_t delta = aligned - size;
    size_t thisSize = s->pc - s->start;

    size += delta;
    size += thisSize;
  }

  return ALIGN_SIZE(size, sizeof(intptr_t));
}


static uint8_t *serializeSections(ElfFile *elfFile, unsigned sectionCount, uint8_t *buffer, unsigned offset) {
  uint8_t *tmp = buffer + offset;

  unsigned idx;

  for (idx = 0; idx < sectionCount; ++idx) {
      Section *s = elfFile->sections.asList[idx];

      if (!s) continue;

      unsigned align = s->align & ~1;

      ptrdiff_t currentOffset = tmp - buffer;

      intptr_t alignOffset = align ? ALIGN_SIZE(currentOffset, align) : currentOffset;

      ptrdiff_t delta = alignOffset - currentOffset;

      uint8_t *sectionAddress = tmp + delta;

      ptrdiff_t sectionOffset = sectionAddress - buffer;

      size_t sectionSize = s->pc - s->start;

      s->offset = sectionOffset;
      if (s->symbol) {
          s->symbol->st_shndx = s->symbolIndex;
      }

      memcpy(sectionAddress, s->start, sectionSize);

      tmp = sectionAddress + sectionSize;
  }
  void *ttmp = tmp;
  void *bbufer =buffer;

  ptrdiff_t off = ALIGN_SIZE(tmp - buffer, sizeof(intptr_t));

  return buffer + off;
}

// The four bytes an emitted instruction left where its displacement goes are
// the 0x7EADBEFF poison (see encodeAR in instructions_x86_64.c), and every one
// of them is covered by a relocation in a section written above. RELA says the
// addend is in the entry and the field is unused, which is what lets the poison
// survive to here at all - but "unused" is a rule linkers are free to read
// generously, and tinycc *adds* the field to what it computes rather than
// overwriting it. An EduCC object linked by tcc came out with every relocated
// address off by 0x7EADBEFF, which is how tests/dlltest segfaulted.
//
// So zero exactly the fields a relocation names, in the serialized copy alone:
// the in-memory section keeps the poison, so '-S' still shows it, and a
// displacement no relocation covers - the failure the poison exists to make
// visible - still reaches the object file as 0x7EADBEFF.
static void clearRelocatedFields(Relocation *reloc, uint8_t *buffer) {
  while (reloc) {
      address applyAddress = buffer + reloc->applySection->offset + reloc->applySectionOffset;

      applyAddress[0] = applyAddress[1] = applyAddress[2] = applyAddress[3] = 0;

      reloc = reloc->next;
  }
}

static void finalizeSectionHeaders(ElfFile *elfFile, unsigned sectionCount, unsigned localIdx) {
  unsigned idx;

  for (idx = 0; idx < sectionCount; ++idx) {
      Section *s = elfFile->sections.asList[idx];

      if (!s) continue;

      Elf64_Shdr *header = s->header;

      header->sh_size = s->pc - s->start;
      header->sh_addr = 0x00;
      header->sh_offset = s->offset;

      switch (s->type) {
        case SHT_RELA:
          header->sh_entsize = sizeof(Elf64_Rela);
          assert(s->relocatedSection);
          header->sh_link = elfFile->sections.asStruct.symtab->headerIndex;
          header->sh_info = s->relocatedSection->headerIndex;
          break;
        case SHT_SYMTAB:
          header->sh_entsize = sizeof(Elf64_Sym);
          header->sh_link = elfFile->sections.asStruct.strtab->headerIndex;
          header->sh_info = localIdx;
          break;
        default:
          break;
        }
  }
}

uint8_t *generateElfFile(ElfFile *elfFile, GeneratedFile *genFile, size_t *elfFileSize) {

  unsigned sectionHeadersLen = (sizeof(elfFile->sections.asList) / sizeof(elfFile->sections.asList[0]));
  size_t sectionHeadersSize = sizeof(Elf64_Shdr) * sectionHeadersLen;

  Elf64_Shdr *sectionHeaders = heapAllocate(sectionHeadersSize);

  unsigned sectionCount = serializeSectionHeaders(elfFile, sectionHeaders);

  unsigned lastLocalIndex = 0;

  unsigned symbolCount = serializeSymbolTable(elfFile, genFile, &lastLocalIndex);

  Section *textReloc = elfFile->sections.asStruct.reText;
  unsigned textRelocCount = serializeTextRelocs(textReloc, elfFile->sections.asStruct.text->reloc);

  Section *rodataLocalReloc = elfFile->sections.asStruct.reRodataLocal;
  unsigned rodataLocalRelocCount = serializeDataReloc(rodataLocalReloc, elfFile->sections.asStruct.rodataLocal->reloc);

  Section *dataLocalReloc = elfFile->sections.asStruct.reDataLocal;
  unsigned dataLocalRelocCount = serializeDataReloc(dataLocalReloc, elfFile->sections.asStruct.dataLocal->reloc);

  Section *symtab = elfFile->sections.asStruct.symtab;
  size_t symbolTableSectionSize = symtab->pc - symtab->start;

  Section *strtab = elfFile->sections.asStruct.strtab;
  size_t stringTableSectionSize = strtab->pc - strtab->start;

  Section *shstrtab = elfFile->sections.asStruct.shstrtab;
  size_t shStringTableSectionSize = shstrtab->pc - shstrtab->start;

  size_t h_s_size = computeSectionsSize(elfFile, sectionHeadersLen, sizeof(Elf64_Ehdr));

  size_t totalSize = sizeof(Elf64_Ehdr) + h_s_size + sectionHeadersSize;

  uint8_t *buffer = heapAllocate(totalSize);

  Elf64_Ehdr *header = (Elf64_Ehdr *)buffer;

  uint8_t *sectionHeaderAddress = serializeSections(elfFile, sectionHeadersLen, buffer, sizeof(Elf64_Ehdr));

  ptrdiff_t sectionHeaderOffset = sectionHeaderAddress - buffer;
  assert(sectionHeaderOffset == h_s_size);

  clearRelocatedFields(elfFile->sections.asStruct.text->reloc, buffer);

  finalizeSectionHeaders(elfFile, sectionHeadersLen, lastLocalIndex);

  memcpy(buffer + sectionHeaderOffset, sectionHeaders, sectionHeadersSize);

  header->e_ident[EI_MAG0] = ELFMAG0;
  header->e_ident[EI_MAG1] = ELFMAG1;
  header->e_ident[EI_MAG2] = ELFMAG2;
  header->e_ident[EI_MAG3] = ELFMAG3;

  header->e_ident[EI_CLASS] = ELFCLASS64;

  header->e_ident[EI_DATA] = ELFDATA2LSB;

  header->e_ident[EI_VERSION] = EV_CURRENT;

  header->e_ident[EI_OSABI] = ELFOSABI_SYSV;

  header->e_ident[EI_ABIVERSION] = 0;

  header->e_type = ET_REL;

  header->e_machine = EM_X86_64;

  header->e_version = EV_CURRENT;

  header->e_entry = 0x00;

  header->e_phoff = 0;
  header->e_shoff = sectionHeaderOffset;

  header->e_flags = 0x00;

  header->e_ehsize = sizeof(Elf64_Ehdr);

  header->e_phentsize = 0;
  header->e_phnum = 0;

  header->e_shentsize = sizeof(Elf64_Shdr);
  header->e_shnum = sectionHeadersLen;
  header->e_shstrndx = elfFile->sections.asStruct.shstrtab->headerIndex;

  releaseHeap(sectionHeaders);

  *elfFileSize = totalSize;

  return buffer;
}

void emitSectionByte(Section *s, uint8_t b) {
  if (s->start + s->size <= s->pc) {
      size_t newSize = ALIGN_SIZE((s->size << 1) - (s->size >> 1) + 1, sizeof(intptr_t));
      address newBuffer = heapReallocate(s->start, s->size, newSize);
      s->pc = newBuffer + s->size;
      s->start = newBuffer;
      s->size = newSize;
  }

  *(s->pc++) = b;
}

void alignSection(Section *s, int32_t align) {
  if (align <= 0) return;
  int32_t offset = s->pc - s->start;
  int32_t aligned = (offset + (align - 1)) & ~(align - 1);
  while (offset < aligned) {
      emitSectionByte(s, 0x00);
      ++offset;
  }
}
