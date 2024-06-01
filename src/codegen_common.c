#include "_elf.h"
#include "codegen.h"
#include "mem.h"
#include "parser.h"
#include "sema.h"

#include <alloca.h>
#include <assert.h>

GeneratedFile *allocateGenFile(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (GeneratedFile));
}

GeneratedFunction *allocateGenFunction(GenerationContext *ctx) {
  GeneratedFunction *f = areanAllocate(ctx->codegenArena, sizeof (GeneratedFunction));
  f->returnStructAddressOffset = -1;
  f->arena = ctx->codegenArena;
  f->section = ctx->text;
  f->sectionOffset = (ctx->text->pc - ctx->text->start);
  f->context = ctx;
  return f;
}

GeneratedVariable *allocateGenVarialbe(GenerationContext *ctx, AstValueDeclaration *d) {
  GeneratedVariable *v = areanAllocate(ctx->codegenArena, sizeof (GeneratedVariable));
  v->name = d->name;
  v->symbol = d->symbol;
  d->gen = v;
  return v;
}

Relocation *allocateRelocation(GenerationContext *ctx) {
  return areanAllocate(ctx->codegenArena, sizeof (Relocation));
}

static int f4HashCode(intptr_t pf) {
  float v = (float)(*(long double*)pf);
  return *(int*)&v;
}

static int f4Cmp(intptr_t pf1, intptr_t pf2) {
  float v1 = (float)(*(long double*)pf1);
  float v2 = (float)(*(long double*)pf2);
  return *(int*)&v2 - *(int*)&v1;
}

static int f8HashCode(intptr_t pf) {
  DoubleBytes db = { 0 };
  db.d = (double)(*(long double*)pf);

  int i, r = 0;

  for (i = 0; i < 8; ++i) {
      r *= 31;
      r ^= db.bytes[i];
  }

  return r;
}

static int f8Cmp(intptr_t pf1, intptr_t pf2) {
  double v1 = (double)(*(long double*)pf1);
  double v2 = (double)(*(long double*)pf2);

  return memcmp((uint8_t*)&v2, (uint8_t*)&v1, 8);
}

static int f10HashCode(intptr_t pf) {
  LongDoubleBytes ldb = { 0 };
  ldb.ld = *(long double*)pf;

  int i, r = 0;

  for (i = 0; i < 10; ++i) {
      r *= 31;
      r ^= ldb.bytes[i];
  }

  return r;
}


int f10Cmp(intptr_t pf1, intptr_t pf2) {
  LongDoubleBytes ldb1 = { 0 };
  ldb1.ld = *(long double*)pf1;

  LongDoubleBytes ldb2 = { 0 };
  ldb2.ld = *(long double*)pf2;

  return memcmp(ldb2.bytes, ldb1.bytes, 10);
}

int strConstHashcode(intptr_t k) {
  AstConst *_const = (AstConst*)k;

  size_t l = _const->l.length;
  const char *s = _const->l.s;

  int result = 0;
  unsigned i;
  for (i = 0; i < l; ++i) {
      result *= 31;
      result ^= s[i];
  }

  return result;
}

int strConstCmp(intptr_t k1, intptr_t k2) {
  AstConst *v1 = (AstConst*)k1;
  AstConst *v2 = (AstConst*)k2;

  if (v1->l.length != v2->l.length) return v2->l.length - v1->l.length;

  return memcmp(v2->l.s, v1->l.s, v1->l.length);
}

void initConstCache(GenerationContext *ctx) {
  ctx->constCache.literalMap = createHashMap(DEFAULT_MAP_CAPACITY, &strConstHashcode, &strConstCmp);
  ctx->constCache.f4ConstMap = createHashMap(DEFAULT_MAP_CAPACITY, &f4HashCode, &f4Cmp);
  ctx->constCache.f8ConstMap = createHashMap(DEFAULT_MAP_CAPACITY, &f8HashCode, &f8Cmp);
  ctx->constCache.f10ConstMap = createHashMap(DEFAULT_MAP_CAPACITY, &f10HashCode, &f10Cmp);
}

void releaseConstCache(GenerationContext *ctx) {
  releaseHashMap(ctx->constCache.literalMap);
  releaseHashMap(ctx->constCache.f4ConstMap);
  releaseHashMap(ctx->constCache.f8ConstMap);
  releaseHashMap(ctx->constCache.f10ConstMap);
}

static void writeObjFile(const char *sourceFileName, const char *outputFile, uint8_t *buffer, size_t bufferSize) {
  if (outputFile == NULL) {
      size_t len = strlen(sourceFileName);
      unsigned j;
      for (j = len - 1; j >= 0; --j) {
          if (sourceFileName[j] == '/') break;
      }
      ++j;
      char *buffer = alloca(len - j + 1);

      unsigned i = 0;

      while (sourceFileName[j] != '.') {
        buffer[i++] = sourceFileName[j++];
      }

      buffer[i++] = '.';
      buffer[i++] = 'o';
      buffer[i++] = '\0';

      outputFile = buffer;
  }

  remove(outputFile);
  FILE* output = fopen(outputFile, "wb");
  if (output) {
    fwrite(buffer, bufferSize, 1, output);
    fclose(output);
  } else {
    fprintf(stderr, "Fatal error: can't create %s: No such file or directory", outputFile);
  }
}

void buildElfFile(GenerationContext *ctx, AstFile *astFile, GeneratedFile *genFile, ElfFile *elfFile) {

  size_t elfFileSize = 0;

  uint8_t *elfFileBytes = generateElfFile(elfFile, genFile, &elfFileSize);

  writeObjFile(astFile->fileName, ctx->parserContext->config->outputFile, elfFileBytes, elfFileSize);

  releaseHeap(elfFile->sections.asStruct.nullSection->start);
  releaseHeap(elfFile->sections.asStruct.text->start);
  releaseHeap(elfFile->sections.asStruct.reText->start);
  releaseHeap(elfFile->sections.asStruct.data->start);
  releaseHeap(elfFile->sections.asStruct.bss->start);
  releaseHeap(elfFile->sections.asStruct.rodata->start);
  releaseHeap(elfFile->sections.asStruct.rodataLocal->start);
  releaseHeap(elfFile->sections.asStruct.reRodataLocal->start);
  releaseHeap(elfFile->sections.asStruct.dataLocal->start);
  releaseHeap(elfFile->sections.asStruct.reDataLocal->start);
  releaseHeap(elfFile->sections.asStruct.symtab->start);
  releaseHeap(elfFile->sections.asStruct.strtab->start);
  releaseHeap(elfFile->sections.asStruct.shstrtab->start);

  releaseHeap(elfFileBytes);
}

GeneratedFile *generateCodeForFile(ParserContext *pctx, ArchCodegen *archCodegen, AstFile *astFile) {
    Section nullSection = { "", SHT_NULL, 0x00, 0 };
    Section text = { ".text", SHT_PROGBITS, SHF_EXECINSTR | SHF_ALLOC, 1 }, reText = { ".rela.text", SHT_RELA, SHF_INFO_LINK, 8 };
    Section data = { ".data", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 16 };
    Section bss = { ".bss", SHT_NOBITS, SHF_WRITE | SHF_ALLOC, 32 };
    Section rodata = { ".rodata", SHT_PROGBITS, SHF_ALLOC, 16 };
    Section dataLocal = { ".data.rel.local", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 16 }, reDataLocal = { ".rela.data.rel.local", SHT_RELA, SHF_INFO_LINK, 8 };
    Section roDataLocal = { "data.rel.ro.local", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 16 }, reRoDataLocal = { ".rela.data.rel.ro.local", SHT_RELA, SHF_INFO_LINK, 8 };
    Section symtab = { ".symtab", SHT_SYMTAB, 0x00, 8 };
    Section strtab = { ".strtab", SHT_STRTAB, 0x00, 1 };
    Section shstrtab = { ".shstrtab", SHT_STRTAB, 0x00, 1 };

    ElfFile elfFile = { 0 };
    elfFile.sections.asStruct.nullSection = &nullSection;
    elfFile.sections.asStruct.text = &text;
    elfFile.sections.asStruct.reText = &reText; reText.relocatedSection = &text;
    elfFile.sections.asStruct.data = &data;
    elfFile.sections.asStruct.bss = &bss;
    elfFile.sections.asStruct.rodata = &rodata;
    elfFile.sections.asStruct.rodataLocal = &roDataLocal;
    elfFile.sections.asStruct.reRodataLocal = &reRoDataLocal; reRoDataLocal.relocatedSection = &roDataLocal;
    elfFile.sections.asStruct.dataLocal = &dataLocal;
    elfFile.sections.asStruct.reDataLocal = &reDataLocal; reDataLocal.relocatedSection = &dataLocal;
    elfFile.sections.asStruct.symtab = &symtab;
    elfFile.sections.asStruct.strtab = &strtab;
    elfFile.sections.asStruct.shstrtab = &shstrtab;

    GenerationContext ctx = { pctx, NULL, pctx->memory.codegenArena };
    GeneratedFile *file = allocateGenFile(&ctx);
    ctx.file = file;
    file->name = astFile->fileName;

    Symbol *memsetSymbol = findSymbol(pctx, "memset");
    if (memsetSymbol == NULL || memsetSymbol->kind != FunctionSymbol) {
        memsetSymbol = newSymbol(pctx, FunctionSymbol, "memset");
    }

    ctx.memsetSymbol = memsetSymbol;

    initConstCache(&ctx);

    ctx.text = &text;
    ctx.bss = &bss;
    ctx.rodata = &rodata;
    ctx.data = &data;
    ctx.dataLocal = &dataLocal;
    ctx.rodataLocal = &roDataLocal;

    AstTranslationUnit *unit = astFile->units;

    assert(archCodegen->generateFunction != NULL);
    assert(archCodegen->generateVaribale != NULL);

    while (unit) {
      if (unit->kind == TU_FUNCTION_DEFINITION) {
          GeneratedFunction *f = archCodegen->generateFunction(&ctx, unit->definition);
          unit->definition->declaration->gen = f;
          unit->definition->declaration->symbol->function->gen = f;

          if (unit->definition->declaration->flags.bits.isStatic) {
              f->next = file->staticFunctions;
              file->staticFunctions= f;
          } else {
              f->next = file->functions;
              file->functions= f;
          }
      } else {
          assert(unit->kind == TU_DECLARATION);
          AstDeclaration *d = unit->declaration;
          if (d->kind == DK_VAR) {
            GeneratedVariable *v = archCodegen->generateVaribale(&ctx, d->variableDeclaration);
            if (v) {
              d->variableDeclaration->gen = v;

              if (d->variableDeclaration->flags.bits.isStatic) {
                v->next = file->staticVariables;
                file->staticVariables = v;
              } else {
                v->next = file->variables;
                file->variables = v;
              }
            }
          }
      }
      unit = unit->next;
    }

    buildElfFile(&ctx, astFile, file, &elfFile);

    releaseConstCache(&ctx);

    return NULL;
}

void emitByte(GeneratedFunction *f, uint8_t b) {
  emitSectionByte(f->section, b);
}

void emitShort(GeneratedFunction *f, uint16_t b) {
  emitByte(f, (uint8_t)b);
  emitByte(f, (uint8_t)(b >> 8));
}

void emitDWord(GeneratedFunction *f, uint32_t b) {
  emitByte(f, (uint8_t)b);
  emitByte(f, (uint8_t)(b >> 8));
  emitByte(f, (uint8_t)(b >> 16));
  emitByte(f, (uint8_t)(b >> 24));
}

void emitQWord(GeneratedFunction *f, uint64_t b) {
  emitByte(f, (uint8_t)b);
  emitByte(f, (uint8_t)(b >> 8));
  emitByte(f, (uint8_t)(b >> 16));
  emitByte(f, (uint8_t)(b >> 24));
  emitByte(f, (uint8_t)(b >> 32));
  emitByte(f, (uint8_t)(b >> 40));
  emitByte(f, (uint8_t)(b >> 48));
  emitByte(f, (uint8_t)(b >> 56));
}

void emitWord(GeneratedFunction *f, uint16_t w) {
   emitByte(f, (uint8_t)w);

   if ((uint16_t)(uint8_t)w != w) {
       emitByte(f, (uint8_t)(w >> 8));
   }
}

void emitDouble(GeneratedFunction *f, uint32_t w) {
    emitByte(f, (uint8_t)(w));
    emitByte(f, (uint8_t)(w >> 8));
    emitByte(f, (uint8_t)(w >> 16));
    emitByte(f, (uint8_t)(w >> 24));
}

void emitDisp32(GeneratedFunction *f, uint32_t w) {
  if ((uint32_t)(uint16_t)w != w) {
      emitWord(f, (uint16_t) w);

      uint16_t high = (uint16_t)(w >> 16);
      emitByte(f, (uint8_t)high);
      emitByte(f, (uint8_t)(high >> 8));
  } else {
      if ((uint32_t)(uint8_t)w != w) {
          emitWord(f, w);
      } else {
          emitByte(f, (uint8_t)w);
          emitByte(f, 0);
      }
      emitByte(f, 0);
      emitByte(f, 0);
  }
}

void emitQuad(GeneratedFunction *f, uint64_t w) {
  emitDouble(f, (uint32_t) w);

  if ((uint64_t)(uint32_t)w != w) {
      emitDouble(f, (uint32_t)(w >> 32));
  }
}

void emitQuadOrDouble(GeneratedFunction *f, uint64_t w) {
  if ((uint64_t)(uint32_t)w == w) {
    emitDisp32(f, w);
  } else {
    emitDouble(f, (uint32_t) w);
    emitDouble(f, (uint32_t)(w >> 32));
  }
}
