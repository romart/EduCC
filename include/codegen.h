
#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include <stdint.h>
#include <stddef.h>
#include "common.h"
#include "mem.h"
#include "tree.h"
#include "instructions.h"
#include "_elf.h"
#include "parser.h"
#include "sema.h"

typedef struct _GeneratedFunction {
  const char *name;

  size_t paramCount;
  size_t localsSize;
  size_t argsSize;
  size_t frameOffset;
  size_t frameSize;

  int32_t allocaOffset;
  int32_t structBufferOffset;
  int32_t returnStructAddressOffset;
  int32_t savedRegOffset;

  int32_t stackOffset;

  size_t bodySize;

  Section *section;
  ptrdiff_t sectionOffset;

  struct _Symbol *symbol;

  struct _GenerationContext *context;

  Arena *arena;

  struct _GeneratedFunction *next;
} GeneratedFunction;

typedef struct _GeneratedVariable {
  const char *name;

  Section *section;
  ptrdiff_t sectionOffset;
  size_t size;

  int32_t baseOffset;

  struct _Symbol *symbol;

  struct _GeneratedVariable *next;
} GeneratedVariable;

typedef struct _GeneratedFile {
  const char *name;

  struct _GeneratedVariable *staticVariables;
  struct _GeneratedFunction *staticFunctions;

  struct _GeneratedVariable *variables;
  struct _GeneratedFunction *functions;

  struct _Relocation *relocations;
} GeneratedFile;

struct CaseLabel {
  int64_t caseConst;
  struct Label label;
};

typedef struct _GenerationContext {
  ParserContext *parserContext;

  GeneratedFile *file;

  Arena *codegenArena;

  Relocation *relocations;

  HashMap *labelMap;

  struct {
    HashMap *literalMap;
    HashMap *f4ConstMap;
    HashMap *f8ConstMap;
    HashMap *f10ConstMap;
  } constCache;

  struct Label *continueLabel;
  struct Label *breakLabel;

  struct Label *defaultLabel;

  unsigned caseCount;
  struct CaseLabel *caseLabels;

  Symbol *memsetSymbol;

  Section *bss;
  Section *rodata;
  Section *data;
  Section *rodataLocal;
  Section *dataLocal;
  Section *text;

} GenerationContext;

GeneratedFile *allocateGenFile(GenerationContext *ctx);
GeneratedFunction *allocateGenFunction(GenerationContext *ctx);
GeneratedVariable *allocateGenVarialbe(GenerationContext *ctx, AstValueDeclaration *d);
Relocation *allocateRelocation(GenerationContext *ctx);

typedef union {
  uint8_t bytes[sizeof(float)];
  float f;
} FloatBytes;

typedef union {
  uint8_t bytes[sizeof(double)];
  double d;
} DoubleBytes;

typedef union {
  uint8_t bytes[sizeof(long double)];
  uint64_t qwords[2];
  long double ld;
} LongDoubleBytes;

void initConstCache(GenerationContext *ctx);
void releaseConstCache(GenerationContext *ctx);

void buildElfFile(GenerationContext *ctx, AstFile *astFile, GeneratedFile *genFile, ElfFile *elfFile);

typedef struct _ArchCodegen {
  GeneratedFunction *(*generateFunction)(GenerationContext *, AstFunctionDefinition *);
  GeneratedVariable *(*generateVaribale)(GenerationContext *, AstValueDeclaration *);
} ArchCodegen;

GeneratedFile *generateCodeForFile(struct _ParserContext *ctx, ArchCodegen *archCodegen, AstFile *astFile);

void initArchCodegen_x86_64(ArchCodegen *cg);
void initArchCodegen_riscv64(ArchCodegen *cg);

void emitByte(GeneratedFunction *f, uint8_t b);
void emitShort(GeneratedFunction *f, uint16_t b);
void emitDWord(GeneratedFunction *f, uint32_t b);
void emitQWord(GeneratedFunction *f, uint64_t b);

void emitWord(GeneratedFunction *f, uint16_t w);
void emitDouble(GeneratedFunction *f, uint32_t w);
void emitDisp32(GeneratedFunction *f, uint32_t w);
void emitQuad(GeneratedFunction *f, uint64_t w);
void emitQuadOrDouble(GeneratedFunction *f, uint64_t w);


#endif // __CODEGEN_H__
