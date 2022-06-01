
#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include <stdint.h>
#include <stddef.h>
#include "common.h"
#include "mem.h"
#include "tree.h"
#include "instructions.h"
#include "_elf.h"

typedef struct _Operand {
  enum OperandKind opKind;
  union {
    struct Label *label;
    enum Registers reg;
    int64_t constant;
    Address address;
    Relocation reloc;
  };
  struct _Operand *next;
} Operand;

typedef struct _Instruction {
  enum Opcodes opcode;

  unsigned flags;

  Operand *operands;

  struct _Instruction *next;

} Instruction;


typedef struct _GeneratedFunction {
  const char *name;

  size_t paramCount;
  size_t localsSize;
  size_t argsSize;
  size_t frameOffset;
  size_t frameSize;

  int32_t allocaOffset;
  int32_t smallStructSlotOffset;
  int32_t returnStructAddressOffset;
  int32_t savedRegOffset;

  int32_t stackOffset;

  size_t bodySize;

  Section *section;
  ptrdiff_t sectionOffset;

  struct _Symbol *symbol;

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

GeneratedFile *generateCodeForFile(struct _ParserContext *ctx, AstFile *astFile);

void emitByte(GeneratedFunction *f, uint8_t b);
void emitWord(GeneratedFunction *f, uint16_t w);
void emitDouble(GeneratedFunction *f, uint32_t w);
void emitDisp32(GeneratedFunction *f, uint32_t w);
void emitQuad(GeneratedFunction *f, uint64_t w);
void emitQuadOrDouble(GeneratedFunction *f, uint64_t w);


#endif // __CODEGEN_H__
