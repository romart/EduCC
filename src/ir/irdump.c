
#include "ir/ir.h"
#include "sema.h"
#include "treeDump.h"
#include <assert.h>

struct _IrInstructionDumpInfo {
  const char *mnemonic;
  const char *comment;
};

static struct _IrInstructionDumpInfo irInstructionsInfo[] = {
#define TO_STR(s) #s
#define IR_INSTRUCTION_DEF(m, c) { .mnemonic = TO_STR(IR_##m), .comment = c }
#include "ir/instructionList.h"
  INSTRUCTIONS
#undef IR_INSTRUCTION_DEF
#undef TO_STR
};


static int32_t dumpIrType(FILE *stream, const enum IrTypeKind type) {
	switch (type) {
	  case IR_BOOL: return fprintf(stream, "BOOL");

	  case IR_I8: return fprintf(stream, "I8");
	  case IR_I16: return fprintf(stream, "I16");
	  case IR_I32: return fprintf(stream, "I32");
	  case IR_I64: return fprintf(stream, "I64");

	  case IR_U8: return fprintf(stream, "U8");
	  case IR_U16: return fprintf(stream, "U16");
	  case IR_U32: return fprintf(stream, "U32");
	  case IR_U64: return fprintf(stream, "U64");

	  case IR_F32: return fprintf(stream, "F32");
	  case IR_F64: return fprintf(stream, "F64");
	  case IR_F80: return fprintf(stream, "F80");

	  case IR_LITERAL: return fprintf(stream, "LIT");

	  case IR_P_AGG: return fprintf(stream, "AGG");

	  case IR_PTR: return fprintf(stream, "PTR");
	  case IR_LABEL: return fprintf(stream, "LABEL");
	  case IR_VOID: return fprintf(stream, "VOID");

	  default: unreachable("Unknown Ir Type");
	}

	return 0;
}

static int32_t dumpBasicBlockId(FILE *stream, IrBasicBlock *bb) {
  return fprintf(stream, "#%u", bb->id);
}

static int32_t dumpIrBlockHeader(FILE *stream, const IrBasicBlock *b) {
  int32_t r = fprintf(stream, "BB #%u, '%s'", b->id, b->name);

  if (b->preds.head) {
	r += fprintf(stream, ", <-");
	for (IrBasicBlockListNode *pn = b->preds.head; pn != NULL; pn  = pn->next) {
	  r += fprintf(stream, " #%u", pn->block->id);
	}
  }

  if (b->succs.head) {
	r += fprintf(stream, ", ->");
	for (IrBasicBlockListNode *sn = b->succs.head; sn != NULL; sn  = sn->next) {
	  r += fprintf(stream, " #%u", sn->block->id);
	}
  }

  return r;
}

static int32_t dumpIrInstructionKind(FILE *stream, const enum IrIntructionKind kind) {
  assert(IR_BAD <= kind && kind < IR_INSTRUCTION_COUNT);
  return fprintf(stream, "%s", irInstructionsInfo[kind].mnemonic);
}

static int32_t dumpIrOperand(FILE *stream, const IrOperand *op);

static int32_t dumpIrOperand(FILE *stream, const IrOperand *op) {
  switch (op->kind) {
	case IR_CONST:
	  return fprintf(stream, "#%u", op->data.literalIndex);
	case IR_VREG:
	  return fprintf(stream, "%c%u", '%', op->data.vid);
	case IR_PREG:
	  return fprintf(stream, "$%u", op->data.pid);
	case IR_LOCAL:
	  return fprintf(stream, "@%u", op->id);
	case IR_BLOCK:
	  return fprintf(stream, "BB#%u", op->data.bb->id);
	case IR_MEMORY: {
	  int32_t r = 0;
	  r += fputc('[', stream);
	  r += dumpIrOperand(stream, op->data.address.base);
	  r += fputc('+', stream);
	  r += dumpIrOperand(stream, op->data.address.offset);
	  r += fputc(']', stream);
	  return r;
	}
	case IR_REFERENCE:
	  return fprintf(stream, "<%s>", op->data.symbol->name);
	case IR_FRAME_PTR:
	  return fprintf(stream, "@FP");
  }
}

static int32_t dumpIrOperandList(FILE *stream, const IrOperandList *list) {
  int32_t r = 0;
  Boolean first = TRUE;
  for (IrOperandListNode *on = list->head; on != NULL; on = on->next) {
	if (first)
	  first = FALSE;
	else
	  r += fprintf(stream, ", ");

	r += dumpIrOperand(stream, on->op);
  }
  return r;
}

static int32_t dumpIrInstructionExtra(FILE *stream, const IrInstruction *instr) {
  int32_t r = 0;

  switch (instr->kind) {
  case IR_E_BITCAST:
	r += fputc('[', stream);
	r += dumpIrType(stream, instr->info.fromCastType);
	r += fprintf(stream, "->");
	r += dumpIrType(stream, instr->uses.head->op->type);
	r += fputc(']', stream);
	break;
  case IR_TBRANCH:
	r += fprintf(stream, "[TABLE_SIZE = %u", instr->meta.switchTable->caseCount);
	if (instr->meta.switchTable->defaultBB) {
	  r += fprintf(stream, ", default = #%u", instr->meta.switchTable->defaultBB->id);
	}
	r += fputc(']', stream);
	break;
  default: // shut up compiler
	break;
  }

  return r;
}

static int32_t dumpIrInstruction(FILE *stream, const IrInstruction *instr) {
  int32_t r = dumpIrInstructionKind(stream, instr->kind);
  r += fputc(' ', stream);

  int32_t r2 = dumpIrInstructionExtra(stream, instr);

  if (r2) {
	r += r2;
  	r += fputc(' ', stream);
  }

  if (instr->uses.head) {
	r += fputc('(', stream);
	r += dumpIrOperandList(stream, &instr->uses);
	r += fputc(')', stream);
  }

  if (instr->defs.head) {
	r += fprintf(stream, " => ");
	r += dumpIrOperandList(stream, &instr->defs);
  }

  return r;
}

static int32_t dumpIrBlockPhis(FILE *stream, const IrBasicBlock *b) {
  int32_t r = 0;

  return r;
}

int32_t dumpIrBlock(FILE *stream, const IrBasicBlock *b) {

  int32_t r = dumpIrBlockHeader(stream, b);
  r += fputc('\n', stream);

  r += dumpIrBlockPhis(stream, b);
  r += fputc('\n', stream);

  for (IrInstructionListNode *in = b->instrs.head; in != NULL; in = in->next) {
	r += fprintf(stream, "  ");
	r += dumpIrInstruction(stream, in->instr);
	r += fputc('\n', stream);
  }

  return r;
}

int32_t dumpIrFunction(FILE *stream, const IrFunction *f) {
	int32_t r = fprintf(stream, "Function '%s'\n", f->ast->declaration->name);

	for (IrBasicBlockListNode *bn = f->blocks.head; bn != NULL; bn = bn->next) {
		r += dumpIrBlock(stream, bn->block);
		r += fputc('\n', stream);
	}
	return r;
}

void dumpIrFunctionList(const char *fileName, const IrFunctionList *functions) {
  FILE *f = fopen(fileName, "w");
  if (f == NULL) {
	fprintf(stderr, "cannot oopen ir dump file '%s'\n", fileName);
	return;
  }

  for (IrFunctionListNode *fn = functions->head; fn; fn = fn->next) {
	fprintf(stdout, "Dump function '%s'\n", fn->function->ast->declaration->name);
	dumpIrFunction(f, fn->function);
	fputc('\n', f);
  }

  fclose(f);
}



