
#include "ir/ir.h"
#include "sema.h"
#include "treeDump.h"
#include <assert.h>

extern IrContext *ctx;

int32_t dumpQuotedBytes(FILE *stream, const char *bytes, size_t size) {
  int32_t r = fputc('"', stream);

  // Every literal ends in one and printing it would only add noise. Interior
  // NULs stay, escaped: a literal is bytes and a length, so stopping at the
  // first NUL - which is what printing it as a C string does - would hide the
  // rest of a constant the compiler is perfectly happy to carry.
  if (size > 0 && bytes[size - 1] == '\0') {
    --size;
  }

  for (size_t i = 0; i < size; ++i) {
    unsigned char c = (unsigned char)bytes[i];
    switch (c) {
    case '\n': r += fprintf(stream, "\\n"); break;
    case '\t': r += fprintf(stream, "\\t"); break;
    case '\r': r += fprintf(stream, "\\r"); break;
    case '"':  r += fprintf(stream, "\\\""); break;
    case '\\': r += fprintf(stream, "\\\\"); break;
    default:
      // Octal, not raw: a dump is compared as text by the test runner, and one
      // byte that is not valid UTF-8 makes the whole file unreadable to it.
      r += (c >= 0x20 && c < 0x7f) ? fputc(c, stream) : fprintf(stream, "\\%03o", c);
    }
  }

  return r + fputc('"', stream);
}

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


const char *irInstructionMnemonic(enum IrIntructionKind kind) {
  size_t count = sizeof(irInstructionsInfo) / sizeof(irInstructionsInfo[0]);
  return kind < count ? irInstructionsInfo[kind].mnemonic : "IR_?";
}

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
      case IR_REF: return fprintf(stream, "REF");

	  default: unreachable("Unknown Ir Type");
	}

	return 0;
}

static int32_t dumpBasicBlockId(FILE *stream, IrBasicBlock *bb) {
  return fprintf(stream, "#%u", bb->id);
}

static int32_t dumpIrBlockHeader(FILE *stream, const IrBasicBlock *b) {
  int32_t r = fprintf(stream, "BB #%u, '%s'", b->id, b->name);

  const Vector *preds = &b->preds;
  if (preds->size > 0) {
	r += fprintf(stream, ", <-");
	for (size_t idx = 0; idx < preds->size; ++idx) {
      IrBasicBlock *pred = getBlockFromVector(preds, idx);
	  r += fprintf(stream, " #%u", pred->id);
	}
  }

  const Vector *succs = &b->succs;
  if (succs->size > 0) {
	r += fprintf(stream, ", ->");
	for (size_t idx = 0; idx < succs->size; ++idx) {
      IrBasicBlock *succ = getBlockFromVector(succs, idx);
	  r += fprintf(stream, " #%u", succ->id);
	}
  }

  if (b->dominators.sdom) {
	r += fprintf(stream, ", strict dom #%u", b->dominators.sdom->id);
  }

  const Vector *df = &b->dominators.dominationFrontier;
  if (df->size > 0) {
	r += fprintf(stream, ", domination frontier [");
    Boolean first = TRUE;

	for (size_t idx = 0; idx < df->size; ++idx) {
      IrBasicBlock *fb = getBlockFromVector(df, idx);
      if (first)
        first = FALSE;
      else
        r += fprintf(stream, ", ");
	  r += fprintf(stream, "#%u", fb->id);
	}
    r += fputc(']', stream);
  }

  const Vector *dominatees = &b->dominators.dominatees;
  if (dominatees->size > 0) {
    r += fprintf(stream, ", dominatees [");
    Boolean first = TRUE;

	for (size_t idx = 0; idx < dominatees->size; ++idx) {
      IrBasicBlock *d = getBlockFromVector(dominatees, idx);
      if (first)
        first = FALSE;
      else
        r += fprintf(stream, ", ");
	  r += fprintf(stream, "#%u", d->id);
	}
    r += fputc(']', stream);

  }

  return r;
}

static int32_t dumpIrInstructionKind(FILE *stream, const enum IrIntructionKind kind) {
  assert(IR_BAD <= kind && kind < IR_INSTRUCTION_COUNT);
  return fprintf(stream, "%s", irInstructionsInfo[kind].mnemonic);
}

static int32_t dumpIrInstructionExtra(FILE *stream, const IrInstruction *instr) {
  int32_t r = fputc('[', stream);

  switch (instr->kind) {
  case IR_M_LOAD:
    r += fprintf(stream, "load type = ");
    r += dumpIrType(stream, instr->info.memory.opType);
    break;
  case IR_M_STORE:
    r += fprintf(stream, "store type = ");
    r += dumpIrType(stream, instr->info.memory.opType);
    break;
  case IR_CALL:
    if (instr->info.call.symbol != NULL) {
      r += fprintf(stream, "symbol = %s", instr->info.call.symbol->name);
    }
  case IR_ALLOCA:
    if (instr->info.alloca.sizeInstr != NULL) {
      r += fprintf(stream, "size = %c%u", '%', instr->info.alloca.sizeInstr->id);
    } else {
      r += fprintf(stream, "size = %lu", instr->info.alloca.stackSize);
    }
    if (instr->info.alloca.v) {
      r += fprintf(stream, ", declaration = %u | '%s'", instr->info.alloca.v->index2, instr->info.alloca.v->name);
    }
    break;
  case IR_CFG_LABEL:
    r += fprintf(stream, "block = #%u", instr->info.block->id);
    break;
  case IR_BRANCH:
    r += fprintf(stream, "target = #%u", instr->info.branch.taken->id);
    break;
  case IR_CBRANCH:
    r += fprintf(stream, "taken = #%u, non taken = #%u", instr->info.branch.taken->id, instr->info.branch.notTaken->id);
    break;
  case IR_DEF_CONST:
    switch (instr->info.constant.kind) {
      case IR_CK_FLOAT:
            r += fprintf(stream, "kind = float, v = %lf", (double)instr->info.constant.data.f);
            break;
      case IR_CK_INTEGER:
            r += fprintf(stream, "kind = integer, v = %lld", instr->info.constant.data.i);
            break;
      case IR_CK_LITERAL:
            r += fprintf(stream, "kind = literal, l = %lu, s = ", instr->info.constant.data.l.length);
            r += dumpQuotedBytes(stream, instr->info.constant.data.l.s, instr->info.constant.data.l.length);
            break;
      case IR_CK_SYMBOL:
            r += fprintf(stream, "kind = symbol, name = %s", instr->info.constant.data.s->name);
            break;
    }
    r += fprintf(stream, ", id = %u", instr->info.constant.cacheIdx);
    break;
  case IR_GET_ELEMENT_PTR:
    r += fprintf(stream, "underlying type = ");
    r += dumpTypeRef(stream, instr->info.gep.underlyingType);
    if (instr->info.gep.member != NULL) {
      r += fprintf(stream, ", member = %s", instr->info.gep.member->name);
    }
    if (instr->info.gep.indexInstr != NULL) {
      r += fprintf(stream, ", index = %c%u", '%', instr->info.gep.indexInstr->id);
    }
    break;
  case IR_M_COPY:
    r += fprintf(stream, "elementType = ");
    r += dumpTypeRef(stream, instr->info.copy.elementType);
    if (instr->info.copy.elementCount) {
        r += fprintf(stream, ", count = %c%u", '%', instr->info.copy.elementCount->id);
    }
    break;
  case IR_P_REG: {
    // Print the name rather than the raw id. The ids are target register
    // numbers, so '$7' says nothing while '$rdi' says which ABI slot this is -
    // which is the whole thing these dumps are being read for.
    const char *name = physRegName(ctx->target, instr->info.physReg);
    if (name != NULL) {
      r += fprintf(stream, "preg = $%s", name);
    } else {
      r += fprintf(stream, "preg = $%u", instr->info.physReg);
    }
    break;
  }
  case IR_E_BITCAST:
	r += dumpIrType(stream, instr->info.fromCastType);
	r += fprintf(stream, "->");
	r += dumpIrType(stream, instr->type);
	break;

  case IR_TBRANCH:
	r += fprintf(stream, "TABLE_SIZE = %u", instr->info.switchTable->caseCount);
	if (instr->info.switchTable->defaultBB) {
	  r += fprintf(stream, ", default = #%u", instr->info.switchTable->defaultBB->id);
	}
	break;
  case IR_PHI: {
    AstValueDeclaration *v = instr->info.phi.declaration;
    if (v != NULL) {
      r += fprintf(stream, "var = %s", v->name);
    }
    break;
  }
  default: // shut up compiler
	break;
  }

  r += fputc(']', stream);
  return r;
}

static int32_t dumpIrInstructionInputs(FILE *stream, const IrInstruction *instr) {
  int32_t r = 0;
  Boolean first = TRUE;
  for (size_t i = 0; i < instr->inputs.size; ++i) {
    const IrInstruction *input = getInstructionFromVector(&instr->inputs, i);
    if (first)
      first = FALSE;
    else
      r += fprintf(stream, ", ");
    r += fprintf(stream, "%c%u", '%', input->id);
  }

  return r;
}

static int32_t dumpPhiInputs(FILE *stream, const IrInstruction *phi) {
  assert(phi->kind == IR_PHI);
  int32_t r = 0;
  Boolean first = TRUE;
  const Vector *inputs = &phi->inputs;
  const Vector *blocks = &phi->info.phi.phiBlocks;

  assert(inputs->size == blocks->size);

  for (size_t i = 0; i < inputs->size; ++i) {
    const IrInstruction *input = getInstructionFromVector(inputs, i);
    const IrBasicBlock *block = getBlockFromVector(blocks, i);
    if (first)
      first = FALSE;
    else
      r += fprintf(stream, ", ");
    r += fprintf(stream, "[%c%u, #%u]", '%', input->id, block->id);
  }

  return r;
}

static int32_t dumpIrInstruction(FILE *stream, const IrInstruction *instr) {

  int32_t r = fprintf(stream, "%c%u = ", '%', instr->id);
  r += dumpIrInstructionKind(stream, instr->kind);
  r += fputc(' ', stream);
  r += fputc('(', stream);
  r += instr->kind == IR_PHI
    ? dumpPhiInputs(stream, instr)
    : dumpIrInstructionInputs(stream, instr);
  r += fputc(')', stream);
  r += fputc(' ', stream);

  r += dumpIrInstructionExtra(stream, instr);
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

  for (IrInstruction *i = b->instrunctions.head; i != NULL; i = i->next) {
	r += fprintf(stream, "  ");
	r += dumpIrInstruction(stream, i);
	r += fputc('\n', stream);
  }

  return r;
}

int32_t dumpIrFunction(FILE *stream, const IrFunction *f) {
	int32_t r = fprintf(stream, "Function '%s'\n", f->ast->declaration->name);

	r += fprintf(stream, "Locals:\n");
	for (size_t i = 0; i < f->numOfLocalSlots; ++i) {
	  const LocalValueInfo *lvi = &f->localOperandMap[i];
	  AstValueDeclaration *d = lvi->declaration;
      char mark = d != NULL ? (d->kind == VD_PARAMETER ? 'p' : 'l') : 'r';
      int32_t idx = d ? d->index2 : -1;
      const char *name = d ? d->name : lvi->name ? lvi->name : "<ret_slot>";


	  r += fprintf(stream, "  %c%c:%d|%s = %c%u", lvi->flags.referenced ? '&' : ' ', mark, idx, name, '%', lvi->stackSlot->id);
	  r += fputc('\n', stream);
	}

	for (IrBasicBlock *block = f->blocks.head; block != NULL; block = block->next) {
		r += dumpIrBlock(stream, block);
		r += fputc('\n', stream);
	}
	return r;
}

void dumpIrFunctionPhase(FILE *stream, const IrFunction *f, const char *phaseName) {
  fprintf(stream, "--- Phase: %s ---\n", phaseName);
  dumpIrFunction(stream, f);
  fputc('\n', stream);
}

void dumpIrFunctionList(const char *fileName, const IrFunctionList *functions) {
  FILE *f = fopen(fileName, "w");
  if (f == NULL) {
	fprintf(stderr, "cannot open ir dump file '%s'\n", fileName);
	return;
  }

  for (IrFunctionListNode *fn = functions->head; fn; fn = fn->next) {
    IrFunction *func = fn->function;
    if (func->ast == NULL)
      continue;
	fprintf(stdout, "Dump function '%s'\n", func->ast->declaration->name);
	dumpIrFunction(f, func);
	fputc('\n', f);
  }

  fclose(f);
}

static void buildDotForFunction(FILE *stream, const IrFunction *f) {
    const char *funcName = f->ast ? f->ast->declaration->name : "__test";
    fprintf(stream, "    label = \"%s\";\n", funcName);

    for (const IrBasicBlock *bb = f->blocks.head; bb != NULL; bb = bb->next) {
      fprintf(stream, "    %s_%u [label=\"#%u", funcName, bb->id, bb->id);
      if (bb->name) {
        fprintf(stream, " | %s", bb->name);
      }
      fprintf(stream, "\"];\n");
    }

    for (const IrBasicBlock *block = f->blocks.head; block != NULL; block = block->next) {
      const Vector *succs = &block->succs;

      for (size_t idx = 0; idx < succs->size; ++idx) {
        const IrBasicBlock *succ = getBlockFromVector(succs, idx);
        fprintf(stream, "    %s_%u -> %s_%u [style = \"solid\", color=\"black\"];\n", funcName, block->id, funcName, succ->id);
      }

      if (block->dominators.sdom) {
        const IrBasicBlock *dom = block->dominators.sdom;
        fprintf(stream, "    %s_%u -> %s_%u [style = \"bold\", color = \"green\"];\n", funcName, block->id, funcName, dom->id);
      }

      const Vector *df = &block->dominators.dominationFrontier;
      for (size_t idx = 0; idx < df->size; ++idx) {
        const IrBasicBlock *f = getBlockFromVector(df, idx);
        fprintf(stream, "    %s_%u -> %s_%u [style = \"dashed\", color = \"blue\"];\n", funcName, block->id, funcName, f->id);
      }
    }
}

void buildDotGraphForFunctionList(const char *fileName, const IrFunctionList *functions) {

  FILE *f = fopen(fileName, "w");
  if (f == NULL) {
	fprintf(stderr, "cannot open dot graph file '%s'\n", fileName);
	return;
  }

  fprintf(f, "digraph CFG {\n");

  uint32_t id = 0;
  for (IrFunctionListNode *fn = functions->head; fn; fn = fn->next) {
	fprintf(f, "  subgraph cluster_%u {\n", ++id);
	buildDotForFunction(f, fn->function);
    fprintf(f, "  }\n");
  }

  fprintf(f, "}\n");

  fclose(f);

}

