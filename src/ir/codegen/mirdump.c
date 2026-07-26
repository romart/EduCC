
#include <assert.h>

#include "ir/ir.h"
#include "ir/machine.h"
#include "sema.h"

static const char *const genericOpcodeNames[] = {
#define MOP_DEF(m, _) "MOP_" #m
    MACHINE_GENERIC_OPCODES
#undef MOP_DEF
};

static const char *regClassName(enum RegClass rc) {
  switch (rc) {
  case RC_GP: return "gp";
  case RC_FP: return "fp";
  default: return "none";
  }
}

static int32_t dumpMachineRegister(FILE *stream, const MachineFunction *mf, uint32_t reg) {
  if (reg == NO_REG) {
    return fprintf(stream, "$none");
  }

  if (isVirtualRegister(reg)) {
    // Printed by index rather than by raw id, so the dumps stay readable and
    // do not all shift if FIRST_VREG ever moves. '$' vs '%' is what says
    // physical vs virtual.
    return fprintf(stream, "%c%c%u", '%', 'v', reg - FIRST_VREG);
  }

  const char *name = physRegName(mf->target, reg);
  return name != NULL ? fprintf(stream, "$%s", name) : fprintf(stream, "$r%u", reg);
}

static int32_t dumpMachineAddress(FILE *stream, const MachineFunction *mf,
                                  const MachineAddress *addr) {
  int32_t r = fputc('[', stream);
  Boolean empty = TRUE;

  if (addr->symbol != NULL) {
    r += fprintf(stream, "%s", addr->symbol->name);
    empty = FALSE;
  }

  if (addr->base != NO_REG) {
    if (!empty) {
      r += fprintf(stream, " + ");
    }
    r += dumpMachineRegister(stream, mf, addr->base);
    empty = FALSE;
  }

  if (addr->index != NO_REG) {
    if (!empty) {
      r += fprintf(stream, " + ");
    }
    r += dumpMachineRegister(stream, mf, addr->index);
    r += fprintf(stream, "*%u", addr->scale);
    empty = FALSE;
  }

  if (addr->disp != 0 || empty) {
    if (!empty) {
      r += fprintf(stream, addr->disp < 0 ? " - " : " + ");
      r += fprintf(stream, "%d", addr->disp < 0 ? -addr->disp : addr->disp);
    } else {
      r += fprintf(stream, "%d", addr->disp);
    }
  }

  r += fputc(']', stream);
  return r;
}

static int32_t dumpMachineOperand(FILE *stream, const MachineFunction *mf,
                                  const MachineOperand *op) {
  int32_t r = 0;

  switch (op->kind) {
  case MO_REG:
    r += dumpMachineRegister(stream, mf, op->info.reg);
    if (op->flags.isKill) {
      r += fprintf(stream, "<kill>");
    }
    if (op->flags.isEarlyClobber) {
      r += fprintf(stream, "<early-clobber>");
    }
    break;
  case MO_IMM:
    r += fprintf(stream, "%lld", (long long)op->info.imm);
    break;
  case MO_MEM:
    r += dumpMachineAddress(stream, mf, &op->info.mem);
    break;
  case MO_FRAME_IDX:
    r += fprintf(stream, "fi#%d", op->info.frameIdx);
    break;
  case MO_MBB:
    r += fprintf(stream, "#%u", op->info.mbb->id);
    break;
  case MO_SYMBOL:
    r += fprintf(stream, "%s", op->info.symbol->name);
    break;
  case MO_NONE:
    // An operand the instruction reserved room for and nobody filled in. Worth
    // seeing in a dump rather than hiding: it means whoever built the
    // instruction miscounted its operands.
    r += fprintf(stream, "<unset>");
    break;
  default:
    unreachable("Unknown machine operand kind");
  }

  return r;
}

static int32_t dumpMachineOpcode(FILE *stream, const MachineInstr *mi) {
  int32_t r = 0;

  if (mi->opcode < MOP_GENERIC_COUNT) {
    r += fprintf(stream, "%s", genericOpcodeNames[mi->opcode]);
  } else {
    // TODO: target opcodes take their mnemonic from the target descriptor once
    // instruction selection exists and there is a table to take it from
    // (see docs/ir-codegen-design.md, step 4). Nothing creates one yet.
    r += fprintf(stream, "op#%u", mi->opcode);
  }

  if (mi->opSize != 0) {
    r += fprintf(stream, ".%u", mi->opSize);
  }

  return r;
}

// Explicit operands are printed positionally, defs before the '=' and uses
// after the mnemonic; implicit ones - clobbers and flag registers, which are
// noise in that position and would misalign the def/use split - go in a
// trailing bracket instead.
static int32_t dumpMachineInstr(FILE *stream, const MachineFunction *mf,
                                const MachineInstr *mi) {
  int32_t r = 0;
  Boolean first = TRUE;

  for (uint16_t idx = 0; idx < mi->numDefs; ++idx) {
    const MachineOperand *op = &mi->operands[idx];
    if (op->flags.isImplicit) {
      continue;
    }
    if (first) {
      first = FALSE;
    } else {
      r += fprintf(stream, ", ");
    }
    r += dumpMachineOperand(stream, mf, op);
  }

  if (!first) {
    r += fprintf(stream, " = ");
  }

  r += dumpMachineOpcode(stream, mi);

  first = TRUE;
  for (uint16_t idx = mi->numDefs; idx < mi->numOperands; ++idx) {
    const MachineOperand *op = &mi->operands[idx];
    if (op->flags.isImplicit) {
      continue;
    }
    r += fprintf(stream, first ? " " : ", ");
    first = FALSE;
    r += dumpMachineOperand(stream, mf, op);
  }

  first = TRUE;
  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    const MachineOperand *op = &mi->operands[idx];
    if (!op->flags.isImplicit) {
      continue;
    }
    r += fprintf(stream, first ? " [" : ", ");
    first = FALSE;
    r += fprintf(stream, op->flags.isDef ? "implicit-def " : "implicit ");
    r += dumpMachineOperand(stream, mf, op);
  }
  if (!first) {
    r += fputc(']', stream);
  }

  if (mi->origin != NULL) {
    r += fprintf(stream, " ; %c%u", '%', mi->origin->id);
  }

  return r;
}

static int32_t dumpMachineBlockHeader(FILE *stream, const MachineBasicBlock *mbb) {
  int32_t r = fprintf(stream, "MBB #%u, '%s'", mbb->id, mbb->name ? mbb->name : "");

  if (mbb->ir != NULL) {
    r += fprintf(stream, ", ir #%u", mbb->ir->id);
  }

  const Vector *preds = &mbb->preds;
  if (preds->size > 0) {
    r += fprintf(stream, ", <-");
    for (size_t idx = 0; idx < preds->size; ++idx) {
      const MachineBasicBlock *pred = (const MachineBasicBlock *)getFromVector(preds, idx);
      r += fprintf(stream, " #%u", pred->id);
    }
  }

  const Vector *succs = &mbb->succs;
  if (succs->size > 0) {
    r += fprintf(stream, ", ->");
    for (size_t idx = 0; idx < succs->size; ++idx) {
      const MachineBasicBlock *succ = (const MachineBasicBlock *)getFromVector(succs, idx);
      r += fprintf(stream, " #%u", succ->id);
    }
  }

  return r;
}

int32_t dumpMachineFunction(FILE *stream, const MachineFunction *mf) {
  const char *name = mf->ast ? mf->ast->declaration->name : "<unnamed>";
  int32_t r = fprintf(stream, "MachineFunction '%s' [target = %s]\n", name, mf->target->name);

  if (mf->vregs.size == 0) {
    r += fprintf(stream, "VRegs: <none>\n");
  } else {
    r += fprintf(stream, "VRegs:\n");
    for (size_t idx = 0; idx < mf->vregs.size; ++idx) {
      const VRegInfo *vri = (const VRegInfo *)getFromVector(&mf->vregs, idx);
      r += fprintf(stream, "  %cv%lu : %s/%u\n", '%', idx, regClassName(vri->rc), vri->size);
    }
  }

  for (const MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    r += dumpMachineBlockHeader(stream, mbb);
    r += fputc('\n', stream);

    for (const MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      r += fprintf(stream, "  ");
      r += dumpMachineInstr(stream, mf, mi);
      r += fputc('\n', stream);
    }

    r += fputc('\n', stream);
  }

  return r;
}

void dumpMachineFunctionPhase(FILE *stream, const MachineFunction *mf, const char *phaseName) {
  fprintf(stream, "--- Phase: %s ---\n", phaseName);
  dumpMachineFunction(stream, mf);
  fputc('\n', stream);
}
