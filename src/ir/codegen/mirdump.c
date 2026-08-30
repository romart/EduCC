
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

  if (addr->kind == MAK_SYMBOL) {
    r += fprintf(stream, "%s", addr->anchor.symbol->name);
    empty = FALSE;
  }

  if (addr->kind == MAK_FRAME) {
    // Spelled exactly like the MO_FRAME_IDX operand a spill carries, because
    // it names the same thing - the frame pointer it is measured from is
    // implied there and here alike.
    r += fprintf(stream, "fi#%d", addr->anchor.frameIdx);
    empty = FALSE;
  }

  if (addr->kind == MAK_CONSTANT) {
    // The bytes, not just the index: a pool entry read as 'cp#0' would send
    // the reader to a table that is not in the dump, and the whole point of
    // these baselines is that a wrong constant is visible in them.
    const MachineConstant *c = machineConstantAt(mf, addr->anchor.constantIdx);

    r += fprintf(stream, "cp#%u ", addr->anchor.constantIdx);
    r += dumpQuotedBytes(stream, c->bytes, c->size);
    empty = FALSE;
  }

  // Both spelled the way a branch target is, since that is what they resolve
  // to: a place in this function rather than a name the linker knows.
  if (addr->kind == MAK_BLOCK) {
    r += fprintf(stream, "#%u", addr->anchor.block->id);
    empty = FALSE;
  }

  if (addr->kind == MAK_JUMPTABLE) {
    r += fprintf(stream, "jt#%u", addr->anchor.jumpTableIdx);
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

static int32_t dumpMachineOpcode(FILE *stream, const MachineFunction *mf, const MachineInstr *mi) {
  int32_t r = 0;

  if (mi->opcode < MOP_GENERIC_COUNT) {
    r += fprintf(stream, "%s", genericOpcodeNames[mi->opcode]);
  } else {
    const char *name = targetOpcodeName(mf->target, mi->opcode);
    r += name != NULL ? fprintf(stream, "%s", name) : fprintf(stream, "op#%u", mi->opcode);
  }

  if (mi->opSize != 0) {
    r += fprintf(stream, ".%u", mi->opSize);
    // 'movsx.8/1' - eight bytes out of one. Printed only for the instructions
    // that name two widths, so every other line keeps the single suffix.
    if (mi->srcSize != 0 && mi->srcSize != mi->opSize) {
      r += fprintf(stream, "/%u", mi->srcSize);
    }
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

  r += dumpMachineOpcode(stream, mf, mi);

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

  // Printed rather than left implied by the opcode, because it is a claim
  // about the machine function and not about x86: whoever adds an allocator
  // that keeps values in registers has to see which instructions it applies to
  // in the dump, the same way they see every other clobber.
  if (mi->flags.isCall) {
    r += fprintf(stream, " <clobbers caller-saved>");
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

static const char *frameObjectKindName(enum MachineFrameObjectKind kind) {
  switch (kind) {
  case MFO_LOCAL: return "local";
  case MFO_INCOMING_PARAM: return "param";
  case MFO_CALL_RESULT: return "call-result";
  case MFO_SCRATCH: return "scratch";
  case MFO_SPILL: return "spill";
  default: return "?";
  }
}

static int32_t dumpMachineFrame(FILE *stream, const MachineFunction *mf) {
  const MachineFrame *frame = &mf->frame;

  if (frame->objects.size == 0) {
    return fprintf(stream, "Frame: <empty>\n");
  }

  int32_t r = fprintf(stream, "Frame: %u bytes%s\n", frame->size,
                      frame->hasDynamicAlloca ? ", dynamic" : "");

  for (size_t idx = 0; idx < frame->objects.size; ++idx) {
    const MachineFrameObject *obj = machineFrameObjectAt(mf, (int32_t)idx);

    r += fprintf(stream, "  fi#%lu : %s ", idx, frameObjectKindName(obj->kind));

    if (obj->isDynamic) {
      r += fprintf(stream, "dynamic");
    } else {
      r += fprintf(stream, "%u/%u @ %d", obj->size, obj->alignment, obj->offset);
    }

    if (obj->vreg != 0) {
      r += fprintf(stream, " ");
      r += dumpMachineRegister(stream, mf, obj->vreg);
    }

    if (obj->declaration != NULL) {
      r += fprintf(stream, " '%s'", obj->declaration->name);
    }

    if (obj->origin != NULL) {
      r += fprintf(stream, " ; %c%u", '%', obj->origin->id);
    }

    r += fputc('\n', stream);
  }

  return r;
}

// The entries, not just how many: a switch dispatched to the wrong block is
// exactly what a table gets wrong, and it is invisible in the dispatch itself -
// those seven instructions are the same whatever the table holds.
static int32_t dumpMachineJumpTables(FILE *stream, const MachineFunction *mf) {
  int32_t r = 0;

  for (size_t idx = 0; idx < mf->jumpTables.size; ++idx) {
    const MachineJumpTable *jt = machineJumpTableAt(mf, idx);

    r += fprintf(stream, "  jt#%lu :", idx);
    for (uint32_t entry = 0; entry < jt->count; ++entry) {
      r += fprintf(stream, " #%u", jt->entries[entry]->id);
    }
    r += fputc('\n', stream);
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
      r += fprintf(stream, "  %cv%lu : %s/%u", '%', idx, regClassName(vri->rc), vri->size);
      if (vri->origin != NULL) {
        r += fprintf(stream, " ; %c%u", '%', vri->origin->id);
      }
      r += fputc('\n', stream);
    }
  }

  r += dumpMachineFrame(stream, mf);

  // Only when there are any, so that every baseline taken before switches were
  // selected reads exactly as it did.
  if (mf->jumpTables.size != 0) {
    r += fprintf(stream, "Jump tables:\n");
    r += dumpMachineJumpTables(stream, mf);
  }

  // Both of these are empty until register allocation has run, and are printed
  // only when they are not, so that a dump taken before it looks exactly as it
  // did before there was an allocator.
  if (mf->usedPhysRegs != 0) {
    r += fprintf(stream, "Physical registers used:");
    for (uint32_t reg = 0; reg < IR_PHYS_REG_MAX; ++reg) {
      if (mf->usedPhysRegs & ((uint64_t)1 << reg)) {
        r += fputc(' ', stream);
        r += dumpMachineRegister(stream, mf, reg);
      }
    }
    r += fputc('\n', stream);
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
