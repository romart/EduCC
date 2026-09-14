#include <assert.h>
#include <string.h>

#include "codegen.h"
#include "mem.h"
#include "ir/emit.h"
#include "ir/ir.h"
#include "instructions_riscv64.h"
#include "machine_riscv64.h"

// -============================ riscv64 emission (stage 3) ================-
//
// One MachineInstr at a time into the assembler in instructions_riscv64.c, the
// mirror of src/x86_64/emit_x86_64.c. See docs/ir-codegen-design.md section 8
// for what stage 3 owns.
//
// Two things are simpler here than on x86-64, and both follow from a
// fixed-width ISA:
//
//   * There is no jump relaxation and no second emission pass. Every RV64G
//     instruction is four bytes, so a branch's displacement is the only thing
//     a forward reference leaves unknown, and patching it afterwards cannot
//     change any address. x86's emit-measure-emit loop exists because a
//     shorter jump moves everything after it; nothing moves here.
//   * Labels are patched in place rather than through struct Label. A RISC-V
//     branch displacement is scattered across five fields of the word, so
//     "write the displacement" is a re-encode either way, and the block
//     offsets this needs are a flat array.

// Callee-saved in LP64D, minus sp and s0, which the frame handles itself.
static const enum XRegister riscvCalleeSaved[] = {
  X_S1, X_S2, X_S3, X_S4, X_S5, X_S6, X_S7, X_S8, X_S9, X_S10, X_S11
};

#define CALLEE_SAVED_COUNT (sizeof(riscvCalleeSaved) / sizeof(riscvCalleeSaved[0]))

// The f-register half, saved the same way. fs0 is an ordinary callee-saved
// register here - unlike s0, which is the frame pointer, the f file has no
// such role.
static const enum FRegister riscvFpCalleeSaved[] = {
  F_S0, F_S1, F_S2, F_S3, F_S4, F_S5, F_S6, F_S7, F_S8, F_S9, F_S10, F_S11
};

#define FP_CALLEE_SAVED_COUNT (sizeof(riscvFpCalleeSaved) / sizeof(riscvFpCalleeSaved[0]))

// ra and s0, which every non-leaf prologue saves and which sit at the bottom
// of the frame - just above the outgoing-argument area. Putting them there
// rather than at the top is what lets every other frame object keep the
// offset stage 0 gave it: locals are addressed from s0 exactly as stage 0
// laid them out, and the pair is below all of them.
#define SAVED_PAIR_SIZE (2 * (int32_t)sizeof(intptr_t))

// A jump or branch that names a block, left with a zero displacement until the
// target's offset is known.
typedef struct _JumpSite {
  ptrdiff_t offset;     // where the instruction starts in the section
  uint32_t block;       // the block it names
  Boolean isBranch;     // B-type if so, J-type otherwise
} JumpSite;

typedef struct _EmitContext {
  GeneratedFunction *gen;
  MachineFunction *mf;

  // Where each machine block begins, relative to the function's own start;
  // -1 until it has been emitted.
  ptrdiff_t *blockOffset;

  JumpSite *jumps;
  size_t numJumps;
  size_t nextJump;

  // Where each saved callee-saved register lives, indexed as the two tables
  // above are; 0 for the ones this function does not use.
  int32_t calleeSavedOffset[CALLEE_SAVED_COUNT];
  int32_t fpCalleeSavedOffset[FP_CALLEE_SAVED_COUNT];

  int32_t frameSize;
} EmitContext;

// -============================ Small translations ========================-

static Boolean isFpReg(const EmitContext *e, uint32_t reg) {
  return e->mf->target->regClass[reg] == RC_FP;
}

// The flat namespace's x half is already the encoding; the f half needs the
// base subtracted. Both halves collapse onto the same five-bit register
// number, so an operand arriving in the wrong bank would encode as a different
// real register rather than failing - which is what the class assertions below
// exist to catch, exactly as on x86-64.
static enum XRegister xReg(const EmitContext *e, uint32_t reg) {
  assert(isPhysicalRegister(reg) && "a virtual register reached emission");
  assert(!isFpReg(e, reg) && "operand is in the wrong register bank");
  return (enum XRegister)reg;
}

static enum FRegister fReg(const EmitContext *e, uint32_t reg) {
  assert(isPhysicalRegister(reg) && "a virtual register reached emission");
  assert(isFpReg(e, reg) && "operand is in the wrong register bank");
  return (enum FRegister)(reg - RISCV_FP_BASE);
}

static uint32_t regIdOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_REG);
  return op->info.reg;
}

static int64_t immOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_IMM);
  return op->info.imm;
}

static int32_t frameIdxOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_FRAME_IDX);
  return op->info.frameIdx;
}

static MachineBasicBlock *blockOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_MBB);
  return op->info.mbb;
}

static ptrdiff_t here(const EmitContext *e) {
  return e->gen->section->pc - e->gen->section->start;
}

// A frame object's offset from s0. Stage 0 gave locals and spills negative
// offsets and incoming parameters positive ones, and both are used as they
// are: s0 points at the caller's stack pointer, so an argument the caller
// pushed is at s0 + 0 and everything this frame owns is below.
static int32_t frameOffset(const EmitContext *e, int32_t frameIdx) {
  const MachineFrameObject *obj = machineFrameObjectAt(e->mf, frameIdx);
  assert(obj != NULL);
  assert(!obj->isDynamic && "a dynamically placed object has no fixed address");
  return obj->offset;
}

// -============================ Wide offsets ==============================-
//
// Every load, store and stack adjustment carries a twelve-bit signed
// displacement, and a frame larger than 2047 bytes does not fit one. t0 is
// reserved for exactly this (see target_riscv64.c): nothing selection emits
// names it and no allocator hands it out, so building an address in it is
// always safe even at the point where the allocator has long since finished.

static Boolean fitsDisplacement(int64_t off) {
  return isInt(off, 12);
}

// A base register to address 'off' bytes from 'base' with, and the
// displacement to use with it: either the original pair, or t0 holding
// base + off and a displacement of zero.
static enum XRegister wideBase(EmitContext *e, enum XRegister base, int32_t *off) {
  if (fitsDisplacement(*off)) {
    return base;
  }

  riscvLoadImmediate(e->gen, *off, X_T0);
  riscvAdd(e->gen, X_T0, X_T0, base);
  *off = 0;
  return X_T0;
}

static void emitLoadWide(EmitContext *e, enum XRegister rd, enum XRegister base, int32_t off,
                         uint32_t width, Boolean isUnsigned) {
  enum XRegister b = wideBase(e, base, &off);
  riscvLoad(e->gen, rd, b, off, width, isUnsigned);
}

static void emitStoreWide(EmitContext *e, enum XRegister src, enum XRegister base, int32_t off,
                          uint32_t width) {
  enum XRegister b = wideBase(e, base, &off);
  riscvStore(e->gen, src, b, off, width);
}

static void emitFLoadWide(EmitContext *e, enum FRegister rd, enum XRegister base, int32_t off,
                          uint32_t width) {
  enum XRegister b = wideBase(e, base, &off);
  riscvFLoad(e->gen, rd, b, off, width);
}

static void emitFStoreWide(EmitContext *e, enum FRegister src, enum XRegister base, int32_t off,
                           uint32_t width) {
  enum XRegister b = wideBase(e, base, &off);
  riscvFStore(e->gen, src, b, off, width);
}

// 'addi rd, rs, imm', or the same through t0 when the constant is too wide.
// Used for the two stack adjustments, where the frame can be any size.
static void emitAddConst(EmitContext *e, enum XRegister rd, enum XRegister rs, int32_t imm) {
  if (fitsDisplacement(imm)) {
    riscvAddi(e->gen, rd, rs, imm);
    return;
  }

  riscvLoadImmediate(e->gen, imm, X_T0);
  riscvAdd(e->gen, rd, rs, X_T0);
}

// -============================ The frame =================================-

static int32_t layoutCalleeSaved(EmitContext *e) {
  int32_t offset = (int32_t)e->mf->frame.size;

  for (size_t idx = 0; idx < CALLEE_SAVED_COUNT; ++idx) {
    uint64_t bit = 1ULL << (uint64_t)riscvCalleeSaved[idx];
    if ((e->mf->usedPhysRegs & bit) == 0) {
      continue;
    }

    offset += sizeof(intptr_t);
    e->calleeSavedOffset[idx] = -offset;
  }

  for (size_t idx = 0; idx < FP_CALLEE_SAVED_COUNT; ++idx) {
    uint64_t bit = 1ULL << (uint64_t)(RISCV_FP_BASE + riscvFpCalleeSaved[idx]);
    if ((e->mf->usedPhysRegs & bit) == 0) {
      continue;
    }

    offset += sizeof(double);
    e->fpCalleeSavedOffset[idx] = -offset;
  }

  // The saved ra/s0 pair, then the outgoing-argument area below everything -
  // the stack pointer addresses that one, and the stack pointer is at the
  // bottom. LP64D wants sp 16-aligned at every call boundary, and rounding
  // here is the whole of what keeps it so: nothing moves sp between calls.
  offset += SAVED_PAIR_SIZE;

  return ALIGN_SIZE(offset, 2 * (int32_t)sizeof(intptr_t)) + (int32_t)e->mf->frame.outgoingSize;
}

// Where the saved ra and s0 sit, measured from the stack pointer.
static int32_t savedS0Offset(const EmitContext *e) {
  return (int32_t)e->mf->frame.outgoingSize;
}

static int32_t savedRaOffset(const EmitContext *e) {
  return savedS0Offset(e) + (int32_t)sizeof(intptr_t);
}

static void emitPrologue(EmitContext *e) {
  emitAddConst(e, X_SP, X_SP, -e->frameSize);

  // Saved before s0 is established, so both stores are sp-relative.
  emitStoreWide(e, X_RA, X_SP, savedRaOffset(e), sizeof(intptr_t));
  emitStoreWide(e, X_S0, X_SP, savedS0Offset(e), sizeof(intptr_t));

  // s0 points at the caller's stack pointer, which is what makes an incoming
  // stack argument sit at s0 + 0 - see TargetDescriptor.incomingStackParamOffset.
  emitAddConst(e, X_S0, X_SP, e->frameSize);

  for (size_t idx = 0; idx < CALLEE_SAVED_COUNT; ++idx) {
    if (e->calleeSavedOffset[idx] == 0) continue;
    emitStoreWide(e, riscvCalleeSaved[idx], X_S0, e->calleeSavedOffset[idx], sizeof(intptr_t));
  }

  for (size_t idx = 0; idx < FP_CALLEE_SAVED_COUNT; ++idx) {
    if (e->fpCalleeSavedOffset[idx] == 0) continue;
    emitFStoreWide(e, riscvFpCalleeSaved[idx], X_S0, e->fpCalleeSavedOffset[idx], sizeof(double));
  }
}

static void emitEpilogue(EmitContext *e) {
  // The callee-saved reloads are s0-relative, so they come before s0 itself is
  // restored.
  for (size_t idx = 0; idx < CALLEE_SAVED_COUNT; ++idx) {
    if (e->calleeSavedOffset[idx] == 0) continue;
    emitLoadWide(e, riscvCalleeSaved[idx], X_S0, e->calleeSavedOffset[idx], sizeof(intptr_t), FALSE);
  }

  for (size_t idx = 0; idx < FP_CALLEE_SAVED_COUNT; ++idx) {
    if (e->fpCalleeSavedOffset[idx] == 0) continue;
    emitFLoadWide(e, riscvFpCalleeSaved[idx], X_S0, e->fpCalleeSavedOffset[idx], sizeof(double));
  }

  emitLoadWide(e, X_RA, X_SP, savedRaOffset(e), sizeof(intptr_t), FALSE);
  emitLoadWide(e, X_S0, X_SP, savedS0Offset(e), sizeof(intptr_t), FALSE);
  emitAddConst(e, X_SP, X_SP, e->frameSize);

  riscvRet(e->gen);
}

// -============================ Jumps =====================================-

static void recordJump(EmitContext *e, const MachineBasicBlock *target, Boolean isBranch) {
  assert(e->nextJump < e->numJumps && "more jumps emitted than the pre-pass counted");

  JumpSite *site = &e->jumps[e->nextJump++];
  site->offset = here(e);
  site->block = target->id;
  site->isBranch = isBranch;
}

// Rewrite a J-type instruction's displacement in place, leaving every other
// field of the word alone. The immediate is scattered across four ranges,
// which is why this is a re-encode and not a store.
static void patchJType(uint8_t *at, int32_t offset) {
  assert(isAligned(offset, 2));
  assert(isInt(offset, 21));

  uint32_t word;
  memcpy(&word, at, sizeof(word));
  word &= 0x00000fffu; // keep rd and the opcode

  uint32_t imm20 = ((uint32_t)offset >> 1) & 0xfffffu;
  word |= (imm20 & 0x80000u) << (31 - 19) | (imm20 & 0x03ffu) << 21 |
          (imm20 & 0x400u) << (20 - 10) | (imm20 & 0x7f800u) << (12 - 11);

  memcpy(at, &word, sizeof(word));
}

static void patchBType(uint8_t *at, int32_t offset) {
  assert(isAligned(offset, 2));
  assert(isInt(offset, 13));

  uint32_t word;
  memcpy(&word, at, sizeof(word));
  word &= 0x01fff07fu; // keep rs2, rs1, funct3 and the opcode

  uint32_t imm12 = ((uint32_t)offset >> 1) & 0xfffu;
  word |= (imm12 & 0x800u) << (31 - 11) | (imm12 & 0x03f0u) << (25 - 4) |
          (imm12 & 0xfu) << 8 | (imm12 & 0x400u) >> (10 - 7);

  memcpy(at, &word, sizeof(word));
}

static void patchJumps(EmitContext *e) {
  Section *section = e->gen->section;

  for (size_t idx = 0; idx < e->nextJump; ++idx) {
    const JumpSite *site = &e->jumps[idx];
    ptrdiff_t target = e->blockOffset[site->block];
    assert(target >= 0 && "a jump names a block that was never emitted");

    int32_t displacement = (int32_t)(target - site->offset);
    uint8_t *at = section->start + site->offset;

    if (site->isBranch) {
      patchBType(at, displacement);
    } else {
      patchJType(at, displacement);
    }
  }
}

static size_t countJumpSites(const MachineFunction *mf) {
  size_t count = 0;

  for (const MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (const MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
        if (machineOperandAt((MachineInstr *)mi, idx)->kind == MO_MBB) {
          ++count;
          break;
        }
      }
    }
  }

  return count;
}

// -============================ The instructions ==========================-

static void emitCopy(EmitContext *e, const MachineInstr *mi) {
  uint32_t dstId = regIdOperand(mi, 0);
  uint32_t srcId = regIdOperand(mi, 1);

  assert(isFpReg(e, dstId) == isFpReg(e, srcId) && "a copy across register banks");

  if (dstId == srcId) {
    return; // the allocator gave both ends the same register
  }

  if (isFpReg(e, srcId)) {
    unreachable("float copies arrive with the float rules");
  }

  // 'mv rd, rs' is 'addi rd, rs, 0'.
  riscvAddi(e->gen, xReg(e, dstId), xReg(e, srcId), 0);
}

static void emitSpill(EmitContext *e, const MachineInstr *mi) {
  int32_t off = frameOffset(e, frameIdxOperand(mi, 0));
  uint32_t srcId = regIdOperand(mi, 1);

  if (isFpReg(e, srcId)) {
    emitFStoreWide(e, fReg(e, srcId), X_S0, off, mi->opSize);
  } else {
    emitStoreWide(e, xReg(e, srcId), X_S0, off, mi->opSize);
  }
}

static void emitReload(EmitContext *e, const MachineInstr *mi) {
  uint32_t dstId = regIdOperand(mi, 0);
  int32_t off = frameOffset(e, frameIdxOperand(mi, 1));

  if (isFpReg(e, dstId)) {
    emitFLoadWide(e, fReg(e, dstId), X_S0, off, mi->opSize);
  } else {
    // A reload restores the whole register, so it extends the way the value
    // was stored - unsigned, since a spill wrote exactly the value's width and
    // the bytes above it were never part of the value.
    emitLoadWide(e, xReg(e, dstId), X_S0, off, mi->opSize, mi->opSize < 8);
  }
}

static void emitInstruction(EmitContext *e, const MachineInstr *mi) {
  switch (mi->opcode) {
  case MOP_COPY:   emitCopy(e, mi); return;
  case MOP_SPILL:  emitSpill(e, mi); return;
  case MOP_RELOAD: emitReload(e, mi); return;

  case MOP_PHI:
    unreachable("a phi node reached emission");

  case RISCV_LI:
    riscvLoadImmediate(e->gen, immOperand(mi, 1), xReg(e, regIdOperand(mi, 0)));
    return;

  case RISCV_J:
    recordJump(e, blockOperand(mi, 0), FALSE);
    riscvJal(e->gen, X_ZERO, 0);
    return;

  case RISCV_RET:
    emitEpilogue(e);
    return;

  default:
    unreachable(targetOpcodeName(e->mf->target, mi->opcode));
  }
}

static void emitFunctionBody(EmitContext *e) {
  MachineFunction *mf = e->mf;
  ptrdiff_t start = here(e);

  emitPrologue(e);

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    assert(mbb->id < mf->numBlocks);
    e->blockOffset[mbb->id] = here(e);

    for (const MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      emitInstruction(e, mi);
    }
  }

  // A C function that runs off its end without returning has undefined
  // behaviour only if the caller reads the result; falling out of the emitted
  // bytes into whatever follows in .text is a different and much worse thing,
  // so the epilogue is repeated when the last block does not end in one.
  const MachineBasicBlock *last = mf->blocks.tail;
  const MachineInstr *lastInstr = last != NULL ? last->instructions.tail : NULL;
  if (lastInstr == NULL || lastInstr->opcode != RISCV_RET) {
    emitEpilogue(e);
  }

  (void)start;
}

GeneratedFunction *emitMachineFunction_riscv64(GenerationContext *ctx, MachineFunction *mf) {
  assert(mf != NULL);

  GeneratedFunction *gen = allocateGenFunction(ctx);
  gen->symbol = mf->ast->declaration->symbol;
  gen->name = mf->ast->declaration->name;

  EmitContext e = { 0 };
  e.gen = gen;
  e.mf = mf;
  e.blockOffset = areanAllocate(ctx->codegenArena, mf->numBlocks * sizeof(ptrdiff_t));
  memset(e.blockOffset, -1, mf->numBlocks * sizeof(ptrdiff_t));

  e.numJumps = countJumpSites(mf);
  if (e.numJumps != 0) {
    e.jumps = areanAllocate(ctx->codegenArena, e.numJumps * sizeof(JumpSite));
  }

  e.frameSize = layoutCalleeSaved(&e);
  gen->frameSize = e.frameSize;

  // One pass, unlike x86-64's emit-measure-emit loop: every instruction is
  // four bytes whatever its operands, so patching a displacement afterwards
  // moves nothing.
  emitFunctionBody(&e);
  patchJumps(&e);

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  return gen;
}

void initArchCodegen_riscv64(ArchCodegen *cg) {
  // No legacy entry point: the AST walker that used to be here went with step
  // 59, and '-legacy' on this target is refused where the options are read.
  // generateVaribale is the shared one - static data has nothing
  // architecture-specific in it.
  cg->generateVaribale = &generateVariable;
  cg->generateFunctionFromIr = &emitMachineFunction_riscv64;
}
