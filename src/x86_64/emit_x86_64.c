#include <assert.h>

#include "codegen.h"
#include "ir/emit.h"
#include "ir/ir.h"
#include "machine_x86_64.h"
#include "instructions_x86_64.h"

// -============================ x86-64 emission (stage 3) =================-
//
// One MachineInstr at a time into the assembler in instructions_x86_64.c. See
// docs/ir-codegen-design.md section 8.
//
// The switch is long but shallow, which is the point: selection already
// decided what instruction this is and allocation already decided which
// registers it names, so each arm is a register translation and one emit*
// call. Anything that needed a decision was taken by an earlier stage, and if
// an arm here starts wanting to know something about the program it is a sign
// the decision is in the wrong place.
//
// Registers arrive in the flat physical namespace (see target_x86_64.c). Its
// GP half is deliberately already the encoding enum Registers uses, so a GP
// register translates by casting; only the xmm half needs arithmetic.

// Callee-saved in SysV AMD64, minus rbp and rsp, which the frame handles.
// Today only rbx can appear - it is the third of the allocator's scratch
// registers - but the prologue costs nothing for the ones that cannot, and
// they will appear as soon as an allocator holds values in registers.
static const enum Registers x86CalleeSaved[] = {
  R_EBX, R_R12, R_R13, R_R14, R_R15
};

#define CALLEE_SAVED_COUNT (sizeof(x86CalleeSaved) / sizeof(x86CalleeSaved[0]))

typedef struct _EmitContext {
  GeneratedFunction *gen;
  MachineFunction *mf;

  // One label per machine block, indexed by MachineBasicBlock.id. A jump to a
  // block that has not been emitted yet leaves a hole that bindLabel patches
  // when the block arrives, which is the existing forward-reference machinery
  // and the reason blocks can be emitted in layout order without a pre-pass.
  struct Label *labels;

  // Where in the frame each saved callee-saved register lives, indexed the
  // same way as x86CalleeSaved; 0 for the ones this function does not use.
  int32_t calleeSavedOffset[CALLEE_SAVED_COUNT];

  int32_t frameSize;
} EmitContext;

// -============================ Small translations ========================-

static enum Registers physReg(const EmitContext *e, uint32_t reg) {
  assert(isPhysicalRegister(reg) && "a virtual register reached emission");

  if (e->mf->target->regClass[reg] == RC_FP) {
    return (enum Registers)(reg - X86_FP_BASE);
  }

  return (enum Registers)reg;
}

static Boolean isFpReg(const EmitContext *e, uint32_t reg) {
  return e->mf->target->regClass[reg] == RC_FP;
}

// The assembler takes a TypeId where it wants a width, and uses it for two
// things: the width itself and, for a 64-bit immediate, whether the encoding
// has to be the full movabs. Signedness beyond that does not reach the
// encoding, so the signed ids stand for every width here.
static TypeId typeIdForSize(uint8_t size) {
  switch (size) {
  case 1: return T_S1;
  case 2: return T_S2;
  case 4: return T_S4;
  case 8: return T_S8;
  default: unreachable("no x86 operand is this wide");
  }
}

// Same, for a move of an immediate. A 64-bit constant that does not fit in the
// sign-extended imm32 form has to be spelled as movabs, and T_U8 is how
// emitMoveCR is asked for it - passing T_S8 there would sign-extend a value
// that was never negative.
static TypeId typeIdForImmediate(uint8_t size, int64_t value) {
  if (size == 8 && value != (int64_t)(int32_t)value) {
    return T_U8;
  }

  return typeIdForSize(size);
}

static Address frameAddress(const EmitContext *e, int32_t frameIdx) {
  const MachineFrameObject *obj = machineFrameObjectAt(e->mf, frameIdx);
  assert(obj != NULL);
  assert(!obj->isDynamic && "a dynamically placed object has no fixed address");

  Address addr = { R_EBP, R_BAD, 0, obj->offset, NULL, NULL };
  return addr;
}

static void bindBlockLabel(GeneratedFunction *f, struct Label *l) {
  l->label_cp = f->section->pc - f->section->start;
  l->binded = 1;

  for (struct LabelJump *jump = l->jumps; jump != NULL; jump = jump->next) {
    patchJumpTo(f, jump->instruction_cp, jump->instSize, l->label_cp);
  }
  l->jumps = NULL;

  for (struct LabelRef *ref = l->refs; ref != NULL; ref = ref->next) {
    patchRefTo(f, ref->offset_cp, l->label_cp);
  }
  l->refs = NULL;
}

// -============================ Operand accessors =========================-
//
// These assert the operand kind rather than tolerating a surprise, because a
// wrong kind here means an earlier stage built the instruction differently
// from how this file reads it, and the useful moment to find that out is the
// first time it happens rather than several instructions later.

static enum Registers regOperand(const EmitContext *e, const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_REG);
  return physReg(e, op->info.reg);
}

static Boolean isImmOperand(const MachineInstr *mi, uint16_t idx) {
  return machineOperandAt((MachineInstr *)mi, idx)->kind == MO_IMM;
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

// A forward jump has to reserve its displacement before the target's address is
// known, and the assembler's 'isNear' picks which: TRUE reserves one byte, FALSE
// four. Always four here. Spilling every value makes blocks long enough that a
// one-byte displacement is a coin flip, and getting it wrong is not a worse
// encoding but a failed assertion in patchJumpTo once the label is bound.
// Choosing the short form where it fits wants a relaxation pass, which is worth
// having and is not this stage's job. A backward jump is not affected - its
// label is already bound, so the assembler measures the distance and picks.
#define JUMP_DISPLACEMENT_IS_SHORT FALSE

// -============================ Instruction arms ==========================-

static void emitCopy(EmitContext *e, const MachineInstr *mi) {
  enum Registers dst = regOperand(e, mi, 0);
  enum Registers src = regOperand(e, mi, 1);
  uint32_t srcId = machineOperandAt((MachineInstr *)mi, 1)->info.reg;

  if (dst == src && isFpReg(e, srcId) == isFpReg(e, machineOperandAt((MachineInstr *)mi, 0)->info.reg)) {
    return; // the allocator gave both ends the same register
  }

  if (isFpReg(e, srcId)) {
    emitMovfpRR(e->gen, src, dst, mi->opSize);
  } else {
    emitMoveRR(e->gen, src, dst, mi->opSize);
  }
}

static void emitSpill(EmitContext *e, const MachineInstr *mi) {
  Address addr = frameAddress(e, frameIdxOperand(mi, 0));
  uint32_t srcId = machineOperandAt((MachineInstr *)mi, 1)->info.reg;
  enum Registers src = physReg(e, srcId);

  if (isFpReg(e, srcId)) {
    emitMovfpRA(e->gen, src, &addr, mi->opSize);
  } else {
    emitMoveRA(e->gen, src, &addr, mi->opSize);
  }
}

static void emitReload(EmitContext *e, const MachineInstr *mi) {
  uint32_t dstId = machineOperandAt((MachineInstr *)mi, 0)->info.reg;
  enum Registers dst = physReg(e, dstId);
  Address addr = frameAddress(e, frameIdxOperand(mi, 1));

  if (isFpReg(e, dstId)) {
    emitMovfpAR(e->gen, &addr, dst, mi->opSize);
  } else {
    emitMoveAR(e->gen, &addr, dst, mi->opSize);
  }
}

// Every two-address ALU opcode: operand 0 is the destination, operand 1 is the
// same register read back, operand 2 is the right-hand side. Selection built
// them that way (see machine_x86_64.h) so there is nothing to reassociate
// here - only the choice between the register and the immediate encoding.
static void emitTwoAddress(EmitContext *e, const MachineInstr *mi, enum Opcodes op) {
  enum Registers dst = regOperand(e, mi, 0);
  assert(regOperand(e, mi, 1) == dst && "two-address form did not survive allocation");

  if (isImmOperand(mi, 2)) {
    emitArithConst(e->gen, op, dst, immOperand(mi, 2), typeIdForSize(mi->opSize));
  } else {
    emitArithRR(e->gen, op, dst, regOperand(e, mi, 2), mi->opSize);
  }
}

// A shift is two-address as well, but its variable count lives in cl and
// nowhere else, which selection already arranged.
static void emitShift(EmitContext *e, const MachineInstr *mi, enum Opcodes op) {
  enum Registers dst = regOperand(e, mi, 0);
  assert(regOperand(e, mi, 1) == dst && "two-address form did not survive allocation");

  if (isImmOperand(mi, 2)) {
    emitArithConst(e->gen, op, dst, immOperand(mi, 2), typeIdForSize(mi->opSize));
  } else {
    assert(regOperand(e, mi, 2) == R_ECX && "a variable shift count has to be in cl");
    emitArithRR(e->gen, op, dst, R_ECX, mi->opSize);
  }
}

static enum JumpCondition conditionFor(uint32_t opcode) {
  switch (opcode) {
  case X86_SETE:  case X86_JE:  return JC_EQ;
  case X86_SETNE: case X86_JNE: return JC_NE;
  case X86_SETL:  case X86_JL:  return JC_L;
  case X86_SETLE: case X86_JLE: return JC_LE;
  case X86_SETG:  case X86_JG:  return JC_G;
  case X86_SETGE: case X86_JGE: return JC_GE;
  case X86_SETB:  case X86_JB:  return JC_BELOW;
  case X86_SETBE: case X86_JBE: return JC_B_E;
  case X86_SETA:  case X86_JA:  return JC_A;
  case X86_SETAE: case X86_JAE: return JC_A_E;
  default: unreachable("not a conditional opcode");
  }
}

static Boolean isSetcc(uint32_t opcode) {
  return opcode >= X86_SETE && opcode <= X86_SETAE;
}

static Boolean isJcc(uint32_t opcode) {
  return opcode >= X86_JE && opcode <= X86_JAE;
}

static void emitInstruction(EmitContext *e, const MachineInstr *mi);

// The epilogue: undo the prologue and return. Emitted at every X86_RET rather
// than once at the end, because selection puts a return wherever the IR had
// one and there is no guarantee the last block holds the only one.
static void emitEpilogue(EmitContext *e) {
  for (size_t idx = CALLEE_SAVED_COUNT; idx > 0; --idx) {
    int32_t offset = e->calleeSavedOffset[idx - 1];
    if (offset == 0) {
      continue;
    }

    Address addr = { R_EBP, R_BAD, 0, offset, NULL, NULL };
    emitMoveAR(e->gen, &addr, x86CalleeSaved[idx - 1], sizeof(intptr_t));
  }

  emitLeave(e->gen);
  emitRet(e->gen, 0);
}

static void emitInstruction(EmitContext *e, const MachineInstr *mi) {
  GeneratedFunction *f = e->gen;

  if (isSetcc(mi->opcode)) {
    // Operand 1, when present, is the destination read back - the dependency
    // on the zeroing move that defined the upper bytes. It constrains
    // allocation and encodes to nothing.
    emitSetccR(f, conditionFor(mi->opcode), regOperand(e, mi, 0));
    return;
  }

  if (isJcc(mi->opcode)) {
    emitCondJump(f, &e->labels[blockOperand(mi, 0)->id], conditionFor(mi->opcode), JUMP_DISPLACEMENT_IS_SHORT);
    return;
  }

  switch (mi->opcode) {
  case MOP_COPY:   emitCopy(e, mi); break;
  case MOP_SPILL:  emitSpill(e, mi); break;
  case MOP_RELOAD: emitReload(e, mi); break;

  case X86_MOV:
    // Register-to-register moves are MOP_COPY; this is only ever an immediate.
    emitMoveCR(f, immOperand(mi, 1), regOperand(e, mi, 0),
               typeIdForImmediate(mi->opSize, immOperand(mi, 1)));
    break;

  case X86_ADD:  emitTwoAddress(e, mi, OP_ADD); break;
  case X86_SUB:  emitTwoAddress(e, mi, OP_SUB); break;
  case X86_IMUL: emitTwoAddress(e, mi, OP_SMUL); break;
  case X86_AND:  emitTwoAddress(e, mi, OP_AND); break;
  case X86_OR:   emitTwoAddress(e, mi, OP_OR); break;
  case X86_XOR:  emitTwoAddress(e, mi, OP_XOR); break;

  case X86_SHL: emitShift(e, mi, OP_SHL); break;
  case X86_SHR: emitShift(e, mi, OP_SHR); break;
  case X86_SAR: emitShift(e, mi, OP_SAR); break;

  case X86_NEG: emitNegR(f, regOperand(e, mi, 0), mi->opSize); break;
  case X86_NOT: emitBitwiseNotR(f, regOperand(e, mi, 0), mi->opSize); break;

  case X86_CMP:
    // No destination: a compare writes only flags, which is why it has no defs
    // and why its two operands start at index 0.
    if (isImmOperand(mi, 1)) {
      emitArithConst(f, OP_CMP, regOperand(e, mi, 0), immOperand(mi, 1),
                     typeIdForSize(mi->opSize));
    } else {
      emitArithRR(f, OP_CMP, regOperand(e, mi, 0), regOperand(e, mi, 1), mi->opSize);
    }
    break;

  case X86_TEST:
    emitTestRR(f, regOperand(e, mi, 0), regOperand(e, mi, 1), mi->opSize);
    break;

  case X86_CDQ:
    // cwd/cdq/cqo, chosen by width. The 16-bit form takes the operand-size
    // prefix; below that there is no sign-extend-into-a-pair instruction at
    // all, so a byte divide would have to widen first - selection never asks
    // for one, and the assert says so rather than emitting the 32-bit form.
    assert(mi->opSize >= 2 && "cdq has no byte form");
    emitConvertWDQ(f, 0x99, mi->opSize);
    break;

  case X86_IDIV:
    // Operand 2 is the divisor; the dividend halves and both results are
    // implicit operands naming rax and rdx, and encode to nothing.
    emitArithRR(f, OP_SDIV, R_BAD, regOperand(e, mi, 2), mi->opSize);
    break;

  case X86_DIV:
    emitArithRR(f, OP_UDIV, R_BAD, regOperand(e, mi, 2), mi->opSize);
    break;

  case X86_JMP:
    emitJumpTo(f, &e->labels[blockOperand(mi, 0)->id], JUMP_DISPLACEMENT_IS_SHORT);
    break;

  case X86_RET:
    emitEpilogue(e);
    break;

  default:
    unreachable("stage 3 has no encoding for this opcode - canEmitMachineFunction should have refused");
  }
}

// -============================ Frame and prologue ========================-

// Places a frame slot for every callee-saved register the code names, on top
// of the frame stages 0 and 2 built, and returns the size of the whole thing.
//
// Slots rather than pushes: 'leave' restores rsp from rbp, so anything pushed
// after the frame pointer is established would be lost by the epilogue unless
// the pushes and pops were balanced by hand around every return. Storing them
// at known offsets is the same number of instructions and does not depend on
// where rsp happens to be.
static int32_t layoutCalleeSaved(EmitContext *e) {
  int32_t offset = (int32_t)e->mf->frame.size;

  for (size_t idx = 0; idx < CALLEE_SAVED_COUNT; ++idx) {
    uint64_t bit = 1ULL << (uint64_t)x86CalleeSaved[idx];
    if ((e->mf->usedPhysRegs & bit) == 0) {
      continue;
    }

    offset += sizeof(intptr_t);
    e->calleeSavedOffset[idx] = -offset;
  }

  // SysV wants rsp 16-aligned at a call boundary. Nothing emittable calls
  // anything yet, but the frame is the wrong place to leave that to step 7.
  return ALIGN_SIZE(offset, 2 * sizeof(intptr_t));
}

static void emitPrologue(EmitContext *e) {
  // push rbp; mov rbp, rsp
  emitPushReg(e->gen, R_EBP);
  emitMoveRR(e->gen, R_ESP, R_EBP, sizeof(intptr_t));

  if (e->frameSize) {
    emitArithConst(e->gen, OP_SUB, R_ESP, e->frameSize, T_S8);
  }

  for (size_t idx = 0; idx < CALLEE_SAVED_COUNT; ++idx) {
    int32_t offset = e->calleeSavedOffset[idx];
    if (offset == 0) {
      continue;
    }

    Address addr = { R_EBP, R_BAD, 0, offset, NULL, NULL };
    emitMoveRA(e->gen, x86CalleeSaved[idx], &addr, sizeof(intptr_t));
  }
}

// -============================ Entry points ==============================-

Boolean canEmitMachineFunction(const MachineFunction *mf) {
  return mf != NULL && !mf->hasUnselected && !mf->hasUnallocated;
}

GeneratedFunction *emitMachineFunction_x86_64(GenerationContext *ctx, MachineFunction *mf) {
  assert(canEmitMachineFunction(mf));

  GeneratedFunction *gen = allocateGenFunction(ctx);
  gen->symbol = mf->ast->declaration->symbol;
  gen->name = mf->ast->declaration->name;

  EmitContext e = { 0 };
  e.gen = gen;
  e.mf = mf;
  e.labels = areanAllocate(ctx->codegenArena, mf->numBlocks * sizeof(struct Label));
  e.frameSize = layoutCalleeSaved(&e);

  gen->frameSize = e.frameSize;

  emitPrologue(&e);

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    assert(mbb->id < mf->numBlocks);
    bindBlockLabel(gen, &e.labels[mbb->id]);

    for (const MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      emitInstruction(&e, mi);
    }
  }

  // A C function that runs off its end without returning has undefined
  // behaviour only if the caller reads the result; falling out of the emitted
  // bytes into whatever follows in .text is a different and much worse thing,
  // so the epilogue is repeated unconditionally when the last block does not
  // end in one.
  const MachineBasicBlock *last = mf->blocks.tail;
  const MachineInstr *lastInstr = last != NULL ? last->instructions.tail : NULL;
  if (lastInstr == NULL || lastInstr->opcode != X86_RET) {
    emitEpilogue(&e);
  }

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  if (ctx->parserContext->config->asmDump) {
    fprintf(stdout, "<<< %s >>>\n", gen->name);
    address b = gen->section->start + gen->sectionOffset;
    address end = gen->section->pc;
    disassemble(stdout, b, end - b);
    fprintf(stdout, "<<<>>>\n");
  }

  return gen;
}
