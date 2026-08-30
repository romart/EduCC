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

  // One per jump table, bound when the table is placed after the body. The lea
  // that reaches a table is emitted long before that, so this is a forward
  // reference like any other and uses the same patching.
  struct Label *jumpTableLabels;

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

// The flat namespace physReg() maps down from is the only thing that tells the
// two banks apart: xmm8 and r8 are different ids but the same encoded register
// number, so an operand arriving in the wrong bank does not fail to encode -
// it encodes as a different real register, and the bytes are wrong while every
// dump still reads correctly. Naming the class the opcode encodes at each use
// is what makes that an assertion instead.
static uint32_t regIdOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_REG);
  return op->info.reg;
}

static enum Registers regOperandIn(const EmitContext *e, const MachineInstr *mi, uint16_t idx,
                                   enum RegClass rc) {
  uint32_t reg = regIdOperand(mi, idx);
  enum Registers r = physReg(e, reg);

  assert(e->mf->target->regClass[reg] == rc && "operand is in the wrong register bank");

  return r;
}

static enum Registers regOperand(const EmitContext *e, const MachineInstr *mi, uint16_t idx) {
  return regOperandIn(e, mi, idx, RC_GP);
}

// For the opcodes that encode an SSE register where regOperand encodes a
// general-purpose one.
static enum Registers fpOperand(const EmitContext *e, const MachineInstr *mi, uint16_t idx) {
  return regOperandIn(e, mi, idx, RC_FP);
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

static struct _Symbol *symbolOperand(const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);
  assert(op->kind == MO_SYMBOL);
  return op->info.symbol;
}

// A relocation against this function's own section, linked into the list the
// object writer walks. What it resolves to is the caller's business.
static Relocation *newSectionRelocation(GeneratedFunction *f) {
  Relocation *reloc = allocateRelocation(f->context);

  reloc->applySection = f->section;
  reloc->next = f->section->reloc;
  f->section->reloc = reloc;

  return reloc;
}

// Addressed relative to the instruction pointer, with the linker filling the
// displacement in - byte for byte what the legacy backend's translateAddress()
// builds for a name that is not a local.
static Address symbolAddress(EmitContext *e, const MachineAddress *m) {
  Relocation *reloc = newSectionRelocation(e->gen);

  reloc->kind = RK_SYMBOL;
  reloc->symbolData.symbol = m->anchor.symbol;
  reloc->symbolData.symbolName = m->anchor.symbol->name;

  Address addr = { R_RIP, R_BAD, 0, 0, reloc, NULL };
  return addr;
}

// The same rip-relative form, resolved here rather than by the linker: these
// bytes are ours, so we place them and point at where they landed. RK_RIP
// against a section and an offset is what the legacy backend builds for a
// string literal, and going through emitLiteralBytes means a literal both
// backends use is stored once.
static Address constantAddress(EmitContext *e, const MachineAddress *m) {
  GenerationContext *ctx = e->gen->context;
  const MachineConstant *c = machineConstantAt(e->mf, m->anchor.constantIdx);

  assert(c->kind == MCK_BYTES);

  Relocation *reloc = newSectionRelocation(e->gen);

  reloc->kind = RK_RIP;
  reloc->sectionData.dataSection = ctx->rodata;
  reloc->sectionData.dataSectionOffset = emitLiteralBytes(ctx, ctx->rodata, c->bytes, c->size);

  Address addr = { R_RIP, R_BAD, 0, 0, reloc, NULL };
  return addr;
}

// A block of this same function, so nothing outside it has to be told: the
// assembler measures the distance itself, or leaves a hole this block's label
// patches when it is bound. Byte for byte what the legacy backend builds for
// '&&label' in codegen_x86_64.c.
static Address blockAddress(EmitContext *e, const MachineAddress *m) {
  Address addr = { R_RIP, R_BAD, 0, 0, NULL, &e->labels[m->anchor.block->id] };
  return addr;
}

static Address jumpTableAddress(EmitContext *e, const MachineAddress *m) {
  Address addr = { R_RIP, R_BAD, 0, 0, NULL, &e->jumpTableLabels[m->anchor.jumpTableIdx] };
  return addr;
}

static Address registerAddress(EmitContext *e, const MachineAddress *m) {
  Address addr = { R_BAD, R_BAD, 0, m->disp, NULL, NULL };

  addr.base = m->base != NO_REG ? physReg(e, m->base) : R_BAD;

  if (m->index != NO_REG) {
    addr.index = physReg(e, m->index);
    // SIB holds the scale as a shift amount, not as the multiplier, and
    // MachineAddress carries the multiplier because that is what a target
    // without a scaled-index mode would have to reject.
    switch (m->scale) {
    case 1: addr.scale = 0; break;
    case 2: addr.scale = 1; break;
    case 4: addr.scale = 2; break;
    case 8: addr.scale = 3; break;
    default: unreachable("x86 scales an index by 1, 2, 4 or 8");
    }
  }

  return addr;
}

// A slot, plus whatever the addressing mode adds to it. The slot's offset is
// only a displacement, so it folds into the one the address already carries
// and leaves the index free - which is the whole point of the anchor.
static Address frameAnchorAddress(EmitContext *e, const MachineAddress *m) {
  const MachineFrameObject *obj = machineFrameObjectAt(e->mf, m->anchor.frameIdx);
  assert(obj != NULL);
  assert(!obj->isDynamic && "a dynamically placed object has no fixed address");

  Address addr = registerAddress(e, m);
  addr.base = R_EBP;
  addr.imm += obj->offset;

  return addr;
}

// The address an operand denotes, in the form the assembler takes: a whole
// frame slot, or one of the anchors MachineAddress distinguishes.
static Address addressOperand(EmitContext *e, const MachineInstr *mi, uint16_t idx) {
  const MachineOperand *op = machineOperandAt((MachineInstr *)mi, idx);

  // Only a spill and a reload still say it this way; everything selection
  // builds carries the slot as an anchor, so that an index can sit beside it.
  if (op->kind == MO_FRAME_IDX) {
    return frameAddress(e, op->info.frameIdx);
  }

  assert(op->kind == MO_MEM);
  const MachineAddress *m = &op->info.mem;

  // encodeAR reads no displacement or register in the rip-relative forms - the
  // relocation is the whole address - so one carrying either would silently
  // lose it.
  assert(isMachineAddressWellFormed(m));

  switch (m->kind) {
  case MAK_SYMBOL:   return symbolAddress(e, m);
  case MAK_CONSTANT: return constantAddress(e, m);
  case MAK_FRAME:    return frameAnchorAddress(e, m);
  case MAK_BLOCK:    return blockAddress(e, m);
  case MAK_JUMPTABLE: return jumpTableAddress(e, m);
  case MAK_REG:      return registerAddress(e, m);
  default: unreachable("unknown address anchor");
  }
}

// Which x87 memory form a width names. Ten and not sixteen: the object
// occupies sixteen bytes of stack for alignment, and fld/fstp move the ten
// that mean anything.
static int fpMemoryTypeId(uint8_t size) {
  switch (size) {
  case 4:  return T_F4;
  case 8:  return T_F8;
  case 10: return T_F10;
  default: unreachable("no x87 memory form of this width");
  }
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

// A copy is the one shape with no bank of its own: it moves whatever it was
// given, and the two ends have to agree - crossing banks is movd, not a copy.
static void emitCopy(EmitContext *e, const MachineInstr *mi) {
  uint32_t dstId = regIdOperand(mi, 0);
  uint32_t srcId = regIdOperand(mi, 1);
  enum Registers dst = physReg(e, dstId);
  enum Registers src = physReg(e, srcId);

  assert(isFpReg(e, dstId) == isFpReg(e, srcId) && "a copy across register banks");

  if (dst == src) {
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
  uint32_t srcId = regIdOperand(mi, 1);
  enum Registers src = physReg(e, srcId);

  if (isFpReg(e, srcId)) {
    emitMovfpRA(e->gen, src, &addr, mi->opSize);
  } else {
    emitMoveRA(e->gen, src, &addr, mi->opSize);
  }
}

static void emitReload(EmitContext *e, const MachineInstr *mi) {
  uint32_t dstId = regIdOperand(mi, 0);
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
static void emitTwoAddressIn(EmitContext *e, const MachineInstr *mi, enum Opcodes op,
                             enum RegClass rc) {
  enum Registers dst = regOperandIn(e, mi, 0, rc);
  assert(regOperandIn(e, mi, 1, rc) == dst && "two-address form did not survive allocation");

  if (isImmOperand(mi, 2)) {
    assert(rc == RC_GP && "no SSE arithmetic takes an immediate");
    emitArithConst(e->gen, op, dst, immOperand(mi, 2), typeIdForSize(mi->opSize));
  } else {
    emitArithRR(e->gen, op, dst, regOperandIn(e, mi, 2, rc), mi->opSize);
  }
}

static void emitTwoAddress(EmitContext *e, const MachineInstr *mi, enum Opcodes op) {
  emitTwoAddressIn(e, mi, op, RC_GP);
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

// A direct call names its target by symbol, and the displacement is only known
// once the linker has placed it - so the four bytes the instruction reserves
// are filled in by a relocation rather than by the assembler. Identical to
// what the legacy backend does in codegen_x86_64.c: the two produce the same
// bytes and the same relocation, which is what lets a file hold functions from
// both backends and link.
static void emitCallInstr(EmitContext *e, const MachineInstr *mi) {
  // Operand 0 is the callee for a call with no result, operand 1 for one with,
  // whose only def is the implicit return register.
  uint16_t idx = mi->numDefs;
  const MachineOperand *callee = machineOperandAt((MachineInstr *)mi, idx);

  if (callee->kind == MO_REG) {
    emitCall(e->gen, regOperand(e, mi, idx));
    return;
  }

  GeneratedFunction *f = e->gen;
  Symbol *s = symbolOperand(mi, idx);
  Relocation *reloc = allocateRelocation(f->context);

  reloc->applySection = f->section;
  reloc->symbolData.symbolName = s->name;
  reloc->symbolData.symbol = s;
  reloc->kind = RK_SYMBOL;
  reloc->next = f->section->reloc;
  f->section->reloc = reloc;

  emitCallLiteral(f, reloc);
}

// A widening move: opSize is the destination's width and srcSize the source's,
// which together pick the opcode. x86 spells the 32-to-64 case with its own
// instruction (movsxd) rather than with the 0F-prefixed family the narrower
// sources use, and has no zero-extending form of it at all - selection knows
// that and emits a plain 32-bit move instead, so it never arrives here.
static void emitWiden(EmitContext *e, const MachineInstr *mi) {
  enum Registers dst = regOperand(e, mi, 0);
  enum Registers src = regOperand(e, mi, 1);
  Boolean isSigned = mi->opcode == X86_MOVSX;
  uint8_t srcSize = machineInstrSrcSize(mi);

  assert(srcSize < mi->opSize && "a widening move that widens nothing");

  if (srcSize == 4) {
    assert(isSigned && "a 32-bit move already zero-extends; selection emits that instead");
    emitMovsxdRR(e->gen, src, dst, mi->opSize);
    return;
  }

  assert((srcSize == 1 || srcSize == 2) && "no widening move has this source width");
  uint8_t opcode = srcSize == 1 ? (isSigned ? 0xBE : 0xB6) : (isSigned ? 0xBF : 0xB7);

  if (mi->opSize == 8) {
    emitMovxxRR64(e->gen, opcode, src, dst);
  } else {
    emitMovxxRR(e->gen, opcode, src, dst);
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
  case X86_SETP:  case X86_JP:  return JC_PARITY;
  case X86_SETNP: case X86_JNP: return JC_NOT_PARITY;
  default: unreachable("not a conditional opcode");
  }
}

static Boolean isSetcc(uint32_t opcode) {
  return opcode >= X86_SETE && opcode <= X86_SETNP;
}

static Boolean isJcc(uint32_t opcode) {
  return opcode >= X86_JE && opcode <= X86_JNP;
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

  case X86_LEA: {
    Address addr = addressOperand(e, mi, 1);
    emitLea(f, &addr, regOperand(e, mi, 0));
    break;
  }

  case X86_LOAD: {
    Address addr = addressOperand(e, mi, 1);
    uint32_t dstId = regIdOperand(mi, 0);

    if (isFpReg(e, dstId)) {
      emitMovfpAR(f, &addr, physReg(e, dstId), mi->opSize);
    } else {
      emitMoveAR(f, &addr, physReg(e, dstId), mi->opSize);
    }
    break;
  }

  case X86_STORE: {
    Address addr = addressOperand(e, mi, 0);
    uint32_t srcId = regIdOperand(mi, 1);

    if (isFpReg(e, srcId)) {
      emitMovfpRA(f, physReg(e, srcId), &addr, mi->opSize);
    } else {
      emitMoveRA(f, physReg(e, srcId), &addr, mi->opSize);
    }
    break;
  }

  case X86_MOVSX:
  case X86_MOVZX:
    emitWiden(e, mi);
    break;

  case X86_FADD: emitTwoAddressIn(e, mi, OP_FADD, RC_FP); break;
  case X86_FSUB: emitTwoAddressIn(e, mi, OP_FSUB, RC_FP); break;
  case X86_FMUL: emitTwoAddressIn(e, mi, OP_FMUL, RC_FP); break;
  case X86_FDIV: emitTwoAddressIn(e, mi, OP_FDIV, RC_FP); break;

  case X86_FCMP:
  case X86_FUCMP:
    // Like the integer compare, no destination: it writes only flags, so its
    // operands start at index 0.
    emitArithRR(f, mi->opcode == X86_FCMP ? OP_FOCMP : OP_FUCMP, fpOperand(e, mi, 0),
                fpOperand(e, mi, 1), mi->opSize);
    break;

  case X86_MOVD:
    // 66 0F 6E: 'movd xmm, r32' and, with REX.W, 'movq xmm, r64'. The bits go
    // across unchanged - this is not a conversion.
    emitMovdq(f, 0x66, 0x0F, 0x6E, regOperand(e, mi, 1), fpOperand(e, mi, 0),
              mi->opSize == 8);
    break;

  case X86_MOVDR:
    // 66 0F 7E, the other direction. The operands go to emitMovdq the other way
    // round too: it takes the r/m register first, and here that is the general
    // one being written rather than the xmm one being read.
    emitMovdq(f, 0x66, 0x0F, 0x7E, regOperand(e, mi, 0), fpOperand(e, mi, 1),
              mi->opSize == 8);
    break;

  // x87. Every one of these is one emit* call and an address, because
  // selection already spelled the sequence out - see selectX87 in
  // isel_x86_64.c. The arithmetic and compare forms name no operand at all:
  // they read st(0) and st(1), which the two loads in front of them put there.
  case X86_FLD: {
    Address addr = addressOperand(e, mi, 0);
    emitFPLoad(f, &addr, fpMemoryTypeId(mi->opSize));
    break;
  }

  case X86_FSTP: {
    Address addr = addressOperand(e, mi, 0);
    emitFPStore(f, &addr, fpMemoryTypeId(mi->opSize));
    break;
  }

  case X86_FILD: {
    Address addr = addressOperand(e, mi, 0);
    emitFPIntLoad(f, &addr, mi->opSize);
    break;
  }

  case X86_FISTP: {
    Address addr = addressOperand(e, mi, 0);
    emitFPIntStore(f, &addr, mi->opSize);
    break;
  }

  // st(1) op= st(0), pop - which leaves the answer where the next fstp will
  // find it. The register number is 1 and never anything else: nothing here
  // reaches deeper into the stack than the two operands it just pushed.
  case X86_FADDP: emitFPArith(f, OP_FADD, 1, TRUE); break;
  case X86_FSUBP: emitFPArith(f, OP_FSUB, 1, TRUE); break;
  case X86_FMULP: emitFPArith(f, OP_FMUL, 1, TRUE); break;
  case X86_FDIVP: emitFPArith(f, OP_FDIV, 1, TRUE); break;

  case X86_FCOMIP:  emitFPArith(f, OP_FOCMP, 1, TRUE); break;
  case X86_FUCOMIP: emitFPArith(f, OP_FUCMP, 1, TRUE); break;

  case X86_FLDZ: emitFPnoArg(f, 0xEE); break;
  case X86_FPOP: emitFPPop(f, 0); break;

  case X86_FNSTCW: {
    Address addr = addressOperand(e, mi, 0);
    emitFPnoArgMem(f, &addr, 7);
    break;
  }

  case X86_FLDCW: {
    Address addr = addressOperand(e, mi, 0);
    emitFPnoArgMem(f, &addr, 5);
    break;
  }

  case X86_CVTF2F:
    // cvtss2sd or cvtsd2ss, chosen by which way the widths go. The prefix
    // names the *source* here, unlike the arithmetic above.
    emitConvertFP(f, machineInstrSrcSize(mi) == 8 ? 0xF2 : 0xF3, 0x5A,
                  fpOperand(e, mi, 1), fpOperand(e, mi, 0), FALSE);
    break;

  case X86_CVTSI2F:
    // cvtsi2ss/cvtsi2sd: the prefix is the destination's float width, REX.W
    // the source's integer width.
    emitConvertFP(f, mi->opSize == 8 ? 0xF2 : 0xF3, 0x2A, regOperand(e, mi, 1),
                  fpOperand(e, mi, 0), machineInstrSrcSize(mi) == 8);
    break;

  case X86_CVTF2SI:
    // cvttss2si/cvttsd2si - the truncating form, which is the one a C cast
    // means; the rounding form would follow the current rounding mode.
    emitConvertFP(f, machineInstrSrcSize(mi) == 8 ? 0xF2 : 0xF3, 0x2C,
                  fpOperand(e, mi, 1), regOperand(e, mi, 0), mi->opSize == 8);
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

  case X86_IJMP:
    emitJumpByReg(f, regOperand(e, mi, 0));
    break;

  case X86_PUSH:
    // Always the full eight bytes, whatever the argument's own width: a stack
    // argument occupies a whole eightbyte and the bytes above it are the
    // callee's business to ignore.
    emitPushReg(f, regOperand(e, mi, 0));
    break;

  case X86_CALL:
    emitCallInstr(e, mi);
    break;

  case X86_RET:
    emitEpilogue(e);
    break;

  default:
    unreachable("stage 3 has no encoding for this opcode");
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

  // SysV wants rsp 16-aligned at a call boundary, and this is half of what
  // keeps it so: rbp is 16-aligned after the prologue's push, so a frame size
  // rounded to 16 leaves rsp aligned everywhere between calls. The other half
  // is selectCall padding an odd number of stack arguments.
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

// -============================ Jump tables ===============================-
//
// Placed after the body, in this function's own section, which is what makes
// them cost nothing to resolve: every block they name has been emitted by
// then, so an entry is the difference between two offsets this file already
// knows and not a relocation for somebody else to fill in.
//
// That an entry is a *distance* rather than an address is the same decision
// from the other side. An address is only known once the program is loaded, so
// a table of addresses would need a relocation per entry and a writable-ish
// section to hold them; a distance within one section is fixed at compile
// time, and the dispatch pays one 'add' for it.
//
// Nothing falls in here - the last thing before a table is a return or an
// unconditional jump - but the padding is 0xCC rather than zero so that a
// mistake about that traps instead of executing whatever the low bytes of an
// address happen to encode.
static void emitJumpTables(EmitContext *e) {
  GeneratedFunction *f = e->gen;

  for (size_t idx = 0; idx < e->mf->jumpTables.size; ++idx) {
    const MachineJumpTable *jt = machineJumpTableAt(e->mf, idx);

    while ((f->section->pc - f->section->start) % sizeof(intptr_t) != 0) {
      emitByte(f, 0xCC);
    }

    bindBlockLabel(f, &e->jumpTableLabels[idx]);
    ptrdiff_t tableOffset = e->jumpTableLabels[idx].label_cp;

    for (uint32_t entry = 0; entry < jt->count; ++entry) {
      const struct Label *target = &e->labels[jt->entries[entry]->id];
      assert(target->binded && "a jump table names a block that was never emitted");

      int64_t delta = (int64_t)(target->label_cp - tableOffset);
      for (size_t byte = 0; byte < sizeof(int64_t); ++byte) {
        emitByte(f, (uint8_t)(delta >> (8 * byte)));
      }
    }
  }
}

// -============================ Entry points ==============================-

GeneratedFunction *emitMachineFunction_x86_64(GenerationContext *ctx, MachineFunction *mf) {
  assert(mf != NULL);

  GeneratedFunction *gen = allocateGenFunction(ctx);
  gen->symbol = mf->ast->declaration->symbol;
  gen->name = mf->ast->declaration->name;

  EmitContext e = { 0 };
  e.gen = gen;
  e.mf = mf;
  e.labels = areanAllocate(ctx->codegenArena, mf->numBlocks * sizeof(struct Label));
  if (mf->jumpTables.size != 0) {
    e.jumpTableLabels = areanAllocate(ctx->codegenArena, mf->jumpTables.size * sizeof(struct Label));
  }
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

  // Where the instructions stop, which is not where the function's bytes do:
  // the tables below belong to it and are counted in its size, but they are
  // data and disassembling them would print nonsense.
  address codeEnd = gen->section->pc;

  emitJumpTables(&e);

  gen->bodySize = (gen->section->pc - gen->section->start) - gen->sectionOffset;

  if (ctx->parserContext->config->asmDump) {
    fprintf(stdout, "<<< %s >>>\n", gen->name);
    address b = gen->section->start + gen->sectionOffset;
    disassemble(stdout, b, codeEnd - b);
    fprintf(stdout, "<<<>>>\n");
  }

  return gen;
}
