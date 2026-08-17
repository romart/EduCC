
#include <assert.h>

#include "ir/ir.h"
#include "ir/machine.h"
#include "sema.h"

extern IrContext *ctx;

// ------------- construction ------------------------

MachineFunction *createMachineFunction(IrFunction *f) {
  // The IR arena, not one of our own, on purpose - see the comment on
  // MachineFunction.arena. Nothing consumes machine code yet, so a per
  // function arena would have no release point and would simply leak.
  Arena *arena = ctx->irArena;
  MachineFunction *mf = areanAllocate(arena, sizeof(MachineFunction));

  memset(mf, 0, sizeof(MachineFunction));

  mf->arena = arena;
  mf->ir = f;
  mf->ast = f->ast;
  mf->target = ctx->target;
  mf->id = f->id;

  initVector(&mf->vregs, INITIAL_VECTOR_CAPACITY);
  initVector(&mf->irToVreg, INITIAL_VECTOR_CAPACITY);
  initVector(&mf->irToFrameIdx, INITIAL_VECTOR_CAPACITY);
  initVector(&mf->frame.objects, INITIAL_VECTOR_CAPACITY);
  initVector(&mf->constants, INITIAL_VECTOR_CAPACITY);

  return mf;
}

MachineBasicBlock *createMachineBasicBlock(MachineFunction *mf, const char *name,
                                           const IrBasicBlock *ir) {
  MachineBasicBlock *mbb = areanAllocate(mf->arena, sizeof(MachineBasicBlock));

  memset(mbb, 0, sizeof(MachineBasicBlock));

  mbb->parent = mf;
  mbb->name = name;
  mbb->ir = ir;
  // Numbered in creation order rather than copied from the IR block: blocks
  // the backend invents (split critical edges) have no IR block to take an id
  // from, and reusing IR ids would collide with them.
  mbb->id = (uint32_t)mf->numBlocks;

  initVector(&mbb->preds, 3);
  initVector(&mbb->succs, 3);

  return mbb;
}

void addMachineBasicBlockTail(MachineFunction *mf, MachineBasicBlock *mbb) {
  assert(mbb->parent == mf);
  assert(mbb->next == NULL && mbb->prev == NULL);

  if (mf->blocks.tail != NULL) {
    mf->blocks.tail->next = mbb;
    mbb->prev = mf->blocks.tail;
    mf->blocks.tail = mbb;
  } else {
    assert(mf->blocks.head == NULL);
    mf->blocks.head = mf->blocks.tail = mbb;
  }

  mf->numBlocks += 1;
}

void addMachineSuccessor(MachineBasicBlock *block, MachineBasicBlock *succ) {
  addToVector(&block->succs, (intptr_t)succ);
  addToVector(&succ->preds, (intptr_t)block);
}

MachineInstr *createMachineInstr(MachineFunction *mf, uint32_t opcode, uint16_t numDefs,
                                 uint16_t numUses) {
  uint16_t numOperands = numDefs + numUses;

  // One allocation for the instruction and its operands together: the operand
  // array is fixed at creation and always outlives exactly as long as the
  // instruction does, so there is nothing to gain from splitting them.
  size_t size = sizeof(MachineInstr) + numOperands * sizeof(MachineOperand);
  MachineInstr *mi = areanAllocate(mf->arena, size);

  memset(mi, 0, size);

  mi->opcode = opcode;
  mi->numOperands = numOperands;
  mi->numDefs = numDefs;
  mi->operands = numOperands > 0 ? (MachineOperand *)(mi + 1) : NULL;

  // Defs come first in the operand array, so mark them up front - every setter
  // then only has to fill in the kind and the payload.
  for (uint16_t idx = 0; idx < numDefs; ++idx) {
    mi->operands[idx].flags.isDef = 1;
  }

  return mi;
}

void addMachineInstrTail(MachineBasicBlock *mbb, MachineInstr *mi) {
  assert(mi->parent == NULL);
  assert(mi->next == NULL && mi->prev == NULL);

  mi->parent = mbb;

  if (mbb->instructions.tail != NULL) {
    mbb->instructions.tail->next = mi;
    mi->prev = mbb->instructions.tail;
    mbb->instructions.tail = mi;
  } else {
    assert(mbb->instructions.head == NULL);
    mbb->instructions.head = mbb->instructions.tail = mi;
  }
}

void addMachineInstrHead(MachineBasicBlock *mbb, MachineInstr *mi) {
  assert(mi->parent == NULL);
  assert(mi->next == NULL && mi->prev == NULL);

  mi->parent = mbb;

  if (mbb->instructions.head != NULL) {
    mbb->instructions.head->prev = mi;
    mi->next = mbb->instructions.head;
    mbb->instructions.head = mi;
  } else {
    assert(mbb->instructions.tail == NULL);
    mbb->instructions.head = mbb->instructions.tail = mi;
  }
}

void addMachineInstrBefore(MachineInstr *at, MachineInstr *mi) {
  MachineBasicBlock *mbb = at->parent;
  assert(mbb != NULL);
  assert(mi->parent == NULL);
  assert(mi->next == NULL && mi->prev == NULL);

  mi->parent = mbb;
  mi->next = at;
  mi->prev = at->prev;

  if (at->prev != NULL) {
    at->prev->next = mi;
  } else {
    assert(mbb->instructions.head == at);
    mbb->instructions.head = mi;
  }

  at->prev = mi;
}

void addMachineInstrAfter(MachineInstr *at, MachineInstr *mi) {
  MachineBasicBlock *mbb = at->parent;
  assert(mbb != NULL);
  assert(mi->parent == NULL);
  assert(mi->next == NULL && mi->prev == NULL);

  mi->parent = mbb;
  mi->prev = at;
  mi->next = at->next;

  if (at->next != NULL) {
    at->next->prev = mi;
  } else {
    assert(mbb->instructions.tail == at);
    mbb->instructions.tail = mi;
  }

  at->next = mi;
}

void eraseMachineInstr(MachineInstr *mi) {
  MachineBasicBlock *mbb = mi->parent;
  assert(mbb != NULL);

  if (mi->prev != NULL) {
    mi->prev->next = mi->next;
  } else {
    assert(mbb->instructions.head == mi);
    mbb->instructions.head = mi->next;
  }

  if (mi->next != NULL) {
    mi->next->prev = mi->prev;
  } else {
    assert(mbb->instructions.tail == mi);
    mbb->instructions.tail = mi->prev;
  }

  mi->next = mi->prev = NULL;
  mi->parent = NULL;
}

MachineOperand *machineOperandAt(MachineInstr *mi, uint16_t idx) {
  assert(idx < mi->numOperands);
  return &mi->operands[idx];
}

void setRegisterOperand(MachineInstr *mi, uint16_t idx, uint32_t reg) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_REG;
  op->info.reg = reg;
}

void setImmediateOperand(MachineInstr *mi, uint16_t idx, int64_t imm) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_IMM;
  op->info.imm = imm;
}

void setMemoryOperand(MachineInstr *mi, uint16_t idx, const MachineAddress *addr) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_MEM;
  op->info.mem = *addr;
}

uint16_t machineOperandRegisters(MachineOperand *op, uint32_t **out) {
  uint16_t count = 0;

  switch (op->kind) {
  case MO_REG:
    out[count++] = &op->info.reg;
    break;
  case MO_MEM:
    // NO_REG for either half is normal rather than exceptional - '[%v1]' has
    // no index and '[rip + g]' has neither - so both are checked.
    if (op->info.mem.base != NO_REG) {
      out[count++] = &op->info.mem.base;
    }
    if (op->info.mem.index != NO_REG) {
      out[count++] = &op->info.mem.index;
    }
    break;
  default:
    break;
  }

  assert(count <= MAX_OPERAND_REGS);
  return count;
}

uint8_t machineInstrSrcSize(const MachineInstr *mi) {
  return mi->srcSize != 0 ? mi->srcSize : mi->opSize;
}

void setFrameIndexOperand(MachineInstr *mi, uint16_t idx, int32_t frameIdx) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_FRAME_IDX;
  op->info.frameIdx = frameIdx;
}

void setBlockOperand(MachineInstr *mi, uint16_t idx, MachineBasicBlock *mbb) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_MBB;
  op->info.mbb = mbb;
}

void setSymbolOperand(MachineInstr *mi, uint16_t idx, Symbol *symbol) {
  MachineOperand *op = machineOperandAt(mi, idx);
  op->kind = MO_SYMBOL;
  op->info.symbol = symbol;
}

// ------------- virtual registers ------------------------

uint32_t createVirtualRegister(MachineFunction *mf, enum RegClass rc, uint8_t size) {
  assert(rc != RC_NONE);

  VRegInfo *info = areanAllocate(mf->arena, sizeof(VRegInfo));
  info->rc = rc;
  info->size = size;

  uint32_t reg = FIRST_VREG + (uint32_t)mf->vregs.size;
  addToVector(&mf->vregs, (intptr_t)info);

  return reg;
}

VRegInfo *virtualRegisterInfo(const MachineFunction *mf, uint32_t reg) {
  assert(isVirtualRegister(reg));
  uint32_t idx = reg - FIRST_VREG;
  assert(idx < mf->vregs.size);
  return (VRegInfo *)getFromVector(&mf->vregs, idx);
}

// A value's register class and width follow from its IR type alone - there is
// no per-value choice to make - so both stages that hand out vregs derive them
// the same way rather than each passing in what it thinks the value is.
static enum RegClass irTypeRegClass(enum IrTypeKind type) {
  return isFloatIrType(type) ? RC_FP : RC_GP;
}

uint8_t irTypeMachineSize(enum IrTypeKind type) {
  switch (type) {
  case IR_BOOL:
  case IR_I8:
  case IR_U8:
    return 1;
  case IR_I16:
  case IR_U16:
    return 2;
  case IR_I32:
  case IR_U32:
  case IR_F32:
    return 4;
  case IR_I64:
  case IR_U64:
  case IR_F64:
    return 8;
  // x87's 80-bit format occupies 16 bytes once aligned. Nothing can hold one
  // in a register of either class, which is the point of the soft-float
  // lowering docs/ir-codegen-design.md section 10 leaves open; sizing it here
  // is only so a long double value can be named before that lands.
  case IR_F80:
    return 16;
  // An aggregate is named by its address, never held whole.
  case IR_P_AGG:
  case IR_PTR:
  case IR_REF:
  case IR_LITERAL:
  case IR_LABEL:
    return 8;
  default:
    unreachable("IR type has no machine width");
  }

  return 0;
}

uint32_t machineVregForValue(MachineFunction *mf, const IrInstruction *value) {
  assert(value->type != IR_VOID && "value-less instruction has no register");

  Vector *map = &mf->irToVreg;

  // Biased by one so that "never asked for" and "vreg id 0" stay distinguishable
  // in a vector whose unwritten entries are zero.
  if (value->id < map->size) {
    intptr_t stored = getFromVector(map, value->id);
    if (stored != 0) {
      return (uint32_t)stored - 1;
    }
  }

  uint32_t reg = createVirtualRegister(mf, irTypeRegClass(value->type), irTypeMachineSize(value->type));
  virtualRegisterInfo(mf, reg)->origin = value;
  putAtVector(map, value->id, (intptr_t)reg + 1);

  return reg;
}

Boolean machineHasVregForValue(const MachineFunction *mf, const IrInstruction *value) {
  const Vector *map = &mf->irToVreg;
  return value->id < map->size && getFromVector(map, value->id) != 0;
}

enum RegClass machineRegisterClass(const MachineFunction *mf, uint32_t reg) {
  if (isVirtualRegister(reg)) {
    return virtualRegisterInfo(mf, reg)->rc;
  }

  if (reg < mf->target->numPhysRegs) {
    return mf->target->regClass[reg];
  }

  return RC_NONE;
}

// ------------- frame ------------------------

int32_t addMachineFrameObject(MachineFunction *mf, enum MachineFrameObjectKind kind, uint32_t size,
                              uint32_t alignment) {
  MachineFrameObject *obj = areanAllocate(mf->arena, sizeof(MachineFrameObject));

  memset(obj, 0, sizeof(MachineFrameObject));

  obj->kind = kind;
  obj->size = size;
  obj->alignment = alignment;

  int32_t frameIdx = (int32_t)mf->frame.objects.size;
  addToVector(&mf->frame.objects, (intptr_t)obj);

  return frameIdx;
}

MachineFrameObject *machineFrameObjectAt(const MachineFunction *mf, int32_t frameIdx) {
  assert(frameIdx >= 0 && (size_t)frameIdx < mf->frame.objects.size);
  return (MachineFrameObject *)getFromVector(&mf->frame.objects, frameIdx);
}

int32_t machineFrameIndexForValue(const MachineFunction *mf, const IrInstruction *value) {
  const Vector *map = &mf->irToFrameIdx;

  if (value->id >= map->size) {
    return -1;
  }

  // Biased by one, so an unwritten entry reads back as "no slot" rather than
  // as frame index 0. See MachineFunction.irToFrameIdx.
  intptr_t stored = getFromVector(map, value->id);
  return stored == 0 ? -1 : (int32_t)stored - 1;
}

// Objects grow downwards from the frame pointer, so an object occupies
// [-offset, -offset + size) and it is 'offset' that has to come out aligned.
// Adding the size first and rounding afterwards is what achieves that.
int32_t placeMachineFrameObject(MachineFunction *mf, int32_t offset, int32_t frameIdx) {
  MachineFrameObject *obj = machineFrameObjectAt(mf, frameIdx);

  offset += obj->size;
  offset = ALIGN_SIZE(offset, obj->alignment);
  obj->offset = -offset;

  return offset;
}

// ------------- constant pool ------------------------

uint32_t addMachineConstant(MachineFunction *mf, enum MachineConstantKind kind, const char *bytes,
                            size_t size, uint32_t alignment) {
  // Linear, like the IR's own constant cache and for the same reason: a
  // function has a handful of these, and a hash map keyed on bytes plus length
  // would cost more to keep honest than the scan costs to run.
  for (size_t idx = 0; idx < mf->constants.size; ++idx) {
    const MachineConstant *c = machineConstantAt(mf, idx);
    if (c->kind == kind && c->size == size && memcmp(c->bytes, bytes, size) == 0) {
      return (uint32_t)idx;
    }
  }

  MachineConstant *c = areanAllocate(mf->arena, sizeof(MachineConstant));

  memset(c, 0, sizeof(MachineConstant));

  c->kind = kind;
  c->bytes = bytes;
  c->size = size;
  c->alignment = alignment;

  uint32_t constantIdx = (uint32_t)mf->constants.size;
  addToVector(&mf->constants, (intptr_t)c);

  return constantIdx;
}

const MachineConstant *machineConstantAt(const MachineFunction *mf, uint32_t constantIdx) {
  assert(constantIdx < mf->constants.size);
  return (const MachineConstant *)getFromVector(&mf->constants, constantIdx);
}

Boolean isMachineAddressWellFormed(const MachineAddress *addr) {
  if (addr->kind == MAK_REG) {
    return TRUE;
  }

  return addr->base == NO_REG && addr->index == NO_REG && addr->disp == 0;
}

// The machine block mirroring a given IR block. A walk rather than a map: the
// callers - phi destruction, then selection resolving a branch target - touch
// each edge once, and carrying the build's id->block table forward is more
// state to keep honest than this costs to recompute.
MachineBasicBlock *machineBlockForIrBlock(MachineFunction *mf, const IrBasicBlock *ir) {
  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    if (mbb->ir == ir) {
      return mbb;
    }
  }

  return NULL;
}

// ------------- build phase ------------------------

// Mirrors the IR CFG into an empty MachineFunction: one MachineBasicBlock per
// IR block, in the IR's own block order, with the edges copied across. No
// instructions - selection (stage 1) fills those in. Having the skeleton land
// on its own means the later stages diff cleanly against it.
MachineFunction *buildMachineFunction(IrFunction *f) {
  MachineFunction *mf = createMachineFunction(f);

  // IR block ids stay put when a pass erases a block, so after gvn/dce they
  // are sparse rather than dense. Size the id->block map by the largest id
  // actually present instead of by the block count.
  uint32_t maxId = 0;
  for (const IrBasicBlock *b = f->blocks.head; b != NULL; b = b->next) {
    if (b->id > maxId) {
      maxId = b->id;
    }
  }

  size_t mapSize = (maxId + 1) * sizeof(MachineBasicBlock *);
  MachineBasicBlock **irToMachine = areanAllocate(mf->arena, mapSize);
  memset(irToMachine, 0, mapSize);

  for (const IrBasicBlock *b = f->blocks.head; b != NULL; b = b->next) {
    MachineBasicBlock *mbb = createMachineBasicBlock(mf, b->name, b);
    addMachineBasicBlockTail(mf, mbb);
    irToMachine[b->id] = mbb;
  }

  // Both edge vectors are copied across rather than one being derived from the
  // other, so that each keeps the IR's own ordering. Deriving preds from succs
  // instead reorders them into block order, which makes an MBB header disagree
  // with the IR block header it mirrors for no reason - and edge order is not
  // an implementation detail here, since phi destruction is about to start
  // reasoning about which edge is which.
  for (const IrBasicBlock *b = f->blocks.head; b != NULL; b = b->next) {
    MachineBasicBlock *mbb = irToMachine[b->id];

    for (size_t idx = 0; idx < b->succs.size; ++idx) {
      const IrBasicBlock *succ = getBlockFromVector(&b->succs, idx);
      // An edge pointing outside the block list would mean the CFG still names
      // a block some pass unlinked, which is a bug in that pass rather than
      // something to paper over here.
      assert(succ->id <= maxId && irToMachine[succ->id] != NULL);
      addToVector(&mbb->succs, (intptr_t)irToMachine[succ->id]);
    }

    for (size_t idx = 0; idx < b->preds.size; ++idx) {
      const IrBasicBlock *pred = getBlockFromVector(&b->preds, idx);
      assert(pred->id <= maxId && irToMachine[pred->id] != NULL);
      addToVector(&mbb->preds, (intptr_t)irToMachine[pred->id]);
    }
  }

  return mf;
}
