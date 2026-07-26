
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

enum RegClass machineRegisterClass(const MachineFunction *mf, uint32_t reg) {
  if (isVirtualRegister(reg)) {
    return virtualRegisterInfo(mf, reg)->rc;
  }

  if (reg < mf->target->numPhysRegs) {
    return mf->target->regClass[reg];
  }

  return RC_NONE;
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
