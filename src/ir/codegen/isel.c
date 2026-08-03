#include <assert.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "ir/machine.h"

// The selector is looked up here rather than hung off TargetDescriptor
// because a cross-TU symbol address in a *static initializer* is currently
// miscompiled by EduCC itself (docs/ir-codegen-design.md section 10):
// targetX86_64 already carries one such field and there is no reason to add a
// second. Inside a function body the same addresses are ordinary .text
// relocations, which work.
static const ArchSelector *archSelectorFor(const TargetDescriptor *target) {
  if (target == &targetX86_64) {
    return &x86Selector;
  }

  // riscv64 has no selector yet.
  return NULL;
}

// -============================ Block layout ============================-
//
// Selection also settles the order the blocks will be emitted in, because
// whether a branch is needed at all depends on it: a jump to the block that
// happens to come next is not a jump.
//
// Reverse postorder is the order used, and the successor order it is built
// from is what decides which arm of a branch gets the fallthrough: a
// depth-first walk finishes the first successor's subtree first, so the second
// successor is what lands immediately after the branch in the reversed order.
//
// Note that this makes layout quality depend on an order nobody is
// maintaining for the purpose. ast2ir adds a conditional branch's successors
// as [taken, notTaken] almost everywhere, so the not-taken arm usually falls
// through - the right call for an if-statement, whose two arms are equally
// likely, and the wrong one for a loop, where the taken edge is the body and
// runs every iteration. The '||' short-circuit adds its two the other way
// round, so it comes out inverted from the rest.
//
// None of that is a correctness question - the terminator reads 'taken' and
// 'notTaken' from the branch itself and never from this order, and inverts the
// condition when the layout calls for it. Making the layout deliberate rather
// than inherited needs it to know which edges are back edges, which is a piece
// of loop analysis this pipeline does not have yet: see
// docs/ir-codegen-design.md section 10.

static void layoutVisit(MachineBasicBlock *mbb, Boolean *visited, Vector *postorder) {
  visited[mbb->id] = TRUE;

  for (size_t idx = 0; idx < mbb->succs.size; ++idx) {
    MachineBasicBlock *succ = (MachineBasicBlock *)getFromVector(&mbb->succs, idx);
    if (!visited[succ->id]) {
      layoutVisit(succ, visited, postorder);
    }
  }

  addToVector(postorder, (intptr_t)mbb);
}

static void appendBlock(MachineFunction *mf, MachineBasicBlock *mbb) {
  mbb->prev = mbb->next = NULL;

  if (mf->blocks.tail != NULL) {
    mf->blocks.tail->next = mbb;
    mbb->prev = mf->blocks.tail;
  } else {
    mf->blocks.head = mbb;
  }

  mf->blocks.tail = mbb;
}

static void layoutBlocks(MachineFunction *mf) {
  size_t count = mf->numBlocks;
  assert(count != 0);

  // Block ids are handed out densely in creation order (see
  // createMachineBasicBlock), so they index this directly.
  Boolean *visited = heapAllocate(count * sizeof(Boolean));
  memset(visited, 0, count * sizeof(Boolean));

  Vector postorder = {0};
  initVector(&postorder, count);

  layoutVisit(mf->blocks.head, visited, &postorder);

  // Anything the walk never reached is not reachable from the entry block. dce
  // deletes those, so in practice there are none - but a layout pass that
  // silently dropped a block would be a miserable thing to debug, so they are
  // collected and kept, after everything reachable, in their existing order.
  Vector unreached = {0};
  initVector(&unreached, 1);
  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    if (!visited[mbb->id]) {
      addToVector(&unreached, (intptr_t)mbb);
    }
  }

  mf->blocks.head = mf->blocks.tail = NULL;

  for (size_t idx = postorder.size; idx > 0; --idx) {
    appendBlock(mf, (MachineBasicBlock *)getFromVector(&postorder, idx - 1));
  }

  for (size_t idx = 0; idx < unreached.size; ++idx) {
    appendBlock(mf, (MachineBasicBlock *)getFromVector(&unreached, idx));
  }

  releaseVector(&postorder);
  releaseVector(&unreached);
  releaseHeap(visited);
}

// -============================ Constants ============================-
//
// IR_DEF_CONSTs are hoisted into the entry block, so a constant that gets a
// register holds it for the whole function whether or not anything is using it
// at the time. Most of them do not need one: x86 arithmetic takes a 32-bit
// immediate directly, and so does a compare.
//
// The decision is per constant rather than per use, and has to be taken before
// anything is selected, because it is the same decision at the definition
// ("emit a materializing move, or nothing?") and at every use ("name a
// register, or an immediate?"). A constant is folded only when *every* use can
// take it as an immediate; one use that cannot forces the register, and the
// others then read it from there rather than the function carrying both forms.
//
// A symbol constant goes the same way for a different reason. The name of a
// directly called function is one - 'f(x)' translates to a call whose first
// input is the address of f - and the call encodes it as a relocated
// displacement, so materializing the address into a register first would be a
// register held across the whole function to hold something the instruction
// spells out itself. It is only ever foldable *there*: taking a function's
// address for any other purpose needs a real 'lea', which is a rule nothing
// has yet, so such a constant keeps its register and comes out unselected.

// Whether a constant can be an operand of every one of its users, rather than
// a value some instruction has to materialize first. Called "immediate" after
// the usual case; a folded symbol becomes an MO_SYMBOL operand rather than an
// MO_IMM one, which is the same decision about a different operand kind.
static Boolean allUsesTakeImmediate(const ArchSelector *sel, const IrInstruction *cnst) {
  for (size_t u = 0; u < cnst->uses.size; ++u) {
    const IrInstruction *use = getInstructionFromVector(&cnst->uses, u);
    Boolean found = FALSE;

    // By position, and every position: the same constant can be both operands
    // of an instruction, and the two need not be foldable alike.
    for (size_t idx = 0; idx < use->inputs.size; ++idx) {
      if (getInstructionFromVector(&use->inputs, idx) != cnst) {
        continue;
      }
      found = TRUE;
      if (!sel->isLegalImmediate(use, idx, cnst)) {
        return FALSE;
      }
    }

    // A user that lists the constant among its uses but not among its inputs
    // holds it somewhere this walk cannot see, so there is no operand position
    // to ask about. Keep the register.
    if (!found) {
      return FALSE;
    }
  }

  return TRUE;
}

static void decideConstants(MachineBuilder *b, const ArchSelector *sel) {
  for (const IrBasicBlock *block = b->mf->ir->blocks.head; block != NULL; block = block->next) {
    for (const IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
      if (i->kind != IR_DEF_CONST) {
        continue;
      }

      // A float or a string literal has no operand form on either target: both
      // live in memory and are reached through an address, which is a rule
      // nothing has yet.
      if (i->info.constant.kind != IR_CK_INTEGER && i->info.constant.kind != IR_CK_SYMBOL) {
        continue;
      }

      // Stage 0 may already have named this value: a constant flowing into a
      // phi is the source of one of its copies, and a copy's source is a
      // register. That decision was taken before selection existed, and
      // selection does not get to overrule it.
      if (machineHasVregForValue(b->mf, i)) {
        continue;
      }

      if (allUsesTakeImmediate(sel, i)) {
        putAtVector(&b->foldedConstants, i->id, (intptr_t)1);
      }
    }
  }
}

// -============================ Builder API ============================-

MachineInstr *buildMachineInstr(MachineBuilder *b, uint32_t opcode, uint16_t numDefs,
                                uint16_t numUses) {
  MachineInstr *mi = createMachineInstr(b->mf, opcode, numDefs, numUses);
  mi->origin = b->origin;

  if (b->insertBefore != NULL) {
    addMachineInstrBefore(b->insertBefore, mi);
  } else {
    addMachineInstrTail(b->mbb, mi);
  }

  return mi;
}

Boolean machineBuilderIsFolded(const MachineBuilder *b, const IrInstruction *value) {
  if (value->id >= b->foldedConstants.size) {
    return FALSE;
  }

  return getFromVector(&b->foldedConstants, value->id) != 0;
}

uint32_t machineBuilderVreg(MachineBuilder *b, const IrInstruction *value) {
  assert(!machineBuilderIsFolded(b, value) && "a folded constant has no register");
  return machineVregForValue(b->mf, value);
}

void setValueOperand(MachineBuilder *b, MachineInstr *mi, uint16_t idx,
                     const IrInstruction *value) {
  if (!machineBuilderIsFolded(b, value)) {
    setRegisterOperand(mi, idx, machineVregForValue(b->mf, value));
    return;
  }

  if (value->info.constant.kind == IR_CK_SYMBOL) {
    setSymbolOperand(mi, idx, value->info.constant.data.s);
  } else {
    setImmediateOperand(mi, idx, value->info.constant.data.i);
  }
}

Boolean machineBuilderFallsThroughTo(const MachineBuilder *b, const IrBasicBlock *target) {
  return b->mbb->next != NULL && b->mbb->next->ir == target;
}

MachineBasicBlock *machineBuilderBlock(MachineBuilder *b, const IrBasicBlock *target) {
  MachineBasicBlock *mbb = machineBlockForIrBlock(b->mf, target);
  assert(mbb != NULL && "branch target has no machine block");
  return mbb;
}

void buildUnselected(MachineBuilder *b, const IrInstruction *i) {
  // Inputs carrying no value of their own have no register to name and are
  // left out. What is kept is enough for the placeholder to look to liveness
  // like the thing it stands for: it reads these, and writes that.
  uint16_t numUses = 0;
  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    if (getInstructionFromVector(&i->inputs, idx)->type != IR_VOID) {
      numUses += 1;
    }
  }

  uint16_t numDefs = i->type != IR_VOID ? 1 : 0;
  MachineInstr *mi = buildMachineInstr(b, MOP_UNSELECTED, numDefs, numUses);

  if (numDefs != 0) {
    setRegisterOperand(mi, 0, machineVregForValue(b->mf, i));
  }

  uint16_t op = numDefs;
  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    const IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    if (input->type != IR_VOID) {
      setValueOperand(b, mi, op++, input);
    }
  }

  b->mf->hasUnselected = TRUE;
}

// -============================ The walk ============================-

static void selectBlock(MachineBuilder *b, const ArchSelector *sel, MachineBasicBlock *mbb) {
  const IrBasicBlock *block = mbb->ir;

  b->mbb = mbb;
  // Whatever is in the block already is stage 0's: the phi copies for this
  // block's outgoing edges. Everything selected here belongs in front of them.
  b->insertBefore = mbb->instructions.head;

  for (const IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
    // Phis were destroyed into copies by stage 0 and deliberately left in the
    // IR; selecting them would compute the same values a second time.
    if (i->kind == IR_PHI) {
      continue;
    }

    // A folded constant is not an instruction, it is an operand, and its uses
    // spell it out where they need it.
    if (machineBuilderIsFolded(b, i)) {
      continue;
    }

    b->origin = i;

    if (i == block->term) {
      // The terminator goes *after* the phi copies: those hand values to the
      // successors along the edge it is about to take.
      b->insertBefore = NULL;
      sel->selectTerminator(b, i);
    } else {
      sel->selectInstruction(b, i);
    }
  }

  b->origin = NULL;
}

void selectInstructions(MachineFunction *mf) {
  const ArchSelector *sel = archSelectorFor(mf->target);

  // riscv64 has no selector yet. Leaving the skeleton stage 0 built is the
  // honest outcome: an empty machine function is visibly unfinished, whereas
  // x86 instructions under a riscv64 header would not be.
  if (sel == NULL) {
    return;
  }

  layoutBlocks(mf);

  MachineBuilder builder = {0};
  builder.mf = mf;
  initVector(&builder.foldedConstants, INITIAL_VECTOR_CAPACITY);

  decideConstants(&builder, sel);

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    // A machine block with no IR behind it would be one the backend invented,
    // and nothing invents blocks before selection - stage 0 splits edges on
    // the IR, before the mirror is taken.
    assert(mbb->ir != NULL);
    selectBlock(&builder, sel, mbb);
  }

  releaseVector(&builder.foldedConstants);
}
