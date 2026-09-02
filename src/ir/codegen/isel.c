#include <assert.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "ir/loops.h"
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
// Which successor that is, is a decision, and until step 39 it was an
// inheritance: ast2ir adds a conditional branch's successors as
// [taken, notTaken] almost everywhere, so the not-taken arm fell through - the
// right call for an if-statement, whose two arms are equally likely, and the
// wrong one for a loop, where the taken edge is the body and runs every
// iteration. A while loop came out as head, exit, <the whole rest of the
// function>, body, and paid a taken conditional branch into the body and a
// taken jump back out of it on every trip.
//
// So the walk orders each block's successors by loop depth, shallowest first,
// and the deepest one is the block that lands next. Ordering by loop depth is
// what makes a loop's body contiguous with its header: the exit leaves the
// loop and so sorts ahead of the body, gets its subtree finished first, and
// ends up after it in the reversal.
//
// The order is stable, so a block whose successors are all at one depth - most
// of them, including every if-statement outside a loop - keeps the order
// ast2ir gave it and lays out exactly as it did before.
//
// None of it is a correctness question. The terminator reads 'taken' and
// 'notTaken' from the branch itself and never from this order, and inverts the
// condition when the layout calls for it.

// -============================ Loop structure ============================-
//
// Which successor is the deeper one is a question about loops, and the answer
// comes from src/ir/codegen/loops.c rather than from here: back edges, natural
// loops and their nesting are wanted by stage 2C's spill ranking too, and two
// passes with their own idea of where the loops are is two things that can
// disagree. See include/ir/loops.h.

static void layoutVisit(MachineBasicBlock *mbb, Boolean *visited, const MachineLoopInfo *li,
                        Vector *postorder) {
  visited[mbb->id] = TRUE;

  const size_t numSuccs = mbb->succs.size;

  if (numSuccs > 1) {
    MachineBasicBlock **order = heapAllocate(numSuccs * sizeof(MachineBasicBlock *));

    for (size_t idx = 0; idx < numSuccs; ++idx) {
      order[idx] = (MachineBasicBlock *)getFromVector(&mbb->succs, idx);
    }

    // Insertion sort, shallowest first, and stable because it only moves a
    // successor past one strictly deeper than it.
    for (size_t idx = 1; idx < numSuccs; ++idx) {
      MachineBasicBlock *succ = order[idx];
      size_t j = idx;

      while (j > 0 && machineLoopDepthOf(li, order[j - 1]) > machineLoopDepthOf(li, succ)) {
        order[j] = order[j - 1];
        j -= 1;
      }

      order[j] = succ;
    }

    for (size_t idx = 0; idx < numSuccs; ++idx) {
      if (!visited[order[idx]->id]) {
        layoutVisit(order[idx], visited, li, postorder);
      }
    }

    releaseHeap(order);
  } else if (numSuccs == 1) {
    MachineBasicBlock *succ = (MachineBasicBlock *)getFromVector(&mbb->succs, 0);
    if (!visited[succ->id]) {
      layoutVisit(succ, visited, li, postorder);
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

  MachineLoopInfo loops = {0};
  computeMachineLoops(mf, &loops);

  layoutVisit(mf->blocks.head, visited, &loops, &postorder);

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
  releaseMachineLoops(&loops);
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

// -============================ Foldings ============================-
//
// Section 6's other two foldings - an addressing mode, and a compare that
// becomes a branch's condition - are decided here, before anything is
// selected, for the same reason decideConstants is: the instruction that
// absorbs a value is usually selected after the one that would have computed
// it, so the question has to be settled while both are still visible.
//
// What both have in common is that the absorbed value stops being an
// instruction and becomes part of another one's operands. The walk skips it,
// and whoever absorbed it spells it out.
//
// A value is only skipped when *every* one of its uses absorbed it. A GEP
// feeding one load and one call still needs its 'lea' for the call - and the
// load still folds, because an addressing mode costs nothing either way.

static const IrInstruction *foldInputAt(const IrInstruction *i, size_t idx) {
  return getInstructionFromVector(&i->inputs, idx);
}

static Boolean isIntegerConstant(const IrInstruction *i) {
  return i->kind == IR_DEF_CONST && i->info.constant.kind == IR_CK_INTEGER;
}

typedef struct _FoldContext {
  MachineBuilder *b;
  const ArchSelector *sel;

  // How many of each value's uses were absorbed, indexed by IrInstruction.id.
  Vector absorbedUses;
} FoldContext;

static void absorbUse(FoldContext *fc, const IrInstruction *value) {
  intptr_t seen =
      value->id < fc->absorbedUses.size ? getFromVector(&fc->absorbedUses, value->id) : 0;

  putAtVector(&fc->absorbedUses, value->id, seen + 1);
}

// The fold recorded for a value, or NULL when it has none. A fold is only ever
// recorded for a value whose own definition it expands - a GEP, or something
// living in a frame slot - so a non-NULL answer also means "using this absorbs
// a use of the value", and a NULL one means the pointer keeps its register.
const AddressFold *machineBuilderAddressFold(const MachineBuilder *b, const IrInstruction *ptr) {
  if (ptr->id >= b->addressFolds.size) {
    return NULL;
  }

  return (const AddressFold *)getFromVector(&b->addressFolds, ptr->id);
}

static void recordFold(FoldContext *fc, const IrInstruction *value, const AddressFold *f) {
  AddressFold *stored = areanAllocate(fc->b->mf->arena, sizeof(AddressFold));

  *stored = *f;
  putAtVector(&fc->b->addressFolds, value->id, (intptr_t)stored);
}

// The index term a GEP's non-constant offset contributes. 'a[i]' reaches the
// IR as 'base + (i << log2 elementSize)' computed at pointer width, so the
// shift is precisely the multiply an addressing mode would do for free - and
// recovering it is what turns three instructions into one.
//
// Only a single-use shift. Unlike a displacement, an index term is not free to
// duplicate: the value being scaled usually needs widening to pointer width
// first, and that is a real instruction at each place the term is folded in.
static void recoverScale(const IrInstruction *offset, const IrInstruction **index,
                         uint32_t *scale) {
  *index = offset;
  *scale = 1;

  if (offset->kind != IR_E_SHL || offset->uses.size != 1) {
    return;
  }

  // At pointer width, so that folding cannot change what the shift's own
  // overflow would have done to the bits above it.
  if (irTypeMachineSize(offset->type) != sizeof(intptr_t)) {
    return;
  }

  const IrInstruction *amount = foldInputAt(offset, 1);

  if (!isIntegerConstant(amount) || amount->info.constant.data.i < 0 ||
      amount->info.constant.data.i > 3) {
    return;
  }

  *index = foldInputAt(offset, 0);
  *scale = 1u << amount->info.constant.data.i;
}

// Adds one GEP's offset to an address. Returns FALSE when the terms no longer
// fit one addressing mode, which is what stops the walk.
static Boolean addOffsetTerm(FoldContext *fc, AddressFold *f, const IrInstruction *offset) {
  if (isIntegerConstant(offset)) {
    int64_t disp = (int64_t)f->disp + offset->info.constant.data.i;

    if (fc->sel->isLegalAddressMode(f->scale, disp)) {
      f->disp = (int32_t)disp;
      return TRUE;
    }

    // Too far away to be a displacement, so it is a value like any other and
    // has to be added through the index instead - unless it was folded into an
    // immediate, in which case there is no register to name it by. That
    // happens only when the constant fits a displacement on its own and the
    // *running* one it is being added to does not, so stopping the walk is the
    // answer: the GEP is then folded afresh, against a base of its own, and
    // the offset fits again.
    if (machineBuilderIsFolded(fc->b, offset)) {
      return FALSE;
    }
  }

  // One index and no more: an addressing mode scales a single register.
  if (f->index != NULL) {
    return FALSE;
  }

  const IrInstruction *index = NULL;
  uint32_t scale = 0;

  recoverScale(offset, &index, &scale);

  if (!fc->sel->isLegalAddressMode(scale, f->disp)) {
    return FALSE;
  }

  if (index != offset) {
    // The shift computed nothing else, so the scale replaces it entirely.
    absorbUse(fc, offset);
  }

  f->index = index;
  f->scale = scale;
  return TRUE;
}

// The addressing mode that computes a value, memoized. Values are visited in
// layout order, so a GEP's base has already been folded by the time the GEP
// is: the walk backwards along a chain is one lookup per link rather than a
// re-walk, which is also what keeps a shared link from being counted twice.
static void foldValue(FoldContext *fc, const IrInstruction *value) {
  MachineFunction *mf = fc->b->mf;

  // A local's address is a fixed displacement from the frame pointer, so
  // anchoring to the slot costs no register and is worth taking however many
  // uses the value has.
  int32_t frameIdx = machineFrameIndexForValue(mf, value);

  if (frameIdx >= 0) {
    // A dynamic allocation has no fixed offset - its address is wherever the
    // stack pointer ended up - which is not something an anchor can say.
    if (!machineFrameObjectAt(mf, frameIdx)->isDynamic) {
      AddressFold f = { NULL, frameIdx, NULL, 0, 0 };
      recordFold(fc, value, &f);
    }

    return;
  }

  if (value->kind != IR_GET_ELEMENT_PTR) {
    return;
  }

  const IrInstruction *base = foldInputAt(value, 0);
  const IrInstruction *offset = foldInputAt(value, 1);
  const AddressFold *inner = machineBuilderAddressFold(fc->b, base);

  // Expanding the base as well is the whole walk; when the combined terms do
  // not fit, the base keeps its register and only this GEP's own offset folds.
  if (inner != NULL) {
    AddressFold combined = *inner;

    if (addOffsetTerm(fc, &combined, offset)) {
      absorbUse(fc, base);
      recordFold(fc, value, &combined);
      return;
    }
  }

  AddressFold f = { base, -1, NULL, 0, 0 };

  // A base and one term always fit on a target with a scaled-index mode. One
  // without would have to spell a GEP as an add, and this is where its
  // selector would find that out.
  Boolean fits = addOffsetTerm(fc, &f, offset);
  assert(fits && "the target cannot address a pointer plus an offset");

  recordFold(fc, value, &f);
}

// A value that every use absorbed computes nothing and is not selected. Stage
// 0 having already named it settles the question the other way: a phi copy
// reads it from a register, and that copy is not an address.
static void markAbsorbed(FoldContext *fc, const IrInstruction *value) {
  intptr_t absorbed =
      value->id < fc->absorbedUses.size ? getFromVector(&fc->absorbedUses, value->id) : 0;

  // Nothing absorbed at all is the answer for most of the function, and it is
  // emphatically not the same as "every use absorbed": an instruction with no
  // uses is one whose whole point is its effect - a store, a call, a return.
  if (absorbed == 0 || (size_t)absorbed != value->uses.size ||
      machineHasVregForValue(fc->b->mf, value)) {
    return;
  }

  putAtVector(&fc->b->absorbed, value->id, (intptr_t)1);
}

// Whether a compare can be the condition of the branch that follows it,
// leaving the boolean it would otherwise materialize unbuilt.
//
// In the same block, because the compare and the jump have to end up adjacent:
// flags are not modelled (section 6.4), so nothing may come between them, and
// a compare from another block would have to be moved here to manage it.
// Single use, because a boolean anything else reads still has to exist -
// markAbsorbed is what turns that into the decision.
static Boolean foldsIntoBranch(const IrInstruction *cond, const IrInstruction *branch) {
  return cond->block == branch->block && cond->uses.size == 1;
}

static void decideFoldings(MachineBuilder *b, const ArchSelector *sel) {
  FoldContext fc = { b, sel };
  initVector(&fc.absorbedUses, INITIAL_VECTOR_CAPACITY);

  // Layout order rather than IR order, so that a value is always folded before
  // anything that could fold it away.
  for (MachineBasicBlock *mbb = b->mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (const IrInstruction *i = mbb->ir->instrunctions.head; i != NULL; i = i->next) {
      uint32_t addressOperands = sel->addressOperands != NULL ? sel->addressOperands(i) : 0;

      foldValue(&fc, i);

      for (size_t idx = 0; idx < i->inputs.size; ++idx) {
        if ((addressOperands & (1u << idx)) == 0) {
          continue;
        }

        const IrInstruction *ptr = foldInputAt(i, idx);

        if (machineBuilderAddressFold(b, ptr) != NULL) {
          absorbUse(&fc, ptr);
        }
      }
    }

    const IrInstruction *term = mbb->ir->term;

    if (term != NULL && term->kind == IR_CBRANCH && sel->foldsIntoCondition != NULL) {
      const IrInstruction *cond = foldInputAt(term, 0);

      if (sel->foldsIntoCondition(cond) && foldsIntoBranch(cond, term)) {
        absorbUse(&fc, cond);
      }
    }
  }

  for (const IrBasicBlock *block = b->mf->ir->blocks.head; block != NULL; block = block->next) {
    for (const IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
      markAbsorbed(&fc, i);
    }
  }

  releaseVector(&fc.absorbedUses);
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

Boolean machineBuilderIsAbsorbed(const MachineBuilder *b, const IrInstruction *value) {
  if (value->id >= b->absorbed.size) {
    return FALSE;
  }

  return getFromVector(&b->absorbed, value->id) != 0;
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
    // spell it out where they need it. An absorbed one is not even that: it
    // is part of the instruction that took it over.
    if (machineBuilderIsFolded(b, i) || machineBuilderIsAbsorbed(b, i)) {
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

  // A target with no selector cannot get here: '-experimental' is refused for
  // any '-march' but x86_64 where the options are read. This used to leave the
  // skeleton stage 0 built and let every function fall back one at a time,
  // which is not a thing that can happen any more.
  assert(sel != NULL && "this target has no instruction selector");

  layoutBlocks(mf);

  MachineBuilder builder = {0};
  builder.mf = mf;
  initVector(&builder.foldedConstants, INITIAL_VECTOR_CAPACITY);
  initVector(&builder.absorbed, INITIAL_VECTOR_CAPACITY);
  initVector(&builder.addressFolds, INITIAL_VECTOR_CAPACITY);

  decideConstants(&builder, sel);
  decideFoldings(&builder, sel);

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    // A machine block with no IR behind it would be one the backend invented,
    // and nothing invents blocks before selection - stage 0 splits edges on
    // the IR, before the mirror is taken.
    assert(mbb->ir != NULL);
    selectBlock(&builder, sel, mbb);
  }

  releaseVector(&builder.foldedConstants);
  releaseVector(&builder.absorbed);
  releaseVector(&builder.addressFolds);

  // Here rather than in stage 2, because this is the last stage that invents
  // defs of virtual registers: what allocation adds is spills and reloads of
  // physical ones.
  verifyMachineDefWidths(mf);

  // The flags are selection's business alone for the same reason: nothing
  // after this emits an instruction that touches them, spills and reloads
  // being moves.
  verifyFlagsDependencies(mf);
}
