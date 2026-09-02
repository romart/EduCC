
#include <assert.h>

#include "ir/ir.h"
#include "ir/machine.h"
#include "sema.h"

// -============================ Stage 0: prepare / legalize ==============-
//
// Everything the machine-level stages need to be able to assume, established
// once, before instruction selection runs. See docs/ir-codegen-design.md 5.
//
// The stage straddles the IR/machine boundary on purpose: some of what it
// establishes is a property of the *control flow graph*, and the machine CFG
// is a mirror of the IR one, so it is cheaper and far less error-prone to fix
// the graph up while there is still only one copy of it. Hence the ordering
// below - the CFG work happens on the IrFunction, the skeleton is built from
// the result, and the rest works on the MachineFunction.

// Every critical edge is gone by the time stage 0 is done with the CFG, with
// one exception it can do nothing about: a computed goto (IR_IBRANCH) branches
// through a block address taken somewhere else entirely, so its successors
// cannot be redirected by rewriting the terminator the way a conditional or a
// switch can.
//
// A phi behind such an edge is allowed all the same: phi destruction takes
// every edge out of one block as a single parallel assignment, which is what
// makes the copies for one target harmless on the way to another. See
// destroyPhiNodes() below for why that works.
static Boolean isUnsplittableEdge(const IrBasicBlock *block, const IrBasicBlock *succ) {
  return block->term != NULL && block->term->kind == IR_IBRANCH;
}

static void assertCriticalEdgesSplit(const IrFunction *f) {
  for (const IrBasicBlock *block = f->blocks.head; block != NULL; block = block->next) {
    for (size_t idx = 0; idx < block->succs.size; ++idx) {
      const IrBasicBlock *succ = getBlockFromVector(&block->succs, idx);
      assert(!isCriticalEdge(block, succ) || isUnsplittableEdge(block, succ));
    }
  }
}

// -============================ Phi destruction ============================-
//
// A phi is not an instruction - it is a statement about how control reached
// the block. Machines have no such statement, so each 'x = phi(v1:B1, ..., vn:Bn)'
// becomes a copy 'x <- vi' at the end of every predecessor Bi, which is where
// the answer to "which edge did we arrive by" is still known.
//
// The copies for one edge are a *parallel* assignment - conceptually they all
// happen at once, on entry to the successor - and writing them out one after
// another is only correct if no copy overwrites a register a later one still
// reads. Blocks holding several phis routinely violate that: 'a = phi(b), b =
// phi(a)' asks for a swap, and emitting 'a <- b; b <- a' leaves both holding
// the old b. sequentializeCopies() below is what turns the parallel form into
// a legal sequence.
//
// The copies are emitted freely, with no attempt to coalesce - a copy whose
// source and destination end up in the same register is the register
// allocator's to delete (docs/ir-codegen-design.md section 7), and trying to
// be clever here instead is the classic way to get this subtly wrong.

typedef struct _ParallelCopy {
  uint32_t dst;
  uint32_t src;
  const IrInstruction *phi; // what this copy is carrying, for dumps
  Boolean emitted;
} ParallelCopy;

// True when 'reg' is still to be read by a copy that has not been emitted yet,
// so overwriting it now would destroy a value someone is waiting for.
static Boolean isPendingSource(const ParallelCopy *copies, size_t count, uint32_t reg) {
  for (size_t idx = 0; idx < count; ++idx) {
    if (!copies[idx].emitted && copies[idx].src == reg) {
      return TRUE;
    }
  }

  return FALSE;
}

static void emitCopy(MachineFunction *mf, MachineBasicBlock *mbb, uint32_t dst, uint32_t src,
                     uint8_t size, const IrInstruction *origin) {
  MachineInstr *mi = createMachineInstr(mf, MOP_COPY, 1, 1);

  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  mi->opSize = size;
  // The phi this copy carries, or NULL when it is the temporary that breaks a
  // cycle - which is exactly the distinction worth seeing in a dump.
  mi->origin = origin;

  addMachineInstrTail(mbb, mi);
}

// Writes a parallel assignment out as a sequence of ordinary copies, appended
// to 'mbb'. Destinations are distinct (they are phi results, one per phi), so
// the dependencies between the copies form chains ending in cycles, and there
// are only two situations to tell apart.
static void sequentializeCopies(MachineFunction *mf, MachineBasicBlock *mbb, ParallelCopy *copies,
                                size_t count) {
  size_t remaining = 0;

  for (size_t idx = 0; idx < count; ++idx) {
    // Distinct destinations are what the whole algorithm rests on, and what
    // stops a predecessor listed twice by the same successor from reaching
    // here - splitCriticalEdges() gives each of those edges a block of its own.
    for (size_t other = 0; other < idx; ++other) {
      assert(copies[other].dst != copies[idx].dst && "two copies into one phi");
    }

    // 'x <- x' says nothing and would otherwise look like a cycle of one.
    copies[idx].emitted = copies[idx].dst == copies[idx].src;
    if (!copies[idx].emitted) {
      remaining += 1;
    }
  }

  while (remaining != 0) {
    Boolean progress = FALSE;

    // Anything whose destination nobody is still waiting to read can go now.
    for (size_t idx = 0; idx < count; ++idx) {
      ParallelCopy *copy = &copies[idx];
      if (copy->emitted || isPendingSource(copies, count, copy->dst)) {
        continue;
      }

      emitCopy(mf, mbb, copy->dst, copy->src, virtualRegisterInfo(mf, copy->dst)->size, copy->phi);
      copy->emitted = TRUE;
      remaining -= 1;
      progress = TRUE;
    }

    if (progress) {
      continue;
    }

    // Every remaining destination is also somebody's source, which can only
    // mean the rest is a permutation - a cycle with no free end to start from.
    // Break one open by parking a single value in a fresh register: the copies
    // that were reading it read the copy instead, its own destination stops
    // being anyone's source, and the pass above can proceed.
    for (size_t idx = 0; idx < count; ++idx) {
      ParallelCopy *copy = &copies[idx];
      if (copy->emitted) {
        continue;
      }

      const VRegInfo *info = virtualRegisterInfo(mf, copy->dst);
      uint32_t tmp = createVirtualRegister(mf, info->rc, info->size);
      emitCopy(mf, mbb, tmp, copy->dst, info->size, NULL);

      for (size_t other = 0; other < count; ++other) {
        if (!copies[other].emitted && copies[other].src == copy->dst) {
          copies[other].src = tmp;
        }
      }

      break;
    }
  }
}

static MachineBasicBlock *machineBlockOf(MachineFunction *mf, const IrBasicBlock *ir) {
  MachineBasicBlock *mbb = machineBlockForIrBlock(mf, ir);
  assert(mbb != NULL && "IR block has no machine block");
  return mbb;
}

static size_t countPhis(const IrBasicBlock *block) {
  size_t count = 0;

  // SSA construction and gvn both put phis at the head of the block and
  // nothing after them is one, so the first non-phi ends the run.
  for (const IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
    if (i->kind != IR_PHI) {
      break;
    }
    count += 1;
  }

  return count;
}

// Where one predecessor's copies accumulate while the walk below collects
// them, so that everything belonging at the end of a block is written out at
// once. Indexed by machine block id, which is dense.
typedef struct _EdgeCopies {
  ParallelCopy *copies;
  size_t count;
} EdgeCopies;

// Adds the copies for the edge at position 'edge' into 'block' to whichever
// predecessor that edge comes from.
//
// Edges are identified by position rather than by which block they come from,
// because a predecessor can legitimately appear twice - a branch with both
// arms landing here - and the two entries then need the copies that belong to
// each edge, not two copies of the first one. The assertion is what keeps that
// positional reading honest: every phi has to list the block's predecessors in
// the block's own order.
static void collectEdgeCopies(MachineFunction *mf, const IrBasicBlock *block, size_t edge,
                              EdgeCopies *pending) {
  IrBasicBlock *pred = getBlockFromVector(&block->preds, edge);
  EdgeCopies *bucket = &pending[machineBlockOf(mf, pred)->id];

  for (const IrInstruction *phi = block->instrunctions.head;
       phi != NULL && phi->kind == IR_PHI; phi = phi->next) {
    const Vector *phiBlocks = &phi->info.phi.phiBlocks;
    assert(phiBlocks->size == block->preds.size);
    assert(getBlockFromVector(phiBlocks, edge) == getBlockFromVector(&block->preds, edge));

    const IrInstruction *value = getInstructionFromVector(&phi->inputs, edge);
    ParallelCopy *copy = &bucket->copies[bucket->count++];
    copy->dst = machineVregForValue(mf, phi);
    copy->src = machineVregForValue(mf, value);
    copy->phi = phi;
  }
}

// The copies for one edge belong at the end of the predecessor that edge comes
// from, so this is driven by predecessor: everything one block owes its
// successors is collected first and written out as a single parallel
// assignment.
//
// Ordinarily a block owes exactly one edge's worth, so the grouping changes
// nothing - a critical edge is split, and a block with several successors
// therefore feeds successors with one predecessor each. A computed goto is the
// exception, its edges being the ones splitCriticalEdges() cannot touch, and
// there the copies for every target land in the one block the goto is in.
//
// Which is why they have to be one assignment and not several concatenated.
// Taken together they are correct for whichever way control then goes: a phi's
// register is read only in the block that phi heads, and every edge into that
// block writes it, so the copies belonging to the targets not taken write
// registers that are dead on the path actually taken. What the parallel form
// adds is the other half - one target's destination is never read as another
// target's source, which sequentializing each edge on its own and running the
// results one after another does not guarantee.
//
// Driven off the IR rather than off the machine blocks because that is where
// the phis still are: stage 0 runs before selection, so the machine blocks are
// empty and the copies below are their first instructions. The IR phis are
// left in place - nothing reads them again, and deleting them would only
// invalidate the inputs of whatever still lists them - so instruction
// selection has to skip IR_PHI when it walks a block.
static void destroyPhiNodes(MachineFunction *mf) {
  const size_t numBlocks = mf->numBlocks;
  EdgeCopies *pending = heapAllocate(numBlocks * sizeof(EdgeCopies));
  memset(pending, 0, numBlocks * sizeof(EdgeCopies));

  for (const IrBasicBlock *block = mf->ir->blocks.head; block != NULL; block = block->next) {
    size_t numPhis = countPhis(block);

    for (size_t edge = 0; numPhis != 0 && edge < block->preds.size; ++edge) {
      IrBasicBlock *pred = getBlockFromVector(&block->preds, edge);
      pending[machineBlockOf(mf, pred)->id].count += numPhis;
    }
  }

  for (size_t id = 0; id < numBlocks; ++id) {
    if (pending[id].count != 0) {
      pending[id].copies = heapAllocate(pending[id].count * sizeof(ParallelCopy));
      pending[id].count = 0;
    }
  }

  for (const IrBasicBlock *block = mf->ir->blocks.head; block != NULL; block = block->next) {
    if (countPhis(block) == 0) {
      continue;
    }

    for (size_t edge = 0; edge < block->preds.size; ++edge) {
      collectEdgeCopies(mf, block, edge, pending);
    }
  }

  for (MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    EdgeCopies *bucket = &pending[mbb->id];
    if (bucket->count != 0) {
      sequentializeCopies(mf, mbb, bucket->copies, bucket->count);
      releaseHeap(bucket->copies);
    }
  }

  releaseHeap(pending);
}

// -============================ Frame layout ============================-
//
// What is left in memory once mem2reg has had its way, given a place to live
// relative to the frame pointer:
//
//   [ fp + 16 .. ]  incoming stack arguments   (the caller laid these out)
//   [ fp +  8    ]  return address
//   [ fp +  0    ]  saved frame pointer
//   [ fp -  ..   ]  locals that survived mem2reg
//   [ fp -  ..   ]  spill area                 <- stage 2, size unknown here
//   [ fp -  ..   ]  callee-saved registers     <- stage 3
//
// Only the top is this pass's business. The two below it are sized by
// decisions nobody has made yet - how much the allocator has to spill, and
// which registers a function ends up clobbering - which is exactly why offsets
// are left symbolic as MO_FRAME_IDX operands and only resolved during
// emission. Each of those stages appends to the frame the one before handed
// it, so neither has to know the other's size and their order is a free
// choice.
//
// Everything that reaches this point as an IR_ALLOCA needs memory by
// definition: mem2reg promoted every local it could into a value, so what is
// left is address-taken, an aggregate, or dynamically sized.

static void layoutIncomingParameters(MachineFunction *mf) {
  const IrFunction *f = mf->ir;

  for (size_t idx = 0; idx < f->numOfLocalSlots; ++idx) {
    const LocalValueInfo *lvi = &f->localOperandMap[idx];

    // A parameter that arrived in a register was given a stack slot to be
    // stored home into, so it is an IR_ALLOCA like any other local and is
    // picked up below. One that arrived on the stack is addressed where the
    // caller put it, which the translator spelled as an offset from the frame
    // rather than as an allocation - that is the case being recorded here.
    if (lvi->stackSlot == NULL || lvi->stackSlot->kind == IR_ALLOCA) {
      continue;
    }

    // No declaration is the variadic overflow area: it starts where the named
    // parameters stopped and runs as far as the caller pushed, which is not
    // something this side can know - hence size 0, the same "only known at run
    // time" that MachineFrameObject.size already means.
    TypeRef *type = lvi->declaration != NULL ? lvi->declaration->type : NULL;
    int32_t frameIdx = addMachineFrameObject(
        mf, MFO_INCOMING_PARAM, type != NULL ? computeTypeSize(type) : 0,
        type != NULL ? typeAlignment(type) : sizeof(intptr_t));
    MachineFrameObject *obj = machineFrameObjectAt(mf, frameIdx);
    obj->declaration = lvi->declaration;
    obj->offset = lvi->frameOffset;
    obj->origin = lvi->stackSlot;

    // The IR spells this address as '$rsp + <offset>', and the offset it uses
    // is measured from the *frame* pointer - it is 16 for the first stack
    // argument, which is where rbp+16 is and not where rsp+16 is. Nothing had
    // noticed, because the legacy backend does not read the IR and the new one
    // could not select anything that touched memory.
    //
    // Rather than correct the base and leave selection to add a constant to
    // it, the value is pointed at the frame object just laid out for it, which
    // is the same thing an alloca gets and reaches emission the same way: as
    // one 'lea' off whatever register the frame is actually addressed by.
    putAtVector(&mf->irToFrameIdx, lvi->stackSlot->id, (intptr_t)frameIdx + 1);
  }
}

static void layoutFrame(MachineFunction *mf) {
  layoutIncomingParameters(mf);

  int32_t offset = 0;

  // Collected in program order so that the layout of a function does not
  // depend on how its blocks happen to be linked.
  Vector allocas = {0};
  initVector(&allocas, INITIAL_VECTOR_CAPACITY);

  for (const IrBasicBlock *block = mf->ir->blocks.head; block != NULL; block = block->next) {
    for (const IrInstruction *i = block->instrunctions.head; i != NULL; i = i->next) {
      if (i->kind == IR_ALLOCA) {
        addToVector(&allocas, (intptr_t)i);
        if (i->info.alloca.sizeInstr != NULL) {
          mf->frame.hasDynamicAlloca = TRUE;
        }
      }
    }
  }

  // A dynamically sized allocation moves the stack pointer at run time and
  // nothing here has to remember where it was: everything this frame holds is
  // addressed from the frame pointer, and the epilogue restores rsp from it.
  // The slot the layout used to reserve for the old stack pointer was never
  // written and never read.

  for (size_t idx = 0; idx < allocas.size; ++idx) {
    const IrInstruction *i = (const IrInstruction *)getFromVector(&allocas, idx);
    AstValueDeclaration *v = i->info.alloca.v;

    // An alloca with no declaration behind it is one the translator made for
    // itself - the return slot, a va_list area - and only its byte count is
    // known, so it is aligned as a word like every other anonymous slot.
    uint32_t alignment = v != NULL ? typeAlignment(v->type) : sizeof(intptr_t);
    int32_t frameIdx =
        addMachineFrameObject(mf, MFO_LOCAL, (uint32_t)i->info.alloca.stackSize, alignment);

    MachineFrameObject *obj = machineFrameObjectAt(mf, frameIdx);
    obj->origin = i;
    obj->declaration = v;

    if (i->info.alloca.sizeInstr != NULL) {
      // A VLA or a call to alloca(): the size is a value, computed on the way
      // past. There is nothing to reserve and no fixed displacement to hand
      // out - the address comes from the stack pointer as it stands then - so
      // the object exists only to be something selection can point at.
      obj->isDynamic = TRUE;
      obj->size = 0;
    } else {
      offset = placeMachineFrameObject(mf, offset, frameIdx);
    }

    putAtVector(&mf->irToFrameIdx, i->id, (intptr_t)frameIdx + 1);
  }

  releaseVector(&allocas);

  // The ABI wants the stack pointer 16-byte aligned at a call, and the frame
  // pointer is what the frame size is measured from, so round here rather than
  // leaving stage 3 to remember to.
  mf->frame.size = ALIGN_SIZE(offset, 2 * sizeof(intptr_t));
}

MachineFunction *prepareMachineFunction(IrFunction *f) {
  // Phi destruction puts a copy on an edge, so every edge needs a place of its
  // own to hold one.
  //
  // As the pass pipeline stands this finds nothing to do: gvn's pre() splits
  // every splittable critical edge for its own reasons, and dce - the only
  // pass in between - only deletes unreachable blocks, which drops predecessors
  // and successors but never adds any, so it cannot turn a non-critical edge
  // critical. Measured over all 122 fixtures under test/testData, this splits
  // zero edges today.
  //
  // It runs anyway, because "the CFG has no critical edges" is a precondition
  // of everything below and not something the backend gets to inherit from an
  // *optimization* pass. gvn is optional by definition; a pipeline that skipped
  // it would otherwise quietly produce wrong code rather than slower code.
  splitCriticalEdges(f);
  assertCriticalEdgesSplit(f);

  MachineFunction *mf = buildMachineFunction(f);

  destroyPhiNodes(mf);
  layoutFrame(mf);

  return mf;
}
