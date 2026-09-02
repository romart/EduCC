#include <assert.h>
#include <string.h>

#include "mem.h"
#include "utils.h"
#include "ir/ir.h"
#include "ir/liveness.h"
#include "ir/regalloc.h"

// -============================ Stage 2C: graph colouring =================-
//
// Chaitin-Briggs over the same liveness stage 2B sweeps as intervals, read
// here as live sets instead: an interference graph, simplified by degree with
// potential spills pushed optimistically, coloured on the way back, and
// Briggs-conservative coalescing to delete the copies phi destruction and the
// ABI left behind.
//
// What it brings over the linear scan is precision in both directions. An
// interval is the hull of every position a value is live at, so a value live
// at the top and the bottom of a loop blocks a register through the middle
// where it is not live at all; an interference graph asks the question at each
// program point and gets a smaller answer. And a copy whose two ends do not
// interfere can be given one register rather than two, which is what makes the
// deliberately naive phi destruction of stage 0 cost nothing.
//
// Four things are worth naming, being the places this is easy to get wrong:
//
//   the copy rule        At a copy, the destination does *not* interfere with
//                        the source: they hold the same value, so one register
//                        for both is exactly right, and if the destination is
//                        redefined later while the source is still live, that
//                        later definition is a program point where the edge
//                        appears. Skipping this edge is what leaves anything
//                        to coalesce.
//
//   defs against uses    A definition interferes with what is live *before*
//                        the instruction as well as after it, the copy rule
//                        aside. Two-address form is why: 'add dst, src' would
//                        otherwise be free to put dst and a dying src in one
//                        register and compute dst+dst. This is the same thing
//                        stage 2B gets from expiring intervals strictly.
//
//   physical registers   are not nodes. They are already allocated - selection
//                        puts values in them where the ABI or the ISA says so
//                        - so what a node carries is a mask of the registers
//                        it may not take: everything the shared liveness says
//                        is busy at a position the node is live at, which at a
//                        call is every caller-saved register.
//
//   K is per node        A node's threshold is how many registers it could
//                        actually take, not how many the class has. Simplify's
//                        guarantee - that a node pushed with degree below its
//                        threshold still has a colour when it comes back - is
//                        only true if the threshold counts the constrained
//                        registers out.
//
// Spilling iterates through the shared spiller exactly as stage 2B's does; see
// include/ir/regalloc.h. A register the spiller invented is never nominated as
// a spill candidate and never coalesced, which is what keeps its range one
// instruction long and the whole loop finite.

#define NO_NODE ((uint32_t)-1)

// How many times the graph is rebuilt to let coalescing see its own results.
// A pass merges each node at most once, so a chain of copies needs one pass
// per doubling; eight is far past anything selection builds, and stopping
// early costs a copy rather than correctness.
#define MAX_COALESCE_PASSES 8

// The most virtual registers one instruction can define. Two-address form
// aside, selection builds nothing with more than one destination; the margin
// is for the ones that name a register pair.
#define MAX_DEFS_PER_INSTR 4

// One virtual register, or - once coalescing has merged some - one set of
// them. Everything here except 'alias' is rebuilt from the code each pass, so
// a merge needs no incremental update of degrees or masks: the next build sees
// the aliases and reaches the merged answer directly.
typedef struct _ColourNode {
  uint32_t alias; // union-find parent; itself for a representative

  uint32_t degree; // neighbours of the same class still in the graph
  Vector adj;      // their node indices

  uint64_t forbidden; // physical registers this node may not take
  uint64_t hint;      // ... and the one a copy would like it to take

  // Occurrences, weighted by the loop depth of the block they are in. What
  // the spill heuristic divides by degree.
  uint32_t cost;

  enum RegClass rc;
  uint32_t phys; // NO_REG until a colour is chosen, and after a spill

  Boolean live;    // occurs somewhere, so needs a colour
  Boolean removed; // simplify has pushed it
  Boolean queued;  // ... or is about to
} ColourNode;

typedef struct _GraphColour {
  MachineFunction *mf;
  const TargetDescriptor *target;
  MachineLiveness *lv;
  const SpillState *spill;

  uint64_t allocatable[RC_CLASS_COUNT];

  uint32_t numNodes;
  ColourNode *nodes;

  // Adjacency as a bit matrix as well as as lists: the lists are what
  // simplify walks, and the matrix is what answers "do these two interfere"
  // in constant time, which is the question coalescing asks per copy.
  BitSet edges;

  Vector worklist; // nodes ready to simplify
  Vector stack;    // nodes simplify removed, in the order it removed them

  BitSet spilled; // by (vreg - FIRST_VREG)
  Boolean anySpilled;

  uint32_t *blockWeight; // [lv->numBlocks]
} GraphColour;

static uint32_t nodeOf(uint32_t vreg) {
  assert(isVirtualRegister(vreg));
  return vreg - FIRST_VREG;
}

// -============================ Nodes and edges ============================-

static uint32_t findNode(GraphColour *gc, uint32_t n) {
  while (gc->nodes[n].alias != n) {
    // Path halving, so a chain of coalesced copies does not cost its length
    // every time the build asks.
    gc->nodes[n].alias = gc->nodes[gc->nodes[n].alias].alias;
    n = gc->nodes[n].alias;
  }

  return n;
}

static uint32_t nodeFor(GraphColour *gc, uint32_t vreg) {
  return findNode(gc, nodeOf(vreg));
}

static Boolean interferes(const GraphColour *gc, uint32_t a, uint32_t b) {
  return getBit(&gc->edges, (size_t)a * gc->numNodes + b) ? TRUE : FALSE;
}

static void addEdge(GraphColour *gc, uint32_t a, uint32_t b) {
  if (a == b) {
    return;
  }

  // Two classes are two register files. A general-purpose value and a
  // floating-point one are never candidates for the same register, so an edge
  // between them would only inflate a degree the threshold is measured
  // against.
  if (gc->nodes[a].rc != gc->nodes[b].rc) {
    return;
  }

  if (interferes(gc, a, b)) {
    return;
  }

  setBit(&gc->edges, (size_t)a * gc->numNodes + b);
  setBit(&gc->edges, (size_t)b * gc->numNodes + a);

  addToVector(&gc->nodes[a].adj, (intptr_t)b);
  addToVector(&gc->nodes[b].adj, (intptr_t)a);

  gc->nodes[a].degree += 1;
  gc->nodes[b].degree += 1;
}

// How many registers this node could take at all. Simplify's guarantee rests
// on this rather than on the size of the class: a node whose every use is next
// to a call has far fewer registers available than the class has, and calling
// it low-degree because the class is large is how an optimistic push turns
// into an uncolourable node.
static uint32_t availableCount(const GraphColour *gc, uint32_t n) {
  const ColourNode *node = &gc->nodes[n];
  uint64_t mask = gc->allocatable[node->rc] & ~node->forbidden;
  uint32_t count = 0;

  while (mask != 0) {
    mask &= mask - 1;
    count += 1;
  }

  return count;
}

// -============================ Loop weights ============================-

// Blocks the layout puts inside a loop, approximately: an edge back to a block
// at or before this one in layout order is a back edge, and everything between
// its two ends is the body. Approximate on purpose - the machine CFG carries
// no loop forest, and the answer is only ever divided by a degree to rank
// spill candidates against each other.
static void computeBlockWeights(GraphColour *gc) {
  MachineLiveness *lv = gc->lv;
  const size_t numBlocks = lv->numBlocks;

  gc->blockWeight = heapAllocate(sizeof(uint32_t) * (numBlocks ? numBlocks : 1));

  if (numBlocks == 0) {
    return;
  }

  uint32_t maxId = 0;
  for (size_t idx = 0; idx < numBlocks; ++idx) {
    if (lv->blockAt[idx]->id > maxId) {
      maxId = lv->blockAt[idx]->id;
    }
  }

  int32_t *indexOf = heapAllocate(sizeof(int32_t) * (maxId + 1));
  uint32_t *depth = heapAllocate(sizeof(uint32_t) * numBlocks);

  for (size_t idx = 0; idx < numBlocks; ++idx) {
    indexOf[lv->blockAt[idx]->id] = (int32_t)idx;
    depth[idx] = 0;
  }

  for (size_t idx = 0; idx < numBlocks; ++idx) {
    const MachineBasicBlock *mbb = lv->blockAt[idx];

    for (size_t p = 0; p < mbb->preds.size; ++p) {
      const MachineBasicBlock *pred = (const MachineBasicBlock *)getFromVector(&mbb->preds, p);
      const size_t latch = (size_t)indexOf[pred->id];

      if (latch < idx) {
        continue;
      }

      for (size_t k = idx; k <= latch; ++k) {
        depth[k] += 1;
      }
    }
  }

  for (size_t idx = 0; idx < numBlocks; ++idx) {
    const uint32_t d = depth[idx] < 6 ? depth[idx] : 6;
    gc->blockWeight[idx] = (uint32_t)1 << (3 * d);
  }

  releaseHeap(indexOf);
  releaseHeap(depth);
}

// -============================ Building the graph =========================-

// A copy between a virtual register and a physical one, when the physical end
// is the destination and is written whole. What it buys is two things at once:
// the physical register is not counted busy against this node at this one
// position - it is busy holding this very value - and it becomes the colour
// the node is offered first. Get both and the copy disappears.
//
// A partial definition is left out deliberately: it reads the register it
// writes, so the bytes above what it writes belong to whatever was there, and
// this node is not what was there.
static void copyExemption(GraphColour *gc, MachineInstr *mi, uint32_t *node, uint64_t *mask) {
  *node = NO_NODE;
  *mask = 0;

  if (mi->opcode != MOP_COPY || mi->numOperands != 2) {
    return;
  }

  const MachineOperand *dst = &mi->operands[0];
  const MachineOperand *src = &mi->operands[1];

  if (dst->kind != MO_REG || src->kind != MO_REG) {
    return;
  }

  if (machineOperandIsRead(dst)) {
    return;
  }

  if (isVirtualRegister(dst->info.reg) && isPhysicalRegister(src->info.reg)) {
    *node = nodeFor(gc, dst->info.reg);
    *mask = (uint64_t)1 << src->info.reg;
  } else if (isPhysicalRegister(dst->info.reg) && isVirtualRegister(src->info.reg)) {
    *node = nodeFor(gc, src->info.reg);
    *mask = (uint64_t)1 << dst->info.reg;
  }
}

// The virtual source of a copy, whose edge with the destination is the one
// Chaitin's rule leaves out. NO_NODE for anything else.
static uint32_t copySource(GraphColour *gc, MachineInstr *mi) {
  if (mi->opcode != MOP_COPY || mi->numOperands != 2) {
    return NO_NODE;
  }

  const MachineOperand *src = &mi->operands[1];

  if (src->kind != MO_REG || !isVirtualRegister(src->info.reg)) {
    return NO_NODE;
  }

  return nodeFor(gc, src->info.reg);
}

// One walk over what is live at a position, doing both jobs it is needed for:
// every physical register busy here is one none of these nodes may take, and
// every one of them interferes with everything this instruction defines.
// What one node takes from one position: every physical register busy here is
// one it may not have, and it interferes with everything defined here.
static void connectOne(GraphColour *gc, uint32_t n, uint64_t busy, uint32_t exempt,
                       uint64_t exemptMask, const uint32_t *defs, uint16_t numDefs,
                       uint32_t copySrc) {
  gc->nodes[n].forbidden |= n == exempt ? busy & ~exemptMask : busy;

  if (n == copySrc) {
    return;
  }

  for (uint16_t idx = 0; idx < numDefs; ++idx) {
    addEdge(gc, defs[idx], n);
  }
}

static void connectLiveSet(GraphColour *gc, const BitSet *live, uint64_t busy, uint32_t exempt,
                           uint64_t exemptMask, const uint32_t *defs, uint16_t numDefs,
                           uint32_t copySrc) {
  // Nothing to say about this position: no register is busy at it and it
  // defines nothing for anything to interfere with. Worth asking, because
  // walking the live set is what this pass spends its time on and most
  // instructions in most functions are this.
  if (busy == 0 && numDefs == 0) {
    return;
  }

  const size_t physBase = gc->mf->target->numPhysRegs;

  for (size_t bit = nextSetBit(live, physBase); bit < live->size;
       bit = nextSetBit(live, bit + 1)) {
    connectOne(gc, findNode(gc, (uint32_t)(bit - physBase)), busy, exempt, exemptMask, defs,
               numDefs, copySrc);
  }
}

// And what is live *before* the instruction, which is what makes a definition
// interfere with a dying use - see the note at the top of the file. The set
// live before differs from the set live after only by this instruction's own
// defs and uses, and the defs are the very things being connected, so walking
// the read operands says everything a second scan of the whole set would.
static void connectReads(GraphColour *gc, MachineInstr *mi, uint64_t busy, uint32_t exempt,
                         uint64_t exemptMask, const uint32_t *defs, uint16_t numDefs,
                         uint32_t copySrc) {
  if (busy == 0 && numDefs == 0) {
    return;
  }

  for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
    MachineOperand *op = &mi->operands[idx];

    if (!machineOperandIsRead(op)) {
      continue;
    }

    uint32_t *regs[MAX_OPERAND_REGS];
    uint16_t numRegs = machineOperandRegisters(op, regs);

    for (uint16_t r = 0; r < numRegs; ++r) {
      if (isVirtualRegister(*regs[r])) {
        connectOne(gc, nodeFor(gc, *regs[r]), busy, exempt, exemptMask, defs, numDefs, copySrc);
      }
    }
  }
}

static void resetGraph(GraphColour *gc) {
  clearAll(&gc->edges);

  for (uint32_t n = 0; n < gc->numNodes; ++n) {
    ColourNode *node = &gc->nodes[n];

    node->degree = 0;
    node->forbidden = 0;
    node->hint = 0;
    node->cost = 0;
    node->phys = NO_REG;
    node->removed = FALSE;
    node->queued = FALSE;
    clearVector(&node->adj);
  }
}

static void addCost(ColourNode *node, uint32_t weight) {
  // Saturating: a value used inside six nested loops is already "do not spill
  // this", and wrapping round to zero would say the opposite.
  const uint32_t sum = node->cost + weight;
  node->cost = sum < node->cost ? (uint32_t)-1 : sum;
}

static void buildInterference(GraphColour *gc) {
  MachineFunction *mf = gc->mf;
  MachineLiveness *lv = gc->lv;

  resetGraph(gc);

  BitSet live;
  initBitSet(&live, lv->setSize);

  for (size_t b = 0; b < lv->numBlocks; ++b) {
    MachineBasicBlock *mbb = lv->blockAt[b];

    if (mbb->instructions.head == NULL) {
      continue;
    }

    copyBitSet(&lv->blockLiveOut[b], &live);

    const uint32_t weight = gc->blockWeight[b];
    uint32_t p = lv->blockLast[b];

    for (MachineInstr *mi = mbb->instructions.tail; mi != NULL; mi = mi->prev, --p) {
      const uint64_t busy = lv->physBusy[p];

      uint32_t exempt = NO_NODE;
      uint64_t exemptMask = 0;
      copyExemption(gc, mi, &exempt, &exemptMask);

      if (exempt != NO_NODE) {
        gc->nodes[exempt].hint |= exemptMask;
      }

      const uint32_t copySrc = copySource(gc, mi);

      uint32_t defs[MAX_DEFS_PER_INSTR];
      uint16_t numDefs = 0;

      for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
        MachineOperand *op = &mi->operands[idx];
        uint32_t *regs[MAX_OPERAND_REGS];
        uint16_t numRegs = machineOperandRegisters(op, regs);

        for (uint16_t r = 0; r < numRegs; ++r) {
          if (!isVirtualRegister(*regs[r])) {
            continue;
          }

          const uint32_t n = nodeFor(gc, *regs[r]);
          addCost(&gc->nodes[n], weight);

          if (!machineOperandIsWritten(op)) {
            continue;
          }

          // A definition is constrained by this position whether or not it is
          // live afterwards: a dead one still needs somewhere to be written.
          gc->nodes[n].forbidden |= n == exempt ? busy & ~exemptMask : busy;

          Boolean seen = FALSE;
          for (uint16_t d = 0; d < numDefs; ++d) {
            seen = defs[d] == n ? TRUE : seen;
          }

          if (!seen) {
            assert(numDefs < MAX_DEFS_PER_INSTR);
            defs[numDefs++] = n;
          }
        }
      }

      // Two definitions of one instruction are simultaneously live by
      // construction, whatever liveness says about either of them afterwards.
      for (uint16_t a = 0; a < numDefs; ++a) {
        for (uint16_t c = (uint16_t)(a + 1); c < numDefs; ++c) {
          addEdge(gc, defs[a], defs[c]);
        }
      }

      connectLiveSet(gc, &live, busy, exempt, exemptMask, defs, numDefs, copySrc);
      connectReads(gc, mi, busy, exempt, exemptMask, defs, numDefs, copySrc);

      machineLivenessTransfer(mf, mi, &live);
    }
  }

  releaseBitSet(&live);
}

// -============================ Coalescing ============================-

// Briggs' conservative test: the merged node is safe to make if fewer than K
// of its neighbours have significant degree, K being how many registers the
// merged node could take. Every such neighbour is one that might not simplify
// on its own; fewer than K of them and the merged node still will.
static Boolean briggsSafe(GraphColour *gc, uint32_t u, uint32_t v, BitSet *seen) {
  const uint64_t merged = gc->allocatable[gc->nodes[u].rc] &
                          ~(gc->nodes[u].forbidden | gc->nodes[v].forbidden);

  uint32_t k = 0;
  uint64_t mask = merged;
  while (mask != 0) {
    mask &= mask - 1;
    k += 1;
  }

  if (k == 0) {
    return FALSE;
  }

  clearAll(seen);

  uint32_t significant = 0;
  const uint32_t ends[2] = {u, v};

  for (size_t e = 0; e < 2; ++e) {
    const Vector *adj = &gc->nodes[ends[e]].adj;

    for (size_t idx = 0; idx < adj->size; ++idx) {
      const uint32_t n = (uint32_t)getFromVector(adj, (int)idx);

      if (n == u || n == v || getBit(seen, n)) {
        continue;
      }

      setBit(seen, n);

      if (gc->nodes[n].degree >= availableCount(gc, n)) {
        significant += 1;
      }
    }
  }

  return significant < k ? TRUE : FALSE;
}

// One pass over every copy in the function. A node takes part in at most one
// merge per pass, because the graph is not updated as merges are made - the
// caller rebuilds it and comes round again, which is cheaper to be right about
// than an incremental degree update.
static Boolean coalescePass(GraphColour *gc) {
  Boolean any = FALSE;

  BitSet touched;
  BitSet seen;
  initBitSet(&touched, gc->numNodes);
  initBitSet(&seen, gc->numNodes);

  for (MachineBasicBlock *mbb = gc->mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (MachineInstr *mi = mbb->instructions.head; mi != NULL; mi = mi->next) {
      if (mi->opcode != MOP_COPY || mi->numOperands != 2) {
        continue;
      }

      const MachineOperand *dst = &mi->operands[0];
      const MachineOperand *src = &mi->operands[1];

      if (dst->kind != MO_REG || src->kind != MO_REG) {
        continue;
      }

      if (!isVirtualRegister(dst->info.reg) || !isVirtualRegister(src->info.reg)) {
        continue;
      }

      // A register the spiller invented is left alone: merging one into a
      // longer-lived node is how it would end up nominated for a spill of its
      // own, and the argument that this whole loop terminates is exactly that
      // it never is.
      if (isSpillerVreg(gc->spill, dst->info.reg) || isSpillerVreg(gc->spill, src->info.reg)) {
        continue;
      }

      const VRegInfo *dstInfo = virtualRegisterInfo(gc->mf, dst->info.reg);
      const VRegInfo *srcInfo = virtualRegisterInfo(gc->mf, src->info.reg);

      // Only equal widths. A narrower destination sharing a register with a
      // wider source would be right about the value and wrong about the slot
      // if either of them were later spilled.
      if (dstInfo->size != srcInfo->size || dstInfo->rc != srcInfo->rc) {
        continue;
      }

      const uint32_t u = nodeFor(gc, dst->info.reg);
      const uint32_t v = nodeFor(gc, src->info.reg);

      if (u == v || interferes(gc, u, v)) {
        continue;
      }

      if (getBit(&touched, u) || getBit(&touched, v)) {
        continue;
      }

      if (!briggsSafe(gc, u, v, &seen)) {
        continue;
      }

      gc->nodes[v].alias = u;
      setBit(&touched, u);
      setBit(&touched, v);
      any = TRUE;
    }
  }

  releaseBitSet(&touched);
  releaseBitSet(&seen);

  return any;
}

// -============================ Simplify ============================-

static Boolean isRepresentative(GraphColour *gc, uint32_t n) {
  return gc->nodes[n].alias == n && gc->nodes[n].live ? TRUE : FALSE;
}

static void enqueue(GraphColour *gc, uint32_t n) {
  if (gc->nodes[n].queued || gc->nodes[n].removed) {
    return;
  }

  gc->nodes[n].queued = TRUE;
  addToVector(&gc->worklist, (intptr_t)n);
}

static void removeNode(GraphColour *gc, uint32_t n) {
  ColourNode *node = &gc->nodes[n];

  node->removed = TRUE;
  pushToStack(&gc->stack, (intptr_t)n);

  for (size_t idx = 0; idx < node->adj.size; ++idx) {
    const uint32_t m = (uint32_t)getFromVector(&node->adj, (int)idx);
    ColourNode *other = &gc->nodes[m];

    if (other->removed) {
      continue;
    }

    assert(other->degree > 0);
    other->degree -= 1;

    if (other->degree < availableCount(gc, m)) {
      enqueue(gc, m);
    }
  }
}

// The node to give up when nothing is trivially colourable: the one whose
// register buys the least, which is cost per unit of degree. Comparing the
// two ratios by cross-multiplication keeps it in integers.
static uint32_t pickSpillCandidate(GraphColour *gc) {
  uint32_t best = NO_NODE;

  for (uint32_t n = 0; n < gc->numNodes; ++n) {
    if (!isRepresentative(gc, n) || gc->nodes[n].removed) {
      continue;
    }

    if (isSpillerVreg(gc->spill, FIRST_VREG + n)) {
      continue;
    }

    if (best == NO_NODE) {
      best = n;
      continue;
    }

    const uint64_t lhs = (uint64_t)gc->nodes[n].cost * (gc->nodes[best].degree + 1);
    const uint64_t rhs = (uint64_t)gc->nodes[best].cost * (gc->nodes[n].degree + 1);

    if (lhs < rhs) {
      best = n;
    }
  }

  return best;
}

static void simplifyGraph(GraphColour *gc) {
  size_t remaining = 0;

  for (uint32_t n = 0; n < gc->numNodes; ++n) {
    if (!isRepresentative(gc, n)) {
      continue;
    }

    remaining += 1;

    if (gc->nodes[n].degree < availableCount(gc, n)) {
      enqueue(gc, n);
    }
  }

  while (remaining > 0) {
    if (gc->worklist.size != 0) {
      const uint32_t n = (uint32_t)popFromStack(&gc->worklist);
      gc->nodes[n].queued = FALSE;

      if (gc->nodes[n].removed) {
        continue;
      }

      removeNode(gc, n);
      remaining -= 1;
      continue;
    }

    // Nothing is trivially colourable. One node goes on the stack anyway, in
    // the hope that its neighbours do not use up every colour between them -
    // which is Briggs' optimism, and the reason a spill is decided when the
    // stack comes back rather than here.
    const uint32_t victim = pickSpillCandidate(gc);

    if (victim == NO_NODE) {
      unreachable("every node left in the graph is a reload the spiller created");
    }

    removeNode(gc, victim);
    remaining -= 1;
  }
}

// -============================ Select ============================-

static uint32_t pickColour(GraphColour *gc, uint32_t n, uint64_t candidates) {
  const ColourNode *node = &gc->nodes[n];
  const uint64_t hinted = candidates & node->hint;

  // A hinted register is one some copy names at the other end, so taking it
  // turns that copy into a move from a register to itself, which rewriting
  // then deletes.
  const uint64_t wanted = hinted != 0 ? hinted : candidates;

  for (uint32_t idx = 0; idx < gc->target->allocatableRegCount[node->rc]; ++idx) {
    const uint32_t reg = gc->target->allocatableRegs[node->rc][idx];

    if (wanted & ((uint64_t)1 << reg)) {
      return reg;
    }
  }

  unreachable("a register in the allocatable mask is not in the allocatable list");
}

static void markSpilled(GraphColour *gc, uint32_t n) {
  assert(!isSpillerVreg(gc->spill, FIRST_VREG + n) &&
         "a one-instruction reload could not be coloured");

  // The node may be several registers by now. All of them go to memory: the
  // copy that made them one is still there, and the next round allocates the
  // rewritten function from scratch.
  for (uint32_t m = 0; m < gc->numNodes; ++m) {
    if (gc->nodes[m].live && findNode(gc, m) == n) {
      setBit(&gc->spilled, m);
    }
  }

  gc->anySpilled = TRUE;
}

static void assignColours(GraphColour *gc) {
  while (gc->stack.size != 0) {
    const uint32_t n = (uint32_t)popFromStack(&gc->stack);
    ColourNode *node = &gc->nodes[n];

    uint64_t used = 0;

    for (size_t idx = 0; idx < node->adj.size; ++idx) {
      const uint32_t m = (uint32_t)getFromVector(&node->adj, (int)idx);

      if (gc->nodes[m].phys != NO_REG) {
        used |= (uint64_t)1 << gc->nodes[m].phys;
      }
    }

    const uint64_t candidates = gc->allocatable[node->rc] & ~node->forbidden & ~used;

    if (candidates == 0) {
      markSpilled(gc, n);
      continue;
    }

    node->phys = pickColour(gc, n, candidates);
  }
}

// -============================ Rewriting ============================-

static Boolean isRedundantCopy(const MachineInstr *mi) {
  if (mi->opcode != MOP_COPY) {
    return FALSE;
  }

  assert(mi->numOperands == 2);
  return mi->operands[0].kind == MO_REG && mi->operands[1].kind == MO_REG &&
                 mi->operands[0].info.reg == mi->operands[1].info.reg
             ? TRUE
             : FALSE;
}

static void rewriteOperands(GraphColour *gc) {
  for (MachineBasicBlock *mbb = gc->mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    MachineInstr *mi = mbb->instructions.head;

    while (mi != NULL) {
      MachineInstr *next = mi->next;

      for (uint16_t idx = 0; idx < mi->numOperands; ++idx) {
        uint32_t *regs[MAX_OPERAND_REGS];
        uint16_t numRegs = machineOperandRegisters(&mi->operands[idx], regs);

        for (uint16_t r = 0; r < numRegs; ++r) {
          if (!isVirtualRegister(*regs[r])) {
            continue;
          }

          const uint32_t n = nodeFor(gc, *regs[r]);
          assert(gc->nodes[n].phys != NO_REG &&
                 "a virtual register reached rewriting without a colour");
          *regs[r] = gc->nodes[n].phys;
        }
      }

      // Coalescing is what makes most of these, and a copy the ABI or phi
      // destruction left behind that happens to land in one register makes the
      // rest.
      if (isRedundantCopy(mi)) {
        eraseMachineInstr(mi);
      }

      mi = next;
    }
  }
}

// -============================ The driver ============================-

static void initGraphColour(GraphColour *gc, MachineFunction *mf, MachineLiveness *lv,
                            const SpillState *spill) {
  memset(gc, 0, sizeof *gc);

  gc->mf = mf;
  gc->target = mf->target;
  gc->lv = lv;
  gc->spill = spill;
  gc->numNodes = lv->numVregs;

  for (size_t rc = 0; rc < RC_CLASS_COUNT; ++rc) {
    for (uint32_t idx = 0; idx < mf->target->allocatableRegCount[rc]; ++idx) {
      gc->allocatable[rc] |= (uint64_t)1 << mf->target->allocatableRegs[rc][idx];
    }
  }

  gc->nodes = heapAllocate(sizeof(ColourNode) * (gc->numNodes ? gc->numNodes : 1));

  for (uint32_t n = 0; n < gc->numNodes; ++n) {
    ColourNode *node = &gc->nodes[n];

    memset(node, 0, sizeof *node);
    node->alias = n;
    node->phys = NO_REG;
    node->rc = machineRegisterClass(mf, FIRST_VREG + n);
    node->live = machineLivenessIntervalFor(lv, FIRST_VREG + n) != NULL ? TRUE : FALSE;
    initVector(&node->adj, 8);
  }

  initBitSet(&gc->edges, (size_t)gc->numNodes * gc->numNodes + 1);
  initBitSet(&gc->spilled, gc->numNodes ? gc->numNodes : 1);
  initVector(&gc->worklist, INITIAL_VECTOR_CAPACITY);
  initVector(&gc->stack, INITIAL_VECTOR_CAPACITY);

  computeBlockWeights(gc);
}

static void releaseGraphColour(GraphColour *gc) {
  for (uint32_t n = 0; n < gc->numNodes; ++n) {
    releaseVector(&gc->nodes[n].adj);
  }

  releaseHeap(gc->nodes);
  releaseHeap(gc->blockWeight);
  releaseBitSet(&gc->edges);
  releaseBitSet(&gc->spilled);
  releaseVector(&gc->worklist);
  releaseVector(&gc->stack);
}

void allocateRegistersColour(MachineFunction *mf) {
  assert(mf->target->allocatableRegCount[RC_GP] != 0 &&
         "this target names no registers stage 2C may hand out");

  SpillState spill;
  initSpillState(&spill, mf);

  // Bounded rather than trusted, as in stage 2B: a round that spills takes at
  // least one original register out of circulation for good, and the registers
  // this creates instead are never spilled.
  const size_t maxRounds = mf->vregs.size + 2;

  for (size_t round = 0;; ++round) {
    if (round >= maxRounds) {
      unreachable("graph colouring did not reach a fixed point");
    }

    MachineLiveness lv;
    computeMachineLiveness(mf, &lv);

    GraphColour gc;
    initGraphColour(&gc, mf, &lv, &spill);

    // Build, coalesce, and build again so that the next round of coalescing
    // sees the merged graph rather than the one that suggested the merges.
    for (size_t pass = 0;; ++pass) {
      buildInterference(&gc);

      if (pass >= MAX_COALESCE_PASSES || !coalescePass(&gc)) {
        break;
      }
    }

    simplifyGraph(&gc);
    assignColours(&gc);

    if (!gc.anySpilled) {
      rewriteOperands(&gc);
      releaseGraphColour(&gc);
      releaseMachineLiveness(&lv);
      break;
    }

    insertSpillCode(&spill, &gc.spilled);

    releaseGraphColour(&gc);
    releaseMachineLiveness(&lv);
  }

  finishSpillFrame(&spill);
  releaseSpillState(&spill);

  mf->allocator = "graph colouring";
}
