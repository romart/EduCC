

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "mem.h"
#include "ir/ir.h"

extern IrContext *ctx;

// Dominator-based global value numbering (Simpson/Briggs-style DVNT).
//
// The dominator tree is walked in preorder carrying a scoped hash table of
// expression -> value number. A redundant pure instruction is therefore
// always replaced by a leader that dominates it by construction; entries
// added while processing a block are popped once its dominator subtree is
// done, so a computation in one branch can never shadow (or be "reused" by)
// a sibling branch.
//
// Only pure instructions - those with no side effects whose value is fully
// determined by (kind, result type, input VNs, extras) - participate in
// value numbering. Everything else (loads, stores, calls, allocas, phis,
// control flow) is assigned a fresh unique VN: loads are never merged since
// there is no alias analysis yet, and side-effecting instructions must
// never be merged at all.
//
// A second stage (pre(), below) then catches the redundancies dominance
// cannot express: values (re)computed at a merge point that are available
// along all - or, with edge insertion, some - of the incoming paths.

static Boolean isPureInstr(const IrInstruction *i) {
  switch (i->kind) {
  case IR_E_ADD:
  case IR_E_SUB:
  case IR_E_MUL:
  case IR_E_DIV:
  case IR_E_MOD:
  case IR_E_SHL:
  case IR_E_SHR:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_CMP:
  case IR_E_FADD:
  case IR_E_FSUB:
  case IR_E_FMUL:
  case IR_E_FDIV:
  case IR_E_FMOD:
  case IR_E_FCMP:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
  case IR_U_NOT:
  case IR_U_BNOT:
  case IR_E_BITCAST:
  case IR_GET_ELEMENT_PTR:
  case IR_DEF_CONST:
  case IR_P_REG:
    return TRUE;
  default:
    return FALSE;
  }
}

static size_t computeExtrasCount(const IrInstruction *instr) {
  switch (instr->kind) {
  case IR_DEF_CONST:
  case IR_P_REG:
    return 1;
  default:
    return 0;
  }
}

static Boolean isCommutativeInstr(enum IrIntructionKind kind) {
  switch (kind) {
  case IR_E_ADD:
  case IR_E_MUL:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_FADD:
  case IR_E_FMUL:
  case IR_E_FEQ:
  case IR_E_FNE:
    return TRUE;
  default: return FALSE;
  }
}

typedef struct {
  enum IrIntructionKind kind;
  enum IrTypeKind type;
  size_t numOfInputs;
  size_t numOfExtras;
  uint32_t *inputs;
  uint32_t *extras;
} GVNExpression;


static int gvn_hash(intptr_t ptr) {
  const GVNExpression *gvne = (const GVNExpression *)ptr;

  int h = (intptr_t) gvne->kind;

  h *= 31;
  h += (intptr_t) gvne->type;

  h *= 31;
  h += gvne->numOfInputs;

  for (size_t i = 0; i < gvne->numOfInputs; ++i) {
    h *= 31;
    h += gvne->inputs[i];
  }

  h *= 31;
  h += gvne->numOfExtras;

  for (size_t i = 0; i < gvne->numOfExtras; ++i) {
    h *= 31;
    h += gvne->extras[i];
  }

  return h;
}

static int gvn_cmp(intptr_t a, intptr_t b) {
  const GVNExpression *lhs = (const GVNExpression *)a;
  const GVNExpression *rhs = (const GVNExpression *)b;

  if (rhs->kind != lhs->kind) {
    return rhs->kind - lhs->kind;
  }

  if (rhs->type != lhs->type) {
    return rhs->type - lhs->type;
  }

  // Call could have different number of inputs
  if (rhs->numOfInputs != lhs->numOfInputs) {
    return rhs->numOfInputs - lhs->numOfInputs;
  }

  int icmp = memcmp(rhs->inputs, lhs->inputs, rhs->numOfInputs * sizeof (uint32_t));

  if (icmp != 0) {
    return icmp;
  }

  if (rhs->numOfExtras != lhs->numOfExtras) {
    return rhs->numOfExtras - lhs->numOfExtras;
  }

  return memcmp(rhs->extras, lhs->extras, rhs->numOfExtras * sizeof (uint32_t));
}


typedef struct {
  HashMap *table;    // GVNExpression* -> VN
  Vector exprMap;    // VN -> leader IrInstruction*
  size_t liveExprs;  // number of keys currently held by 'table'
  Arena *arena;
} VNTable;

static void initVNTable(VNTable *vnt) {
  // initVector() insists on being handed a vector that has no storage yet,
  // so the whole table starts from a clean slate rather than from whatever
  // the caller's stack happened to hold.
  memset(vnt, 0, sizeof (VNTable));

  vnt->arena = createArena("GVN Arena", DEFAULT_CHUNCK_SIZE);
  vnt->table = createHashMap(DEFAULT_MAP_CAPACITY, &gvn_hash, &gvn_cmp);
  vnt->liveExprs = 0;
  initVector(&vnt->exprMap, ctx->instrCnt);
}

// Both stages number from scratch, but they can share the storage: the
// expression arena and the table/vector buffers are kept, only the mapping
// is dropped. The scoped walk removes every key it inserts as it unwinds,
// so the table is already empty by the time the second stage starts.
static void resetVNTable(VNTable *vnt) {
  assert(vnt->liveExprs == 0);
  clearVector(&vnt->exprMap);
}

static void releaseVNTable(VNTable *vnt) {
  releaseHashMap(vnt->table);
  releaseVector(&vnt->exprMap);
  releaseArena(vnt->arena);
}

static int vn_cmp(const void *ap, const void *bp) {
  uint32_t a = *(uint32_t *)ap;
  uint32_t b = *(uint32_t *)bp;
  return a < b ? 1 : a > b ? -1 : 0;
}

static void fillExtras(GVNExpression *gvne, const IrInstruction *i) {
  switch (i->kind) {
  case IR_DEF_CONST:
    gvne->extras[0] = i->info.constant.cacheIdx;
    break;
  case IR_P_REG:
    gvne->extras[0] = i->info.physReg;
    break;
  default:
    return;
  }
}

static GVNExpression *createGVNExpression(VNTable *vnt, const IrInstruction *i) {
  size_t inputs = i->inputs.size;
  size_t extras = computeExtrasCount(i);
  GVNExpression *gvne = areanAllocate(vnt->arena, sizeof (GVNExpression) + ((inputs + extras) * sizeof (uint32_t)));
  gvne->kind = i->kind;
  gvne->type = i->type;

  uint32_t *ptr = (uint32_t *)&gvne[1];
  gvne->numOfInputs = inputs;
  gvne->inputs = inputs != 0 ? ptr : NULL;

  ptr += inputs;
  gvne->numOfExtras = extras;
  gvne->extras = extras != 0 ? ptr : NULL;

  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    // Defs dominate uses in SSA, so every input has been numbered already.
    assert(input->algoIdx != (uint32_t)-1);
    gvne->inputs[idx] = input->algoIdx;
  }

  if (isCommutativeInstr(i->kind)) {
    qsort(gvne->inputs, gvne->numOfInputs, sizeof (uint32_t), &vn_cmp);
  }

  fillExtras(gvne, i);

  return gvne;
}

static uint32_t assignUniqueVN(VNTable *vnt, IrInstruction *i) {
  uint32_t vn = vnt->exprMap.size;
  addInstructionToVector(&vnt->exprMap, i);
  return vn;
}

// The value numbering step shared by both stages: gives 'instr' its value
// number and records it in instr->algoIdx. Impure instructions always get a
// fresh number of their own; a pure one gets the number of an equal
// expression already in the table, or a fresh one if it is the first of its
// kind - in which case it becomes that number's leader and, when 'key' is
// requested, the inserted expression is handed back so the caller can scope
// it out later.
static uint32_t valueNumberInstruction(VNTable *vnt, IrInstruction *instr, GVNExpression **key) {
  if (key != NULL) {
    *key = NULL;
  }

  if (!isPureInstr(instr)) {
    return instr->algoIdx = assignUniqueVN(vnt, instr);
  }

  GVNExpression *gvne = createGVNExpression(vnt, instr);
  uint32_t newVN = vnt->exprMap.size;
  uint32_t vn = putIfNotExistsToHashMap(vnt->table, (intptr_t)gvne, newVN);

  if (vn == newVN) {
    addInstructionToVector(&vnt->exprMap, instr);
    vnt->liveExprs += 1;
    if (key != NULL) {
      *key = gvne;
    }
  }

  return instr->algoIdx = vn;
}

static IrInstruction *leaderOf(const VNTable *vnt, uint32_t vn) {
  return getInstructionFromVector(&vnt->exprMap, vn);
}

static void poisonAlgoIdx(IrFunction *func) {
  // Unreachable blocks are not visited by the walks; poison every algoIdx
  // so a stale value can never be mistaken for a valid VN.
  for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
    for (IrInstruction *i = bb->instrunctions.head; i != NULL; i = i->next) {
      i->algoIdx = (uint32_t)-1;
    }
  }
}

static void gvnBlock(VNTable *vnt, IrBasicBlock *block) {
  // Expressions first defined in this block; popped from the table when its
  // dominator subtree is done so they are not visible to non-dominated blocks.
  Vector scopeKeys = { 0 };
  initVector(&scopeKeys, INITIAL_VECTOR_CAPACITY);

  for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
    GVNExpression *gvne = NULL;
    uint32_t vn = valueNumberInstruction(vnt, instr, &gvne);

    if (gvne != NULL) {
      // First occurrence along this dominator path - 'instr' is the leader,
      // and its expression stays visible only within this subtree.
      addToVector(&scopeKeys, (intptr_t)gvne);
      continue;
    }

    IrInstruction *leader = leaderOf(vnt, vn);
    if (leader != instr) {
      replaceUsageWith(instr, leader); // the dead instruction is left for dce
    }
  }

  Vector *dominatees = &block->dominators.dominatees;
  for (size_t idx = 0; idx < dominatees->size; ++idx) {
    gvnBlock(vnt, getBlockFromVector(dominatees, idx));
  }

  for (size_t idx = 0; idx < scopeKeys.size; ++idx) {
    removeFromHashMap(vnt->table, scopeKeys.storage[idx]);
  }
  vnt->liveExprs -= scopeKeys.size;

  releaseVector(&scopeKeys);
}

// -============================ PRE stage ============================-
//
// Partial redundancy elimination for merge points. The dominator walk
// above cannot fold a computation at a join whose value is produced in
// the joining branches (neither branch dominates the join). This stage
// runs a *global* (unscoped) value numbering in reverse post order while
// tracking, per block, which instruction carries each value number at the
// block's exit (the AvailabilityMap). At a redundant instruction it then either
//  - reuses a single representative that dominates the block, or
//  - builds a phi over the per-predecessor representatives, cloning the
//    computation into predecessors that miss it (classic PRE insertion).
// Critical edges are split up front so an inserted clone executes exactly
// when its edge is taken - never speculatively (which would be wrong for
// trapping ops like division and wasteful for everything else).
//
// Loops are handled conservatively: a predecessor not yet processed in
// RPO is a back edge, and no availability is assumed across it. In-loop
// values available via dominance were already folded by the walk above;
// hoisting genuinely loop-carried redundancies needs phi-translation
// (full GVN-PRE anticipation), which is future work.

static Boolean dominates(const IrBasicBlock *dom, const IrBasicBlock *block) {
  const IrBasicBlock *cur = block;
  while (cur != NULL) {
    if (cur == dom)
      return TRUE;

    cur = cur->dominators.sdom;
  }

  return FALSE;
}

static void computePostOrderImpl(IrBasicBlock *block, Vector *po_order, BitSet *visited) {
  if (getBit(visited, block->id)) {
    return;
  }

  setBit(visited, block->id);
  Vector *succs = &block->succs;

  for (size_t i = 0; i < succs->size; ++i) {
    IrBasicBlock *succ = getBlockFromVector(succs, i);
    computePostOrderImpl(succ, po_order, visited);
  }

  block->po = po_order->size;
  addBlockToVector(po_order, block);
}

static void computePostOrder(IrFunction *func, Vector *po_order) {
  for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
    bb->po = -1;
  }

  BitSet visited;
  initBitSet(&visited, ctx->bbCnt);

  computePostOrderImpl(func->entry, po_order, &visited);

  assert(po_order->size - 1 == func->entry->po);
  releaseBitSet(&visited);
}

// splitCriticalEdges() and isCriticalEdge() used to live here; they moved to
// src/ir/ir.c when codegen's stage 0 needed the same transform for a different
// reason (see the comment there).

static void insertBeforeTerm(IrBasicBlock *block, IrInstruction *i) {
  if (block->term == NULL) {
    addInstructionTail(block, i);
    return;
  }

  IrInstruction *term = block->instrunctions.tail;
  assert(term == block->term);

  i->prev = term->prev;

  if (term->prev == NULL) {
    block->instrunctions.head = i;
  } else {
    term->prev->next = i;
  }

  term->prev = i;
  i->next = term;
  i->block = block;
}

static IrInstruction *clonePureInstruction(IrInstruction *i) {
  assert(isPureInstr(i));
  assert(i->kind != IR_PHI);

  IrInstruction *copy = newInstruction(i->kind, i->type);
  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    addInstructionInput(copy, input);
  }
  memcpy(&copy->info, &i->info, sizeof (i->info));

  copy->astType = i->astType;
  copy->meta = i->meta;

  return copy;
}

// Cloning 'i' into a predecessor of 'block' is only sound if every input
// strictly dominates 'block': such a def dominates every predecessor as
// well (any path to a pred extends to a path to 'block' by one edge, so it
// must already contain the def), and its SSA value at the predecessor's
// exit is the very value 'i' would use. An input defined in 'block' itself
// (e.g. one of its phis) would need phi-translation - refuse instead.
static Boolean inputsStrictlyDominate(const IrInstruction *i, const IrBasicBlock *block) {
  for (size_t idx = 0; idx < i->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&i->inputs, idx);
    if (input->block == block || !dominates(input->block, block))
      return FALSE;
  }
  return TRUE;
}

// rows[b][v] is the instruction carrying VN v at the exit of block b, whose
// definition dominates that exit; NULL when the value is unavailable there.
// A whole row is NULL until its block has been processed, so an incoming
// edge from a NULL row is exactly a back edge.
typedef struct {
  IrInstruction ***rows;
  size_t blockCap;
  size_t vnCap;
} AvailabilityMap;

static void initAvailabilityMap(AvailabilityMap *am) {
  // Only instructions existing when the walk starts get numbered (phis and
  // clones inserted along the way reuse the VN of the instruction they stand
  // for), so every VN a row is indexed by stays below vnCap.
  am->blockCap = ctx->bbCnt;
  am->vnCap = ctx->instrCnt;
  am->rows = heapAllocate(am->blockCap * sizeof (IrInstruction **));
}

static void releaseAvailabilityMap(AvailabilityMap *am) {
  for (size_t b = 0; b < am->blockCap; ++b) {
    if (am->rows[b] != NULL) {
      releaseHeap(am->rows[b]);
    }
  }
  releaseHeap(am->rows);
}

static IrInstruction **newAvailabilityRow(const AvailabilityMap *am) {
  return heapAllocate(am->vnCap * sizeof (IrInstruction *));
}

// A block's row becomes visible only once the block is fully processed, so
// that a block cannot read a half-filled row of its own through a self edge.
static void publishAvailability(AvailabilityMap *am, const IrBasicBlock *block, IrInstruction **row) {
  assert(am->rows[block->id] == NULL);
  am->rows[block->id] = row;
}

static Boolean isProcessed(const AvailabilityMap *am, const IrBasicBlock *block) {
  return am->rows[block->id] != NULL;
}

static IrInstruction *availableAtExit(const AvailabilityMap *am, const IrBasicBlock *block, uint32_t vn) {
  assert(isProcessed(am, block));
  return am->rows[block->id][vn];
}

static void setAvailableAtExit(AvailabilityMap *am, const IrBasicBlock *block, uint32_t vn, IrInstruction *i) {
  assert(isProcessed(am, block));
  am->rows[block->id][vn] = i;
}

// The entry block, and any loop header reached from its own latch, has a
// predecessor the reverse post order has not visited yet; nothing is known
// about availability across such an edge.
static Boolean allPredecessorsProcessed(const AvailabilityMap *am, const IrBasicBlock *block) {
  if (block->preds.size == 0)
    return FALSE;

  for (size_t pi = 0; pi < block->preds.size; ++pi) {
    if (!isProcessed(am, getBlockFromVector(&block->preds, pi)))
      return FALSE;
  }

  return TRUE;
}

// Values reaching 'block' through one and the same representative on every
// incoming edge are available on entry: that single definition dominates the
// block. Values reaching it through *differing* representatives need a phi,
// and those are materialized lazily - only when a redundant instruction
// actually asks for one.
static void seedAvailableOnEntry(const AvailabilityMap *am, const IrBasicBlock *block, IrInstruction **row) {
  const Vector *preds = &block->preds;
  const IrBasicBlock *first = getBlockFromVector(preds, 0);

  for (size_t vn = 0; vn < am->vnCap; ++vn) {
    IrInstruction *rep = availableAtExit(am, first, vn);
    for (size_t pi = 1; rep != NULL && pi < preds->size; ++pi) {
      if (availableAtExit(am, getBlockFromVector(preds, pi), vn) != rep) {
        rep = NULL;
      }
    }
    row[vn] = rep;
  }
}

// Whether the value 'instr' recomputes at merge point 'block' can be taken
// from the incoming edges instead. Fully redundant (every predecessor
// carries it) is always worth a phi; partially redundant additionally needs
// the missing predecessors to accept a clone - sound only on an edge that
// leads nowhere else, and only when the clone's inputs reach it.
static Boolean canObtainFromPredecessors(const AvailabilityMap *am, const IrBasicBlock *block,
                                         const IrInstruction *instr, uint32_t vn) {
  const Vector *preds = &block->preds;
  size_t carried = 0;
  Boolean insertable = TRUE;

  for (size_t pi = 0; pi < preds->size; ++pi) {
    const IrBasicBlock *pred = getBlockFromVector(preds, pi);
    if (availableAtExit(am, pred, vn) != NULL) {
      carried += 1;
    } else if (pred->succs.size != 1) {
      // Unsplit (IBRANCH) edge - a clone there would execute speculatively.
      insertable = FALSE;
    }
  }

  if (carried == preds->size)
    return TRUE;

  return carried != 0 && insertable && inputsStrictlyDominate(instr, block);
}

// Materializes a representative of 'vn' at the head of 'block' by phi-ing
// the per-predecessor representatives together, cloning 'instr' into the
// predecessors that carry none. Returns the phi, which supersedes 'instr'.
static IrInstruction *insertPhiForValue(AvailabilityMap *am, IrBasicBlock *block,
                                        IrInstruction *instr, uint32_t vn) {
  const Vector *preds = &block->preds;
  IrInstruction *phiInstr = newPhiInstruction(instr->type);
  phiInstr->astType = instr->astType;
  phiInstr->meta = instr->meta;

  for (size_t pi = 0; pi < preds->size; ++pi) {
    IrBasicBlock *pred = getBlockFromVector(preds, pi);
    IrInstruction *rep = availableAtExit(am, pred, vn);
    if (rep == NULL) {
      rep = clonePureInstruction(instr);
      insertBeforeTerm(pred, rep);
      rep->algoIdx = vn;
      setAvailableAtExit(am, pred, vn, rep);
    }
    addPhiInput(phiInstr, rep, pred);
  }

  addInstructionHead(block, phiInstr);
  phiInstr->algoIdx = vn;
  replaceUsageWith(instr, phiInstr); // the dead instruction is left for dce

  return phiInstr;
}

static void preBlock(VNTable *vnt, AvailabilityMap *am, IrBasicBlock *block) {
  IrInstruction **row = newAvailabilityRow(am);
  Boolean predsKnown = allPredecessorsProcessed(am, block);

  if (predsKnown) {
    seedAvailableOnEntry(am, block, row);
  }

  // Only a join of several known paths can hold a redundancy that dominance
  // missed - anywhere else there is nothing to phi over.
  Boolean isKnownMerge = predsKnown && block->preds.size >= 2;

  for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
    uint32_t vn = valueNumberInstruction(vnt, instr, NULL);

    // Side effecting instructions are never reused and never cloned; their
    // unique value number stays unavailable to everyone.
    if (!isPureInstr(instr))
      continue;

    assert(vn < am->vnCap);

    if (row[vn] != NULL) {
      if (row[vn] != instr && instr->uses.size != 0) {
        replaceUsageWith(instr, row[vn]);
      }
      continue;
    }

    // No representative dominates this point, but at a merge the value may
    // still be available across the incoming edges - fully, or partially
    // once the computation is cloned into the paths that miss it.
    if (isKnownMerge && instr->uses.size != 0 && instr->inputs.size != 0 &&
        canObtainFromPredecessors(am, block, instr, vn)) {
      row[vn] = insertPhiForValue(am, block, instr, vn);
    } else {
      row[vn] = instr;
    }
  }

  publishAvailability(am, block, row);
}

static void pre(VNTable *vnt, IrFunction *func) {
  splitCriticalEdges(func);

  Vector poOrder = { 0 };
  initVector(&poOrder, func->numOfBlocks);
  computePostOrder(func, &poOrder);

  AvailabilityMap am;
  initAvailabilityMap(&am);

  // Reverse post order: a block is processed only after every predecessor
  // that is not a back edge, so their availability is already known.
  for (size_t idx = poOrder.size - 1; idx != -1; --idx) {
    preBlock(vnt, &am, getBlockFromVector(&poOrder, idx));
  }

  releaseAvailabilityMap(&am);
  releaseVector(&poOrder);
}

// -============================ Phi dedup ============================-
//
// Neither stage above value-numbers a phi: what it evaluates to depends on
// the edge control arrived by, not on its operands alone, so it cannot be
// hashed like an expression. Duplicates do pile up though - mem2reg emits
// one phi per promoted variable, and pre() adds its own for every value it
// merges, so a join block routinely ends up holding several phis carrying
// the same value along the same edges.
//
// Two phis of one block are interchangeable exactly when they map every
// incoming edge to the same definition: control enters by a single edge, and
// both then yield whatever that edge carries. Comparing the operands by
// identity keeps this independent of value numbering, which is what makes it
// safe to run on phis whose inputs come back around a loop edge.

static Boolean phisAreEqual(const IrInstruction *lhs, const IrInstruction *rhs) {
  assert(lhs->kind == IR_PHI && rhs->kind == IR_PHI);
  assert(lhs->block == rhs->block);

  if (lhs->type != rhs->type)
    return FALSE;

  const Vector *lEdges = &lhs->info.phi.phiBlocks;
  const Vector *rEdges = &rhs->info.phi.phiBlocks;

  if (lEdges->size != rEdges->size)
    return FALSE;

  // The entries are the block's predecessors in no particular order, so pair
  // them up by edge rather than by position.
  for (size_t l = 0; l < lEdges->size; ++l) {
    const IrBasicBlock *edge = getBlockFromVector(lEdges, l);
    const IrInstruction *value = getInstructionFromVector(&lhs->inputs, l);
    Boolean matched = FALSE;

    for (size_t r = 0; r < rEdges->size; ++r) {
      if (getBlockFromVector(rEdges, r) != edge)
        continue;

      // A predecessor listed twice (a branch with both arms to this block)
      // is only matched at its first entry, so an unequal pairing is
      // reported as a difference rather than searched past.
      if (getInstructionFromVector(&rhs->inputs, r) != value)
        return FALSE;

      matched = TRUE;
      break;
    }

    if (!matched)
      return FALSE;
  }

  return TRUE;
}

static Boolean dedupPhisInBlock(IrBasicBlock *block) {
  Boolean changed = FALSE;

  for (IrInstruction *phi = block->instrunctions.head;
       phi != NULL && phi->kind == IR_PHI; phi = phi->next) {
    IrInstruction *candidate = phi->next;

    while (candidate != NULL && candidate->kind == IR_PHI) {
      IrInstruction *dup = candidate;
      candidate = candidate->next;

      // Testing 'uses' first is what makes the fixed point below terminate:
      // a duplicate is rewired exactly once, and afterwards it has no users
      // left to report progress with.
      if (dup->uses.size != 0 && phisAreEqual(phi, dup)) {
        replaceUsageWith(dup, phi); // the dead phi is left for dce
        changed = TRUE;
      }
    }
  }

  return changed;
}

static void dedupPhis(IrFunction *func) {
  // Merging a pair of phis can make two of their users equal in turn - and a
  // user may itself be a phi, in an earlier block or the same one - so the
  // sweep repeats until nothing more collapses.
  Boolean changed = TRUE;

  while (changed) {
    changed = FALSE;
    for (IrBasicBlock *block = func->blocks.head; block != NULL; block = block->next) {
      if (dedupPhisInBlock(block)) {
        changed = TRUE;
      }
    }
  }
}

void gvn(IrFunction *func) {
  VNTable vnt;
  initVNTable(&vnt);

  // Stage one: fold what dominance alone can express.
  poisonAlgoIdx(func);
  gvnBlock(&vnt, func->entry);

  // Stage two: the merge points dominance cannot reach. It renumbers from
  // scratch, globally rather than scoped, but on the same storage.
  resetVNTable(&vnt);
  poisonAlgoIdx(func);
  pre(&vnt, func);

  releaseVNTable(&vnt);

  // Stage three: the phis the two stages above could not consider, including
  // the ones stage two has just introduced.
  dedupPhis(func);

  func->phases.gvn = 1;
}
