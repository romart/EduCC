
#include <assert.h>
#include "ir/ir.h"
#include <signal.h>
#include <math.h>
#include "mem.h"

extern IrContext *ctx;


static IrInstruction *topI = (IrInstruction *)0;
static IrInstruction *bottomI = (IrInstruction *)-1;

static void cleanAndErase(IrInstruction *i) {
    assert(i->uses.size == 0);
    for (size_t ii = 0; ii < i->inputs.size; ++ii) {
      IrInstruction *input = getInstructionFromVector(&i->inputs, ii);
      removeFromVector(&input->uses, (intptr_t)i);
    }
    clearVector(&i->inputs);
    eraseInstructionFromBlock(i);
    releaseInstruction(i);
}


static Boolean isLeafInstr(const IrInstruction *instr) {
  return instr->inputs.size == 0;
}

static Boolean isConstantInstr(const IrInstruction *i) {
  return i->kind == IR_DEF_CONST;
}

static IrInstruction *putAtInstrVector(Vector *v, IrInstruction *instr, size_t idx) {
  return (IrInstruction *)putAtVector(v, idx, (intptr_t)instr);
}

static void initLattices(IrFunction *func, Vector *LVs, Vector *instrMapping) {
  const size_t instrCount = ctx->instrCnt;
  
  for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
    IrBasicBlock *block = bn->block;
    for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
      uint32_t iid = instr->id;
      assert(iid < instrCount);
      putAtInstrVector(instrMapping, instr, iid);
      if (isConstantInstr(instr)) {
        printf("Put into LVs[%c%u] <- %c%u\n", '%', iid, '%', iid);
        putAtInstrVector(LVs, instr, iid);
        continue;
      }
      if (isLeafInstr(instr)) {
        printf("Put into LVs[%c%u] <- BOTTOM\n", '%', iid);
        putAtInstrVector(LVs, bottomI, iid);
        continue;
      }
      printf("Put into LVs[%c%u] <- TOP\n", '%', iid);
      putAtInstrVector(LVs, topI, iid);
    }
  }
}

static IrInstruction *evaluateUnary(IrInstruction *i) {
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(arg))
    return bottomI;

  if (i->kind == IR_U_NOT) {
    return createIntegerConstant(i->type, !arg->info.constant.data.i);
  } else if (i->kind == IR_U_BNOT) {
    return createIntegerConstant(i->type, ~arg->info.constant.data.i);
  } else {
    unreachable("Unexpected unary operand");
  }

  return NULL;
}

enum IrTypeClass {
  IR_TC_BAD,
  IR_TC_FLOAT,
  IR_TC_SIGNED,
  IR_TC_UNSIGNED,
  IR_TC_PTR,
};

static Boolean isFloatIrType(enum IrTypeKind k) {
    return IR_F32 <= k && k <= IR_F80;
}

static Boolean isIntegerIrType(enum IrTypeKind k) {
    return IR_I8 <= k && k <= IR_U64;
}

static Boolean isSignedIrType(enum IrTypeKind k) {
    return IR_I8 <= k && k <= IR_I64;
}

static Boolean isUnsignedIrType(enum IrTypeKind k) {
    return IR_U8 <= k && k <= IR_U64;
}

static enum IrTypeClass irTypeClass(enum IrTypeKind k) {
  switch (k) {
    case IR_BOOL: // predicates
    case IR_I8:
    case IR_I16:
    case IR_I32:
    case IR_I64:
      return IR_TC_SIGNED;

    case IR_U8:
    case IR_U16:
    case IR_U32:
    case IR_U64:
      return IR_TC_UNSIGNED;

    case IR_F32:
    case IR_F64:
    case IR_F80:
      return IR_TC_FLOAT;

    case IR_LITERAL:
	case IR_P_AGG: // packed aggregate
    case IR_PTR:
    case IR_REF:
    case IR_LABEL:
      return IR_TC_PTR;

    case IR_VOID:
      return IR_TC_BAD;
  }
}

static IrInstruction *evaluateBitCast(IrInstruction *i) {
  assert(i->kind == IR_E_BITCAST);
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(arg))
    return bottomI;

  enum IrTypeKind fromT = i->info.fromCastType;
  enum IrTypeKind toT = i->type;

  enum IrTypeClass fromC = irTypeClass(fromT);
  enum IrTypeClass toC = irTypeClass(toT);

  if (fromC == toC)
    return arg;

  if (toC == IR_TC_FLOAT) {
    return createFloatConstant(toT, (float80_const_t)i->info.constant.data.i);
  }

  if (fromC == IR_TC_FLOAT) {
    return createIntegerConstant(toT, (int64_const_t)i->info.constant.data.f);
  }
 
  // be more accuare with pointers
  return arg;
}

static IrInstruction *evaluateBinary(IrInstruction *i) {
  // assert(isBinary());
  IrInstruction *lhs = getInstructionFromVector(&i->inputs, 0);
  IrInstruction *rhs = getInstructionFromVector(&i->inputs, 1);

  if (!isConstantInstr(lhs) || !isConstantInstr(rhs))
    return bottomI;

  switch (i->kind) {
  case IR_E_ADD: return createIntegerConstant(i->type, lhs->info.constant.data.i + rhs->info.constant.data.i);
  case IR_E_SUB: return createIntegerConstant(i->type, lhs->info.constant.data.i - rhs->info.constant.data.i);
  case IR_E_MUL: return createIntegerConstant(i->type, lhs->info.constant.data.i * rhs->info.constant.data.i);
  case IR_E_DIV:
  case IR_E_MOD:
   if (rhs->info.constant.data.i != 0) {
     int64_const_t l = lhs->info.constant.data.i;
     int64_const_t r = rhs->info.constant.data.i;
     int64_const_t v = i->kind == IR_E_DIV ? l / r : l % r;
     return createIntegerConstant(i->type, v);
   } else {
     return bottomI;
   }
  case IR_E_OR: return createIntegerConstant(i->type, lhs->info.constant.data.i | rhs->info.constant.data.i);
  case IR_E_XOR: return createIntegerConstant(i->type, lhs->info.constant.data.i ^ rhs->info.constant.data.i);
  case IR_E_AND: return createIntegerConstant(i->type, lhs->info.constant.data.i & rhs->info.constant.data.i);
  case IR_E_SHL: return createIntegerConstant(i->type, lhs->info.constant.data.i << rhs->info.constant.data.i);
  case IR_E_SHR: return createIntegerConstant(i->type, lhs->info.constant.data.i >> rhs->info.constant.data.i);
  case IR_E_EQ: return createIntegerConstant(i->type, lhs->info.constant.data.i == rhs->info.constant.data.i);
  case IR_E_NE: return createIntegerConstant(i->type, lhs->info.constant.data.i != rhs->info.constant.data.i);
  case IR_E_LT: return createIntegerConstant(i->type, lhs->info.constant.data.i < rhs->info.constant.data.i);
  case IR_E_LE: return createIntegerConstant(i->type, lhs->info.constant.data.i <= rhs->info.constant.data.i);
  case IR_E_GT: return createIntegerConstant(i->type, lhs->info.constant.data.i > rhs->info.constant.data.i);
  case IR_E_GE: return createIntegerConstant(i->type, lhs->info.constant.data.i >= rhs->info.constant.data.i);
  case IR_E_FADD: return createFloatConstant(i->type, lhs->info.constant.data.f + rhs->info.constant.data.f);
  case IR_E_FSUB: return createFloatConstant(i->type, lhs->info.constant.data.f - rhs->info.constant.data.f);
  case IR_E_FMUL: return createFloatConstant(i->type, lhs->info.constant.data.f * rhs->info.constant.data.f);
  case IR_E_FDIV: return createFloatConstant(i->type, lhs->info.constant.data.f / rhs->info.constant.data.f);
  case IR_E_FMOD: return createFloatConstant(i->type, fmod(lhs->info.constant.data.f, rhs->info.constant.data.f));
  case IR_E_FEQ: return createIntegerConstant(i->type, lhs->info.constant.data.f == rhs->info.constant.data.f);
  case IR_E_FNE: return createIntegerConstant(i->type, lhs->info.constant.data.f != rhs->info.constant.data.f);
  case IR_E_FLT: return createIntegerConstant(i->type, lhs->info.constant.data.f < rhs->info.constant.data.f);
  case IR_E_FLE: return createIntegerConstant(i->type, lhs->info.constant.data.f <= rhs->info.constant.data.f);
  case IR_E_FGT: return createIntegerConstant(i->type, lhs->info.constant.data.f > rhs->info.constant.data.f);
  case IR_E_FGE: return createIntegerConstant(i->type, lhs->info.constant.data.f >= rhs->info.constant.data.f);
  default:
    unreachable("Unexpected binary op");
  }

  return NULL;
}


static Boolean computeBranchCondition(IrInstruction *condition) {
  assert(isConstantInstr(condition));

  switch (condition->info.constant.kind) {
  case IR_CK_FLOAT: return condition->info.constant.data.f != 0.0f;
  case IR_CK_INTEGER: return condition->info.constant.data.i != 0;
  case IR_CK_LITERAL:
  case IR_CK_SYMBOL: return TRUE;
  }
}

static void eraseFromBlockList2(IrBasicBlockList *list, IrBasicBlock *block) {
}


static void processPhiNodes(IrBasicBlock *phiBlock, IrBasicBlock *removedEdge) {
}

static void removeSuccessor(IrBasicBlock *block, IrBasicBlock *succ) {
  eraseFromBlockList2(&block->succs, succ);
  eraseFromBlockList2(&succ->preds, block);

  processPhiNodes(succ, block);
}

static void evaluateCondBranch(IrInstruction *i) {
  IrInstruction *condition = getInstructionFromVector(&i->inputs, 0);
  if (!isConstantInstr(condition)) 
    return;

  Boolean condValue = computeBranchCondition(condition);

  IrBasicBlock *target = NULL, *nonTarget = NULL;

  if (condValue) {
    target = i->info.branch.taken;
    nonTarget = i->info.branch.notTaken;
  } else  {
    target = i->info.branch.notTaken;
    nonTarget = i->info.branch.taken;
  }

  assert(target != NULL);
  assert(nonTarget != NULL);

  IrInstruction *newBranch = newGotoInstruction(target);
  IrBasicBlock *curBlock = i->block;

  removeSuccessor(curBlock, nonTarget);

  removeFromVector(&condition->uses, (intptr_t)i);

  clearVector(&i->inputs);
  cleanAndErase(i);
}

static void updateBlockTerminator(IrBasicBlock *block, IrInstruction *newTerminator) {
}

static void evaluateSwitch(IrInstruction *i) {
  assert(i->kind == IR_TBRANCH);

  IrInstruction *cond = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(cond))
    return;

  assert(cond->info.constant.kind == IR_CK_INTEGER);

  int64_const_t switchValue = cond->info.constant.data.i;
  SwitchTable *table = i->info.switchTable;
  assert(table != NULL);

  IrBasicBlock *targetBlock = table->defaultBB;
  for (uint32_t i = 0; i < table->caseCount; ++i) {
    int64_const_t c = table->caseBlocks[i].caseConst;
    IrBasicBlock *cb = table->caseBlocks[i].block;
    if (c == switchValue) {
      targetBlock = cb;
      break;
    }
  }

  assert(targetBlock != NULL);

  IrBasicBlock *currentBlock = i->block;
  assert(currentBlock != NULL);

  if (targetBlock != table->defaultBB) {
    removeSuccessor(currentBlock, table->defaultBB);
  }

  for (uint32_t i = 0; i < table->caseCount; ++i) {
    int64_const_t c = table->caseBlocks[i].caseConst;
    IrBasicBlock *cb = table->caseBlocks[i].block;
    if (cb != targetBlock) {
      removeSuccessor(currentBlock, cb);
    }
  }

  cleanAndErase(i);

  IrInstruction *gotoInstr = newGotoInstruction(targetBlock);
  updateBlockTerminator(currentBlock, gotoInstr);
}

static IrInstruction *evaluate(IrInstruction *i) {
  switch (i->kind) {
  case IR_E_ADD: 
  case IR_E_SUB:
  case IR_E_MUL:
  case IR_E_DIV:
  case IR_E_MOD:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_SHL:
  case IR_E_SHR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
  case IR_E_FADD:
  case IR_E_FSUB:
  case IR_E_FMUL:
  case IR_E_FDIV:
  case IR_E_FMOD:
  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    return evaluateBinary(i);
  case IR_U_NOT:
  case IR_U_BNOT:
    return evaluateUnary(i);
  case IR_E_BITCAST:
    return evaluateBitCast(i);
  case IR_CBRANCH:
    evaluateCondBranch(i);
    return bottomI;
  case IR_TBRANCH:
    evaluateSwitch(i);
    return bottomI;
  default: return bottomI;
  }
}

static IrInstruction *recompute(IrInstruction *i) {
  IrInstruction *evaluated = evaluate(i);
  if (evaluated == bottomI)
    return bottomI;

  return evaluated;
}

static IrInstruction *meetLattice(IrInstruction *lhs, IrInstruction *rhs) {
  if (lhs == bottomI)
    return rhs;

  if (rhs == bottomI)
    return lhs;

  return lhs == rhs ? lhs : bottomI;
}

static void propagate(IrInstruction *i, IrInstruction *newValue) {
  if (newValue == bottomI || newValue == topI) 
    return;
  replaceUsageWith(i, newValue);
}

void dumpInstr(IrInstruction *i) {
  if (i == topI)
    printf("TOP");
  else if (i == bottomI) {
    printf("BOTTOM");
  } else {
    printf("%c%u", '%', i->id);
  }
}

void scp(IrFunction *func) {
  Vector stackImpl = { 0 }, *stack = &stackImpl;

  initVector(stack, INITIAL_VECTOR_CAPACITY);

  const size_t instrCount = ctx->instrCnt;
  IrInstruction **LVs = heapAllocate(instrCount * sizeof (IrInstruction *));

  for (IrBasicBlockListNode *bn = func->blocks.head; bn != NULL; bn = bn->next) {
    IrBasicBlock *block = bn->block;
    for (IrInstruction *instr = block->instrunctions.head; instr != NULL; instr = instr->next) {
      assert(instr->id < instrCount);
      if (isConstantInstr(instr)) {
        LVs[instr->id] = instr;
        pushToStack(stack, (intptr_t) instr);
      } else  {
        LVs[instr->id] = topI;
      }
    }
  }

  while (stack->size != 0) {
    IrInstruction *i = (IrInstruction *)popFromStack(stack);
    assert(i != topI);
    
    assert(i->id < instrCount);

    IrInstruction *computed = LVs[i->id];
    printf("Try to fold "); dumpInstr(i); printf(", LV = "); dumpInstr(computed); printf("\n");
    if (computed == topI) {
      computed = recompute(i);
      printf(" computed new node "); dumpInstr(computed); printf("\n");
      if (computed != bottomI) {
        LVs[i->id] = computed;
        for (size_t ui = 0; ui < i->uses.size; ++ui) {
          IrInstruction *use = getInstructionFromVector(&i->uses, ui);
          pushToStack(stack, (intptr_t)use);
        }
        printf("  replace and erase "); dumpInstr(i); printf("\n");
        replaceUsageWith(i, computed);
        cleanAndErase(i);
      }
    } else {
      printf("Put into stack usages of "); dumpInstr(i); printf(" size = %u\n", i->uses.size);
      for (size_t ui = 0; ui < i->uses.size; ++ui) {
        IrInstruction *use = getInstructionFromVector(&i->uses, ui);
        printf("Put into stack "); dumpInstr(use); printf("\n");
        pushToStack(stack, (intptr_t)use);
      }
    }
  }

  releaseHeap(LVs);
  releaseVector(stack);
}


