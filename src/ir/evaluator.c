
#include <assert.h>
#include "ir/ir.h"
#include <stdlib.h>
#include <math.h>
#include "mem.h"

extern IrContext *ctx;
extern IrInstruction *topI;
extern IrInstruction *bottomI;


IrInstruction *evaluateUnary(IrInstruction *i, IrInstruction *arg) {
  if (i->kind == IR_U_NOT) {
    return createIntegerConstant(i->type, !arg->info.constant.data.i);
  } else if (i->kind == IR_U_BNOT) {
    return createIntegerConstant(i->type, ~arg->info.constant.data.i);
  } else {
    unreachable("Unexpected unary operand");
  }

  return NULL;
}

static IrInstruction *evaluateUnaryInternal(IrInstruction *i) {
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(arg))
    return i;

  return evaluateUnary(i, arg);
}

enum IrTypeClass {
  IR_TC_BAD,
  IR_TC_FLOAT,
  IR_TC_SIGNED,
  IR_TC_UNSIGNED,
  IR_TC_PTR,
};

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

IrInstruction *evaluateBitCast(IrInstruction *i, IrInstruction *arg) {
  assert(isConstantInstr(arg));

  enum IrTypeKind fromT = i->info.fromCastType;
  enum IrTypeKind toT = i->type;

  enum IrTypeClass fromC = irTypeClass(fromT);
  enum IrTypeClass toC = irTypeClass(toT);

  if (fromC == toC)
    return arg;

  if (toC == IR_TC_FLOAT) {
    return createFloatConstant(toT, (float80_const_t)arg->info.constant.data.i);
  }

  if (fromC == IR_TC_FLOAT) {
    int64_const_t ic = (int64_const_t)arg->info.constant.data.f;
    return createIntegerConstant(toT, ic);
  }

  // be more accuare with pointers
  return arg;
}

static IrInstruction *evaluateBitCastInternal(IrInstruction *i) {
  assert(i->kind == IR_E_BITCAST);
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(arg))
    return i;

  return evaluateBitCast(i, arg);
}

IrInstruction *evaluateBinary(IrInstruction *i, IrInstruction *lhs, IrInstruction *rhs) {
  assert(isConstantInstr(lhs));
  assert(isConstantInstr(rhs));

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
     return i;
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

static IrInstruction *evaluateBinaryInternal(IrInstruction *i) {
  // assert(isBinary());
  IrInstruction *lhs = getInstructionFromVector(&i->inputs, 0);
  IrInstruction *rhs = getInstructionFromVector(&i->inputs, 1);

  if (!isConstantInstr(lhs) || !isConstantInstr(rhs))
    return i;

  return evaluateBinary(i, lhs, rhs);
}

IrInstruction *evaluate(IrInstruction *i) {
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
    return evaluateBinaryInternal(i);
  case IR_U_NOT:
  case IR_U_BNOT:
    return evaluateUnaryInternal(i);
  case IR_E_BITCAST:
    return evaluateBitCastInternal(i);
  default: return i;
  }
}

