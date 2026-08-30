
#include <assert.h>
#include "ir/ir.h"
#include <stdlib.h>
#include <math.h>
#include "mem.h"

extern IrContext *ctx;
extern IrInstruction *topI;
extern IrInstruction *bottomI;


// An address whose bits are not known until link time: the payload is a
// Symbol* or a char*, not the value the program will see, so folding with it
// computes on this compiler's own pointers. The type used to say so - a
// literal was IR_LITERAL - and since it says IR_PTR like any other address,
// the kind is the only thing left that can.
static Boolean isComputableConstant(const IrInstruction *c) {
  return c->info.constant.kind == IR_CK_INTEGER ||
         c->info.constant.kind == IR_CK_FLOAT;
}

IrInstruction *evaluateUnary(IrInstruction *i, IrInstruction *arg) {
  if (!isComputableConstant(arg))
    return i;

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

	case IR_P_AGG: // packed aggregate
    case IR_PTR:
    case IR_LABEL:
      return IR_TC_PTR;

    case IR_VOID:
      return IR_TC_BAD;
  }
}

// An integer constant converted to another integer type, per C's conversion
// rules: reduce it modulo the target's width, then read the result back with
// the target's signedness.
//
// The stored value is always the mathematical value of the constant in its own
// type, so widening needs no work beyond re-reading it - it is narrowing, and
// the change of sign at the same width, that actually compute something.
static int64_const_t convertIntegerConstant(int64_const_t v, enum IrTypeKind toT) {
  // A conversion to bool is a test against zero, not a truncation: (_Bool)256
  // is 1, where taking the low byte would make it 0.
  if (toT == IR_BOOL) {
    return v != 0;
  }

  uint32_t bits = (uint32_t)irTypeMachineSize(toT) * 8u;
  if (bits >= sizeof(int64_const_t) * 8u) {
    return v;
  }

  uint64_t truncated = (uint64_t)v & ((1ULL << bits) - 1ULL);

  if (isUnsignedIrType(toT)) {
    return (int64_const_t)truncated;
  }

  uint64_t signBit = 1ULL << (bits - 1u);
  return (int64_const_t)((truncated ^ signBit) - signBit);
}

IrInstruction *evaluateBitCast(IrInstruction *i, IrInstruction *arg) {
  assert(isConstantInstr(arg));

  enum IrTypeKind fromT = i->info.fromCastType;
  enum IrTypeKind toT = i->type;

  // A cast of a link-time address folds only when it changes nothing but the
  // label; anything narrowing or reinterpreting would have to know the bits.
  if (!isComputableConstant(arg))
    return fromT == toT ? arg : i;

  enum IrTypeClass fromC = irTypeClass(fromT);
  enum IrTypeClass toC = irTypeClass(toT);

  if (toC == IR_TC_FLOAT) {
    if (fromC == IR_TC_FLOAT) {
      return fromT == toT ? arg : createFloatConstant(toT, arg->info.constant.data.f);
    }
    // Signedness comes from the *source*, as it does for the divides and the
    // right shift below: a constant is held in an int64_const_t, which is
    // unsigned, so converting one without asking makes every negative value a
    // number just under 2^64. (double)(-1) folded to 18446744073709551616.
    // Selection has always asked - selectConversion widens by
    // isUnsignedIrOperand and then converts signed - so this was the two of
    // them disagreeing about a value only one of them could see.
    int64_const_t iv = arg->info.constant.data.i;
    float80_const_t fv = isUnsignedIrOperand(fromT) ? (float80_const_t)iv
                                                    : (float80_const_t)(sint64_const_t)iv;
    return createFloatConstant(toT, fv);
  }

  if (fromC == IR_TC_FLOAT) {
    // The same asymmetry the other way up. The cast has to land on a signed
    // integer first: C leaves a negative float converted to an unsigned type
    // undefined, and selection reaches the unsigned destinations it does cover
    // through a signed conversion of its own.
    int64_const_t ic = (int64_const_t)(sint64_const_t)arg->info.constant.data.f;
    return createIntegerConstant(toT, convertIntegerConstant(ic, toT));
  }

  // Everything left is integer-to-integer, counting pointers: a pointer is a
  // 64-bit integer here and casting one to an integer type of the same width
  // changes nothing but the label.
  //
  // Returning 'arg' unchanged whenever the two sat in the same class was the
  // bug this replaced. It kept the *source's* type as well as its value, so a
  // narrowing cast folded to a constant that had never been narrowed - (char)300
  // stayed 300 - and a widening one left a value that does not fit the type
  // downstream believes it has, which instruction selection then materializes
  // with a move too narrow to carry it.
  if (fromT == toT) {
    return arg;
  }

  return createIntegerConstant(toT, convertIntegerConstant(arg->info.constant.data.i, toT));
}

static IrInstruction *evaluateBitCastInternal(IrInstruction *i) {
  assert(i->kind == IR_E_BITCAST);
  IrInstruction *arg = getInstructionFromVector(&i->inputs, 0);

  if (!isConstantInstr(arg))
    return i;

  return evaluateBitCast(i, arg);
}

// Constants are stored as int64_const_t, which is *unsigned*, so C's own
// operators on them are the unsigned ones and the signed cases have to say so.
// Which cases those are is exactly the set where the two differ: division,
// remainder, the right shift, and the four ordered comparisons. Addition,
// subtraction, multiplication, the left shift and equality are the same bits
// either way in two's complement, and are left alone.
//
// Getting this wrong is invisible in the ordinary case and wrong in the
// interesting one: '-7 / 2' folded to 9223372036854775804 rather than to -3,
// and then narrowing it to int left -4 - the floor result, which is not what C
// says - so a program computing it in a constant disagreed with the same
// program computing it at run time. The same split is already made by the
// AST-level evaluator in src/evaluate.c; this is the IR-level one catching up.
#define SIGNED_OP(l, op, r) ((int64_const_t)((sint64_const_t)(l) op (sint64_const_t)(r)))

IrInstruction *evaluateBinary(IrInstruction *i, IrInstruction *lhs, IrInstruction *rhs) {
  assert(isConstantInstr(lhs));
  assert(isConstantInstr(rhs));

  // One constant compared against itself is equal to itself whatever it holds,
  // which is the only thing worth folding about an address: 'a == b' with both
  // sides the same string literal is one IR_DEF_CONST twice over.
  if (lhs == rhs && (i->kind == IR_E_EQ || i->kind == IR_E_NE))
    return createIntegerConstant(i->type, i->kind == IR_E_EQ);

  if (!isComputableConstant(lhs) || !isComputableConstant(rhs))
    return i;

  // Division, remainder and the right shift take their signedness from the
  // type of the value being operated on, which for all three is the
  // instruction's own; a comparison produces an int whatever it compares, so
  // its operands are what has to be asked.
  Boolean isUnsigned = isUnsignedIrOperand(i->type);
  Boolean isUnsignedCmp = isUnsignedIrOperand(lhs->type);

  switch (i->kind) {
  case IR_E_ADD: return createIntegerConstant(i->type, lhs->info.constant.data.i + rhs->info.constant.data.i);
  case IR_E_SUB: return createIntegerConstant(i->type, lhs->info.constant.data.i - rhs->info.constant.data.i);
  case IR_E_MUL: return createIntegerConstant(i->type, lhs->info.constant.data.i * rhs->info.constant.data.i);
  case IR_E_DIV:
  case IR_E_MOD:
   if (rhs->info.constant.data.i != 0) {
     int64_const_t l = lhs->info.constant.data.i;
     int64_const_t r = rhs->info.constant.data.i;
     int64_const_t v;

     if (i->kind == IR_E_DIV) {
       v = isUnsigned ? l / r : SIGNED_OP(l, /, r);
     } else {
       v = isUnsigned ? l % r : SIGNED_OP(l, %, r);
     }

     return createIntegerConstant(i->type, v);
   } else {
     return i;
   }
  case IR_E_OR: return createIntegerConstant(i->type, lhs->info.constant.data.i | rhs->info.constant.data.i);
  case IR_E_XOR: return createIntegerConstant(i->type, lhs->info.constant.data.i ^ rhs->info.constant.data.i);
  case IR_E_AND: return createIntegerConstant(i->type, lhs->info.constant.data.i & rhs->info.constant.data.i);
  case IR_E_SHL: return createIntegerConstant(i->type, lhs->info.constant.data.i << rhs->info.constant.data.i);
  // An arithmetic shift for a signed value and a logical one otherwise, which
  // is the same split IR_E_SHR gets at selection - sar against shr.
  case IR_E_SHR:
    return createIntegerConstant(i->type, isUnsigned
        ? lhs->info.constant.data.i >> rhs->info.constant.data.i
        : SIGNED_OP(lhs->info.constant.data.i, >>, rhs->info.constant.data.i));
  case IR_E_EQ: return createIntegerConstant(i->type, lhs->info.constant.data.i == rhs->info.constant.data.i);
  case IR_E_NE: return createIntegerConstant(i->type, lhs->info.constant.data.i != rhs->info.constant.data.i);
  case IR_E_LT:
    return createIntegerConstant(i->type, isUnsignedCmp
        ? lhs->info.constant.data.i < rhs->info.constant.data.i
        : SIGNED_OP(lhs->info.constant.data.i, <, rhs->info.constant.data.i));
  case IR_E_LE:
    return createIntegerConstant(i->type, isUnsignedCmp
        ? lhs->info.constant.data.i <= rhs->info.constant.data.i
        : SIGNED_OP(lhs->info.constant.data.i, <=, rhs->info.constant.data.i));
  case IR_E_GT:
    return createIntegerConstant(i->type, isUnsignedCmp
        ? lhs->info.constant.data.i > rhs->info.constant.data.i
        : SIGNED_OP(lhs->info.constant.data.i, >, rhs->info.constant.data.i));
  case IR_E_GE:
    return createIntegerConstant(i->type, isUnsignedCmp
        ? lhs->info.constant.data.i >= rhs->info.constant.data.i
        : SIGNED_OP(lhs->info.constant.data.i, >=, rhs->info.constant.data.i));
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

