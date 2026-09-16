#include "ir/ir.h"
#include "ir/target.h"
#include "sema.h"

#include <assert.h>
#include <string.h>

// Long double where the target has no hardware for it: every operation becomes
// a call into libgcc's soft-float set, and this is where that happens.
//
// It happens *here* - in the IR, before instruction selection - rather than in
// the selector, and that is the whole design decision. A call the register
// allocator did not see as a call destroys the caller-saved registers behind
// its back, so what the allocator has to be handed is an IR_CALL like any
// other. Lowering early also gets the argument walk, the liveness and the
// spiller for free, none of which knows anything about this.
//
// The shape suits it. An IR_F80 value is *the address of* sixteen bytes rather
// than the bytes (see include/ir/ir.h), so an argument is two words loaded from
// that address, and a result is two words the ABI hands back in a register pair
// - which is what a two-eightbyte aggregate already is on this target.

extern IrContext *ctx;

// libgcc spells binary128 'tf', for "tetra float".
static const char *binaryRoutine(enum IrIntructionKind kind) {
  switch (kind) {
  case IR_E_FADD: return "__addtf3";
  case IR_E_FSUB: return "__subtf3";
  case IR_E_FMUL: return "__multf3";
  case IR_E_FDIV: return "__divtf3";
  default: return NULL;
  }
}

// The comparison routines answer an integer whose sign is the comparison's,
// which is then compared against zero - so each of these pairs a routine with
// the integer test that reads its answer.
//
// Unordered is why there are six routines and not one. The 'lt' and 'le' ones
// answer a positive number when either operand is a NaN and the 'gt' and 'ge'
// ones a negative, which makes every relational operator false on a NaN - the
// answer C wants, arrived at without asking whether the operands are ordered.
static const char *compareRoutine(enum IrIntructionKind kind, enum IrIntructionKind *test) {
  switch (kind) {
  case IR_E_FEQ: *test = IR_E_EQ; return "__eqtf2";
  case IR_E_FNE: *test = IR_E_NE; return "__netf2";
  case IR_E_FLT: *test = IR_E_LT; return "__lttf2";
  case IR_E_FLE: *test = IR_E_LE; return "__letf2";
  case IR_E_FGT: *test = IR_E_GT; return "__gttf2";
  case IR_E_FGE: *test = IR_E_GE; return "__getf2";
  default: return NULL;
  }
}

// One Symbol per name for the whole translation unit: the object writer marks a
// symbol emitted by writing an index into it, so two Symbols for one name would
// be two undefined entries naming the same function.
static Symbol *routineSymbol(const char *name) {
  Symbol *s = (Symbol *)getFromHashMap(ctx->libcallSymbols, (intptr_t)name);

  if (s != NULL) {
    return s;
  }

  // Nobody writes a prototype for __addtf3, so this is not the translation
  // unit's symbol table's business. A FunctionSymbol with no declaration is
  // exactly what serializeExternalSymbols reads as "defined elsewhere".
  s = newSymbol(ctx->pctx, FunctionSymbol, name);
  putToHashMap(ctx->libcallSymbols, (intptr_t)name, (intptr_t)s);

  return s;
}

static IrInstruction *emitBefore(IrInstruction *at, IrInstruction *instr) {
  addInstructionBefore(at, instr);
  return instr;
}

static IrInstruction *emitBinary(IrInstruction *at, enum IrIntructionKind kind,
                                 IrInstruction *lhs, IrInstruction *rhs,
                                 enum IrTypeKind type) {
  IrInstruction *i = newInstruction(kind, type);

  addInstructionInput(i, lhs);
  addInstructionInput(i, rhs);

  return emitBefore(at, i);
}

static IrInstruction *emitLoad(IrInstruction *at, enum IrTypeKind type, IrInstruction *ptr) {
  IrInstruction *load = newInstruction(IR_M_LOAD, type);

  addInstructionInput(load, ptr);
  load->info.memory.opType = type;

  return emitBefore(at, load);
}

static IrInstruction *emitCast(IrInstruction *at, IrInstruction *value,
                               enum IrTypeKind from, enum IrTypeKind to) {
  IrInstruction *cast = newInstruction(IR_E_BITCAST, to);

  addInstructionInput(cast, value);
  cast->info.fromCastType = from;

  return emitBefore(at, cast);
}

// The two halves of a sixteen-byte object, as the eightbytes an argument pair
// is made of. IR_P_AGG rather than IR_I64 because that is the spelling
// initializeParamterLocal and translateCall already use for one, and what the
// callee stores back into its own slot has to be the same shape.
static void pushWideArgument(IrInstruction *at, IrInstruction *call, IrInstruction *address) {
  for (uint32_t half = 0; half < 2; ++half) {
    IrInstruction *ptr = address;

    if (half != 0) {
      ptr = emitBinary(at, IR_E_ADD, address,
                       createIntegerConstant(IR_I64, (int64_t)sizeof(intptr_t)), IR_PTR);
    }

    if (half == 0) {
      setCallWideArg(call, call->inputs.size);
    } else {
      setCallPairedArg(call, call->inputs.size);
    }

    addInstructionInput(call, emitLoad(at, IR_P_AGG, ptr));
  }
}

// A call to one of the routines, inserted ahead of the instruction it replaces.
// Every long double argument becomes two inputs; everything else is one.
static IrInstruction *emitLibcall(IrInstruction *at, const char *name,
                                  enum IrTypeKind resultType, IrInstruction *const *args,
                                  size_t argCount) {
  IrInstruction *call = newInstruction(IR_CALL, resultType);
  size_t inputs = 1;
  size_t idx;

  for (idx = 0; idx < argCount; ++idx) {
    inputs += args[idx]->type == IR_F80 ? 2 : 1;
  }

  allocateCallArgMaps(call, inputs);
  addInstructionInput(call, createSymbolConstant(routineSymbol(name)));

  call->info.call.symbol = NULL;
  call->info.call.isVariadic = FALSE;

  for (idx = 0; idx < argCount; ++idx) {
    if (args[idx]->type == IR_F80) {
      pushWideArgument(at, call, args[idx]);
    } else {
      addInstructionInput(call, args[idx]);
    }
  }

  // Nothing here is variadic, so every input is a named argument.
  call->info.call.firstUnnamedInput = call->inputs.size;

  return emitBefore(at, call);
}

// Replace 'i' with 'with' and take it out of the block.
static void replaceWith(IrInstruction *i, IrInstruction *with) {
  replaceUsageWith(i, with);
  cleanAndErase(i);
}

// Which libgcc conversion, and what the routine's own operand or result type is
// - libgcc has an 'si' form for 32 bits and a 'di' form for 64, so a narrower
// integer is widened on the way in and truncated on the way out.
static const char *integerToQuad(enum IrTypeKind from, enum IrTypeKind *widened) {
  if (isUnsignedIrOperand(from)) {
    *widened = irTypeMachineSize(from) > 4 ? IR_U64 : IR_U32;
    return *widened == IR_U64 ? "__floatunditf" : "__floatunsitf";
  }

  *widened = irTypeMachineSize(from) > 4 ? IR_I64 : IR_I32;
  return *widened == IR_I64 ? "__floatditf" : "__floatsitf";
}

static const char *quadToInteger(enum IrTypeKind to, enum IrTypeKind *result) {
  if (isUnsignedIrOperand(to)) {
    *result = irTypeMachineSize(to) > 4 ? IR_U64 : IR_U32;
    return *result == IR_U64 ? "__fixunstfdi" : "__fixunstfsi";
  }

  *result = irTypeMachineSize(to) > 4 ? IR_I64 : IR_I32;
  return *result == IR_I64 ? "__fixtfdi" : "__fixtfsi";
}

static void lowerConversion(IrInstruction *i) {
  enum IrTypeKind from = i->info.fromCastType;
  enum IrTypeKind to = i->type;
  IrInstruction *value = getInstructionFromVector(&i->inputs, 0);
  IrInstruction *args[2];
  IrInstruction *call;

  if (from == IR_F80 && to == IR_F80) {
    // A cast of a long double to its own type yields an rvalue, and an rvalue
    // of one is an address like any other - so this is the address it already
    // had, and nothing is converted.
    replaceWith(i, value);
    return;
  }

  if (to == IR_F80) {
    if (from == IR_F32 || from == IR_F64) {
      args[0] = value;
      replaceWith(i, emitLibcall(i, from == IR_F32 ? "__extendsftf2" : "__extenddftf2",
                                 IR_F80, args, 1));
      return;
    }

    {
      enum IrTypeKind widened;
      const char *name = integerToQuad(from, &widened);

      args[0] = from == widened ? value : emitCast(i, value, from, widened);
      replaceWith(i, emitLibcall(i, name, IR_F80, args, 1));
    }

    return;
  }

  assert(from == IR_F80);

  if (to == IR_F32 || to == IR_F64) {
    args[0] = value;
    replaceWith(i, emitLibcall(i, to == IR_F32 ? "__trunctfsf2" : "__trunctfdf2", to, args, 1));
    return;
  }

  if (to == IR_BOOL) {
    // '(_Bool)x' is 'x != 0', which is the inequality routine and the same
    // comparison against its answer that a written '!=' would get.
    args[0] = value;
    args[1] = createFloatConstant(IR_F80, 0.0L);
    call = emitLibcall(i, "__netf2", IR_I32, args, 2);
    replaceWith(i, emitBinary(i, IR_E_NE, call, createIntegerConstant(IR_I32, 0), IR_BOOL));
    return;
  }

  {
    enum IrTypeKind result;
    const char *name = quadToInteger(to, &result);

    args[0] = value;
    call = emitLibcall(i, name, result, args, 1);
    replaceWith(i, result == to ? call : emitCast(i, call, result, to));
  }
}

static void lowerInstruction(IrInstruction *i) {
  IrInstruction *args[2];
  enum IrIntructionKind test;
  const char *name;

  if (i->type == IR_F80 && (name = binaryRoutine(i->kind)) != NULL) {
    args[0] = getInstructionFromVector(&i->inputs, 0);
    args[1] = getInstructionFromVector(&i->inputs, 1);
    replaceWith(i, emitLibcall(i, name, IR_F80, args, 2));
    return;
  }

  if (i->inputs.size == 2 && getInstructionFromVector(&i->inputs, 0)->type == IR_F80 &&
      (name = compareRoutine(i->kind, &test)) != NULL) {
    IrInstruction *call;

    args[0] = getInstructionFromVector(&i->inputs, 0);
    args[1] = getInstructionFromVector(&i->inputs, 1);
    call = emitLibcall(i, name, IR_I32, args, 2);
    replaceWith(i, emitBinary(i, test, call, createIntegerConstant(IR_I32, 0), i->type));
    return;
  }

  if (i->kind == IR_E_BITCAST && (i->type == IR_F80 || i->info.fromCastType == IR_F80)) {
    lowerConversion(i);
    return;
  }

  // Everything a long double can still be the type of after this, and each one
  // is a move of sixteen bytes or an address rather than arithmetic: a load,
  // a store, a constant, a phi over addresses, a call's result, an argument,
  // a return. The riscv64 selector answers for those directly.
  assert((i->type != IR_F80 ||
          i->kind == IR_M_LOAD || i->kind == IR_M_STORE || i->kind == IR_DEF_CONST ||
          i->kind == IR_PHI || i->kind == IR_CALL || i->kind == IR_ICALL ||
          i->kind == IR_ALLOCA || i->kind == IR_BAD) &&
         "an unlowered long double operation");
}

// A long double argument or return value: two words out of the sixteen it
// names, which is how the ABI passes and returns one.
static void widenCallArguments(IrInstruction *call) {
  IrInstruction *rebuilt;
  size_t idx;
  size_t inputs = 0;
  Boolean any = FALSE;

  for (idx = 0; idx < call->inputs.size; ++idx) {
    Boolean wide = idx != 0 && getInstructionFromVector(&call->inputs, idx)->type == IR_F80;
    inputs += wide ? 2 : 1;
    any = any || wide;
  }

  if (!any) {
    return;
  }

  // Rebuilt rather than edited in place: an input is inserted in the middle,
  // and the three bitmaps are indexed by position, so every bit past the
  // insertion would have to move anyway.
  rebuilt = newInstruction(IR_CALL, call->type);
  rebuilt->astType = call->astType;
  rebuilt->meta = call->meta;
  rebuilt->info.call.returnBuffer = call->info.call.returnBuffer;
  rebuilt->info.call.symbol = call->info.call.symbol;
  rebuilt->info.call.isVariadic = call->info.call.isVariadic;
  rebuilt->info.call.firstUnnamedInput = 0;

  allocateCallArgMaps(rebuilt, inputs);

  for (idx = 0; idx < call->inputs.size; ++idx) {
    IrInstruction *input = getInstructionFromVector(&call->inputs, idx);

    if (idx == call->info.call.firstUnnamedInput) {
      rebuilt->info.call.firstUnnamedInput = rebuilt->inputs.size;
    }

    if (idx != 0 && input->type == IR_F80) {
      pushWideArgument(call, rebuilt, input);
      continue;
    }

    if (idx != 0 && isCallMemoryArg(call, idx)) {
      setCallMemoryArg(rebuilt, rebuilt->inputs.size);
    }

    if (idx != 0 && isCallPairedArg(call, idx)) {
      setCallPairedArg(rebuilt, rebuilt->inputs.size);
    }

    addInstructionInput(rebuilt, input);
  }

  if (call->info.call.firstUnnamedInput >= call->inputs.size) {
    rebuilt->info.call.firstUnnamedInput = rebuilt->inputs.size;
  }

  emitBefore(call, rebuilt);
  replaceWith(call, rebuilt);
}

static void widenReturn(IrInstruction *ret) {
  IrInstruction *value;
  IrInstruction *rebuilt;

  if (ret->inputs.size != 1 || getInstructionFromVector(&ret->inputs, 0)->type != IR_F80) {
    return;
  }

  value = getInstructionFromVector(&ret->inputs, 0);
  rebuilt = newInstruction(IR_RET, IR_VOID);

  addInstructionInput(rebuilt, emitLoad(ret, IR_P_AGG, value));
  addInstructionInput(
      rebuilt,
      emitLoad(ret, IR_P_AGG,
               emitBinary(ret, IR_E_ADD, value,
                          createIntegerConstant(IR_I64, (int64_t)sizeof(intptr_t)), IR_PTR)));

  emitBefore(ret, rebuilt);
  ret->block->term = rebuilt;
  replaceWith(ret, rebuilt);
}

void lowerLongDoubleCalls(IrFunction *func) {
  for (IrBasicBlock *bb = func->blocks.head; bb != NULL; bb = bb->next) {
    IrInstruction *i, *next;

    // The calls and returns first: lowering an operation produces a call of
    // its own, whose long double inputs this has already widened.
    for (i = bb->instrunctions.head; i != NULL; i = next) {
      next = i->next;

      if (i->kind == IR_CALL || i->kind == IR_ICALL) {
        widenCallArguments(i);
      } else if (i->kind == IR_RET) {
        widenReturn(i);
      }
    }

    for (i = bb->instrunctions.head; i != NULL; i = next) {
      next = i->next;
      lowerInstruction(i);
    }
  }
}
