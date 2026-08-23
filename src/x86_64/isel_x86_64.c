#include <assert.h>
#include <string.h>

#include "ir/ir.h"
#include "ir/isel.h"
#include "sema.h"
#include "machine_x86_64.h"
#include "instructions_x86_64.h"

static const char *callRefusalReason(const TargetDescriptor *target, const IrInstruction *call);

// -============================ x86-64 instruction selection ==============-
//
// One IR instruction at a time, into the two-address form the ISA actually
// has. See docs/ir-codegen-design.md section 6 for the shape and
// machine_x86_64.h for the opcodes.
//
// What is covered is the integer subset - constants, values arriving in
// registers, arithmetic, compares and branches - plus calls and returns,
// memory, floats and conversions, and the aggregate cases the IR expresses.
// What is not - string literals, switch tables, computed gotos, dynamic
// allocas, long double, and the aggregate and variadic cases sections 6.10 and
// 6.11 of the design document set out - falls through to buildUnselected(),
// which leaves a well-formed placeholder rather than a hole and says out loud
// why, so the rest of the function still selects and can still be read.
//
// Flags are not modelled. x86's compare writes EFLAGS and the setcc or jcc
// after it reads them, and nothing here says so: the two are simply emitted
// adjacent. That holds only because nothing between selection and emission
// reorders instructions, which is true today and is written down in section 10
// as the precondition it is.

static uint8_t valueSize(const IrInstruction *i) {
  return irTypeMachineSize(i->type);
}

static const IrInstruction *inputAt(const IrInstruction *i, size_t idx) {
  return getInstructionFromVector(&i->inputs, idx);
}

// A register holding 'value' at 'size' bytes, widening it first if it is
// narrower than that.
//
// x86 has no move that both writes a wide register and leaves the bytes above
// the source alone, so a value used at more than its own width has to be
// extended explicitly, and which extension is the *value's* signedness rather
// than the using operation's.
//
// This is what makes 'a[i]' with a signed index correct. The IR scales the
// index to a byte offset at the index's own width and hands the 32-bit result
// to a GEP that adds it to a 64-bit pointer (see translateArrayAccess in
// ast2ir.c). Reading the low half of a register as a 64-bit index would pick
// up whatever the top half held, and zero-extending it - which is what a plain
// 32-bit move does - would turn 'a[-1]' into 'a[4294967295]'. This is the gap
// docs/ir-codegen-design.md section 10 recorded as unreachable while nothing
// touching memory was selected; selecting GEPs is what reaches it.
static uint32_t selectWidened(MachineBuilder *b, const IrInstruction *value, uint8_t size) {
  uint32_t src = machineBuilderVreg(b, value);
  uint8_t srcSize = valueSize(value);

  // Narrowing needs no instruction at all - the low bytes of the register are
  // already the answer - and a float is never widened implicitly: converting
  // one width to another changes the bits, so the IR spells it as a cast.
  if (srcSize >= size || isFloatIrType(value->type)) {
    return src;
  }

  Boolean isUnsigned = isUnsignedIrOperand(value->type);
  // A 32-bit move zeroes the top half of its destination, which is exactly an
  // unsigned widening from four bytes - just as well, since x86 has no
  // 'movzx r64, r32' to spell it with.
  Boolean byPlainMove = isUnsigned && srcSize == 4;

  uint32_t dst = createVirtualRegister(b->mf, RC_GP, size);
  MachineInstr *mi =
      buildMachineInstr(b, byPlainMove ? MOP_COPY : isUnsigned ? X86_MOVZX : X86_MOVSX, 1, 1);

  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  mi->opSize = byPlainMove ? srcSize : size;
  mi->srcSize = srcSize;

  return dst;
}

// 'dst <- value' at 'size' bytes, in whichever form the value has. A folded
// constant has no register to copy from and is spelled out as an immediate
// instead; a narrower value is widened on the way.
//
// The size is the using operation's, not the value's, which is what makes the
// widening above happen where it is needed. The two agree everywhere except
// pointer arithmetic on a narrow index.
static void selectLoadInto(MachineBuilder *b, uint32_t dst, const IrInstruction *value,
                           uint8_t size) {
  if (machineBuilderIsFolded(b, value)) {
    MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setValueOperand(b, mi, 1, value);
    mi->opSize = size;
    return;
  }

  // Widened first: selectWidened emits an instruction of its own, and
  // building the copy before asking for the source would leave that
  // instruction *after* the copy that reads it.
  uint32_t src = selectWidened(b, value, size);

  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  mi->opSize = size;
}

// -============================ Leaves ============================-

// Defined with the rest of the float rules below, which need the compare and
// setcc helpers that sit between here and there.
static void selectFloatConstant(MachineBuilder *b, const IrInstruction *i);

static void selectConstant(MachineBuilder *b, const IrInstruction *i) {
  // Only the ones no use could take as an immediate reach here; the driver
  // dropped the rest (see decideConstants in src/ir/codegen/isel.c).
  if (i->info.constant.kind == IR_CK_SYMBOL) {
    // A global's address, taken relative to the instruction pointer and
    // finished by the linker. A directly called function's name never gets
    // here - that one folds into the call - so this is a variable, or a
    // function whose address is wanted as a value.
    MachineAddress addr = { MAK_SYMBOL, NO_REG, NO_REG, 0, 0, { i->info.constant.data.s } };
    MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);

    setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
    setMemoryOperand(mi, 1, &addr);
    mi->opSize = sizeof(intptr_t);
    return;
  }

  if (i->info.constant.kind == IR_CK_LITERAL) {
    // A string literal's value *is* an address - of bytes that do not exist
    // yet. They go in the pool; emission places the pool and turns the index
    // below back into a section and an offset, which is the same rip-relative
    // LEA the symbol case above builds, only resolved by us and not the
    // linker. Alignment is 1: a char array has no other requirement, and
    // asking for more would pad .rodata for nothing.
    uint32_t constantIdx = addMachineConstant(b->mf, MCK_BYTES, i->info.constant.data.l.s,
                                              i->info.constant.data.l.length, 1);

    MachineAddress addr = { MAK_CONSTANT, NO_REG, NO_REG, 0, 0 };
    addr.anchor.constantIdx = constantIdx;

    MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);

    setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
    setMemoryOperand(mi, 1, &addr);
    mi->opSize = sizeof(intptr_t);
    return;
  }

  if (i->info.constant.kind == IR_CK_FLOAT) {
    selectFloatConstant(b, i);
    return;
  }

  assert(i->info.constant.kind == IR_CK_INTEGER);

  MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setImmediateOperand(mi, 1, i->info.constant.data.i);
  mi->opSize = valueSize(i);
}

// -============================ Memory ============================-
//
// Every address here comes from the AddressFold the driver worked out before
// selection started (decideFoldings in src/ir/codegen/isel.c), which is what
// turns 'a[i]' from a shift, a lea and a load into one 'mov eax, [rdi+rsi*4]'.
// Building it there rather than here is what keeps one answer: the same fold
// decides both what this instruction addresses and whether the GEP behind it
// is emitted at all.

// Whether a value of this type is something a single load or store can move.
static Boolean isAddressableIrType(enum IrTypeKind t) {
  // IR_P_AGG *is* one, and is exactly eight bytes when it appears here. The
  // translator emits an aggregate load or store in one situation only - a
  // composite small enough to travel in a single register, either being read
  // out to become a call argument or being stored home from the register it
  // arrived in (see translateCall and initializeParamterLocal in ast2ir.c).
  // Anything larger is an IR_M_COPY between two addresses and never a load.
  //
  // Long double is the type left out: it lives on the x87 stack, which this
  // backend does not model at all.
  return t != IR_F80 && t != IR_VOID;
}

// The addressing mode a pointer denotes, in the form an operand takes.
//
// Call it *before* building the instruction that reads it: widening an index
// to pointer width is a real instruction, and one built afterwards would sit
// behind the instruction that reads what it produces.
//
// A pointer with no fold is one whose computation stayed where it was - a
// loaded pointer, a parameter, a call's result - and it is simply the register
// holding it.
static MachineAddress addressFor(MachineBuilder *b, const IrInstruction *ptr, int32_t disp) {
  const AddressFold *f = machineBuilderAddressFold(b, ptr);
  MachineAddress addr = { MAK_REG, NO_REG, NO_REG, 0, disp };

  if (f == NULL) {
    addr.base = machineBuilderVreg(b, ptr);
    return addr;
  }

  addr.disp = disp + f->disp;

  if (f->index != NULL) {
    addr.index = selectWidened(b, f->index, sizeof(intptr_t));
    addr.scale = f->scale;
  }

  if (f->base != NULL) {
    addr.base = machineBuilderVreg(b, f->base);
  } else {
    addr.kind = MAK_FRAME;
    addr.anchor.frameIdx = f->frameIdx;
  }

  return addr;
}

static void setFrameAddressOperand(MachineInstr *mi, uint16_t idx, int32_t frameIdx) {
  MachineAddress addr = { MAK_FRAME, NO_REG, NO_REG, 0, 0 };
  addr.anchor.frameIdx = frameIdx;
  setMemoryOperand(mi, idx, &addr);
}

// A VLA or a call to alloca(): the block is carved out of the stack where the
// allocation stands, so its address is the stack pointer afterwards rather
// than a displacement from the frame pointer.
//
// Nothing puts rsp back. Every local, spill and callee-saved slot is addressed
// from rbp, and the epilogue's 'leave' restores rsp from rbp on every return
// path, so the allocation lasts exactly as long as C says it does - to the end
// of the function - and costs nothing to end. That is also why the frame needs
// no slot to park the old stack pointer in, which is what stage 0 used to lay
// one out for.
static void selectDynamicAlloca(MachineBuilder *b, const IrInstruction *i) {
  const int64_t alignment = 2 * sizeof(intptr_t);
  uint32_t sp = b->mf->target->sp;
  uint32_t bytes = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  // At a word, whatever width the size was computed at: this is about to be
  // subtracted from a pointer, and the bytes above a narrow count are not the
  // count's to supply.
  selectLoadInto(b, bytes, inputAt(i, 0), sizeof(intptr_t));

  // Rounded up to 16 rather than to the requested object's alignment, which is
  // not known here - and which 16 covers, being the strictest an x86-64 scalar
  // asks for. It is also what keeps rsp where SysV wants it at the next call:
  // the prologue left it 16-aligned and only a multiple of 16 leaves it so.
  MachineInstr *round = buildMachineInstr(b, X86_ADD, 1, 2);
  setRegisterOperand(round, 0, bytes);
  setRegisterOperand(round, 1, bytes);
  setImmediateOperand(round, 2, alignment - 1);
  round->opSize = sizeof(intptr_t);

  MachineInstr *mask = buildMachineInstr(b, X86_AND, 1, 2);
  setRegisterOperand(mask, 0, bytes);
  setRegisterOperand(mask, 1, bytes);
  setImmediateOperand(mask, 2, -alignment);
  mask->opSize = sizeof(intptr_t);

  MachineInstr *carve = buildMachineInstr(b, X86_SUB, 1, 2);
  setRegisterOperand(carve, 0, sp);
  setRegisterOperand(carve, 1, sp);
  setRegisterOperand(carve, 2, bytes);
  carve->opSize = sizeof(intptr_t);

  // The result is the new top of the stack. Copied out into a register of its
  // own rather than left as rsp, which the next call is about to move.
  MachineInstr *result = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(result, 0, machineBuilderVreg(b, i));
  setRegisterOperand(result, 1, sp);
  result->opSize = sizeof(intptr_t);
}

// Reading and writing the stack pointer, which is all a loop needs to give
// back what its body carved out of the stack. Both are plain moves; what makes
// them worth two opcodes rather than an IR_P_REG read is that the stack
// pointer is not a value GVN may assume two reads of agree about.
static void selectStackSave(MachineBuilder *b, const IrInstruction *i) {
  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, b->mf->target->sp);
  mi->opSize = sizeof(intptr_t);
}

static void selectStackRestore(MachineBuilder *b, const IrInstruction *i) {
  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, b->mf->target->sp);
  setRegisterOperand(mi, 1, machineBuilderVreg(b, inputAt(i, 0)));
  mi->opSize = sizeof(intptr_t);
}

// A value stage 0 gave a frame slot to. Two kinds of IR value get one - an
// alloca, and the address the ABI left an incoming stack argument at - and
// both are an address and nothing else, so both are one 'lea'. Asking the
// frame rather than the opcode is what keeps them a single rule, and what
// makes whatever stage 0 decides to put in the frame next work already.
static void selectFrameAddress(MachineBuilder *b, const IrInstruction *i, int32_t frameIdx) {
  const MachineFrameObject *obj = machineFrameObjectAt(b->mf, frameIdx);

  if (obj->isDynamic) {
    selectDynamicAlloca(b, i);
    return;
  }

  // lea, not a load: what this produces is the slot's address, not what is in
  // it.
  MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setFrameAddressOperand(mi, 1, frameIdx);
  mi->opSize = sizeof(intptr_t);
}

// Only reached when something still needs the pointer as a value - a GEP every
// one of whose uses is an address is never selected at all. What is left is
// the address it stands for, computed rather than dereferenced.
static void selectGep(MachineBuilder *b, const IrInstruction *i) {
  MachineAddress addr = addressFor(b, i, 0);

  MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = sizeof(intptr_t);
}

static void selectMemoryLoad(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;

  if (!isAddressableIrType(t)) {
    buildUnselected(b, i, "no single load moves a value of this type");
    return;
  }

  MachineAddress addr = addressFor(b, inputAt(i, 0), 0);

  MachineInstr *mi = buildMachineInstr(b, X86_LOAD, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = irTypeMachineSize(t);
}

// Whether the copy is one the rule below covers. Asked twice: once by that
// rule, and once by the driver, which has to know whether this instruction's
// pointers will reach an addressing mode before it decides to fold them away.
//
// The size is read from the IR rather than from the operand, because whether
// it was folded into an immediate is a question about *uses* and this one is
// not a use at all - the count is spent at selection time.
static Boolean isUnrollableCopy(const IrInstruction *i) {
  const IrInstruction *size = inputAt(i, 2);

  if (size->kind != IR_DEF_CONST || size->info.constant.kind != IR_CK_INTEGER) {
    return FALSE;
  }

  return size->info.constant.data.i > 0;
}

// 'copy n bytes from src to dst', as a run of load/store pairs at increasing
// displacements, largest chunk first.
//
// No alignment handling: x86 permits an unaligned load or store of any width,
// so the chunk size follows from what is left to copy and nothing else. The
// legacy backend's copyStructTo caps each chunk at the type's alignment, which
// costs instructions and buys nothing on this target.
//
// One register reused for every chunk, not one apiece. The trivial allocator
// gives each virtual register a frame slot of its own and never reuses it, so
// a register per chunk would grow the frame by as many bytes as the copy moves
// - on top of the source and the destination themselves. Reuse costs a future
// scheduler the freedom to see the chunks as independent, which they are; a
// frame that scales with the copy is the worse of the two.
static void selectMemoryCopy(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *size = inputAt(i, 2);

  // A size known only at run time is the one shape left: it needs a loop, and
  // nothing here builds one. Size alone is no longer a reason to refuse - see
  // docs/ir-codegen-design.md section 10.
  if (!isUnrollableCopy(i)) {
    buildUnselected(b, i, "copies a number of bytes not known until run time");
    return;
  }

  int64_t bytes = size->info.constant.data.i;

  // Both addresses once, outside the loop: the chunks differ only in their
  // displacement, and an address that had to widen an index would otherwise
  // widen it again for every eight bytes copied.
  MachineAddress from = addressFor(b, inputAt(i, 1), 0);
  MachineAddress to = addressFor(b, inputAt(i, 0), 0);

  // Widest the chunks get, so the narrow tail borrows the same slot; the
  // instructions carry their own width and only ever read back what they wrote.
  uint32_t tmp = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  for (int32_t done = 0; done < (int32_t)bytes;) {
    int32_t left = (int32_t)bytes - done;
    uint8_t chunk = left >= 8 ? 8 : left >= 4 ? 4 : left >= 2 ? 2 : 1;

    MachineAddress fromChunk = from;
    MachineAddress toChunk = to;
    fromChunk.disp += done;
    toChunk.disp += done;

    MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
    setRegisterOperand(load, 0, tmp);
    setMemoryOperand(load, 1, &fromChunk);
    load->opSize = chunk;

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &toChunk);
    setRegisterOperand(store, 1, tmp);
    store->opSize = chunk;

    done += chunk;
  }
}

static void selectMemoryStore(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;

  if (!isAddressableIrType(t)) {
    buildUnselected(b, i, "no single store moves a value of this type");
    return;
  }

  uint8_t size = irTypeMachineSize(t);
  // Widened first, because the store writes the slot's whole width and a
  // narrower value would otherwise leave the bytes above it as they were.
  uint32_t value = selectWidened(b, inputAt(i, 1), size);
  MachineAddress addr = addressFor(b, inputAt(i, 0), 0);

  // No defs: a store writes memory, and the registers in its address operand
  // are reads like any other address's.
  MachineInstr *mi = buildMachineInstr(b, X86_STORE, 0, 2);
  setMemoryOperand(mi, 0, &addr);
  setRegisterOperand(mi, 1, value);
  mi->opSize = size;
}

static void selectPhysReg(MachineBuilder *b, const IrInstruction *i) {
  // Read straight out of the fixed register the ABI put the value in, into one
  // the allocator is free to move. Leaving a parameter sitting in $rdi and
  // hoping nothing wants $rdi is how a backend acquires mysterious bugs.
  MachineInstr *mi = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, i->info.physReg);
  mi->opSize = valueSize(i);
}

// -============================ Arithmetic ============================-

// 'dst = lhs op rhs' as x86 spells it: 'dst <- lhs' and then 'dst op= rhs'.
// The leading copy is not waste - it is what makes the destructive form safe
// when lhs is still live afterwards - and the register allocator deletes it
// whenever it can give dst and lhs the same register.
static void selectBinary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, dst, inputAt(i, 0), size);

  const IrInstruction *rhs = inputAt(i, 1);
  Boolean folded = machineBuilderIsFolded(b, rhs);
  // Before the instruction that reads it, for the reason selectLoadInto gives.
  uint32_t rhsReg = folded ? NO_REG : selectWidened(b, rhs, size);

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  if (folded) {
    setValueOperand(b, mi, 2, rhs);
  } else {
    setRegisterOperand(mi, 2, rhsReg);
  }
  mi->opSize = size;
}

// Same, except that a shift by a value rather than by a constant has to take
// its count from cl and nowhere else.
static void selectShift(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);
  const IrInstruction *count = inputAt(i, 1);

  selectLoadInto(b, dst, inputAt(i, 0), size);

  if (!machineBuilderIsFolded(b, count)) {
    MachineInstr *toCl = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(toCl, 0, R_ECX);
    setRegisterOperand(toCl, 1, machineBuilderVreg(b, count));
    // The count is one byte however wide the shifted value is.
    toCl->opSize = 1;
  }

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  if (machineBuilderIsFolded(b, count)) {
    setImmediateOperand(mi, 2, count->info.constant.data.i);
  } else {
    setRegisterOperand(mi, 2, R_ECX);
  }
  mi->opSize = size;
}

// Division is the one place the ISA dictates which registers are involved: the
// dividend has to be in rdx:rax, and the instruction leaves the quotient in
// rax and the remainder in rdx whether or not both were wanted. Those are
// written as physical registers here, and the allocator has to treat them as
// pre-coloured - it may not reassign them, and it may not leave anything live
// in them across this.
static void selectDivMod(MachineBuilder *b, const IrInstruction *i, Boolean wantRemainder) {
  uint8_t size = valueSize(i);
  Boolean isSigned = !isUnsignedIrOperand(i->type);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, R_EAX, inputAt(i, 0), size);

  if (isSigned) {
    // Sign-extend rax into rdx:rax. One opcode for the whole cwd/cdq/cqo
    // family - opSize is what picks between them at emission.
    MachineInstr *ext = buildMachineInstr(b, X86_CDQ, 1, 1);
    setRegisterOperand(ext, 0, R_EDX);
    setRegisterOperand(ext, 1, R_EAX);
    machineOperandAt(ext, 1)->flags.isImplicit = 1;
    ext->opSize = size;
  } else {
    // An unsigned divide wants the high half zero rather than sign-extended.
    MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(zero, 0, R_EDX);
    setImmediateOperand(zero, 1, 0);
    zero->opSize = size;
  }

  // The divisor is never an immediate on x86, which is why isLegalImmediate
  // refuses to fold a constant into a divide - it arrives here in a register.
  uint32_t divisor = selectWidened(b, inputAt(i, 1), size);

  MachineInstr *div = buildMachineInstr(b, isSigned ? X86_IDIV : X86_DIV, 2, 3);
  setRegisterOperand(div, 0, R_EAX);
  setRegisterOperand(div, 1, R_EDX);
  setRegisterOperand(div, 2, divisor);
  setRegisterOperand(div, 3, R_EAX);
  setRegisterOperand(div, 4, R_EDX);
  // Only the divisor is written down in the instruction; the dividend halves
  // and both results are the ISA's own doing.
  machineOperandAt(div, 0)->flags.isImplicit = 1;
  machineOperandAt(div, 1)->flags.isImplicit = 1;
  machineOperandAt(div, 3)->flags.isImplicit = 1;
  machineOperandAt(div, 4)->flags.isImplicit = 1;
  div->opSize = size;

  MachineInstr *out = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(out, 0, dst);
  setRegisterOperand(out, 1, wantRemainder ? R_EDX : R_EAX);
  out->opSize = size;
}

// '~x' is the destructive one-operand form, so it reads like a binary op with
// only a left-hand side.
static void selectBitwiseNot(MachineBuilder *b, const IrInstruction *i) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, dst, inputAt(i, 0), size);

  MachineInstr *mi = buildMachineInstr(b, X86_NOT, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  mi->opSize = size;
}

// setcc writes one byte and one byte only. When the value being computed is
// wider than that - and it usually is, since C's comparisons and '!' both
// produce an int, not a predicate - the other bytes would be whatever the
// register happened to hold. Zeroing the destination first is what makes the
// whole width defined.
//
// It has to happen before the compare, not after: a move leaves the flags
// alone, so it is harmless there, whereas afterwards it would overwrite the
// answer the setcc is about to read out of them.
//
// Returns whether it emitted anything, because that is exactly the question
// "does the setcc read its own destination?" - see selectSetcc below.
static Boolean selectZeroExtendedSetup(MachineBuilder *b, uint32_t dst, uint8_t size) {
  if (size <= 1) {
    return FALSE;
  }

  MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(zero, 0, dst);
  setImmediateOperand(zero, 1, 0);
  zero->opSize = size;

  return TRUE;
}

// A setcc writes the low byte of its destination and leaves the rest as it
// found it. That makes it a read-modify-write of the whole register whenever
// the register is wider than a byte, and it is written down as one - operand 0
// defines it, operand 1 reads it - rather than as a plain def.
//
// Saying so is not decoration. The three upper bytes come from the zeroing
// move above, and nothing else in the machine function records that the setcc
// depends on it. A register allocator that believes the def is total will
// happily put the move's result somewhere the setcc never sees: the trivial
// allocator spills the zero to the frame and then reloads a *different* value
// into the scratch register the setcc writes, so the three bytes stored back
// are whatever the comparison's operand left behind. The dependency has to be
// in the operand list for the allocator to keep it.
//
// When the destination is a single byte there is no zeroing move and the def
// really is total, so the use is left off - claiming a read of a register
// nothing has written would be a use before def, and liveness would be right
// to complain about it.
static void selectSetcc(MachineBuilder *b, uint32_t opcode, uint32_t dst, Boolean readsDst) {
  MachineInstr *set = buildMachineInstr(b, opcode, 1, readsDst ? 1 : 0);

  setRegisterOperand(set, 0, dst);
  if (readsDst) {
    setRegisterOperand(set, 1, dst);
  }

  set->opSize = 1;
}

static void selectLogicalNot(MachineBuilder *b, const IrInstruction *i) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);
  const IrInstruction *arg = inputAt(i, 0);

  Boolean zeroed = selectZeroExtendedSetup(b, dst, size);

  MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
  setRegisterOperand(test, 0, machineBuilderVreg(b, arg));
  setRegisterOperand(test, 1, machineBuilderVreg(b, arg));
  test->opSize = valueSize(arg);

  selectSetcc(b, X86_SETE, dst, zeroed);
}

// -============================ Compares ============================-

// The jcc that branches on the flags a setcc would have read. X86_CONDITIONS
// generates both lists in the same order, so the two are a fixed distance
// apart and a condition needs naming only once.
static uint32_t jumpOpcodeFor(uint32_t setOpcode) {
  assert(setOpcode >= X86_SETE && setOpcode <= X86_SETNP);
  return X86_JE + (setOpcode - X86_SETE);
}

// The condition that is true exactly when this one is not, for a branch whose
// taken arm is the block that comes next: inverting is free, and jumping over
// a jump is not.
static uint32_t invertedCondition(uint32_t setOpcode) {
  switch (setOpcode) {
  case X86_SETE:  return X86_SETNE;
  case X86_SETNE: return X86_SETE;
  case X86_SETL:  return X86_SETGE;
  case X86_SETGE: return X86_SETL;
  case X86_SETLE: return X86_SETG;
  case X86_SETG:  return X86_SETLE;
  case X86_SETB:  return X86_SETAE;
  case X86_SETAE: return X86_SETB;
  case X86_SETBE: return X86_SETA;
  case X86_SETA:  return X86_SETBE;
  case X86_SETP:  return X86_SETNP;
  case X86_SETNP: return X86_SETP;
  default: unreachable("not a condition");
  }
}

static uint32_t setOpcodeFor(enum IrIntructionKind kind, Boolean isUnsigned) {
  switch (kind) {
  case IR_E_EQ: return X86_SETE;
  case IR_E_NE: return X86_SETNE;
  case IR_E_LT: return isUnsigned ? X86_SETB : X86_SETL;
  case IR_E_LE: return isUnsigned ? X86_SETBE : X86_SETLE;
  case IR_E_GT: return isUnsigned ? X86_SETA : X86_SETG;
  case IR_E_GE: return isUnsigned ? X86_SETAE : X86_SETGE;
  default: unreachable("not an integer comparison");
  }

  return X86_SETE;
}

// The comparison itself, and the condition its flags are then read with -
// by a setcc that materializes the boolean, or by the jcc of a branch that
// absorbed the whole compare.
//
// Whatever zeroing a setcc needs has to be emitted before this and not after:
// a move leaves the flags alone, an overwrite of them does not.
static uint32_t emitIntegerCompare(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *lhs = inputAt(i, 0);

  MachineInstr *cmp = buildMachineInstr(b, X86_CMP, 0, 2);
  // The left-hand side is never an immediate: x86 encodes the immediate as the
  // source operand, which is what isLegalImmediate's position rule enforces.
  setRegisterOperand(cmp, 0, machineBuilderVreg(b, lhs));
  setValueOperand(b, cmp, 1, inputAt(i, 1));
  cmp->opSize = valueSize(lhs);

  return setOpcodeFor(i->kind, isUnsignedIrOperand(lhs->type));
}

static void selectCompare(MachineBuilder *b, const IrInstruction *i) {
  uint32_t dst = machineBuilderVreg(b, i);
  Boolean zeroed = selectZeroExtendedSetup(b, dst, valueSize(i));
  uint32_t cc = emitIntegerCompare(b, i);

  selectSetcc(b, cc, dst, zeroed);
}

// -============================ Floats ============================-
//
// SSE only. x87 is used for nothing here, which is what makes long double the
// one floating type with no rule - it has no SSE representation at all.

// The IEEE bits of a float constant, as an integer of the same width. This is
// what lets a float constant be materialized without a constant pool: the bit
// pattern goes into a GP register as an ordinary immediate and then moves
// across into an xmm one unchanged.
//
// The alternative is what the legacy backend does - park the value in .rodata
// and load it rip-relative - which is one instruction instead of two. The
// constant pool string literals brought in could hold these just as well; it
// is a size question rather than a coverage one, since nothing refuses for
// want of it.
static int64_t floatConstantBits(const IrInstruction *i) {
  float80_const_t v = i->info.constant.data.f;

  if (i->type == IR_F32) {
    float f = (float)v;
    uint32_t bits = 0;
    memcpy(&bits, &f, sizeof(bits));
    return (int64_t)(uint64_t)bits;
  }

  double d = (double)v;
  uint64_t bits = 0;
  memcpy(&bits, &d, sizeof(bits));
  return (int64_t)bits;
}

static void selectFloatConstant(MachineBuilder *b, const IrInstruction *i) {
  // Not through floatConstantBits: it would answer for a long double by
  // rounding it to a double, and the bits of a value this does not represent
  // are worse than no rule at all.
  if (i->type == IR_F80) {
    buildUnselected(b, i, "a long double constant, which has no SSE form");
    return;
  }

  uint8_t size = valueSize(i);
  uint32_t bits = createVirtualRegister(b->mf, RC_GP, size);

  MachineInstr *mov = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(mov, 0, bits);
  setImmediateOperand(mov, 1, floatConstantBits(i));
  mov->opSize = size;

  MachineInstr *mi = buildMachineInstr(b, X86_MOVD, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setRegisterOperand(mi, 1, bits);
  mi->opSize = size;
}

// 'dst = lhs op rhs' in SSE's two-address form, which is the same shape as the
// integer one - and shares selectLoadInto with it, since a copy between xmm
// registers is still a copy.
static void selectFloatBinary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);

  selectLoadInto(b, dst, inputAt(i, 0), size);

  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, machineBuilderVreg(b, inputAt(i, 1)));
  mi->opSize = size;
}

// comis/ucomis report a float comparison in the *unsigned* flags - carry for
// "below", zero for "equal" - and set both, plus parity, when either operand
// is a NaN. So:
//
//   a > b   is 'above' on (a, b), and 'above' is false when unordered, which
//   a >= b  is 'above or equal' on (a, b), likewise            - both correct,
//           since every relational operator is false on a NaN.
//   a < b   is the same two with the operands swapped, rather than 'below':
//   a <= b  'below' is *true* when unordered, and a < NaN is false.
//
// Equality is the pair that cannot be read off one flag. Unordered sets the
// zero flag, so 'sete' alone would say NaN == NaN; the parity flag is what
// distinguishes the two, and the answer is 'equal and ordered' for == and
// 'not equal or unordered' for !=.
static uint32_t floatSetOpcodeFor(enum IrIntructionKind kind) {
  switch (kind) {
  case IR_E_FEQ: return X86_SETE;
  case IR_E_FNE: return X86_SETNE;
  case IR_E_FLT:
  case IR_E_FGT: return X86_SETA;
  case IR_E_FLE:
  case IR_E_FGE: return X86_SETAE;
  default: unreachable("not a float comparison");
  }

  return X86_SETE;
}

// As emitIntegerCompare, for the SSE comparisons.
static uint32_t emitFloatCompare(MachineBuilder *b, const IrInstruction *i) {
  Boolean isEquality = i->kind == IR_E_FEQ || i->kind == IR_E_FNE;
  // '<' and '<=' are '>' and '>=' with the operands the other way round.
  Boolean swap = i->kind == IR_E_FLT || i->kind == IR_E_FLE;
  const IrInstruction *lhs = inputAt(i, swap ? 1 : 0);
  const IrInstruction *rhs = inputAt(i, swap ? 0 : 1);

  MachineInstr *cmp = buildMachineInstr(b, isEquality ? X86_FUCMP : X86_FCMP, 0, 2);
  setRegisterOperand(cmp, 0, machineBuilderVreg(b, lhs));
  setRegisterOperand(cmp, 1, machineBuilderVreg(b, rhs));
  cmp->opSize = valueSize(lhs);

  return floatSetOpcodeFor(i->kind);
}

static void selectFloatCompare(MachineBuilder *b, const IrInstruction *i) {
  Boolean isEquality = i->kind == IR_E_FEQ || i->kind == IR_E_FNE;

  uint8_t size = valueSize(i);
  uint32_t dst = machineBuilderVreg(b, i);
  Boolean zeroed = selectZeroExtendedSetup(b, dst, size);
  uint32_t cc = emitFloatCompare(b, i);

  selectSetcc(b, cc, dst, zeroed);

  if (!isEquality) {
    return;
  }

  // Fold the ordered-ness in. The second setcc reads the same flags the first
  // did - nothing in between touches them - and combining is 'and' for ==,
  // which wants both, and 'or' for !=, which wants either.
  uint32_t ordered = createVirtualRegister(b->mf, RC_GP, size);
  Boolean orderedZeroed = selectZeroExtendedSetup(b, ordered, size);
  selectSetcc(b, i->kind == IR_E_FEQ ? X86_SETNP : X86_SETP, ordered, orderedZeroed);

  MachineInstr *mi = buildMachineInstr(b, i->kind == IR_E_FEQ ? X86_AND : X86_OR, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, ordered);
  mi->opSize = size;
}

// -============================ Conversions ============================-

// The integer width a conversion instruction can actually name. cvtsi2sd and
// cvttsd2si come in a 32-bit and a 64-bit form and in no others, so a narrower
// value is widened to four bytes first - which is free, in the sense that the
// widening is a real part of the conversion's meaning rather than a workaround:
// '(double)someChar' is '(double)(int)someChar'.
static uint8_t conversionIntSize(uint8_t size) {
  return size < 4 ? 4 : size;
}

// Whether an integer type of this kind can be converted to or from a float by
// the two instructions above, both of which read and write a *signed* integer.
//
// Unsigned 64-bit is the one that cannot. Its top half does not fit in a
// signed 64-bit integer, so both directions need the halving-and-doubling
// dance the legacy backend spells out; unsigned 32-bit is fine, because
// widening it to a signed 64-bit value loses nothing.
static Boolean isConvertibleIntType(enum IrTypeKind t) {
  return !(isUnsignedIrOperand(t) && irTypeMachineSize(t) == 8);
}

// A float register holding zero, for comparing against. Materialized the same
// way any other float constant is - the bits through a general register -
// rather than with the 'xorps reg, reg' idiom, which would need an operand
// that is both a def and a use of a register nothing has written, and the
// trivial allocator would reload that from an untouched frame slot.
static uint32_t selectFloatZero(MachineBuilder *b, uint8_t size) {
  uint32_t bits = createVirtualRegister(b->mf, RC_GP, size);
  uint32_t zero = createVirtualRegister(b->mf, RC_FP, size);

  MachineInstr *mov = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(mov, 0, bits);
  setImmediateOperand(mov, 1, 0);
  mov->opSize = size;

  MachineInstr *mi = buildMachineInstr(b, X86_MOVD, 1, 1);
  setRegisterOperand(mi, 0, zero);
  setRegisterOperand(mi, 1, bits);
  mi->opSize = size;

  return zero;
}

// 'dst = value != 0', which is what a conversion to _Bool means.
static void selectBooleanConversion(MachineBuilder *b, const IrInstruction *i, uint32_t dst,
                                    const IrInstruction *value, Boolean fromFloat) {
  uint8_t size = valueSize(value);

  if (!fromFloat) {
    uint32_t src = machineBuilderVreg(b, value);

    MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
    setRegisterOperand(test, 0, src);
    setRegisterOperand(test, 1, src);
    test->opSize = size;

    // _Bool is one byte, so there are no upper bytes for the setcc to leave
    // behind and no zeroing move is needed - see selectZeroExtendedSetup.
    selectSetcc(b, X86_SETNE, dst, FALSE);
    return;
  }

  uint32_t zero = selectFloatZero(b, size);

  MachineInstr *cmp = buildMachineInstr(b, X86_FUCMP, 0, 2);
  setRegisterOperand(cmp, 0, machineBuilderVreg(b, value));
  setRegisterOperand(cmp, 1, zero);
  cmp->opSize = size;

  selectSetcc(b, X86_SETNE, dst, FALSE);

  // A NaN is not equal to zero, so (_Bool)NaN is 1 - but an unordered compare
  // sets the zero flag, which alone would say otherwise. Same shape as the
  // '!=' in selectFloatCompare: not-equal *or* unordered.
  uint32_t unordered = createVirtualRegister(b->mf, RC_GP, 1);
  selectSetcc(b, X86_SETP, unordered, FALSE);

  MachineInstr *mi = buildMachineInstr(b, X86_OR, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, unordered);
  mi->opSize = 1;
}

static void selectConversion(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind fromType = i->info.fromCastType;
  enum IrTypeKind toType = i->type;
  uint8_t toSize = valueSize(i);
  const IrInstruction *value = inputAt(i, 0);
  uint32_t dst = machineBuilderVreg(b, i);

  Boolean fromFloat = isFloatIrType(fromType);
  Boolean toFloat = isFloatIrType(toType);

  if (fromType == IR_F80 || toType == IR_F80 || fromType == IR_P_AGG || toType == IR_P_AGG) {
    buildUnselected(b, i, "converts to or from a type with no register class");
    return;
  }

  // A conversion to _Bool is the one conversion that is not a change of
  // width: C defines it as 'x != 0', so (_Bool)0x100 is 1 rather than the low
  // byte of 0x100, which is what truncating would give.
  if (toType == IR_BOOL) {
    selectBooleanConversion(b, i, dst, value, fromFloat);
    return;
  }

  // Integer to integer. Widening is the only one that costs an instruction;
  // narrowing is a copy, because the low bytes of the register are already the
  // answer. selectLoadInto covers both, and picks the extension from the
  // *source's* signedness, which is what C says a conversion does.
  if (!fromFloat && !toFloat) {
    selectLoadInto(b, dst, value, toSize);
    return;
  }

  if (fromFloat && toFloat) {
    // float <-> double. Not a widening in the integer sense - the bits change
    // completely - so it is its own instruction rather than a move.
    MachineInstr *mi = buildMachineInstr(b, X86_CVTF2F, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
    mi->opSize = toSize;
    mi->srcSize = valueSize(value);
    return;
  }

  if (toFloat) {
    if (!isConvertibleIntType(fromType)) {
      buildUnselected(b, i, "converts an unsigned 64-bit integer to a float");
      return;
    }

    uint8_t srcSize = conversionIntSize(valueSize(value));
    // An unsigned source is widened to eight bytes and then converted as a
    // signed one, which is exact: no unsigned 32-bit value is negative as a
    // signed 64-bit one.
    if (isUnsignedIrOperand(fromType)) {
      srcSize = sizeof(intptr_t);
    }

    uint32_t src = selectWidened(b, value, srcSize);

    MachineInstr *mi = buildMachineInstr(b, X86_CVTSI2F, 1, 1);
    setRegisterOperand(mi, 0, dst);
    setRegisterOperand(mi, 1, src);
    mi->opSize = toSize;
    mi->srcSize = srcSize;
    return;
  }

  if (!isConvertibleIntType(toType)) {
    buildUnselected(b, i, "converts a float to an unsigned 64-bit integer");
    return;
  }

  // Float to integer, truncating toward zero, which is what a C cast does.
  // The instruction writes four or eight bytes; a narrower destination takes
  // the low ones, and since the value is in range by C's rules whenever the
  // result is defined at all, that truncation is the conversion.
  // An unsigned 32-bit destination goes through the 64-bit form. The 32-bit
  // one converts to a *signed* int and answers 0x80000000 for anything that
  // does not fit - and half of the unsigned range does not. Converting to a
  // signed 64-bit value instead is exact for every unsigned 32-bit result, and
  // the low four bytes of it are the answer.
  Boolean wideForUnsigned = isUnsignedIrOperand(toType) && toSize == 4;

  MachineInstr *mi = buildMachineInstr(b, X86_CVTF2SI, 1, 1);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, machineBuilderVreg(b, value));
  mi->opSize = wideForUnsigned ? sizeof(intptr_t) : conversionIntSize(toSize);
  mi->srcSize = valueSize(value);
}

// -============================ Calls ============================-
//
// SysV AMD64, the scalar half of it. An IR_CALL's inputs are the callee, then
// the hidden return buffer when the ABI needs one, then the arguments in
// order. Each argument goes to the next free register of its class - rdi, rsi,
// rdx, rcx, r8, r9 for integers and pointers, xmm0..xmm7 for floats - and once
// a class runs out, the rest of *that class* goes on the stack. Aggregates and
// long double are not classified here at all; see canSelectCall.
//
// The classification is written as two walks rather than as a table filled in
// once, because both callers want a different slice of it and neither wants to
// own an allocation: selection asks where one argument goes, the immediate
// rule asks the same about one operand, and the totals are needed before
// either. Calls have a handful of arguments, so walking the list again is
// cheaper than the array would be.

// Whether input 'idx' is an aggregate the ABI passes as bytes on the stack.
//
// Read from the mask translateCall filled in, because nothing about the
// instruction says it: such an argument and a genuine pointer argument are the
// same IR_PTR here, the temporary the bytes were copied into being the only
// difference and not one this file can see. See IrInstruction.info.call.
static Boolean callArgInMemory(const IrInstruction *call, size_t idx) {
  return idx < 8 * sizeof(call->info.call.memArgs) &&
         (call->info.call.memArgs & ((uint64_t)1 << idx)) != 0;
}

// The eightbytes a memory argument occupies, which is its size rounded up.
//
// The same count the callee advances its incoming cursor by: every parameter
// is aligned to at least eight bytes there, so the bytes above an aggregate
// that does not fill its last eightbyte are padding on both sides. See
// classifyParametersGeneric.
static uint32_t memArgStackSlots(const IrInstruction *arg) {
  // The mask is the authority on which inputs get here; astType only supplies
  // the size, and translateCall sets the two together.
  assert(arg->astType != NULL && isPointerLikeType(arg->astType));

  const TypeRef *type = arg->astType->pointed;
  int32_t size = computeTypeSize(type);

  return (uint32_t)((size + sizeof(intptr_t) - 1) / sizeof(intptr_t));
}

static size_t firstCallArgIndex(const IrInstruction *call) {
  // Input 0 is the callee; everything after it is an argument. When the call
  // returns a large struct, input 1 is the hidden buffer pointer - and it is
  // classified and placed like any other pointer argument, which is exactly
  // what SysV says to do with it, so it needs no case of its own here. That
  // it lands in rdi and pushes the real arguments along by one register is
  // then just the classification walk doing its job.
  return 1;
}

static enum RegClass callArgClass(const IrInstruction *arg) {
  return isFloatIrType(arg->type) ? RC_FP : RC_GP;
}

static uint32_t argRegCountOf(const TargetDescriptor *target, enum RegClass rc) {
  return rc == RC_FP ? target->fpArgRegCount : target->intArgRegCount;
}

static uint32_t argRegOf(const TargetDescriptor *target, enum RegClass rc, uint32_t idx) {
  return rc == RC_FP ? target->fpArgRegs[idx] : target->intArgRegs[idx];
}

// Where the argument at input position 'inputIdx' is passed: an argument
// register of its class, or NO_REG when that class has run out and it goes on
// the stack.
static uint32_t callArgLocation(const TargetDescriptor *target, const IrInstruction *call,
                                size_t inputIdx) {
  assert(inputIdx >= firstCallArgIndex(call) && inputIdx < call->inputs.size);

  uint32_t used[RC_CLASS_COUNT] = {0};

  for (size_t idx = firstCallArgIndex(call);; ++idx) {
    uint32_t reg = NO_REG;

    // A memory argument consumes no register of either class - it is on the
    // stack because of what it is and not because a class ran out, so the
    // arguments after it are unaffected.
    if (!callArgInMemory(call, idx)) {
      enum RegClass rc = callArgClass(inputAt(call, idx));
      reg = used[rc] < argRegCountOf(target, rc) ? argRegOf(target, rc, used[rc]++) : NO_REG;
    }

    if (idx == inputIdx) {
      return reg;
    }
  }
}

// How many SSE registers the call passes arguments in - which is what all has
// to hold for a variadic callee - how many arguments go in registers, and how
// many eightbytes of stack the rest need.
//
// Eightbytes rather than arguments: an aggregate passed in memory takes as
// many as it has, and the padding above it is part of what the callee expects.
static void callArgCounts(const TargetDescriptor *target, const IrInstruction *call,
                          uint32_t *numFpRegs, uint32_t *numRegArgs, uint32_t *numStackSlots) {
  uint32_t used[RC_CLASS_COUNT] = {0};
  uint32_t inRegs = 0;
  uint32_t onStack = 0;

  for (size_t idx = firstCallArgIndex(call); idx < call->inputs.size; ++idx) {
    if (callArgInMemory(call, idx)) {
      onStack += memArgStackSlots(inputAt(call, idx));
      continue;
    }

    enum RegClass rc = callArgClass(inputAt(call, idx));

    if (used[rc] < argRegCountOf(target, rc)) {
      used[rc] += 1;
      inRegs += 1;
    } else {
      onStack += 1;
    }
  }

  *numFpRegs = used[RC_FP];
  *numRegArgs = inRegs;
  *numStackSlots = onStack;
}

// The stack pointer moves twice around a call with stack arguments: down to
// make room and back up afterwards. Written as an ordinary two-address add or
// sub over a physical register, which allocation leaves alone and emission
// already knows how to encode.
static void selectStackAdjust(MachineBuilder *b, uint32_t opcode, int64_t bytes) {
  MachineInstr *mi = buildMachineInstr(b, opcode, 1, 2);
  uint32_t sp = b->mf->target->sp;

  setRegisterOperand(mi, 0, sp);
  setRegisterOperand(mi, 1, sp);
  setImmediateOperand(mi, 2, bytes);
  mi->opSize = sizeof(intptr_t);
}

// Give a struct returned in rax somewhere to live, and produce its address.
//
// The two halves of the composite-return convention are not symmetrical. A
// struct too big for a register is written by the callee into a buffer the
// caller passed the address of, so by the time the call returns the value is
// already in memory the IR allocated and named. One that fits is handed back
// in rax as bytes, and there is no such memory anywhere - the IR did not ask
// for any, because at IR level a call returning a struct is just an
// instruction of type IR_P_AGG and where the target keeps it is the target's
// business. This is where that gets settled: eight bytes of frame, the whole
// register stored into them, and the slot's address as the call's value.
//
// A slot per call site rather than one buffer per function - which is what the
// legacy backend uses, and gets away with. Translation emits the load out of
// the returned struct straight after the call that produced it, so the bytes
// are in a register again before the next call can land on them, and today
// nothing would notice the difference. That is a property of how ast2ir orders
// what it emits rather than anything this file can check, and the eight bytes
// a slot costs are cheaper than depending on it.
static void selectRegisterReturnedStruct(MachineBuilder *b, const IrInstruction *i) {
  MachineFunction *mf = b->mf;

  // A whole eightbyte, whatever the struct's own size: rax is stored in full
  // because storing part of it would need the size rounded to something
  // encodable, and the bytes above the struct are ours either way.
  int32_t frameIdx =
      addMachineFrameObject(mf, MFO_CALL_RESULT, sizeof(intptr_t), sizeof(intptr_t));

  mf->frame.size = (uint32_t)ALIGN_SIZE(
      placeMachineFrameObject(mf, (int32_t)mf->frame.size, frameIdx), 2 * sizeof(intptr_t));

  MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
  setFrameAddressOperand(store, 0, frameIdx);
  setRegisterOperand(store, 1, mf->target->intRetReg);
  store->opSize = sizeof(intptr_t);

  MachineInstr *addr = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(addr, 0, machineBuilderVreg(b, i));
  setFrameAddressOperand(addr, 1, frameIdx);
  addr->opSize = sizeof(intptr_t);
}

// Put an aggregate argument on the stack, one eightbyte at a time.
//
// The push *is* the copy the ABI asks for - there is no outgoing area to store
// into, and none is wanted here: the callee reads the bytes where the call
// left them, so a struct that spans three eightbytes is three pushes and the
// stack pointer does the arithmetic. Highest eightbyte first, because the rest
// of the argument list is pushed backwards too and the struct has to come out
// the right way up.
//
// Reading up to seven bytes past the end of the struct is deliberate and safe:
// the source is the temporary translateCall copied into, and createAllocaSlot
// rounds a slot to a whole eightbyte. The bytes above the struct are padding
// the callee's own classification skips.
//
// Through a register rather than 'push [mem]', which exists: the trivial
// allocator gives the load's result a frame slot and reloads it for the push,
// and a frame slot is addressed off rbp - so it survives the stack pointer
// moving underneath it, which is the only thing that could go wrong here.
static void selectMemoryArgument(MachineBuilder *b, const IrInstruction *arg) {
  uint32_t slots = memArgStackSlots(arg);

  // Once, outside the loop, for the reason selectMemoryCopy takes both of its
  // addresses once: an address that had to widen an index would otherwise
  // widen it again per eightbyte.
  MachineAddress from = addressFor(b, arg, 0);

  // Reused across eightbytes for selectMemoryCopy's reason: a register apiece
  // would put as many frame slots under the call as the argument is wide.
  uint32_t tmp = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  for (uint32_t slot = slots; slot > 0; --slot) {
    MachineAddress chunk = from;
    chunk.disp += (int32_t)((slot - 1) * sizeof(intptr_t));

    MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
    setRegisterOperand(load, 0, tmp);
    setMemoryOperand(load, 1, &chunk);
    load->opSize = sizeof(intptr_t);

    MachineInstr *push = buildMachineInstr(b, X86_PUSH, 0, 1);
    setRegisterOperand(push, 0, tmp);
    push->opSize = sizeof(intptr_t);
  }
}

static void selectCall(MachineBuilder *b, const IrInstruction *i) {
  const TargetDescriptor *target = b->mf->target;

  const char *refusal = callRefusalReason(target, i);

  if (refusal != NULL) {
    buildUnselected(b, i, refusal);
    return;
  }

  uint32_t numFpRegs = 0, numRegArgs = 0, numStackSlots = 0;
  callArgCounts(target, i, &numFpRegs, &numRegArgs, &numStackSlots);

  // SysV wants rsp 16-byte aligned when the call executes. It already is
  // everywhere else in the function - the entry misalignment is undone by
  // pushing rbp, and every frame size stage 3 subtracts is rounded to 16 - so
  // the only thing that can break it is an odd number of eight-byte arguments,
  // and eight bytes of padding above them puts it back.
  uint32_t padding = (numStackSlots & 1) != 0 ? sizeof(intptr_t) : 0;
  uint32_t stackBytes = numStackSlots * sizeof(intptr_t) + padding;

  if (padding != 0) {
    selectStackAdjust(b, X86_SUB, padding);
  }

  // Backwards, so that the last stack argument is pushed first and the first
  // one ends up at [rsp] where the callee looks for it.
  for (size_t idx = i->inputs.size; idx > firstCallArgIndex(i); --idx) {
    const IrInstruction *arg = inputAt(i, idx - 1);

    if (callArgLocation(target, i, idx - 1) != NO_REG) {
      continue;
    }

    if (callArgInMemory(i, idx - 1)) {
      selectMemoryArgument(b, arg);
      continue;
    }

    // Always a register: x86IsLegalImmediate folds a constant only into an
    // argument that is passed in one, exactly so that this cannot be an
    // immediate - the assembler has no push of one.
    MachineInstr *push = buildMachineInstr(b, X86_PUSH, 0, 1);
    setRegisterOperand(push, 0, machineBuilderVreg(b, arg));
    // A stack argument occupies a whole eightbyte however narrow it is, and
    // push is the instruction that says so.
    push->opSize = sizeof(intptr_t);
  }

  for (size_t idx = firstCallArgIndex(i); idx < i->inputs.size; ++idx) {
    uint32_t reg = callArgLocation(target, i, idx);

    if (reg != NO_REG) {
      // At the argument's own width, not widened: SysV leaves the bytes above
      // a narrow argument unspecified, and the callee knows its own prototype.
      const IrInstruction *arg = inputAt(i, idx);
      selectLoadInto(b, reg, arg, valueSize(arg));
    }
  }

  if (i->info.call.isVariadic) {
    // al, not rax: a variadic callee reads the number of SSE registers used
    // out of the low byte, and writing only that byte leaves the rest of rax
    // alone - which matters not at all here, and is what the ABI says.
    MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mi, 0, target->intRetReg);
    setImmediateOperand(mi, 1, numFpRegs);
    mi->opSize = 1;
  }

  // Defs: the return register, when the call produces something. Uses: the
  // callee, then every argument register the copies above filled in. The
  // argument registers are implicit because the instruction does not name them
  // - they are where the ABI says the arguments are - but they have to be in
  // the operand list all the same, or nothing connects the copies to the call
  // and liveness is free to conclude they are dead.
  Boolean hasResult = i->type != IR_VOID;
  uint16_t numArgRegs = (uint16_t)numRegArgs;

  MachineInstr *call = buildMachineInstr(b, X86_CALL, hasResult ? 1 : 0, 1 + numArgRegs);
  uint16_t op = 0;

  if (hasResult) {
    setRegisterOperand(call, op, isFloatIrType(i->type) ? target->fpRetReg : target->intRetReg);
    machineOperandAt(call, op)->flags.isImplicit = 1;
    op += 1;
  }

  // A folded symbol constant becomes the relocated call target; anything else
  // is a register holding the address, and the call goes through it.
  setValueOperand(b, call, op++, inputAt(i, 0));

  for (size_t idx = firstCallArgIndex(i); idx < i->inputs.size; ++idx) {
    uint32_t reg = callArgLocation(target, i, idx);

    if (reg != NO_REG) {
      setRegisterOperand(call, op, reg);
      machineOperandAt(call, op)->flags.isImplicit = 1;
      op += 1;
    }
  }

  assert(op == call->numOperands);
  call->flags.isCall = 1;

  if (stackBytes != 0) {
    selectStackAdjust(b, X86_ADD, stackBytes);
  }

  if (i->type == IR_P_AGG && i->info.call.returnBuffer == NULL) {
    // A struct small enough to come back in a register. Everything downstream
    // of the call reads a composite as an *address* - that is what IR_P_AGG
    // means - so the bytes have to be given one, and giving them a frame slot
    // is the whole of it. The buffered case needs none of this: there the
    // callee has already written the struct into the slot the IR allocated,
    // and returns that same address in rax.
    selectRegisterReturnedStruct(b, i);
  } else if (hasResult) {
    // Straight back out of the fixed register into one the allocator can move,
    // for the same reason a parameter is copied out of the register it arrives
    // in - see selectPhysReg.
    MachineInstr *out = buildMachineInstr(b, MOP_COPY, 1, 1);
    setRegisterOperand(out, 0, machineBuilderVreg(b, i));
    setRegisterOperand(out, 1, isFloatIrType(i->type) ? target->fpRetReg : target->intRetReg);
    out->opSize = valueSize(i);
  }
}

// -============================ Terminators ============================-

static void selectJumpTo(MachineBuilder *b, const IrBasicBlock *target) {
  // A jump to the block that comes next is not a jump.
  if (machineBuilderFallsThroughTo(b, target)) {
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, X86_JMP, 0, 1);
  setBlockOperand(mi, 0, machineBuilderBlock(b, target));
}

static void selectBranch(MachineBuilder *b, const IrInstruction *i) {
  selectJumpTo(b, i->info.branch.taken);
}

// The condition a branch reads, and the instructions that set it up.
//
// When the compare feeding the branch was absorbed, this *is* the compare:
// there is no boolean, and the flags it leaves are what the jcc reads. When
// there is a boolean - because something else reads it too, or because the
// value came from somewhere other than a comparison - the branch tests it
// against itself, which is the same question asked of a register instead.
static uint32_t selectBranchCondition(MachineBuilder *b, const IrInstruction *cond) {
  if (machineBuilderIsAbsorbed(b, cond)) {
    // Attributed to the compare rather than to the branch, since that is the
    // instruction a reader of the dump is looking for.
    const IrInstruction *branch = b->origin;
    b->origin = cond;

    uint32_t cc = isFloatIrType(inputAt(cond, 0)->type) ? emitFloatCompare(b, cond)
                                                        : emitIntegerCompare(b, cond);

    b->origin = branch;
    return cc;
  }

  uint32_t condReg = machineBuilderVreg(b, cond);

  MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
  setRegisterOperand(test, 0, condReg);
  setRegisterOperand(test, 1, condReg);
  test->opSize = valueSize(cond);

  return X86_SETNE;
}

static void selectCondBranch(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *cond = inputAt(i, 0);
  const IrBasicBlock *taken = i->info.branch.taken;
  const IrBasicBlock *notTaken = i->info.branch.notTaken;
  uint32_t cc = selectBranchCondition(b, cond);

  // Whichever way the layout fell, one of the two successors is next and needs
  // no branch. Block layout arranges for that to be the not-taken one wherever
  // it can (see layoutBlocks), so the first arm is the usual one.
  if (machineBuilderFallsThroughTo(b, notTaken)) {
    MachineInstr *jcc = buildMachineInstr(b, jumpOpcodeFor(cc), 0, 1);
    setBlockOperand(jcc, 0, machineBuilderBlock(b, taken));
    return;
  }

  if (machineBuilderFallsThroughTo(b, taken)) {
    // Branch on the condition being false instead, and fall into the taken
    // block. Inverting is free; jumping over a jump is not.
    MachineInstr *jcc = buildMachineInstr(b, jumpOpcodeFor(invertedCondition(cc)), 0, 1);
    setBlockOperand(jcc, 0, machineBuilderBlock(b, notTaken));
    return;
  }

  // Neither is next, so both need a jump. No fixture reaches this and none can
  // today: ast2ir gives every conditional branch two blocks created at the
  // moment the branch is, nothing merges an empty block away afterwards, and a
  // block with a single predecessor cannot have been reached by the layout walk
  // before the branch that dominates it. So the second successor is always
  // still unvisited and always lands next. The arm stays because that is a
  // property of the frontend rather than of this file - merging empty blocks,
  // or ordering by profitability instead of by reverse postorder, reaches it
  // immediately. See docs/ir-codegen-design.md section 10.
  MachineInstr *jcc = buildMachineInstr(b, jumpOpcodeFor(cc), 0, 1);
  setBlockOperand(jcc, 0, machineBuilderBlock(b, taken));

  MachineInstr *jmp = buildMachineInstr(b, X86_JMP, 0, 1);
  setBlockOperand(jmp, 0, machineBuilderBlock(b, notTaken));
}

// The other half of the ABI, and deliberately the same shape as an argument:
// the value is loaded into the one register the ABI reads it out of, and the
// allocator is left with nothing to decide. selectLoadInto is what both use,
// which is what lets a constant return be an immediate - 'return 42' is a
// 'mov eax, 42' and not a register, a spill slot and a reload of one.
//
// A composite needs no case here. The translator gives the function a return
// slot whose contents are what the ABI hands back - the caller's buffer
// pointer for a large one, the eightbyte the value travels in for a small one
// - and the exit block reads it out, so what arrives here is already a value
// bound for rax. See generateExitBlock in src/ir/ast2ir.c.
static void selectReturn(MachineBuilder *b, const IrInstruction *i) {
  if (i->inputs.size != 0) {
    const IrInstruction *value = inputAt(i, 0);

    if (value->type == IR_F80) {
      buildUnselected(b, i, "returns a long double, which lives on the x87 stack");
      return;
    }

    selectLoadInto(b, isFloatIrType(value->type) ? b->mf->target->fpRetReg
                                                 : b->mf->target->intRetReg,
                   value, valueSize(value));
  }

  // Just the return. The prologue and epilogue around it are stage 3's, which
  // is the first point at which the frame size and the callee-saved registers
  // actually used are both known.
  buildMachineInstr(b, X86_RET, 0, 0);
}

// -============================ Switches ============================-
//
// A switch is one IR instruction with n+1 successors, and x86 has no
// instruction that branches n ways. It becomes either a chain of compares or a
// jump through a table, and which one is a property of the case *values*
// rather than of the target: a table indexed by the condition is only possible
// when the values it has to cover form a range small enough to build one for.
//
// The IR calls this a table branch throughout, which it is not - the frontend
// takes no view on how it is dispatched, and the legacy backend emits a chain
// for every switch there is.

// One case: compare, and jump if equal. The condition stays in its register
// across the whole chain, so the only instruction per case is this pair.
static void selectCaseTest(MachineBuilder *b, uint32_t condReg, uint8_t size, int64_t value,
                           const IrBasicBlock *target) {
  uint32_t valueReg = NO_REG;

  // An ALU immediate is 32 bits sign-extended to the operand width, so a case
  // value outside that range needs a register of its own. Only a switch on a
  // 64-bit type can reach it.
  if (value < INT32_MIN || value > INT32_MAX) {
    valueReg = createVirtualRegister(b->mf, RC_GP, size);

    MachineInstr *mov = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mov, 0, valueReg);
    setImmediateOperand(mov, 1, value);
    mov->opSize = size;
  }

  MachineInstr *cmp = buildMachineInstr(b, X86_CMP, 0, 2);
  setRegisterOperand(cmp, 0, condReg);
  if (valueReg != NO_REG) {
    setRegisterOperand(cmp, 1, valueReg);
  } else {
    setImmediateOperand(cmp, 1, value);
  }
  cmp->opSize = size;

  MachineInstr *je = buildMachineInstr(b, X86_JE, 0, 1);
  setBlockOperand(je, 0, machineBuilderBlock(b, target));
}

// The universal lowering: ask about every case in turn and fall out into the
// default. Linear in the number of cases both to emit and to run, which is
// what the table below is for - but it needs nothing of the values at all, so
// it is what a switch too sparse to tabulate gets.
static void selectSwitchChain(MachineBuilder *b, const IrInstruction *i, uint32_t condReg) {
  const SwitchTable *table = i->info.switchTable;
  uint8_t size = valueSize(inputAt(i, 0));

  for (uint32_t c = 0; c < table->caseCount; ++c) {
    selectCaseTest(b, condReg, size, table->caseBlocks[c].caseConst, table->caseBlocks[c].block);
  }

  selectJumpTo(b, table->defaultBB);
}

// When a table is worth building. Two independent questions:
//
// Size - the table costs eight bytes per value in the range whether a case
// lands on it or not, so a switch on a handful of scattered constants would
// spend kilobytes to save a few compares. The density floor is what stops
// that, and the absolute cap is what stops a dense but enormous range
// ('case 0' and 'case 100000') from doing the same.
//
// Speed - the dispatch below is seven instructions regardless of how many
// cases there are, against two per case for the chain, so a table only starts
// paying somewhere around four. Below that it is bigger *and* no faster.
#define JUMP_TABLE_MIN_CASES 5
#define JUMP_TABLE_MAX_ENTRIES 4096
#define JUMP_TABLE_MIN_DENSITY 3 // at least one case per this many entries

// The range a table would have to cover, or FALSE when there is no usable one.
// Both ends are held as int64 and the span as int64 too, because 'case
// INT64_MIN' and 'case INT64_MAX' in one switch overflow every narrower type
// on the way to being rejected.
static Boolean switchTableRange(const SwitchTable *table, int64_t *min, int64_t *span) {
  if (table->caseCount < JUMP_TABLE_MIN_CASES) {
    return FALSE;
  }

  int64_t lo = table->caseBlocks[0].caseConst;
  int64_t hi = lo;

  for (uint32_t c = 1; c < table->caseCount; ++c) {
    int64_t value = table->caseBlocks[c].caseConst;
    if (value < lo) lo = value;
    if (value > hi) hi = value;
  }

  // As unsigned, so that a range spanning the whole signed axis does not wrap
  // to something small and pass the tests below.
  uint64_t entries = (uint64_t)hi - (uint64_t)lo + 1;

  if (entries > JUMP_TABLE_MAX_ENTRIES ||
      entries > (uint64_t)table->caseCount * JUMP_TABLE_MIN_DENSITY) {
    return FALSE;
  }

  // 'cond - lo' is subtracted as an immediate and the span compared against
  // one, and both encodings are 32 bits.
  if (lo < INT32_MIN || lo > INT32_MAX || entries > INT32_MAX) {
    return FALSE;
  }

  *min = lo;
  *span = (int64_t)entries;
  return TRUE;
}

// The condition normalized to an index into the table: widened to a full
// register, then shifted down so that the lowest case is zero.
//
// Widening is what makes the range check below a single unsigned compare. Once
// the value is 64 bits and biased by the lowest case, everything before the
// first case has wrapped round to an enormous unsigned number and everything
// after the last is simply too big, so one 'ja' turns both into the default.
static uint32_t selectTableIndex(MachineBuilder *b, const IrInstruction *cond, int64_t min) {
  uint32_t widened = selectWidened(b, cond, sizeof(intptr_t));

  if (min == 0) {
    return widened;
  }

  uint32_t index = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  MachineInstr *copy = buildMachineInstr(b, MOP_COPY, 1, 1);
  setRegisterOperand(copy, 0, index);
  setRegisterOperand(copy, 1, widened);
  copy->opSize = sizeof(intptr_t);

  MachineInstr *sub = buildMachineInstr(b, X86_SUB, 1, 2);
  setRegisterOperand(sub, 0, index);
  setRegisterOperand(sub, 1, index);
  setImmediateOperand(sub, 2, min);
  sub->opSize = sizeof(intptr_t);

  return index;
}

// The table itself: one entry per value of the range, holding the block that
// value dispatches to, with the default filling everything no case claims.
static uint32_t buildJumpTable(MachineBuilder *b, const SwitchTable *table, int64_t min,
                               int64_t span) {
  MachineBasicBlock **entries = heapAllocate((size_t)span * sizeof(MachineBasicBlock *));
  MachineBasicBlock *fallback = machineBuilderBlock(b, table->defaultBB);

  for (int64_t idx = 0; idx < span; ++idx) {
    entries[idx] = fallback;
  }

  for (uint32_t c = 0; c < table->caseCount; ++c) {
    entries[table->caseBlocks[c].caseConst - min] = machineBuilderBlock(b, table->caseBlocks[c].block);
  }

  uint32_t jumpTableIdx = addMachineJumpTable(b->mf, entries, (uint32_t)span);
  releaseHeap(entries);

  return jumpTableIdx;
}

// The dispatch: bounds-check, then jump through the table.
//
// Entries are distances from the table to their block rather than addresses,
// which is why the base register is both what the load is indexed off and what
// the loaded value is added to. An address would have to be relocated - it is
// only known once the program is loaded - whereas a distance between two
// points of the same section is known as soon as both have been emitted, and
// the table is emitted after everything it names.
static void selectSwitchTable(MachineBuilder *b, const IrInstruction *i, int64_t min,
                              int64_t span) {
  const SwitchTable *table = i->info.switchTable;
  uint32_t index = selectTableIndex(b, inputAt(i, 0), min);

  MachineInstr *cmp = buildMachineInstr(b, X86_CMP, 0, 2);
  setRegisterOperand(cmp, 0, index);
  setImmediateOperand(cmp, 1, span - 1);
  cmp->opSize = sizeof(intptr_t);

  MachineInstr *ja = buildMachineInstr(b, X86_JA, 0, 1);
  setBlockOperand(ja, 0, machineBuilderBlock(b, table->defaultBB));

  MachineAddress tableAddr = { MAK_JUMPTABLE, NO_REG, NO_REG, 0, 0 };
  tableAddr.anchor.jumpTableIdx = buildJumpTable(b, table, min, span);

  uint32_t base = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));
  MachineInstr *lea = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(lea, 0, base);
  setMemoryOperand(lea, 1, &tableAddr);
  lea->opSize = sizeof(intptr_t);

  MachineAddress entryAddr = { MAK_REG, base, index, sizeof(intptr_t), 0 };
  uint32_t target = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
  setRegisterOperand(load, 0, target);
  setMemoryOperand(load, 1, &entryAddr);
  load->opSize = sizeof(intptr_t);

  MachineInstr *add = buildMachineInstr(b, X86_ADD, 1, 2);
  setRegisterOperand(add, 0, target);
  setRegisterOperand(add, 1, target);
  setRegisterOperand(add, 2, base);
  add->opSize = sizeof(intptr_t);

  MachineInstr *jmp = buildMachineInstr(b, X86_IJMP, 0, 1);
  setRegisterOperand(jmp, 0, target);
  jmp->opSize = sizeof(intptr_t);
}

static void selectTableBranch(MachineBuilder *b, const IrInstruction *i) {
  int64_t min = 0, span = 0;

  if (switchTableRange(i->info.switchTable, &min, &span)) {
    selectSwitchTable(b, i, min, span);
    return;
  }

  selectSwitchChain(b, i, machineBuilderVreg(b, inputAt(i, 0)));
}

// -============================ Computed goto ============================-
//
// '&&label' is the address of a block of this same function, which is a thing
// only the emitter can put a number on - so it is carried to stage 3 as the
// block itself and resolved there, exactly as a branch target is. That it
// arrives as an ordinary rip-relative lea is what makes the result a real
// pointer: it survives being stored, passed and returned, which is the whole
// point of taking it.

static void selectBlockAddress(MachineBuilder *b, const IrInstruction *i) {
  MachineAddress addr = { MAK_BLOCK, NO_REG, NO_REG, 0, 0 };
  addr.anchor.block = machineBuilderBlock(b, i->info.block);

  MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = sizeof(intptr_t);
}

static void selectIndirectBranch(MachineBuilder *b, const IrInstruction *i) {
  MachineInstr *mi = buildMachineInstr(b, X86_IJMP, 0, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, inputAt(i, 0)));
  mi->opSize = sizeof(intptr_t);
}

// -============================ Dispatch ============================-

static void selectInstruction_x86_64(MachineBuilder *b, const IrInstruction *i) {
  int32_t frameIdx = machineFrameIndexForValue(b->mf, i);

  if (frameIdx >= 0) {
    selectFrameAddress(b, i, frameIdx);
    return;
  }

  switch (i->kind) {
  case IR_DEF_CONST: selectConstant(b, i); break;
  case IR_P_REG: selectPhysReg(b, i); break;

  case IR_E_ADD: selectBinary(b, i, X86_ADD); break;
  case IR_E_SUB: selectBinary(b, i, X86_SUB); break;
  case IR_E_MUL: selectBinary(b, i, X86_IMUL); break;
  case IR_E_AND: selectBinary(b, i, X86_AND); break;
  case IR_E_OR: selectBinary(b, i, X86_OR); break;
  case IR_E_XOR: selectBinary(b, i, X86_XOR); break;

  case IR_E_SHL: selectShift(b, i, X86_SHL); break;
  // Right shift is where signedness stops being cosmetic: an arithmetic shift
  // keeps the sign bit, a logical one does not.
  case IR_E_SHR:
    selectShift(b, i, isUnsignedIrOperand(i->type) ? X86_SHR : X86_SAR);
    break;

  case IR_E_DIV: selectDivMod(b, i, FALSE); break;
  case IR_E_MOD: selectDivMod(b, i, TRUE); break;

  case IR_U_BNOT: selectBitwiseNot(b, i); break;
  case IR_U_NOT: selectLogicalNot(b, i); break;

  // IR_ALLOCA is not here: every one of them has a frame slot and was taken
  // by selectFrameAddress above, dynamic ones included.
  case IR_STACK_SAVE: selectStackSave(b, i); break;
  case IR_STACK_RESTORE: selectStackRestore(b, i); break;

  case IR_GET_ELEMENT_PTR: selectGep(b, i); break;
  case IR_M_LOAD: selectMemoryLoad(b, i); break;
  case IR_M_STORE: selectMemoryStore(b, i); break;
  case IR_M_COPY: selectMemoryCopy(b, i); break;

  // Both spellings reach the same rule: what distinguishes a direct call from
  // an indirect one is whether the callee operand came out a symbol or a
  // register, and setValueOperand decides that from the value itself.
  case IR_CALL:
  case IR_ICALL:
    selectCall(b, i);
    break;

  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    selectCompare(b, i);
    break;

  case IR_E_FADD: selectFloatBinary(b, i, X86_FADD); break;
  case IR_E_FSUB: selectFloatBinary(b, i, X86_FSUB); break;
  case IR_E_FMUL: selectFloatBinary(b, i, X86_FMUL); break;
  case IR_E_FDIV: selectFloatBinary(b, i, X86_FDIV); break;

  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    selectFloatCompare(b, i);
    break;

  case IR_E_BITCAST: selectConversion(b, i); break;
  case IR_CFG_LABEL: selectBlockAddress(b, i); break;

  // The placeholder buildSSA leaves where a promoted local is read on a path
  // that never wrote it - see renameLocals in src/ir/ssa.c. Its value is
  // whatever C says an uninitialized object holds, which is to say anything,
  // so every instruction that defines the register is a correct one. Zero is
  // the one that makes the resulting misbehaviour reproducible rather than
  // dependent on what the previous function happened to leave behind.
  case IR_BAD: {
    MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
    setImmediateOperand(mi, 1, 0);
    mi->opSize = valueSize(i);
    break;
  }

  // Floats, casts, aggregate copies. Each of these is a step of its own in
  // docs/ir-codegen-design.md section 11, and until then a placeholder is more
  // useful than an abort: the rest of the function still selects, the dump
  // names exactly what is missing, and buildUnselected says so out loud.
  default:
    buildUnselected(b, i, "no rule yet");
    break;
  }
}

static void selectTerminator_x86_64(MachineBuilder *b, const IrInstruction *i) {
  switch (i->kind) {
  case IR_BRANCH: selectBranch(b, i); break;
  case IR_CBRANCH: selectCondBranch(b, i); break;
  case IR_RET: selectReturn(b, i); break;
  case IR_TBRANCH: selectTableBranch(b, i); break;
  case IR_IBRANCH: selectIndirectBranch(b, i); break;

  default:
    buildUnselected(b, i, "no rule yet");
    break;
  }
}

// -============================ Immediates ============================-

static Boolean x86IsLegalImmediate(const IrInstruction *use, size_t operandIdx,
                                   const IrInstruction *cnst) {
  Boolean isCall = use->kind == IR_CALL || use->kind == IR_ICALL;

  // The address of a directly called function, which the call encodes as a
  // relocated displacement rather than reading out of a register. Input 0 of a
  // call and nowhere else: every other use of a function's address needs it
  // materialized, and there is no rule for that yet.
  if (cnst->info.constant.kind == IR_CK_SYMBOL) {
    return isCall && operandIdx == 0;
  }

  // Both ends of the ABI take one. An argument passed in a register and a
  // returned value are each loaded with a plain move into a fixed register,
  // and a move into a register can carry any width of constant - the wide ones
  // as the movabs emitMoveCR falls back to - so there is no range test here
  // where the ALU forms below need one.
  //
  // An argument passed on the *stack* is the exception: that one is pushed,
  // the assembler has no push of an immediate, and folding it would leave
  // selectCall with an operand it cannot place.
  if (isCall) {
    return operandIdx >= firstCallArgIndex(use) &&
           callArgLocation(&targetX86_64, use, operandIdx) != NO_REG;
  }

  if (use->kind == IR_RET) {
    return TRUE;
  }

  // The right-hand operand only. x86 encodes an immediate as the source, so
  // 'c - x' has nowhere to put one; gvn already canonicalizes a commutative
  // operation's constant into this position, so the restriction costs almost
  // nothing, and swapping the rest is a peephole nothing does yet.
  if (operandIdx != 1) {
    return FALSE;
  }

  int64_t v = cnst->info.constant.data.i;

  switch (use->kind) {
  // A GEP's offset folds into the displacement, which is a signed 32-bit
  // field. Everything past that range has to be added in a register.
  case IR_GET_ELEMENT_PTR:
    return v >= INT32_MIN && v <= INT32_MAX;

  case IR_E_ADD:
  case IR_E_SUB:
  case IR_E_AND:
  case IR_E_OR:
  case IR_E_XOR:
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    // ALU and compare immediates are 32 bits, sign-extended to the operand
    // width; a 64-bit constant outside that range has to be materialized.
    return v >= INT32_MIN && v <= INT32_MAX;

  case IR_E_SHL:
  case IR_E_SHR:
    // A shift count is 8 bits, and the hardware masks it to the operand width
    // anyway, so anything at or beyond 64 is not a shift this can encode.
    return v >= 0 && v < 64;

  // IR_E_MUL is deliberately absent. imul's immediate form is the
  // three-operand one, which is a different encoding from the two-address
  // shape everything above shares, so folding into it wants a rule of its own
  // rather than a special case here.
  //
  // Divides are absent because x86 has no immediate divisor at all, and
  // everything else because it has no rule yet.
  default:
    return FALSE;
  }
}


// What this rule covers. Everything it turns away becomes a placeholder, which
// is what it already was.
// Returns NULL when this rule covers the call, and otherwise what stopped it,
// for the log line the placeholder prints. Everything it turns away becomes a
// placeholder, which is what it already was.
static const char *callRefusalReason(const TargetDescriptor *target, const IrInstruction *call) {
  if (call->type == IR_F80) {
    return "returns a long double, which lives on the x87 stack";
  }

  // Bit 0 is the callee's, so it cannot mean an argument; translateCall sets it
  // to say there was a memory argument too far along the list for the mask to
  // name. Refusing is the whole of the handling - the alternative is passing
  // its address where the callee reads bytes.
  if (callArgInMemory(call, 0)) {
    return "passes an aggregate argument past the sixty-fourth, which the call's"
           " memory-argument mask cannot name";
  }

  for (size_t idx = firstCallArgIndex(call); idx < call->inputs.size; ++idx) {
    const IrInstruction *arg = inputAt(call, idx);

    if (callArgInMemory(call, idx)) {
      // Pushed eightbyte by eightbyte, which lands the struct at whatever
      // alignment the eightbyte before it left the stack pointer at. SysV
      // gives an over-aligned aggregate its own alignment in the argument
      // area, and getting there means laying the area out rather than pushing
      // into it - so this refuses instead of misplacing the bytes. Nothing in
      // the corpus reaches it: a struct is eight-aligned unless it contains a
      // long double, and that is refused a line below anyway.
      if (typeAlignment(arg->astType->pointed) > (int32_t)sizeof(intptr_t)) {
        return "passes an aggregate argument aligned wider than an eightbyte,"
               " which pushing cannot place";
      }
      continue;
    }

    if (arg->type == IR_F80 || arg->type == IR_VOID) {
      return "passes an argument of a type with no register class";
    }

    // A float argument past xmm7 would have to be pushed, and there is no
    // 'push xmm'. What it wants now that aggregates are pushed too is the same
    // shape selectMemoryArgument uses - get the value into a general register
    // and push that - which needs a 'movq r64, xmm' this backend does not have
    // an opcode for yet (X86_MOVD is 66 0F 6E and only goes the other way).
    // That is a small, self-contained addition rather than the frame-layout
    // change this refusal used to be waiting on.
    if (callArgClass(arg) == RC_FP && callArgLocation(target, call, idx) == NO_REG) {
      return "passes a float argument on the stack, and there is no 'push xmm'";
    }
  }

  return NULL;
}


// What one memory operand can hold. The scale lives in the SIB byte as a shift
// amount, so it is 1, 2, 4 or 8 and nothing else, and the displacement is a
// signed 32-bit field. Zero means "no index at all", which is legal alongside
// anything.
static Boolean x86IsLegalAddressMode(uint32_t scale, int64_t disp) {
  if (scale != 0 && scale != 1 && scale != 2 && scale != 4 && scale != 8) {
    return FALSE;
  }

  return disp >= INT32_MIN && disp <= INT32_MAX;
}

// Which inputs reach an addressing mode, as a bit per position. Zero for
// anything the rules above are going to refuse: a placeholder names its
// inputs' registers, so a pointer folded away underneath one would leave it
// reading a register nothing ever wrote.
static uint32_t x86AddressOperands(const IrInstruction *i) {
  switch (i->kind) {
  case IR_M_LOAD:
  case IR_M_STORE:
    return isAddressableIrType(i->info.memory.opType) ? 1u : 0;

  // Both of them: a copy addresses its destination and its source alike.
  case IR_M_COPY:
    return isUnrollableCopy(i) ? 3u : 0;

  default:
    return 0;
  }
}

static Boolean x86FoldsIntoCondition(const IrInstruction *cond) {
  switch (cond->kind) {
  case IR_E_EQ:
  case IR_E_NE:
  case IR_E_LT:
  case IR_E_LE:
  case IR_E_GT:
  case IR_E_GE:
    return TRUE;

  // The ordered float comparisons are one setcc and fold like the integer
  // ones - all four are false when either operand is a NaN, and so are their
  // inversions' opposites, which is what lets a single jcc stand for them.
  //
  // Float equality is the one that cannot: it needs the ordered-ness folded in
  // with a second setcc and an 'and', which is two flags to branch on rather
  // than one.
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    return TRUE;

  default:
    return FALSE;
  }
}

const ArchSelector x86Selector = {
  .selectInstruction = &selectInstruction_x86_64,
  .selectTerminator = &selectTerminator_x86_64,
  .isLegalImmediate = &x86IsLegalImmediate,
  .isLegalAddressMode = &x86IsLegalAddressMode,
  .addressOperands = &x86AddressOperands,
  .foldsIntoCondition = &x86FoldsIntoCondition
};
