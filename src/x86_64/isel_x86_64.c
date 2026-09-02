#include <assert.h>
#include <string.h>

#include "codegen.h"
#include "ir/ir.h"
#include "ir/isel.h"
#include "sema.h"
#include "machine_x86_64.h"
#include "instructions_x86_64.h"

// -============================ x86-64 instruction selection ==============-
//
// One IR instruction at a time, into the two-address form the ISA actually
// has. See docs/ir-codegen-design.md section 6 for the shape and
// machine_x86_64.h for the opcodes.
//
// Every IR instruction this compiler builds has a rule here. There is no
// placeholder and no fallback any more: what the switches below do not name is
// something the IR never contains, and each such arm says which side of the
// pipeline makes it so rather than deferring to another backend. A gap is
// therefore a crash rather than quieter code, which is the point - see section
// 6.21 of the design document.
//
// The flags are modelled, but only as far as an opcode: x86OpcodeFlags in
// target_x86_64.c says what each instruction does to $eflags, and the rules
// below still place a compare and the setcc or jcc that reads it adjacent by
// hand. verifyFlagsDependencies() is what checks they did - see section 6.4 of
// the design document for what it can and cannot notice.

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
// Reading the low half of a register as a 64-bit index would pick up whatever
// the top half held, and zero-extending it - which is what a plain 32-bit move
// does - would turn 'a[-1]' into 'a[4294967295]'.
//
// The IR asks for the conversion itself now (widenOperand in ast2ir.c), so
// this covers the uses the IR has no instruction for: an argument, a store, a
// compare against a wider operand.
static uint32_t widenRegisterInto(MachineBuilder *b, uint32_t dst, uint32_t src, uint8_t srcSize,
                                  uint8_t size, Boolean isUnsigned) {
  MachineInstr *mi = buildMachineInstr(b, isUnsigned ? X86_MOVZX : X86_MOVSX, 1, 1);

  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, src);
  // Eight bytes even for the four-byte unsigned source, which x86 spells as a
  // plain 32-bit move (emitWiden). Saying four here would be a def of half a
  // register: true of the hardware, which zeroes the rest, and not of the
  // machine model, whose next reader is a spill of the whole slot.
  mi->opSize = size;
  mi->srcSize = srcSize;

  return dst;
}

static uint32_t widenRegister(MachineBuilder *b, uint32_t src, uint8_t srcSize, uint8_t size,
                              Boolean isUnsigned) {
  return widenRegisterInto(b, createVirtualRegister(b->mf, RC_GP, size), src, srcSize, size,
                           isUnsigned);
}

static uint32_t selectWidened(MachineBuilder *b, const IrInstruction *value, uint8_t size) {
  uint32_t src = machineBuilderVreg(b, value);
  uint8_t srcSize = valueSize(value);

  // Narrowing needs no instruction at all - the low bytes of the register are
  // already the answer - and a float is never widened implicitly: converting
  // one width to another changes the bits, so the IR spells it as a cast.
  if (srcSize >= size || isFloatIrType(value->type)) {
    return src;
  }

  const Boolean isUnsigned = isUnsignedIrOperand(value->type);

  // The definition may already have written the extension this use wants: a
  // byte load widens as it loads (selectMemoryLoad), so 'movzx.4/1 [%v5]'
  // followed by 'movzx.4/1 %v8' is the same extension performed twice, the
  // second time on its own output.
  if (machineTakeRegisterExtension(b->mf, src, size, isUnsigned)) {
    return src;
  }

  return widenRegister(b, src, srcSize, size, isUnsigned);
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
  // IR_F80 is one too, and the odd one: it *is* an address, so the "load" is
  // that address arriving in a register and the "store" is a copy of the bytes
  // it points at. Both still address memory, which is what this question is
  // for - see selectX87Load and selectX87Store.
  return t != IR_VOID;
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

// Somewhere in the outgoing-argument area, which is the one part of the frame
// addressed off the stack pointer rather than off the frame pointer. It has to
// be: the callee reads its stack arguments at [rsp] and upwards, so the area is
// wherever rsp is and not at a fixed distance below rbp. Which also means the
// offset is final as soon as it is computed - the frame above it can still
// grow, and the area does not move.
static MachineAddress outgoingArgAddress(const MachineFunction *mf, int32_t offset) {
  MachineAddress addr = { MAK_REG, mf->target->sp, NO_REG, 0, offset };
  return addr;
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

  // The result is the new top of the stack, above whatever of it the outgoing
  // arguments have reserved: the area is always the lowest bytes of the stack,
  // so carving below it moves it down with rsp and leaves the block that was
  // just carved starting where it ends. Copied out into a register of its own
  // rather than left as rsp, which the next allocation is about to move.
  uint32_t outgoing = b->mf->frame.outgoingSize;

  if (outgoing != 0) {
    MachineAddress addr = outgoingArgAddress(b->mf, (int32_t)outgoing);

    MachineInstr *result = buildMachineInstr(b, X86_LEA, 1, 1);
    setRegisterOperand(result, 0, machineBuilderVreg(b, i));
    setMemoryOperand(result, 1, &addr);
    result->opSize = sizeof(intptr_t);
    return;
  }

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

static void selectX87Load(MachineBuilder *b, const IrInstruction *i);
static void selectX87Store(MachineBuilder *b, const IrInstruction *i);

static void selectMemoryLoad(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;

  // addLoadInstr asserts the same thing: a load of nothing is not a load.
  assert(isAddressableIrType(t) && "a load of a value with no register class");

  if (t == IR_F80) {
    selectX87Load(b, i);
    return;
  }

  MachineAddress addr = addressFor(b, inputAt(i, 0), 0);
  const uint8_t size = irTypeMachineSize(t);

  // A byte or halfword load widens as it loads rather than writing part of a
  // register and leaving the rest. 'mov cl, [rax]' merges with whatever was in
  // rcx, which chains consecutive iterations of a loop like 'while (*p) ++p;'
  // through a register they share nothing else with - correct, since nothing
  // reads above the byte, and measurably slower. A widening load writes the
  // register whole and breaks the chain, in the same one instruction.
  if (!isFloatIrType(t) && size < sizeof(int32_t)) {
    MachineInstr *widening =
        buildMachineInstr(b, isUnsignedIrOperand(t) ? X86_MOVZX : X86_MOVSX, 1, 1);

    const uint32_t dst = machineBuilderVreg(b, i);

    setRegisterOperand(widening, 0, dst);
    setMemoryOperand(widening, 1, &addr);
    widening->opSize = sizeof(int32_t);
    widening->srcSize = size;

    // Said rather than left to be rediscovered: four bytes of this register
    // are the value, and a use wanting it that wide wants nothing further.
    machineNoteRegisterExtension(b->mf, dst, sizeof(int32_t), isUnsignedIrOperand(t));
    return;
  }

  MachineInstr *mi = buildMachineInstr(b, X86_LOAD, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = size;
}

// Whether the copy's count is known here. Every copy this frontend builds has
// a constant one - generateCompositeCopy is the only producer and it takes the
// count from computeTypeSize() - but that is a property of today's frontend
// and not of the instruction, so the question is asked rather than assumed.
//
// Zero is a count like any other - an empty struct is a GNU extension this
// frontend accepts, and copying nothing is the right amount of code for it.
//
// The size is read from the IR rather than from the operand, because whether
// it was folded into an immediate is a question about *uses* and this one is
// not a use at all - the count is spent at selection time.
static Boolean hasConstantCount(const IrInstruction *i) {
  const IrInstruction *size = inputAt(i, 2);

  return size->kind == IR_DEF_CONST && size->info.constant.kind == IR_CK_INTEGER &&
         size->info.constant.data.i >= 0;
}

// Past this many bytes a copy is emitted as a string move rather than unrolled
// into load/store pairs. The number is a *cost* boundary and not a capability
// one: both forms copy any size, and section 6.16 removed the refusal that
// used to sit at this same 128 because it had been written down as if it were
// the second kind.
//
// Under it, the pairs win: they are two instructions per eight bytes against
// the tens of cycles a 'rep' takes to start, and the widths taper so the tail
// costs nothing. Over it the unrolling is what costs - a 4KB struct assignment
// is a thousand load/store pairs of straight-line code - and the string move
// is four instructions whatever the size.
#define X86_UNROLLED_COPY_LIMIT 128

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
static void selectFixedCopy(MachineBuilder *b, const MachineAddress *to,
                            const MachineAddress *from, int32_t bytes) {
  if (bytes == 0) {
    return;
  }

  // One register per chunk width rather than one for all of them: a copy uses
  // at most four widths whatever its size, and a register whose width is the
  // load's is a register the spill of it writes whole. The single eight-byte
  // temporary the chunks used to share was the tail's half-written slot -
  // section 9's allocation checker (b) reported every such copy.
  uint32_t tmp[4] = { NO_REG, NO_REG, NO_REG, NO_REG };

  for (int32_t done = 0; done < bytes;) {
    int32_t left = bytes - done;
    uint8_t chunk = left >= 8 ? 8 : left >= 4 ? 4 : left >= 2 ? 2 : 1;
    uint32_t *slot = &tmp[chunk == 8 ? 3 : chunk == 4 ? 2 : chunk == 2 ? 1 : 0];

    if (*slot == NO_REG) {
      *slot = createVirtualRegister(b->mf, RC_GP, chunk);
    }

    MachineAddress fromChunk = *from;
    MachineAddress toChunk = *to;
    fromChunk.disp += done;
    toChunk.disp += done;

    MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
    setRegisterOperand(load, 0, *slot);
    setMemoryOperand(load, 1, &fromChunk);
    load->opSize = chunk;

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &toChunk);
    setRegisterOperand(store, 1, *slot);
    store->opSize = chunk;

    done += chunk;
  }
}

// The same copy as a string move: the count in rcx, the two addresses in rsi
// and rdi, and one instruction that walks all three. It copies forwards, which
// is what the direction flag being clear at every ABI boundary guarantees, and
// forwards is what a C assignment of one object to another needs - the two
// either coincide or do not overlap at all.
//
// The three registers are the ISA's choice and are written as physical ones
// here, exactly as a divide's rax and rdx are: the allocator may not reassign
// them and may not leave anything live in them across this.
//
// The count goes in first, before any of the three is written, because it is
// the operand that can need arbitrary computation to produce - a run-time size
// is an ordinary value with an ordinary def - and everything that computes it
// is then outside the window where three registers are pinned.
static void selectRepCopy(MachineBuilder *b, const IrInstruction *i,
                          const MachineAddress *to, const MachineAddress *from) {
  // Whatever width the count was computed at, rcx is read whole, so a narrow
  // one is widened by its own signedness rather than left with the bytes above
  // it as they were.
  selectLoadInto(b, R_ECX, inputAt(i, 2), sizeof(intptr_t));

  MachineInstr *dst = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(dst, 0, R_EDI);
  setMemoryOperand(dst, 1, to);
  dst->opSize = sizeof(intptr_t);

  MachineInstr *src = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(src, 0, R_ESI);
  setMemoryOperand(src, 1, from);
  src->opSize = sizeof(intptr_t);

  // All six operands implicit: the encoding names none of them, and what they
  // are here for is to tell liveness that this reads three registers and
  // leaves all three changed.
  MachineInstr *mi = buildMachineInstr(b, X86_REP_MOVSB, 3, 3);
  setRegisterOperand(mi, 0, R_EDI);
  setRegisterOperand(mi, 1, R_ESI);
  setRegisterOperand(mi, 2, R_ECX);
  setRegisterOperand(mi, 3, R_EDI);
  setRegisterOperand(mi, 4, R_ESI);
  setRegisterOperand(mi, 5, R_ECX);

  for (uint16_t op = 0; op < mi->numOperands; ++op) {
    machineOperandAt(mi, op)->flags.isImplicit = 1;
  }

  // The registers it advances, not the byte it moves at a time: opSize is what
  // a def's width is read from, and all three come out eight bytes wide.
  mi->opSize = sizeof(intptr_t);
}

static void selectMemoryCopy(MachineBuilder *b, const IrInstruction *i) {
  const IrInstruction *size = inputAt(i, 2);

  // Both addresses once, outside whichever form is chosen: the chunks of an
  // unrolled copy differ only in their displacement, and an address that had
  // to widen an index would otherwise widen it again for every eight bytes.
  MachineAddress from = addressFor(b, inputAt(i, 1), 0);
  MachineAddress to = addressFor(b, inputAt(i, 0), 0);

  // A count known only at run time takes the string move by necessity rather
  // than by cost - there is nothing to unroll against. Nothing the frontend
  // accepts builds such a copy today, C having no assignment of an object
  // whose size is not known: a VLA cannot be assigned, and a flexible array
  // member is not part of its struct's size. The rule is here because the
  // alternative to a rule is an assertion, and the two forms differ by which
  // branch of one 'if' this takes.
  if (hasConstantCount(i) && size->info.constant.data.i <= X86_UNROLLED_COPY_LIMIT) {
    selectFixedCopy(b, &to, &from, (int32_t)size->info.constant.data.i);
    return;
  }

  selectRepCopy(b, i, &to, &from);
}

static void selectMemoryStore(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind t = i->info.memory.opType;

  // The stored value's own type, so this is "a void expression was assigned to
  // something" - which sema rejects now (DIAG_VOID_NOT_IGNORED).
  assert(isAddressableIrType(t) && "a store of a value with no register class");

  if (t == IR_F80) {
    selectX87Store(b, i);
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

  // Always at least four bytes. C promotes both operands to int before
  // dividing, so a narrower divide is the wrong one twice over: there is no
  // sign-extend-into-a-pair below a word at all (and the byte divide's
  // remainder lands in ah, which nothing here can name), and at 16 bits
  // '(short)-32768 / -1' overflows where the promoted division does not. The
  // copy at the end takes the narrow result back out.
  const uint8_t divSize = size < sizeof(int32_t) ? sizeof(int32_t) : size;

  selectLoadInto(b, R_EAX, inputAt(i, 0), size);
  if (divSize != size) {
    // Extended after the load rather than by it, so that a folded operand is
    // still read at its own width.
    widenRegisterInto(b, R_EAX, R_EAX, size, divSize,
                      isUnsignedIrOperand(inputAt(i, 0)->type));
  }

  if (isSigned) {
    // Sign-extend rax into rdx:rax. One opcode for the whole cwd/cdq/cqo
    // family - opSize is what picks between them at emission.
    MachineInstr *ext = buildMachineInstr(b, X86_CDQ, 1, 1);
    setRegisterOperand(ext, 0, R_EDX);
    setRegisterOperand(ext, 1, R_EAX);
    machineOperandAt(ext, 1)->flags.isImplicit = 1;
    ext->opSize = divSize;
  } else {
    // An unsigned divide wants the high half zero rather than sign-extended.
    MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(zero, 0, R_EDX);
    setImmediateOperand(zero, 1, 0);
    zero->opSize = divSize;
  }

  // The divisor is never an immediate on x86, which is why isLegalImmediate
  // refuses to fold a constant into a divide - it arrives here in a register.
  uint32_t divisor = selectWidened(b, inputAt(i, 1), divSize);

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
  div->opSize = divSize;

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
// It goes in front of the compare rather than between the compare and the
// setcc. A move of an immediate leaves the flags alone - X86_MOV is MFE_NONE -
// so either place would work as this is written; in front is where it goes so
// that the zeroing idiom stays a free choice, 'xor dst, dst' being the obvious
// alternative and one that does clobber them.
//
// The width comes from the destination register rather than from the caller,
// so that this and selectSetcc below decide "is there anything above the low
// byte?" from the one fact instead of from two that could drift apart.
static void selectZeroExtendedSetup(MachineBuilder *b, uint32_t dst) {
  uint8_t size = virtualRegisterInfo(b->mf, dst)->size;

  if (size <= 1) {
    return;
  }

  MachineInstr *zero = buildMachineInstr(b, X86_MOV, 1, 1);
  setRegisterOperand(zero, 0, dst);
  setImmediateOperand(zero, 1, 0);
  zero->opSize = size;
}

// A setcc writes the low byte of its destination and leaves the rest as it
// found it, so whenever the register is wider than a byte the def is partial
// and says so.
//
// Saying so is not decoration. The upper bytes come from the zeroing move
// above, and nothing else in the machine function records that the setcc
// depends on it. An allocator that believes the def is total will happily put
// the move's result somewhere the setcc never sees: stage 2A spills the zero
// to the frame and then reloads a *different* value into the scratch register
// the setcc writes, so the bytes stored back are whatever the comparison's
// operand left behind.
//
// When the destination is a single byte there is no zeroing move and the def
// really is total, so nothing is marked - claiming a read of a register
// nothing has written would be a use before def, and liveness would be right
// to complain about it.
static void selectSetcc(MachineBuilder *b, uint32_t opcode, uint32_t dst) {
  MachineInstr *set = buildMachineInstr(b, opcode, 1, 0);

  setRegisterOperand(set, 0, dst);
  set->opSize = 1;

  if (virtualRegisterInfo(b->mf, dst)->size > 1) {
    setPartialDefOperand(set, 0);
  }
}

static void selectLogicalNot(MachineBuilder *b, const IrInstruction *i) {
  uint32_t dst = machineBuilderVreg(b, i);
  const IrInstruction *arg = inputAt(i, 0);

  selectZeroExtendedSetup(b, dst);

  MachineInstr *test = buildMachineInstr(b, X86_TEST, 0, 2);
  setRegisterOperand(test, 0, machineBuilderVreg(b, arg));
  setRegisterOperand(test, 1, machineBuilderVreg(b, arg));
  test->opSize = valueSize(arg);

  selectSetcc(b, X86_SETE, dst);
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
  const IrInstruction *rhs = inputAt(i, 1);

  // The wider of the two, and both operands brought up to it. A compare of a
  // pointer against a null constant typed 'int' is the shape that needs it:
  // one side is eight bytes and the other four, and reading eight of the
  // narrow one is reading four bytes nothing wrote.
  uint8_t size = valueSize(lhs);
  if (valueSize(rhs) > size) {
    size = valueSize(rhs);
  }

  uint32_t lhsReg = selectWidened(b, lhs, size);
  Boolean folded = machineBuilderIsFolded(b, rhs);
  uint32_t rhsReg = folded ? NO_REG : selectWidened(b, rhs, size);

  MachineInstr *cmp = buildMachineInstr(b, X86_CMP, 0, 2);
  // The left-hand side is never an immediate: x86 encodes the immediate as the
  // source operand, which is what isLegalImmediate's position rule enforces.
  setRegisterOperand(cmp, 0, lhsReg);
  if (folded) {
    setValueOperand(b, cmp, 1, rhs);
  } else {
    setRegisterOperand(cmp, 1, rhsReg);
  }
  cmp->opSize = size;

  return setOpcodeFor(i->kind, isUnsignedIrOperand(lhs->type));
}

static void selectCompare(MachineBuilder *b, const IrInstruction *i) {
  uint32_t dst = machineBuilderVreg(b, i);
  selectZeroExtendedSetup(b, dst);
  uint32_t cc = emitIntegerCompare(b, i);

  selectSetcc(b, cc, dst);
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

static void selectX87Constant(MachineBuilder *b, const IrInstruction *i);

static void selectFloatConstant(MachineBuilder *b, const IrInstruction *i) {
  // Not through floatConstantBits, which would answer for a long double by
  // rounding it to a double: x87 has no immediate form at all, so the value
  // goes to memory whole and is loaded from there.
  if (i->type == IR_F80) {
    selectX87Constant(b, i);
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
static void selectX87Binary(MachineBuilder *b, const IrInstruction *i);

static void selectFloatBinary(MachineBuilder *b, const IrInstruction *i, uint32_t opcode) {
  if (i->type == IR_F80) {
    selectX87Binary(b, i);
    return;
  }

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

static uint32_t emitX87Compare(MachineBuilder *b, const IrInstruction *i);

// As emitIntegerCompare, for the SSE comparisons.
static uint32_t emitFloatCompare(MachineBuilder *b, const IrInstruction *i) {
  if (inputAt(i, 0)->type == IR_F80) {
    return emitX87Compare(b, i);
  }

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
  selectZeroExtendedSetup(b, dst);
  uint32_t cc = emitFloatCompare(b, i);

  selectSetcc(b, cc, dst);

  if (!isEquality) {
    return;
  }

  // Fold the ordered-ness in. The second setcc reads the same flags the first
  // did - nothing in between touches them - and combining is 'and' for ==,
  // which wants both, and 'or' for !=, which wants either.
  uint32_t ordered = createVirtualRegister(b->mf, RC_GP, size);
  selectZeroExtendedSetup(b, ordered);
  selectSetcc(b, i->kind == IR_E_FEQ ? X86_SETNP : X86_SETP, ordered);

  MachineInstr *mi = buildMachineInstr(b, i->kind == IR_E_FEQ ? X86_AND : X86_OR, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, ordered);
  mi->opSize = size;
}

// -============================ x87 ============================-
//
// Long double, and nothing else on this target uses x87 at all.
//
// An IR_F80 value is the *address* of the sixteen bytes rather than the bytes
// (see the note on it in include/ir/ir.h), which is what makes a stack
// register file usable behind an allocator that only knows about flat ones:
// every rule below pushes its operands out of memory, does the one x87
// instruction, and pops the answer back into memory, so the stack is exactly
// as deep after the rule as before it and no value is ever live in st(i)
// across an instruction boundary. The allocator never learns x87 exists.
//
// The two exceptions are the ABI's, and they are exceptions on purpose: a
// return leaves its value in st(0) and a call takes its result out of st(0),
// because that is where SysV says a long double is handed over. The stack is
// balanced across the pair.

// fld and fstp move the ten bytes that mean anything. The object occupies
// sixteen so that its address has the alignment the ABI gives it, and nothing
// ever reads the six above the ten.
#define X87_MEM_SIZE 10
#define X87_OBJECT_SIZE 16
#define X87_OBJECT_ALIGN 16

// Somewhere to put a value that has to pass through memory. Laid out here for
// the reason selectRegisterReturnedStruct lays its slot out here: the IR asked
// for no allocation, because at IR level a long double add simply has a value.
static int32_t x87Scratch(MachineBuilder *b, uint32_t size, uint32_t align) {
  MachineFunction *mf = b->mf;
  int32_t frameIdx = addMachineFrameObject(mf, MFO_SCRATCH, size, align);

  mf->frame.size = (uint32_t)ALIGN_SIZE(
      placeMachineFrameObject(mf, (int32_t)mf->frame.size, frameIdx), 2 * sizeof(intptr_t));

  return frameIdx;
}

// One instruction naming one address, which is every x87 form here except the
// arithmetic and the compares - those name no operand at all, reading st(0)
// and st(1) where the pushes in front of them left their operands.
static void x87Mem(MachineBuilder *b, uint32_t opcode, const MachineAddress *addr, uint8_t size) {
  MachineInstr *mi = buildMachineInstr(b, opcode, 0, 1);
  setMemoryOperand(mi, 0, addr);
  mi->opSize = size;
}

static void x87Frame(MachineBuilder *b, uint32_t opcode, int32_t frameIdx, int32_t disp,
                     uint8_t size) {
  MachineAddress addr = { MAK_FRAME, NO_REG, NO_REG, 0, disp };
  addr.anchor.frameIdx = frameIdx;
  x87Mem(b, opcode, &addr, size);
}

// Push what an IR_F80 value names onto the x87 stack.
static void x87Push(MachineBuilder *b, const IrInstruction *value) {
  MachineAddress addr = addressFor(b, value, 0);
  x87Mem(b, X86_FLD, &addr, X87_MEM_SIZE);
}

// The address of a frame slot, in a register - which is what an IR_F80 result
// is, so this is how every rule producing one finishes.
static void x87SlotAddress(MachineBuilder *b, const IrInstruction *i, int32_t frameIdx) {
  MachineInstr *lea = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(lea, 0, machineBuilderVreg(b, i));
  setFrameAddressOperand(lea, 1, frameIdx);
  lea->opSize = sizeof(intptr_t);
}

// Pop the top of the stack into a slot of its own and give the instruction's
// value that slot's address.
static void x87PopToValue(MachineBuilder *b, const IrInstruction *i) {
  int32_t frameIdx = x87Scratch(b, X87_OBJECT_SIZE, X87_OBJECT_ALIGN);

  x87Frame(b, X86_FSTP, frameIdx, 0, X87_MEM_SIZE);
  x87SlotAddress(b, i, frameIdx);
}

// A long double constant, written into a slot an eightbyte at a time.
//
// Both eightbytes, though only ten bytes mean anything: writing the slot whole
// is one 'mov' more and saves needing a two-byte store form, and the six bytes
// above the value are inside the slot the object was given.
static void selectX87Constant(MachineBuilder *b, const IrInstruction *i) {
  LongDoubleBytes ld = { 0 };
  ld.ld = (long double)i->info.constant.data.f;

  int32_t frameIdx = x87Scratch(b, X87_OBJECT_SIZE, X87_OBJECT_ALIGN);
  uint32_t bits = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  for (int32_t half = 0; half < 2; ++half) {
    MachineInstr *mov = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mov, 0, bits);
    setImmediateOperand(mov, 1, (int64_t)ld.qwords[half]);
    mov->opSize = sizeof(intptr_t);

    MachineAddress addr = { MAK_FRAME, NO_REG, NO_REG, 0, half * (int32_t)sizeof(intptr_t) };
    addr.anchor.frameIdx = frameIdx;

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &addr);
    setRegisterOperand(store, 1, bits);
    store->opSize = sizeof(intptr_t);
  }

  x87SlotAddress(b, i, frameIdx);
}

// Loading a long double moves nothing: the value *is* the address, so this is
// the address arriving in a register. What reads the bytes is whatever reads
// the value afterwards.
static void selectX87Load(MachineBuilder *b, const IrInstruction *i) {
  MachineAddress addr = addressFor(b, inputAt(i, 0), 0);

  MachineInstr *mi = buildMachineInstr(b, X86_LEA, 1, 1);
  setRegisterOperand(mi, 0, machineBuilderVreg(b, i));
  setMemoryOperand(mi, 1, &addr);
  mi->opSize = sizeof(intptr_t);
}

// Storing one is the copy that a load is not - 'a = b' has to leave a with its
// own bytes, or a later write to b would be visible through it.
static void selectX87Store(MachineBuilder *b, const IrInstruction *i) {
  MachineAddress to = addressFor(b, inputAt(i, 0), 0);
  MachineAddress from = addressFor(b, inputAt(i, 1), 0);

  selectFixedCopy(b, &to, &from, X87_MEM_SIZE);
}

static uint32_t x87ArithOpcode(enum IrIntructionKind kind) {
  switch (kind) {
  case IR_E_FADD: return X86_FADDP;
  case IR_E_FSUB: return X86_FSUBP;
  case IR_E_FMUL: return X86_FMULP;
  case IR_E_FDIV: return X86_FDIVP;
  default: unreachable("not an x87 arithmetic operation");
  }

  return X86_FADDP;
}

// The left operand is pushed first, so it lands in st(1) and the right in
// st(0) - which is the order 'st(1) op= st(0)' needs for a subtract and a
// divide to come out the right way round.
static void selectX87Binary(MachineBuilder *b, const IrInstruction *i) {
  x87Push(b, inputAt(i, 0));
  x87Push(b, inputAt(i, 1));
  buildMachineInstr(b, x87ArithOpcode(i->kind), 0, 0);
  x87PopToValue(b, i);
}

// fcomip compares st(0) against st(1), so the operands go on in the opposite
// order to the arithmetic - the left one last, on top. The flags it leaves are
// the ones comis* leaves, which is what lets floatSetOpcodeFor and every jcc
// downstream be shared with SSE rather than duplicated for x87.
//
// It pops one of the two operands; the other is dropped explicitly, which is
// what keeps the stack balanced across the rule.
static uint32_t emitX87Compare(MachineBuilder *b, const IrInstruction *i) {
  Boolean isEquality = i->kind == IR_E_FEQ || i->kind == IR_E_FNE;
  Boolean swap = i->kind == IR_E_FLT || i->kind == IR_E_FLE;

  x87Push(b, inputAt(i, swap ? 0 : 1));
  x87Push(b, inputAt(i, swap ? 1 : 0));

  buildMachineInstr(b, isEquality ? X86_FUCOMIP : X86_FCOMIP, 0, 0);
  buildMachineInstr(b, X86_FPOP, 0, 0);

  return floatSetOpcodeFor(i->kind);
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
    selectSetcc(b, X86_SETNE, dst);
    return;
  }

  uint32_t zero = selectFloatZero(b, size);

  MachineInstr *cmp = buildMachineInstr(b, X86_FUCMP, 0, 2);
  setRegisterOperand(cmp, 0, machineBuilderVreg(b, value));
  setRegisterOperand(cmp, 1, zero);
  cmp->opSize = size;

  selectSetcc(b, X86_SETNE, dst);

  // A NaN is not equal to zero, so (_Bool)NaN is 1 - but an unordered compare
  // sets the zero flag, which alone would say otherwise. Same shape as the
  // '!=' in selectFloatCompare: not-equal *or* unordered.
  uint32_t unordered = createVirtualRegister(b->mf, RC_GP, 1);
  selectSetcc(b, X86_SETP, unordered);

  MachineInstr *mi = buildMachineInstr(b, X86_OR, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, unordered);
  mi->opSize = 1;
}

// (_Bool)someLongDouble, which is 'x != 0' like every other conversion to
// _Bool. Same shape as the SSE one in selectBooleanConversion, down to folding
// the ordered-ness in: an unordered compare sets the zero flag, so a NaN would
// otherwise come out 0 where C says 1.
static void selectX87BooleanConversion(MachineBuilder *b, uint32_t dst,
                                       const IrInstruction *value) {
  x87Push(b, value);
  buildMachineInstr(b, X86_FLDZ, 0, 0);
  buildMachineInstr(b, X86_FUCOMIP, 0, 0);
  buildMachineInstr(b, X86_FPOP, 0, 0);

  selectSetcc(b, X86_SETNE, dst);

  uint32_t unordered = createVirtualRegister(b->mf, RC_GP, 1);
  selectSetcc(b, X86_SETP, unordered);

  MachineInstr *mi = buildMachineInstr(b, X86_OR, 1, 2);
  setRegisterOperand(mi, 0, dst);
  setRegisterOperand(mi, 1, dst);
  setRegisterOperand(mi, 2, unordered);
  mi->opSize = 1;
}

// x87 rounds by whatever the control word says, and C casts truncate. So the
// word is saved, the rounding field set to "toward zero", the store done, and
// the word put back - which is the sequence the legacy backend's
// generateF10toInt spells out, and there is no shorter one that does not raise
// the ISA baseline (fisttp is SSE3).
//
// Always into eight bytes, whatever the destination is. A narrower fistp
// answers "integer indefinite" for anything that does not fit, where the wide
// form is exact for every value a defined conversion can produce and the low
// bytes of it are then the answer - the same reasoning selectConversion's
// wideForUnsigned uses.
static void selectX87ToInteger(MachineBuilder *b, uint32_t dst, const IrInstruction *value,
                               uint8_t toSize) {
  int32_t cw = x87Scratch(b, 2 * sizeof(uint32_t), sizeof(uint32_t));
  int32_t result = x87Scratch(b, sizeof(intptr_t), sizeof(intptr_t));
  // The control word is sixteen bits, and so is every instruction below that
  // touches it; a wider register would be one the loads leave half written.
  uint32_t word = createVirtualRegister(b->mf, RC_GP, sizeof(uint16_t));

  x87Push(b, value);
  x87Frame(b, X86_FNSTCW, cw, 0, sizeof(uint16_t));

  MachineAddress saved = { MAK_FRAME, NO_REG, NO_REG, 0, 0 };
  saved.anchor.frameIdx = cw;
  MachineAddress truncating = saved;
  truncating.disp = sizeof(uint32_t);

  MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
  setRegisterOperand(load, 0, word);
  setMemoryOperand(load, 1, &saved);
  load->opSize = sizeof(uint16_t);

  // Bits 10 and 11 are the rounding control; 11 is "toward zero".
  MachineInstr *set = buildMachineInstr(b, X86_OR, 1, 2);
  setRegisterOperand(set, 0, word);
  setRegisterOperand(set, 1, word);
  setImmediateOperand(set, 2, 0x0C00);
  set->opSize = sizeof(uint16_t);

  MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
  setMemoryOperand(store, 0, &truncating);
  setRegisterOperand(store, 1, word);
  store->opSize = sizeof(uint16_t);

  x87Mem(b, X86_FLDCW, &truncating, sizeof(uint16_t));
  x87Frame(b, X86_FISTP, result, 0, sizeof(intptr_t));
  x87Mem(b, X86_FLDCW, &saved, sizeof(uint16_t));

  MachineAddress at = { MAK_FRAME, NO_REG, NO_REG, 0, 0 };
  at.anchor.frameIdx = result;

  MachineInstr *out = buildMachineInstr(b, X86_LOAD, 1, 1);
  setRegisterOperand(out, 0, dst);
  setMemoryOperand(out, 1, &at);
  out->opSize = toSize;
}

// Everything with a long double on one side of it. The two register files
// cannot see each other's values, so each direction goes through a frame slot:
// x87 only ever loads and stores memory, and SSE and the GP file only ever
// reach memory the same way.
static void selectX87Conversion(MachineBuilder *b, const IrInstruction *i, uint32_t dst) {
  enum IrTypeKind fromType = i->info.fromCastType;
  enum IrTypeKind toType = i->type;
  const IrInstruction *value = inputAt(i, 0);

  if (toType == IR_F80) {
    if (fromType == IR_F80) {
      // A cast of a long double to its own type. It yields an rvalue, and an
      // rvalue of one is an address like any other, so nothing is copied - the
      // same aliasing a load does.
      selectX87Load(b, i);
      return;
    }

    // An SSE value or an integer, neither of which x87 can be handed directly.
    // Widened first for a narrow integer, for selectConversion's reason: the
    // widening is part of what '(long double)someChar' means.
    uint8_t srcSize = isFloatIrType(fromType) ? valueSize(value)
                                              : conversionIntSize(valueSize(value));
    if (!isFloatIrType(fromType) && isUnsignedIrOperand(fromType)) {
      srcSize = sizeof(intptr_t);
    }

    int32_t slot = x87Scratch(b, sizeof(intptr_t), sizeof(intptr_t));
    MachineAddress at = { MAK_FRAME, NO_REG, NO_REG, 0, 0 };
    at.anchor.frameIdx = slot;

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &at);
    setRegisterOperand(store, 1, isFloatIrType(fromType)
                                     ? machineBuilderVreg(b, value)
                                     : selectWidened(b, value, srcSize));
    store->opSize = srcSize;

    x87Mem(b, isFloatIrType(fromType) ? X86_FLD : X86_FILD, &at, srcSize);
    x87PopToValue(b, i);
    return;
  }

  if (toType == IR_BOOL) {
    selectX87BooleanConversion(b, dst, value);
    return;
  }

  if (isFloatIrType(toType)) {
    uint8_t toSize = valueSize(i);
    int32_t slot = x87Scratch(b, sizeof(intptr_t), sizeof(intptr_t));
    MachineAddress at = { MAK_FRAME, NO_REG, NO_REG, 0, 0 };
    at.anchor.frameIdx = slot;

    x87Push(b, value);
    x87Mem(b, X86_FSTP, &at, toSize);

    MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
    setRegisterOperand(load, 0, dst);
    setMemoryOperand(load, 1, &at);
    load->opSize = toSize;
    return;
  }

  selectX87ToInteger(b, dst, value, valueSize(i));
}

static void selectConversion(MachineBuilder *b, const IrInstruction *i) {
  enum IrTypeKind fromType = i->info.fromCastType;
  enum IrTypeKind toType = i->type;
  uint8_t toSize = valueSize(i);
  const IrInstruction *value = inputAt(i, 0);
  uint32_t dst = machineBuilderVreg(b, i);

  Boolean fromFloat = isFloatIrType(fromType);
  Boolean toFloat = isFloatIrType(toType);

  if (fromType == IR_F80 || toType == IR_F80) {
    selectX87Conversion(b, i, dst);
    return;
  }

  // C has no cast to or from a struct or union type, and the frontend rejects
  // one before this could see it.
  assert(fromType != IR_P_AGG && toType != IR_P_AGG &&
         "a conversion to or from an aggregate");

  // A conversion to _Bool is the one conversion that is not a change of
  // width: C defines it as 'x != 0', so (_Bool)0x100 is 1 rather than the low
  // byte of 0x100, which is what truncating would give.
  if (toType == IR_BOOL) {
    selectBooleanConversion(b, i, dst, value, fromFloat);
    return;
  }

  // Integer to integer. Widening is the only one that costs an instruction;
  // narrowing is a copy, because the low bytes of the register are already the
  // answer. Either way the extension follows the *source's* signedness, which
  // is what C says a conversion does.
  if (!fromFloat && !toFloat) {
    uint8_t srcSize = valueSize(value);

    // Straight into the conversion's own register when it widens: going
    // through selectLoadInto would widen into a register of its own and then
    // copy that here, and this is the one place where the widening *is* the
    // instruction being selected rather than something a use needed.
    // Unless the value's definition already wrote that extension, in which
    // case the conversion is a move of a register that is already the answer -
    // and one the coalescer can delete outright, the two registers now being
    // the same width as well as the same value.
    if (srcSize < toSize && !machineBuilderIsFolded(b, value) &&
        !machineTakeRegisterExtension(b->mf, machineBuilderVreg(b, value), toSize,
                                      isUnsignedIrOperand(fromType))) {
      widenRegisterInto(b, dst, machineBuilderVreg(b, value), srcSize, toSize,
                        isUnsignedIrOperand(fromType));
      return;
    }

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
    // A backstop since ast2ir grew the lowering, not a gap. Both directions of
    // a 64-bit unsigned conversion are expanded into a branch and a phi during
    // translation (translateWideUnsignedToFloat and its twin), because the half
    // of the range at or above 2^63 needs control flow and selection is not
    // allowed to invent a block. Nothing that goes through translateCast
    // reaches here any more; anything that finds another way to build one
    // must not be converted as signed, so this is a crash and not a fallback.
    assert(isConvertibleIntType(fromType) &&
           "an unsigned 64-bit integer reached selection unlowered");

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

  // The same backstop the other way up; see above.
  assert(isConvertibleIntType(toType) &&
         "an unsigned 64-bit integer reached selection unlowered");

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
  // A long double is always one, and needs no bit in the mask to say so: SysV
  // gives it the X87 class, which is passed in memory, and the type already
  // says everything the mask would.
  if (inputAt(call, idx)->type == IR_F80) {
    return TRUE;
  }

  return isCallMemoryArg(call, idx);
}

// What a memory argument takes in the outgoing area: its size rounded up to a
// whole number of eightbytes, and the alignment the *callee* will read it at.
//
// Both come from the callee's view of the same argument - see
// classifyParametersGeneric - because the two sides have to agree about where
// the bytes are, and only the caller can put them there.
static void memArgShape(const IrInstruction *arg, uint32_t *size, uint32_t *align) {
  if (arg->type == IR_F80) {
    *size = X87_OBJECT_SIZE;
    *align = X87_OBJECT_ALIGN;
    return;
  }

  // The mask is the authority on which inputs get here; astType only supplies
  // the size, and translateCall sets the two together.
  assert(arg->astType != NULL && isPointerLikeType(arg->astType));

  const TypeRef *type = arg->astType->pointed;

  *size = (uint32_t)ALIGN_SIZE(computeTypeSize(type), sizeof(intptr_t));
  *align = (uint32_t)max((int32_t)sizeof(intptr_t), typeAlignment(type));
}

static uint32_t memArgStackSlots(const IrInstruction *arg) {
  uint32_t size = 0, align = 0;
  memArgShape(arg, &size, &align);
  return size / (uint32_t)sizeof(intptr_t);
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

// How many SSE registers the call passes arguments in - which is what al has
// to hold for a variadic callee - and how many arguments go in registers,
// which is how many implicit uses the call instruction gets.
static void callArgCounts(const TargetDescriptor *target, const IrInstruction *call,
                          uint32_t *numFpRegs, uint32_t *numRegArgs) {
  uint32_t used[RC_CLASS_COUNT] = {0};
  uint32_t inRegs = 0;

  for (size_t idx = firstCallArgIndex(call); idx < call->inputs.size; ++idx) {
    if (callArgInMemory(call, idx)) {
      continue;
    }

    enum RegClass rc = callArgClass(inputAt(call, idx));

    if (used[rc] < argRegCountOf(target, rc)) {
      used[rc] += 1;
      inRegs += 1;
    }
  }

  *numFpRegs = used[RC_FP];
  *numRegArgs = inRegs;
}

// Lays the outgoing argument area out the way classifyParametersGeneric lays
// the incoming one out, and answers two questions about it: how many bytes it
// takes, and where one particular argument sits inside it. Offsets are from
// the bottom, which is where the stack pointer ends up once everything is on.
//
// This exists because of alignment, and alignment exists because of long
// double. A stack argument is aligned to at least an eightbyte and a long
// double to sixteen, so an odd number of eightbytes in front of one leaves a
// gap the callee expects - and pushing eightbyte by eightbyte would close it
// and hand the callee its argument eight bytes low. Everything else lands
// where a plain sequence of pushes would put it, which is why nothing needed
// this until now.
//
// Pass SIZE_MAX for wantedIdx to ask only for the total.
static uint32_t callStackArea(const TargetDescriptor *target, const IrInstruction *call,
                              size_t wantedIdx, int32_t *wantedOffset) {
  int32_t offset = 0;

  for (size_t idx = firstCallArgIndex(call); idx < call->inputs.size; ++idx) {
    if (callArgLocation(target, call, idx) != NO_REG) {
      continue;
    }

    uint32_t size = sizeof(intptr_t), align = sizeof(intptr_t);

    if (callArgInMemory(call, idx)) {
      memArgShape(inputAt(call, idx), &size, &align);
    }

    offset = (int32_t)ALIGN_SIZE(offset, align);

    if (idx == wantedIdx) {
      *wantedOffset = offset;
    }

    offset += (int32_t)size;
  }

  // SysV wants rsp sixteen-byte aligned when the call executes. It already is
  // everywhere else in the function - the entry misalignment is undone by
  // pushing rbp, and every frame size stage 3 subtracts is rounded to 16 - so
  // rounding the area up is the whole of keeping it that way.
  return (uint32_t)ALIGN_SIZE(offset, 2 * sizeof(intptr_t));
}

// How much stack the widest call in this function passes arguments on, settled
// before anything is selected. See ArchSelector.reserveFrame.
//
// One area for every call rather than one per call site: they are used one at
// a time, and reserving the widest is what lets the stack pointer stand still
// through the whole function. Measuring it here rather than growing it call by
// call is what a dynamic alloca needs - it has to allocate above an area whose
// size is settled, and it may well come before the call that would have set it.
//
// Every call in the IR is here to be measured. A call is never folded into an
// operand and never absorbed into another instruction, so there is no call the
// walk below can see and selection will not.
static void x86ReserveFrame(MachineFunction *mf) {
  uint32_t widest = 0;

  for (const MachineBasicBlock *mbb = mf->blocks.head; mbb != NULL; mbb = mbb->next) {
    for (const IrInstruction *i = mbb->ir->instrunctions.head; i != NULL; i = i->next) {
      if (i->kind != IR_CALL && i->kind != IR_ICALL) {
        continue;
      }

      uint32_t bytes = callStackArea(mf->target, i, SIZE_MAX, NULL);

      if (bytes > widest) {
        widest = bytes;
      }
    }
  }

  mf->frame.outgoingSize = widest;
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

// Copy an aggregate argument into the outgoing area, one eightbyte at a time,
// starting at 'offset' - which is where the layout put it and where the callee
// will read it.
//
// The copy *is* what the ABI asks for: the callee reads the bytes where the
// call left them, so a struct that spans three eightbytes is three loads and
// three stores. There is no memcpy to defer to and no reason to want one; the
// widest aggregate any of this compiles is a handful of eightbytes.
//
// Reading up to seven bytes past the end of the struct is deliberate and safe:
// the source is the temporary translateCall copied into, and createAllocaSlot
// rounds a slot to a whole eightbyte. The bytes above the struct are padding
// the callee's own classification skips.
static void selectMemoryArgument(MachineBuilder *b, const IrInstruction *arg, int32_t offset) {
  uint32_t slots = memArgStackSlots(arg);

  // Once, outside the loop, for the reason selectMemoryCopy takes both of its
  // addresses once: an address that had to widen an index would otherwise
  // widen it again per eightbyte.
  MachineAddress from = addressFor(b, arg, 0);

  // Reused across eightbytes for selectMemoryCopy's reason: a register apiece
  // would put as many frame slots under the call as the argument is wide.
  uint32_t tmp = createVirtualRegister(b->mf, RC_GP, sizeof(intptr_t));

  for (uint32_t slot = 0; slot < slots; ++slot) {
    int32_t disp = (int32_t)(slot * sizeof(intptr_t));

    MachineAddress chunk = from;
    chunk.disp += disp;

    MachineInstr *load = buildMachineInstr(b, X86_LOAD, 1, 1);
    setRegisterOperand(load, 0, tmp);
    setMemoryOperand(load, 1, &chunk);
    load->opSize = sizeof(intptr_t);

    MachineAddress to = outgoingArgAddress(b->mf, offset + disp);

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &to);
    setRegisterOperand(store, 1, tmp);
    store->opSize = sizeof(intptr_t);
  }
}

static void selectCall(MachineBuilder *b, const IrInstruction *i) {
  const TargetDescriptor *target = b->mf->target;

  uint32_t numFpRegs = 0, numRegArgs = 0;
  callArgCounts(target, i, &numFpRegs, &numRegArgs);

  // Forwards, in the argument list's own order, because a store says where it
  // goes and does not have to arrive there. The area was reserved for the
  // whole function before any of this was selected, so nothing here moves the
  // stack pointer and there is nothing to give back afterwards.
  for (size_t idx = firstCallArgIndex(i); idx < i->inputs.size; ++idx) {
    const IrInstruction *arg = inputAt(i, idx);

    if (callArgLocation(target, i, idx) != NO_REG) {
      continue;
    }

    int32_t offset = 0;
    callStackArea(target, i, idx, &offset);

    if (callArgInMemory(i, idx)) {
      selectMemoryArgument(b, arg, offset);
      continue;
    }

    // Always a register: x86IsLegalImmediate folds a constant only into an
    // argument that is passed in one, exactly so that this cannot be an
    // immediate.
    uint32_t src = machineBuilderVreg(b, arg);
    uint8_t argSize = valueSize(arg);

    if (callArgClass(arg) == RC_FP) {
      // There is no store of an xmm register at an integer's width, and the
      // eightbyte is written whole, so the bits come out into a general
      // register first, at the float's own width.
      uint32_t bits = createVirtualRegister(b->mf, RC_GP, argSize);

      MachineInstr *out = buildMachineInstr(b, X86_MOVDR, 1, 1);
      setRegisterOperand(out, 0, bits);
      setRegisterOperand(out, 1, src);
      out->opSize = argSize;

      // The half above a 'float' is written rather than left as whatever the
      // area held last time round. Zero-extended, which is what the bits of a
      // stack argument's padding are worth.
      src = argSize < sizeof(intptr_t)
                ? widenRegister(b, bits, argSize, sizeof(intptr_t), TRUE)
                : bits;
    } else if (argSize < sizeof(intptr_t)) {
      // Same for a narrow integer, by its own signedness. SysV leaves the
      // bytes above it unspecified and a four-byte store would be shorter,
      // but the area is written over by every call in the function and a
      // callee that reads its argument wide would read the last call's.
      src = selectWidened(b, arg, sizeof(intptr_t));
    }

    MachineAddress to = outgoingArgAddress(b->mf, offset);

    MachineInstr *store = buildMachineInstr(b, X86_STORE, 0, 2);
    setMemoryOperand(store, 0, &to);
    setRegisterOperand(store, 1, src);
    // A stack argument occupies a whole eightbyte however narrow it is.
    store->opSize = sizeof(intptr_t);
  }

  for (size_t idx = firstCallArgIndex(i); idx < i->inputs.size; ++idx) {
    uint32_t reg = callArgLocation(target, i, idx);

    if (reg != NO_REG) {
      // At the argument's own width, not widened: SysV leaves the bytes above
      // a narrow argument unspecified, and the callee knows its own prototype.
      // Which rests on sema having converted the argument to the parameter's
      // type - where it does not, the callee reads bytes nothing wrote.
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
  // A long double comes back in st(0), which is not in the register file at
  // all, so the call names no destination for it. What takes it out of st(0)
  // is the fstp below.
  Boolean hasResult = i->type != IR_VOID && i->type != IR_F80;
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

  if (i->type == IR_F80) {
    // Off the x87 stack and into a slot of this frame, which is what makes the
    // pair balanced: the callee's return pushed one value, and this pops it.
    x87PopToValue(b, i);
  } else if (i->type == IR_P_AGG && i->info.call.returnBuffer == NULL) {
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

    // isRealIrType and not isFloatIrType: a long double comparison is a float
    // comparison, whatever register file it does or does not have.
    uint32_t cc = isRealIrType(inputAt(cond, 0)->type) ? emitFloatCompare(b, cond)
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

    // A long double is returned in st(0), which no register class names, so
    // this leaves it on the x87 stack rather than loading it into anything.
    // The stack is balanced across the call all the same: the caller's fstp is
    // the pop that matches this push. See selectCall.
    if (value->type == IR_F80) {
      x87Push(b, value);
      buildMachineInstr(b, X86_RET, 0, 0);
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
  //
  // A floating one allocates in an xmm register and there is no move of an
  // immediate into one, so the zero goes through a general register and across,
  // exactly as a float constant does. Writing the immediate straight to the
  // destination encoded the xmm as the same-numbered GP register instead.
  case IR_BAD: {
    uint8_t size = valueSize(i);
    uint32_t dst = machineBuilderVreg(b, i);
    Boolean viaGp = isFloatIrType(i->type);
    uint32_t zero = viaGp ? createVirtualRegister(b->mf, RC_GP, size) : dst;

    MachineInstr *mi = buildMachineInstr(b, X86_MOV, 1, 1);
    setRegisterOperand(mi, 0, zero);
    setImmediateOperand(mi, 1, 0);
    mi->opSize = size;

    if (viaGp) {
      MachineInstr *across = buildMachineInstr(b, X86_MOVD, 1, 1);
      setRegisterOperand(across, 0, dst);
      setRegisterOperand(across, 1, zero);
      across->opSize = size;
    }
    break;
  }

  // What is left of the IR opcode list is what nothing builds. IR_MOVE and
  // IR_BLOCK_PTR have no producer anywhere in the compiler; IR_E_CMP and
  // IR_E_FCMP are named only by gvn's classification switch and by nothing
  // that creates one; IR_E_FMOD is what getBinaryArith would make of '%' on
  // floating operands, which sema rejects before it gets there. IR_PHI and
  // IR_ALLOCA never arrive - the walk skips a phi and the frame takes an
  // alloca, both above.
  //
  // So this is a crash rather than a placeholder. Anything that starts
  // building one of these needs a rule in the same commit, and a backend that
  // quietly did less instead is what step 18 removed.
  default:
    unreachable("no selection rule for this IR instruction");
  }
}

static void selectTerminator_x86_64(MachineBuilder *b, const IrInstruction *i) {
  switch (i->kind) {
  case IR_BRANCH: selectBranch(b, i); break;
  case IR_CBRANCH: selectCondBranch(b, i); break;
  case IR_RET: selectReturn(b, i); break;
  case IR_TBRANCH: selectTableBranch(b, i); break;
  case IR_IBRANCH: selectIndirectBranch(b, i); break;

  // Every terminator the IR has is above; a block ends in one of these five or
  // termintateBlock would not have accepted it.
  default:
    unreachable("no selection rule for this IR terminator");
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

  // A returned constant is a move into the return register, which takes any
  // width - except a long double, which is returned by being loaded onto the
  // x87 stack out of memory, and an immediate has no address.
  if (use->kind == IR_RET) {
    return cnst->type != IR_F80;
  }

  // Two sizes that are spent at selection time rather than read out of a
  // register. A block copy's count decides which form the copy takes and, in
  // the unrolled one, reaches no operand at all; a static allocation's size
  // was spent by stage 0 laying out the frame, and a dynamic one's is a move
  // into a register that takes an immediate like any other (selectDynamicAlloca).
  //
  // Both matter because a constant that any use refuses holds a register for
  // the whole function - and these are the same constants: createAllocaSlot
  // and generateCompositeCopy both ask createIntegerConstant for the type's
  // size, and constants are shared, so one unfoldable use of the number kept
  // the other's register too.
  if (use->kind == IR_M_COPY) {
    return operandIdx == 2;
  }

  if (use->kind == IR_ALLOCA) {
    return operandIdx == 0;
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


// Every call is covered now, and the two things that used to stop one are both
// gone rather than merely unreached.
//
// A memory argument past the sixty-fourth was refused while the mask saying
// which inputs are aggregate bytes was one word: past that it had to answer
// "no", which is an address passed where the callee reads bytes. It is a
// bitmap sized to the input count now (see setCallMemoryArg), so the position
// no longer means anything.
//
// An argument of no register class - a void expression handed to a function
// expecting a value - was a frontend hole rather than a backend gap. Sema
// rejects it now (DIAG_VOID_NOT_IGNORED, see isCompatibleType), so nothing of
// the kind reaches translation at all.
//
// An over-aligned aggregate was the third, and step 17 removed it: pushing
// eightbyte by eightbyte landed a struct at whatever alignment the eightbyte
// before it left the stack pointer at, and callStackArea lays the area out
// instead.

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

// Which inputs reach an addressing mode, as a bit per position.
static uint32_t x86AddressOperands(const IrInstruction *i) {
  switch (i->kind) {
  case IR_M_LOAD:
  case IR_M_STORE:
    return 1u;

  // Both of them: a copy addresses its destination and its source alike.
  case IR_M_COPY:
    return 3u;

  // An x87 operand is an address that fld reads through, so it folds the way
  // a load's pointer does - and both of them, an x87 instruction naming two.
  case IR_E_FADD:
  case IR_E_FSUB:
  case IR_E_FMUL:
  case IR_E_FDIV:
  case IR_E_FEQ:
  case IR_E_FNE:
  case IR_E_FLT:
  case IR_E_FLE:
  case IR_E_FGT:
  case IR_E_FGE:
    return inputAt(i, 0)->type == IR_F80 ? 3u : 0;

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
  .reserveFrame = &x86ReserveFrame,
  .selectInstruction = &selectInstruction_x86_64,
  .selectTerminator = &selectTerminator_x86_64,
  .isLegalImmediate = &x86IsLegalImmediate,
  .isLegalAddressMode = &x86IsLegalAddressMode,
  .addressOperands = &x86AddressOperands,
  .foldsIntoCondition = &x86FoldsIntoCondition
};
