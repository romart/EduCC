#ifndef __IR_MACHINE_H__
#define __IR_MACHINE_H__ 1

#include "common.h"
#include "mem.h"
#include "tree.h"
#include "utils.h"
#include "ir/target.h"

struct _IrFunction;
struct _IrBasicBlock;
struct _IrInstruction;
struct _Symbol;
struct _MachineBasicBlock;
struct _MachineFunction;

// Physical and virtual registers share one integer space: ids below FIRST_VREG
// are physical (indices into the TargetDescriptor's flat namespace, which tops
// out at IR_PHYS_REG_MAX == 64), ids at or above it are virtual. Register
// allocation is then "rewrite every operand >= FIRST_VREG" and nothing else
// about the representation changes shape. The gap between the two is slack for
// targets that grow their register file.
#define FIRST_VREG 256

// An absent register - an addressing mode with no index, mostly.
#define NO_REG ((uint32_t)-1)

#define isVirtualRegister(r) ((r) >= FIRST_VREG && (r) != NO_REG)
#define isPhysicalRegister(r) ((r) < FIRST_VREG)

// Opcode numbering is split in two. [0, MOP_TARGET_FIRST) are the
// target-independent pseudo-opcodes below, which the target-independent stages
// create directly and every target has to be able to expand or emit; a target
// numbers its own ISA opcodes from MOP_TARGET_FIRST upwards. Keeping both in
// one integer means a MachineInstr never has to say which namespace its opcode
// came from.
//
// MOP_UNSELECTED is the placeholder selection leaves behind for an IR
// instruction it has no rule for yet - loads, floats, aggregates, everything
// outside the integer subset stage 1 covers today. It is deliberately
// well-formed rather than a hole: it defines the value's register and uses its
// inputs, so the machine function stays a connected graph that liveness and
// the dumper can walk. What it is *not* is emittable, which is what
// MachineFunction.hasUnselected records.
//
// MOP_SPILL/MOP_RELOAD are the register allocator's two moves between a
// register and a frame slot. They are generic rather than per-target because
// the allocator that creates them is, and because every target spells them as
// one store and one load - stage 3 expands each into a single emit* call. The
// frame index is operand 0 in both, so the def-before-use operand order still
// holds: a reload defines a register, a spill defines nothing.
#define MACHINE_GENERIC_OPCODES                                             \
  MOP_DEF(PHI, "phi node, destroyed by stage 0 - never reaches allocation"), \
  MOP_DEF(COPY, "register to register move, either register class"),        \
  MOP_DEF(UNSELECTED, "an IR instruction stage 1 has no rule for yet"),     \
  MOP_DEF(SPILL, "store a register into its frame slot"),                   \
  MOP_DEF(RELOAD, "load a register back out of its frame slot")

enum MachineGenericOpcode {
#define MOP_DEF(m, _) MOP_##m
  MACHINE_GENERIC_OPCODES,
#undef MOP_DEF

  MOP_GENERIC_COUNT,

  MOP_TARGET_FIRST = 64
};

enum MachineOperandKind {
  // Zero on purpose: an operand array is zeroed when the instruction is
  // created, so an operand nobody filled in reads back as "not set" rather
  // than as whichever kind happens to be listed first.
  MO_NONE = 0,
  MO_REG,       // physical or virtual register, see FIRST_VREG
  MO_IMM,       // integer immediate
  MO_MEM,       // an address, see MachineAddress
  MO_FRAME_IDX, // a whole frame slot, which is all a spill or a reload means
  MO_MBB,       // branch target
  MO_SYMBOL,    // call target or global; becomes a Relocation during emission
};

// What an address is measured from. The register terms of an address say how
// far, this says from where, and the two are independent: keeping the second
// as a tag rather than as "whichever pointer field is non-NULL" is what stops
// the struct from growing one more mutually exclusive field per kind of thing
// an address can name - a literal, a jump table, a float pool entry.
enum MachineAddressKind {
  // Zero on purpose: an address built by filling in registers and nothing else
  // is anchored to those registers, which is the common case and the one a
  // zeroed struct should mean.
  MAK_REG = 0,  // base + index * scale + disp, and nothing else
  MAK_FRAME,    // a slot of this frame; the frame pointer is the base
  MAK_SYMBOL,   // a named object, addressed relative to the instruction pointer
  MAK_CONSTANT, // an entry of this function's constant pool, likewise
  MAK_BLOCK,    // a block of this function, for '&&label' and the tables below
  MAK_JUMPTABLE, // a jump table of this function, likewise
};

// Target-independent addressing mode. x86-64 lowers this to the Address struct
// its assembler already takes (src/x86_64/instructions_x86_64.h); riscv64 has
// no scaled-index mode, so its selector only ever fills base and disp. Address
// itself cannot be used here because it names x86 registers by their own enum
// and lives in an arch-private header.
//
// MAK_FRAME is what makes '[rbp - 8 + rax*4]' expressible, so a local array's
// subscript is one instruction. It spends the base on the frame pointer, which
// is why an address anchored to a slot has none of its own; MO_FRAME_IDX
// survives only as the operand of the allocator's own spill and reload, where
// a whole slot and nothing else is what is meant.
typedef struct _MachineAddress {
  enum MachineAddressKind kind;

  uint32_t base;  // register id, or NO_REG
  uint32_t index; // register id, or NO_REG
  uint32_t scale; // 1/2/4/8, 0 when there is no index
  int32_t disp;

  union {
    struct _Symbol *symbol; // MAK_SYMBOL
    uint32_t constantIdx;   // MAK_CONSTANT, into MachineFunction.constants
    int32_t frameIdx;       // MAK_FRAME, into MachineFunction.frame.objects
    struct _MachineBasicBlock *block; // MAK_BLOCK
    uint32_t jumpTableIdx;  // MAK_JUMPTABLE, into MachineFunction.jumpTables
  } anchor;
} MachineAddress;

// A constant this function needs a copy of in memory, because there is no
// instruction that materializes it and no name to reach it by. String literals
// today; a float too big for an immediate is the same thing and lands here
// when it arrives. The bytes are placed in a read-only section by emission -
// selection has no section to put them in - so what an address carries is an
// index into the pool, not a placed address.
enum MachineConstantKind {
  MCK_BYTES = 0, // literal bytes, copied out as they are
};

typedef struct _MachineConstant {
  enum MachineConstantKind kind;
  const char *bytes;
  size_t size;
  uint32_t alignment;
} MachineConstant;

// One switch's dispatch table: the block to enter for each value of a
// contiguous range, with the switch's default filling whatever the cases skip.
//
// Deliberately not a MachineConstant, though both are memory this function
// needs placed. A constant's bytes are known here and are the same wherever
// they land; a table's are addresses of blocks of this very function, which
// nothing knows until the blocks have been emitted. So the entries stay blocks
// all the way to stage 3, exactly as a branch target does.
typedef struct _MachineJumpTable {
  struct _MachineBasicBlock **entries;
  uint32_t count;
} MachineJumpTable;

typedef struct _MachineOperand {
  enum MachineOperandKind kind;

  struct {
    // The register class is deliberately *not* stored here. It is already
    // known - from TargetDescriptor.regClass for a physical register and from
    // VRegInfo for a virtual one - and a second copy is a second thing that
    // can disagree. Ask machineRegisterClass() instead.
    unsigned isDef : 1;
    unsigned isImplicit : 1;     // clobbers and flag registers, not written by selection
    unsigned isKill : 1;         // last use; set by liveness, read by allocation
    unsigned isEarlyClobber : 1; // defined before the uses are read
  } flags;

  union {
    uint32_t reg;
    int64_t imm;
    MachineAddress mem;
    int32_t frameIdx;
    struct _MachineBasicBlock *mbb;
    struct _Symbol *symbol;
  } info;
} MachineOperand;

typedef struct _MachineInstr {
  struct _MachineInstr *next, *prev;
  struct _MachineBasicBlock *parent;

  uint32_t opcode;

  // Operands are laid out defs first, then uses, so that liveness can scan the
  // array linearly and know which half it is in from the index alone.
  MachineOperand *operands;
  uint16_t numOperands;
  uint16_t numDefs;

  uint8_t opSize; // operand width in bytes; 0 when the opcode has no width

  // The *source* width, for the handful of opcodes that name two: a sign- or
  // zero-extending move, and every conversion between an integer and a float.
  // 'movsx r64, r8' is one instruction with an eight-byte destination and a
  // one-byte source, and opSize alone cannot say that.
  //
  // Zero for everything else, which is almost everything - an ALU instruction
  // has one width and both of its operands have it. Read it through
  // machineInstrSrcSize() rather than directly, so that "not set" reads as
  // "the same as the destination" in the one place instead of at every use.
  uint8_t srcSize;

  struct {
    // This instruction destroys every caller-saved register, whether or not it
    // names one. A call is the only thing that does.
    //
    // A bit rather than an implicit-def operand per register, which is what
    // the operand list would otherwise have to carry: SysV makes 9 GP and all
    // 16 xmm registers caller-saved, and 25 extra operands on every call would
    // bury the three or four that say something specific to it. The set itself
    // is TargetDescriptor.callerSavedRegs, and this is the flag that says to
    // go and read it. Nothing does yet - the trivial allocator keeps nothing
    // in a register across an instruction boundary, so there is nothing for a
    // call to destroy - and stage 2B is where that stops being true.
    unsigned isCall : 1;
  } flags;

  // Which IR instruction this was selected from, for dumps and -S comments.
  // NULL for anything the backend invents - frame setup, phi copies, spills.
  const struct _IrInstruction *origin;
} MachineInstr;

typedef struct _MachineBasicBlock {
  struct _MachineBasicBlock *next, *prev; // layout order, not CFG order
  struct _MachineFunction *parent;

  Vector preds; // of MachineBasicBlock *
  Vector succs;

  struct {
    MachineInstr *head;
    MachineInstr *tail;
  } instructions;

  // The IR block this mirrors, or NULL for blocks the backend invents - split
  // critical edges, most of all.
  const struct _IrBasicBlock *ir;

  const char *name;
  uint32_t id;
} MachineBasicBlock;

typedef struct _VRegInfo {
  enum RegClass rc;
  uint8_t size; // in bytes

  // The IR value this register holds, or NULL for one the backend invented
  // (the temporary that breaks a copy cycle, a spill slot's reload). Only ever
  // read by the dumper - without it a dump is a wall of %v numbers with
  // nothing to check them against.
  const struct _IrInstruction *origin;
} VRegInfo;

enum MachineFrameObjectKind {
  // A local that outlived mem2reg - address-taken, an aggregate, or a VLA -
  // and so still needs memory rather than a register.
  MFO_LOCAL = 0,
  // An argument the caller left on the stack. It is above the frame pointer
  // and was laid out by the ABI, not by us; it is listed so that everything
  // reachable through a frame index is reachable the same way.
  MFO_INCOMING_PARAM,
  // Where the stack pointer is parked before a dynamically sized allocation
  // moves it, so the frame can be restored.
  MFO_DYNAMIC_ALLOCA_SAVE,
  // Where a call's returned struct is put when the ABI hands it back in a
  // register rather than through a buffer the caller passed in. The IR has no
  // allocation for it - it asked for none, because at IR level the call simply
  // has a composite value - so selection is what discovers the need, and these
  // appear during stage 1 rather than stage 0.
  MFO_CALL_RESULT,
  // A virtual register's home, handed out by register allocation - the last of
  // the three sources the frame grows from, after what the IR asked for in
  // stage 0 and what selection discovered in stage 1. Each of the three leaves
  // MachineFrame.size correct for everything placed so far, so the prologue is
  // emitted in stage 3 and not before.
  MFO_SPILL,
};

typedef struct _MachineFrameObject {
  enum MachineFrameObjectKind kind;

  uint32_t size;      // 0 when the size is only known at run time
  uint32_t alignment; // in bytes, always a power of two

  // Displacement from the frame pointer: negative for anything this frame
  // allocates, positive for an incoming stack argument. Meaningless while
  // 'isDynamic' is set - such an allocation has no fixed home.
  int32_t offset;

  Boolean isDynamic;

  // Which virtual register this is the home of; 0 - never a valid register id,
  // see FIRST_VREG - for everything that is not an MFO_SPILL. Only the dumper
  // reads it, and without it a frame of spill slots is a list of anonymous
  // offsets with nothing to check against the code that uses them.
  uint32_t vreg;

  const struct _IrInstruction *origin;  // the IR_ALLOCA, when there is one
  struct _AstValueDeclaration *declaration; // the C variable, when there is one
} MachineFrameObject;

typedef struct _MachineFrame {
  Vector objects; // of MachineFrameObject *, indexed by frame index

  // Bytes reserved below the frame pointer. The spill area is *not* included:
  // its size is unknown until register allocation is done, which is why the
  // prologue is emitted in stage 3 and not here.
  uint32_t size;

  Boolean hasDynamicAlloca;
} MachineFrame;

typedef struct _MachineFunction {
  struct _IrFunction *ir;
  AstFunctionDefinition *ast;
  const TargetDescriptor *target;

  struct {
    MachineBasicBlock *head;
    MachineBasicBlock *tail;
  } blocks; // layout order, not CFG order

  size_t numBlocks;

  Vector vregs; // of VRegInfo *, indexed by (register id - FIRST_VREG)

  // Which virtual register holds each IR value, indexed by IrInstruction.id
  // (per-function and dense enough to index directly, if sparse after dce);
  // NO_REG for values that have not been asked for. Entries are handed out
  // lazily by machineVregForValue().
  //
  // Naming an IR value is not one stage's private business: stage 0 needs
  // vregs for the values phis carry before selection has run, and selection
  // has to reach the same answer for those same values afterwards. One map
  // both consult is what keeps them from disagreeing.
  Vector irToVreg;

  MachineFrame frame;

  // Of MachineConstant *, indexed by the constantIdx a MAK_CONSTANT address
  // carries. Per function, like the frame and for the same reason - every
  // machine structure here is; two functions naming the same bytes are made to
  // share them by the section writer that places them, not by this vector.
  Vector constants;

  // Of MachineJumpTable *, indexed by the jumpTableIdx a MAK_JUMPTABLE address
  // carries. Never shared between functions even in principle - the entries
  // name blocks of one particular function.
  Vector jumpTables;

  // Set when selection left at least one MOP_UNSELECTED behind, i.e. this
  // function contains something stage 1 cannot express yet. Register
  // allocation is still meaningful on such a function - the placeholder has
  // ordinary defs and uses - but emission is not, so stage 3 has to refuse it
  // rather than emit nonsense for the opcode it does not recognise.
  Boolean hasUnselected;

  // Why, when the whole function was turned away before selection rather than
  // one instruction at a time. NULL otherwise, including when hasUnselected
  // was set by an ordinary MOP_UNSELECTED placeholder - those say what they
  // are in the dump themselves.
  //
  // It exists so that a dump of such a function is not just mysteriously
  // empty: refusing before selection means the blocks hold nothing but stage
  // 0's phi copies, and without this the baseline would show a function that
  // looks finished and is not.
  const char *refusalReason;

  // The reason the first MOP_UNSELECTED placeholder was built with, so that a
  // report of this function falling back can say why without the reader having
  // to go find the matching 'ISEL:' line. NULL when nothing was refused.
  const char *firstUnselectedReason;

  // Set when register allocation declined this function - see
  // allocateRegisters(). Like hasUnselected it is a statement about
  // emittability and not about well-formedness: the machine function is
  // exactly as selection left it, virtual registers and all.
  Boolean hasUnallocated;

  // Which physical registers the finished code names, as a bit per register id
  // (IR_PHYS_REG_MAX is 64, so one word covers the namespace). Filled in by
  // register allocation, which is the first point at which the answer is
  // settled, and read by stage 3 to decide which callee-saved registers the
  // prologue has to preserve.
  uint64_t usedPhysRegs;

  // Which frame object holds each IR value, indexed by IrInstruction.id and
  // biased the same way as irToVreg. Only IR_ALLOCA values are ever in here;
  // it is how selection turns one into an MO_FRAME_IDX operand.
  Vector irToFrameIdx;

  // Everything below the MachineFunction is allocated from here. Today this is
  // the shared IrContext arena, which is what keeps the lifetime honest while
  // nothing releases machine code yet; once emission exists this becomes a
  // per-function arena released right after the function is emitted, and
  // because every allocation goes through mf->arena that is a one line change
  // in createMachineFunction().
  Arena *arena;

  uint32_t id;
} MachineFunction;

// ------------- construction ------------------------
MachineFunction *createMachineFunction(struct _IrFunction *f);
MachineBasicBlock *createMachineBasicBlock(MachineFunction *mf, const char *name,
                                           const struct _IrBasicBlock *ir);
void addMachineBasicBlockTail(MachineFunction *mf, MachineBasicBlock *mbb);
void addMachineSuccessor(MachineBasicBlock *block, MachineBasicBlock *succ);

MachineInstr *createMachineInstr(MachineFunction *mf, uint32_t opcode, uint16_t numDefs,
                                 uint16_t numUses);
void addMachineInstrTail(MachineBasicBlock *mbb, MachineInstr *mi);
void addMachineInstrHead(MachineBasicBlock *mbb, MachineInstr *mi);
// Inserts 'mi' immediately ahead of 'at', which must already be in a block.
// Selection needs this because stage 0 got to the block first: the phi copies
// it left at the tail carry values *out* of the block and have to stay there.
void addMachineInstrBefore(MachineInstr *at, MachineInstr *mi);
void addMachineInstrAfter(MachineInstr *at, MachineInstr *mi);
void eraseMachineInstr(MachineInstr *mi);

MachineOperand *machineOperandAt(MachineInstr *mi, uint16_t idx);

// Every register an operand names, as pointers into the operand so a caller
// can rewrite them in place. One for MO_REG; up to two - base and index - for
// MO_MEM; none for anything else. Writes at most MAX_OPERAND_REGS entries and
// returns how many.
//
// This exists because a register hidden inside an addressing mode is still a
// register: allocation has to reload it and rewrite it exactly like any other,
// and reading 'op->kind == MO_REG' - which is what every pass did before
// memory operands existed - silently walks past it and leaves a virtual
// register in the finished code.
//
// A memory operand's registers are always *reads*, whichever half of the
// operand list it sits in. Computing an address reads the registers it is
// made of even when the instruction is a store, which writes what they point
// at and not them; that is why a store's memory operand is built as a use.
#define MAX_OPERAND_REGS 2
uint16_t machineOperandRegisters(MachineOperand *op, uint32_t **out);

// The source width of an instruction that names two, and the destination width
// for the many that name one. See MachineInstr.srcSize.
uint8_t machineInstrSrcSize(const MachineInstr *mi);
void setRegisterOperand(MachineInstr *mi, uint16_t idx, uint32_t reg);
void setImmediateOperand(MachineInstr *mi, uint16_t idx, int64_t imm);
void setMemoryOperand(MachineInstr *mi, uint16_t idx, const MachineAddress *addr);
void setFrameIndexOperand(MachineInstr *mi, uint16_t idx, int32_t frameIdx);
void setBlockOperand(MachineInstr *mi, uint16_t idx, MachineBasicBlock *mbb);
void setSymbolOperand(MachineInstr *mi, uint16_t idx, struct _Symbol *symbol);

// ------------- virtual registers ------------------------
// irTypeMachineSize() is declared in ir/ir.h, with the other questions one can
// ask about an IR type - this header cannot name enum IrTypeKind.
uint32_t createVirtualRegister(MachineFunction *mf, enum RegClass rc, uint8_t size);
VRegInfo *virtualRegisterInfo(const MachineFunction *mf, uint32_t reg);
enum RegClass machineRegisterClass(const MachineFunction *mf, uint32_t reg);

// The virtual register holding an IR value, created on first ask. See
// MachineFunction.irToVreg.
uint32_t machineVregForValue(MachineFunction *mf, const struct _IrInstruction *value);
// Whether a value has been named already, without naming it. Selection asks
// this before deciding to fold a constant away: if stage 0 already put the
// value in a register, that is settled.
Boolean machineHasVregForValue(const MachineFunction *mf, const struct _IrInstruction *value);

// ------------- frame ------------------------
int32_t addMachineFrameObject(MachineFunction *mf, enum MachineFrameObjectKind kind, uint32_t size,
                              uint32_t alignment);
MachineFrameObject *machineFrameObjectAt(const MachineFunction *mf, int32_t frameIdx);
// The frame slot an IR_ALLOCA was given, or -1 if it was not given one.
int32_t machineFrameIndexForValue(const MachineFunction *mf, const struct _IrInstruction *value);

// Gives an object a frame-pointer-relative home, 'offset' bytes into a frame
// that grows downwards, and returns the new depth. Shared by the two stages
// that place objects - stage 0 for what the IR asked for, stage 2 for what it
// had to spill - so that the second cannot round or sign things differently
// from the first.
int32_t placeMachineFrameObject(MachineFunction *mf, int32_t offset, int32_t frameIdx);

// ------------- constant pool ------------------------

// Deduplicating: the same bytes asked for twice give back the same index, so a
// literal used twice in one function is one entry. Bytes are borrowed, not
// copied - they outlive the pool, being the parser's or the IR's own storage.
uint32_t addMachineConstant(MachineFunction *mf, enum MachineConstantKind kind, const char *bytes,
                            size_t size, uint32_t alignment);
const MachineConstant *machineConstantAt(const MachineFunction *mf, uint32_t constantIdx);

// ------------- jump tables ------------------------

// Takes a copy of 'entries'. Not deduplicating, unlike the constant pool: two
// switches with the same table would be the same switch.
uint32_t addMachineJumpTable(MachineFunction *mf, struct _MachineBasicBlock **entries,
                             uint32_t count);
const MachineJumpTable *machineJumpTableAt(const MachineFunction *mf, uint32_t jumpTableIdx);

// The rip-relative anchors carry the whole address in the relocation, so a
// register term or a displacement beside one would be silently dropped by
// every emitter. Stated once, rather than asserted at each of them.
Boolean isMachineAddressWellFormed(const MachineAddress *addr);

// ------------- build phase ------------------------
MachineFunction *buildMachineFunction(struct _IrFunction *f);

// ------------- stage 0: prepare / legalize ------------------------
MachineFunction *prepareMachineFunction(struct _IrFunction *f);

// The machine block mirroring an IR block, or NULL if there is none.
MachineBasicBlock *machineBlockForIrBlock(MachineFunction *mf, const struct _IrBasicBlock *ir);

// ------------- dump utils ------------------------
int32_t dumpMachineFunction(FILE *stream, const MachineFunction *mf);
void dumpMachineFunctionPhase(FILE *stream, const MachineFunction *mf, const char *phaseName);

#endif // __IR_MACHINE_H__
