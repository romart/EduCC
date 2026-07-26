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
// instruction it has no rule for yet - loads, calls, floats, everything
// outside the integer subset stage 1 covers today. It is deliberately
// well-formed rather than a hole: it defines the value's register and uses its
// inputs, so the machine function stays a connected graph that liveness and
// the dumper can walk. What it is *not* is emittable, which is what
// MachineFunction.hasUnselected records.
#define MACHINE_GENERIC_OPCODES                                             \
  MOP_DEF(PHI, "phi node, destroyed by stage 0 - never reaches allocation"), \
  MOP_DEF(COPY, "register to register move, either register class"),        \
  MOP_DEF(UNSELECTED, "an IR instruction stage 1 has no rule for yet")

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
  MO_MEM,       // base + index * scale + disp [+ symbol]
  MO_FRAME_IDX, // a frame slot; becomes frame-pointer-relative during emission
  MO_MBB,       // branch target
  MO_SYMBOL,    // call target or global; becomes a Relocation during emission
};

// Target-independent addressing mode. x86-64 lowers this to the Address struct
// its assembler already takes (src/x86_64/instructions_x86_64.h); riscv64 has
// no scaled-index mode, so its selector only ever fills base and disp. Address
// itself cannot be used here because it names x86 registers by their own enum
// and lives in an arch-private header.
typedef struct _MachineAddress {
  uint32_t base;  // register id, or NO_REG
  uint32_t index; // register id, or NO_REG
  uint32_t scale; // 1/2/4/8, 0 when there is no index
  int32_t disp;
  struct _Symbol *symbol; // NULL unless this is a symbol-relative reference
} MachineAddress;

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

  // Set when selection left at least one MOP_UNSELECTED behind, i.e. this
  // function contains something stage 1 cannot express yet. Register
  // allocation is still meaningful on such a function - the placeholder has
  // ordinary defs and uses - but emission is not, so stage 3 has to refuse it
  // rather than emit nonsense for the opcode it does not recognise.
  Boolean hasUnselected;

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
void eraseMachineInstr(MachineInstr *mi);

MachineOperand *machineOperandAt(MachineInstr *mi, uint16_t idx);
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
