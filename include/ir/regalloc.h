#ifndef __IR_REGALLOC_H__
#define __IR_REGALLOC_H__ 1

#include "ir/machine.h"

// -============================ Stage 2: register allocation ==============-
//
// Two allocators, and the choice between them is '-Xregalloc'.
//
// 2A, "spill everything" (src/ir/codegen/regalloc.c), gives every virtual
// register a frame slot and nothing else: each instruction reloads the
// registers it reads into a small reserved scratch set just before it runs and
// stores whatever it wrote straight back out afterwards. No liveness, no
// interference graph, no coalescing. The code is as bad as it sounds, and that
// is the point - it closed the pipeline so stages 1 and 3 could be debugged
// against output that was merely terrible, and it stays as a differential
// oracle, since any later allocator has to produce programs with identical
// observable behaviour and the whole codegen suite then tests the allocator
// specifically.
//
// 2B, linear scan (src/ir/codegen/regalloc_linear.c), is the default: live
// intervals over the linearized order, a sweep that hands out registers and
// spills the interval reaching furthest ahead. See
// docs/ir-codegen-design.md section 7.
//
// After either runs, no MO_REG operand names a virtual register - that is the
// one postcondition everything downstream depends on, and it is asserted.

void allocateRegisters(MachineFunction *mf, Boolean trivial);

// -============================ The shared spiller ========================-
//
// Deliberately not part of either allocator. Both of them - and the graph
// colouring of step 35 after them - answer the same two questions differently
// ("which values get registers", "which one goes when there are none left")
// and the same three questions identically: where a spilled value lives, what
// the code around it looks like, and what happens next.
//
// What happens next is the part worth naming. A spilled value's *uses* still
// need a register, and this is where they get one: rewriting a spill site
// introduces a fresh virtual register live across one instruction, and the
// allocator then runs again over the rewritten function. Reloads are therefore
// allocated by the same machinery as everything else rather than out of a
// reserved pool, which is what lets stage 2B need no scratch registers at all
// and what makes the same spiller serve an iterating colouring allocator
// unchanged.
//
// That it terminates is Briggs' argument. A register this spiller created is
// never spilled again: in 2B that falls out of the heuristic rather than
// needing enforcement - a one-instruction interval has the nearest end point,
// so "spill the interval reaching furthest ahead" never picks it - and it is
// asserted, because a heuristic that quietly stopped holding would loop here
// rather than fail.
typedef struct _SpillState {
  MachineFunction *mf;

  // The frame slot holding each spilled virtual register, indexed by
  // (id - FIRST_VREG) and biased by one so an unwritten entry reads as "no
  // slot yet" rather than as frame index 0.
  Vector vregToSlot;

  // How far below the frame pointer the frame reaches so far. Starts at what
  // stages 0 and 1 laid out, so spill slots sit underneath the locals.
  int32_t frameOffset;

  // The first virtual register this spiller invented, or 0 while it has
  // invented none. Everything from there up is reload and spill traffic; see
  // the termination argument above.
  uint32_t firstSpillerVreg;
} SpillState;

void initSpillState(SpillState *ss, MachineFunction *mf);
void releaseSpillState(SpillState *ss);

// Hands out the slot for a virtual register, on first ask.
int32_t spillSlotForVreg(SpillState *ss, uint32_t vreg);

// Whether this register is one the spiller invented rather than one selection
// asked for.
Boolean isSpillerVreg(const SpillState *ss, uint32_t vreg);

// Rewrites every site naming a register in 'spilled' - a bitset indexed by
// (id - FIRST_VREG) - to name a fresh register instead, with a reload ahead of
// each read and a spill behind each write.
void insertSpillCode(SpillState *ss, const BitSet *spilled);

// Rounds the frame out to what stage 3 takes as final. The ABI wants the stack
// pointer 16-byte aligned at a call.
void finishSpillFrame(SpillState *ss);

// The allocator's two moves between a register and a frame slot. The register
// may still be virtual - that is exactly what the spiller builds.
MachineInstr *buildReloadInstr(MachineFunction *mf, uint32_t reg, int32_t frameIdx, uint8_t size);
MachineInstr *buildSpillInstr(MachineFunction *mf, int32_t frameIdx, uint32_t reg, uint8_t size);

// Every physical register the finished function names, which is what stage 3
// needs in order to know which callee-saved registers its prologue has to
// preserve. Computed over the final code rather than accumulated as registers
// are handed out: selection's own fixed registers count too, and reading them
// off the result cannot get out of step with it.
void recordUsedPhysRegs(MachineFunction *mf);

// -============================ Stage 2B ============================-
void allocateRegistersLinear(MachineFunction *mf);

#endif // __IR_REGALLOC_H__
