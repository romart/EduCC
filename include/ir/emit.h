#ifndef __IR_EMIT_H__
#define __IR_EMIT_H__ 1

#include "ir/machine.h"

struct _GenerationContext;
struct _GeneratedFunction;

// -============================ Stage 3: emission =========================-
//
// The last stage: a MachineFunction whose registers are all physical becomes
// bytes in the .text section, through the same assembler the legacy backend
// uses (src/x86_64/instructions_x86_64.c). See docs/ir-codegen-design.md
// section 8.
//
// This is where the frame is finally known - stage 0 sized what the IR asked
// for, stage 2 added what it had to spill, and the set of callee-saved
// registers the code actually names is only settled once allocation has run -
// so the prologue and epilogue are built here and nowhere earlier.
//
// Not every machine function can be emitted. One that still contains
// MOP_UNSELECTED has instructions with no encoding, and one register
// allocation declined still has virtual registers in its operands; both are
// recorded on the MachineFunction and both make this refuse. Refusing is a
// supported outcome rather than an error: the caller falls back to the legacy
// backend for that function, which is what lets the new pipeline take over one
// construct at a time instead of all at once.
Boolean canEmitMachineFunction(const MachineFunction *mf);

// Emits 'mf' into the context's .text section. Returns the GeneratedFunction
// describing it, exactly as the legacy generateFunction would, so everything
// downstream - symbols, relocations, ELF layout - cannot tell the two apart.
// Only valid when canEmitMachineFunction() says so.
struct _GeneratedFunction *emitMachineFunction_x86_64(struct _GenerationContext *ctx,
                                                     MachineFunction *mf);

#endif // __IR_EMIT_H__
