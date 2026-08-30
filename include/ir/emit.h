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
// Every machine function that reaches here can be emitted. There used to be a
// pair of "not this one" flags on the MachineFunction - selection had left a
// placeholder, or allocation had declined - and a caller that fell back to the
// legacy backend when either was set. Step 18 removed both: what the earlier
// stages cannot express aborts where it is found, so by the time emission runs
// every operand is a physical register and every opcode has an encoding.
//
// Emits 'mf' into the context's .text section. Returns the GeneratedFunction
// describing it, exactly as the legacy generateFunction would, so everything
// downstream - symbols, relocations, ELF layout - cannot tell the two apart.
struct _GeneratedFunction *emitMachineFunction_x86_64(struct _GenerationContext *ctx,
                                                     MachineFunction *mf);

#endif // __IR_EMIT_H__
