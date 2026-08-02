#ifndef __IR_REGALLOC_H__
#define __IR_REGALLOC_H__ 1

#include "ir/machine.h"

// -============================ Stage 2A: trivial allocation ==============-
//
// Gives every virtual register a frame slot and nothing else: each instruction
// reloads the registers it reads into a small reserved scratch set just before
// it runs, and stores whatever it wrote straight back out afterwards. No
// liveness, no interference graph, no coalescing. See
// docs/ir-codegen-design.md section 7.
//
// The code this produces is as bad as it sounds and that is the point. It
// closes the pipeline, so stages 1 and 3 can be developed and debugged against
// output that is merely terrible rather than against half an allocator; and it
// stays afterwards as a differential oracle, since any later allocator has to
// produce programs with identical observable behaviour and the whole codegen
// suite then tests the allocator specifically.
//
// After this runs, no MO_REG operand names a virtual register - that is the
// one postcondition everything downstream depends on, and it is asserted.

void allocateRegisters(MachineFunction *mf);

#endif // __IR_REGALLOC_H__
