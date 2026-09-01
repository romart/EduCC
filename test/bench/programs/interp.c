// A switch-dispatched bytecode loop: one basic block per opcode, all of them
// merging back to the same header, so the values live across the switch are
// live across every arm of it.
#include <stdio.h>

#define STACKSZ 256
#define ROUNDS 1800

enum Op {
  OP_PUSH, OP_LOAD, OP_STORE, OP_ADD, OP_SUB, OP_MUL, OP_LT,
  OP_JMP, OP_JZ, OP_HALT
};

static int prog[] = {
  OP_PUSH, 0,  OP_STORE, 0,        /* i = 0 */
  OP_PUSH, 0,  OP_STORE, 1,        /* acc = 0 */
  /* 8: */
  OP_LOAD, 0,  OP_PUSH, 3000, OP_LT, OP_JZ, 34,
  OP_LOAD, 1,  OP_LOAD, 0,  OP_LOAD, 0,  OP_MUL, OP_ADD, OP_STORE, 1,
  OP_LOAD, 0,  OP_PUSH, 1,  OP_ADD, OP_STORE, 0,
  OP_JMP, 8,
  /* 34: */
  OP_LOAD, 1,  OP_HALT
};

static int vars[8];

static int run() {
  int stack[STACKSZ];
  int sp = 0, pc = 0;

  for (;;) {
    int op = prog[pc++];
    switch (op) {
    case OP_PUSH:  stack[sp++] = prog[pc++]; break;
    case OP_LOAD:  stack[sp++] = vars[prog[pc++]]; break;
    case OP_STORE: vars[prog[pc++]] = stack[--sp]; break;
    case OP_ADD:   --sp; stack[sp - 1] += stack[sp]; break;
    case OP_SUB:   --sp; stack[sp - 1] -= stack[sp]; break;
    case OP_MUL:   --sp; stack[sp - 1] *= stack[sp]; break;
    case OP_LT:    --sp; stack[sp - 1] = stack[sp - 1] < stack[sp]; break;
    case OP_JMP:   pc = prog[pc]; break;
    case OP_JZ:    if (stack[--sp] == 0) pc = prog[pc]; else ++pc; break;
    case OP_HALT:  return stack[sp - 1];
    }
  }
}

int main() {
  long total = 0;
  int round;

  for (round = 0; round < ROUNDS; ++round) {
    vars[0] = vars[1] = 0;
    total += run();
  }

  printf("%ld\n", total);
  return 0;
}
