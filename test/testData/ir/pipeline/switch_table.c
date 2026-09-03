// The two lowerings of one IR instruction, side by side.
//
// IR_TBRANCH says nothing about how a switch is dispatched - the frontend
// takes no view and the legacy backend emits a compare chain for every switch
// there is. Selection chooses, and it chooses on the case *values*: a table
// costs eight bytes per value of the range whether a case lands on it or not,
// so it is only built when the range is small and mostly claimed.
//
// 'dense' is the shape that earns one - six cases over seven values, with the
// hole at 6 filled by the default, which is what the jt#0 line below is for.
// 'sparse' is the same six cases spread over a range no table could cover, and
// 'few' is dense but too small to pay for the seven-instruction dispatch. Both
// come out as compares.
//
// The baseline is the point of the fixture. The dispatch is identical whatever
// the table holds, so a switch sent to the wrong block is invisible in the
// instructions and visible only in the entries.

int dense(int op) {
  switch (op) {
  case 1: return 100;
  case 2: return 200;
  case 3: return 300;
  case 4: return 400;
  case 5: return 500;
  case 7: return 700;
  default: return -1;
  }
}

int sparse(int op) {
  switch (op) {
  case 1: return 100;
  case 200: return 200;
  case 3000: return 300;
  case 40000: return 400;
  case 500000: return 500;
  case 6000000: return 700;
  default: return -1;
  }
}

int few(int op) {
  switch (op) {
  case 1: return 10;
  case 2: return 20;
  case 3: return 30;
  default: return -1;
  }
}

// Negative cases, so the bias subtracted before the index is used is not zero
// and not something a wrong sign would still get right.
int negative(int op) {
  switch (op) {
  case -4: return 1;
  case -3: return 2;
  case -2: return 3;
  case -1: return 4;
  case 0: return 5;
  default: return -1;
  }
}
