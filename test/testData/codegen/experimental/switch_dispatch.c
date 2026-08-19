// Switches, run rather than read.
//
// switch_table.c in the IR fixtures says which of the two lowerings each shape
// gets; this says both of them dispatch to the right place. The failures it is
// looking for are the ones a dump cannot show.
//
// For the table: an index biased by the wrong amount, or biased right and then
// widened with the wrong sign - both of which land inside the table and return
// some other case's answer rather than crashing. The bounds check is the other
// half, and it is one unsigned compare doing two jobs, so every switch here is
// asked about values below its lowest case and above its highest.
//
// For the chain: a case value that does not fit the 32-bit immediate an ALU
// compare encodes, which is silently truncated by an emitter that does not
// notice - so 'huge' below has cases that differ only above the low 32 bits.
//
// Every arm returns a value that identifies it, so a dispatch to the wrong
// block is a wrong number rather than a plausible one.

// Dense with a hole: six cases over seven values, 6 falling to the default.
static int dense(int op) {
  switch (op) {
  case 1: return 101;
  case 2: return 102;
  case 3: return 103;
  case 4: return 104;
  case 5: return 105;
  case 7: return 107;
  default: return -1;
  }
}

// The same, biased below zero, so the index is the condition plus four rather
// than the condition itself.
static int negative(int op) {
  switch (op) {
  case -4: return 201;
  case -3: return 202;
  case -2: return 203;
  case -1: return 204;
  case 0: return 205;
  default: return -1;
  }
}

// Too sparse for any table: a compare chain, six cases wide.
static int sparse(int op) {
  switch (op) {
  case 1: return 301;
  case 200: return 302;
  case 3000: return 303;
  case 40000: return 304;
  case 500000: return 305;
  case 6000000: return 306;
  default: return -1;
  }
}

// Case values a 32-bit immediate cannot hold, and which agree in their low 32
// bits so that truncating one into the other is what a wrong encoding does.
static int huge(long long op) {
  switch (op) {
  case 0x100000001LL: return 401;
  case 0x200000001LL: return 402;
  case 1: return 403;
  default: return -1;
  }
}

// Grouped labels and fallthrough, where several values reach one block and one
// block runs on into the next.
static int grouped(int op) {
  int r = 0;

  switch (op) {
  case 1:
  case 2:
  case 3:
    r += 1;
    /* falls through */
  case 4:
  case 5:
    r += 10;
    break;
  case 6:
    r += 100;
    break;
  default:
    r += 1000;
    break;
  }

  return r;
}

// No default at all: everything unmatched leaves the switch, which is the same
// edge as a default for dispatch but a different block for the frontend.
static int noDefault(int op) {
  int r = 7;

  switch (op) {
  case 10: r = 1; break;
  case 11: r = 2; break;
  case 12: r = 3; break;
  case 13: r = 4; break;
  case 14: r = 5; break;
  }

  return r;
}

// An unsigned condition narrower than a register, so the widening before the
// index has to fill with zeroes and not with the sign.
static int unsignedChar(unsigned char op) {
  switch (op) {
  case 250: return 501;
  case 251: return 502;
  case 252: return 503;
  case 253: return 504;
  case 254: return 505;
  case 255: return 506;
  default: return -1;
  }
}

int main(void) {
  int i;

  // Inside, in the hole, and outside on both sides.
  if (dense(1) != 101) return 1;
  if (dense(5) != 105) return 2;
  if (dense(7) != 107) return 3;
  if (dense(6) != -1) return 4;
  if (dense(0) != -1) return 5;
  if (dense(8) != -1) return 6;
  if (dense(-1) != -1) return 7;
  if (dense(-1000000) != -1) return 8;

  if (negative(-4) != 201) return 9;
  if (negative(0) != 205) return 10;
  if (negative(-5) != -1) return 11;
  if (negative(1) != -1) return 12;

  if (sparse(1) != 301) return 13;
  if (sparse(6000000) != 306) return 14;
  if (sparse(2) != -1) return 15;
  if (sparse(0) != -1) return 16;

  if (huge(0x100000001LL) != 401) return 17;
  if (huge(0x200000001LL) != 402) return 18;
  if (huge(1) != 403) return 19;
  if (huge(0x300000001LL) != -1) return 20;

  if (grouped(1) != 11) return 21;
  if (grouped(3) != 11) return 22;
  if (grouped(4) != 10) return 23;
  if (grouped(6) != 100) return 24;
  if (grouped(99) != 1000) return 25;

  if (noDefault(10) != 1) return 26;
  if (noDefault(14) != 5) return 27;
  if (noDefault(9) != 7) return 28;
  if (noDefault(15) != 7) return 29;

  if (unsignedChar(250) != 501) return 30;
  if (unsignedChar(255) != 506) return 31;
  if (unsignedChar(249) != -1) return 32;
  if (unsignedChar(0) != -1) return 33;

  // Every value of the dense range in turn, so no single entry of the table is
  // left untried and a table off by one anywhere shows up here.
  for (i = -2; i <= 10; ++i) {
    int expect = -1;

    if (i >= 1 && i <= 5) expect = 100 + i;
    if (i == 7) expect = 107;

    if (dense(i) != expect) return 34;
  }

  for (i = -6; i <= 2; ++i) {
    int expect = i >= -4 && i <= 0 ? 205 + i : -1;

    if (negative(i) != expect) return 35;
  }

  return 0;
}
