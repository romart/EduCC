// A byte or halfword load widens as it loads, and since roadmap step 40 the
// register says so, which lets the next use skip an extension it would
// otherwise repeat. The saving is invisible from an exit code; what is not
// invisible is getting it wrong, and there are two ways to.
//
// The first is signedness: believing a zero-extended register is a
// sign-extended one turns -1 into 255. Every function here reads a negative
// signed byte and a large unsigned one so that the two cannot pass for each
// other.
//
// The second is the width the note claims. Four bytes of a register are only
// four bytes for as long as the register is declared that wide - a one-byte
// virtual register is a one-byte spill slot, and a reload out of it brings
// back one byte and three of whatever the scratch register held. That is why
// taking the note widens the register, and why 'spilled' below keeps more
// narrow loads live at once than the machine has registers: under
// -Xregalloc=trivial every one of them makes the round trip, and under the
// other two the ones that lose the colouring do.

static signed char bytes[8] = { -1, -128, 127, 0, 1, -2, 100, -100 };
static unsigned char ubytes[8] = { 255, 128, 127, 0, 1, 254, 100, 156 };
static short halves[4] = { -1, -32768, 32767, 300 };

static int sumSigned(const signed char *p, int n) {
  int total = 0;

  for (int i = 0; i < n; ++i) {
    total += p[i];
  }

  return total;
}

static int sumUnsigned(const unsigned char *p, int n) {
  int total = 0;

  for (int i = 0; i < n; ++i) {
    total += p[i];
  }

  return total;
}

// The load's register is read at one byte and at four in the same function,
// which is the pair the note has to keep consistent.
static int bothWidths(const signed char *p, signed char limit) {
  int wide = *p;

  return (*p == limit) + (wide < 0) * 2;
}

// The load feeds a phi, whose register stage 0 named before selection ran.
static int throughPhi(const unsigned char *p, int take) {
  int c = 7;

  if (take) {
    c = *p;
  }

  return c;
}

// A narrow load used as a subscript, which wants eight bytes where the note
// promises four - so this one still widens, and from the value's own width.
static int indexed(const int *a, const signed char *p) {
  return a[*p + 3];
}

// More live narrow values than there are registers, so they are spilled and
// reloaded at whatever width their declaration says.
static int spilled(const signed char *p) {
  int a = p[0];
  int b = p[1];
  int c = p[2];
  int d = p[3];
  int e = p[4];
  int f = p[5];
  int g = p[6];
  int h = p[7];

  int sum = a + b + c + d + e + f + g + h;

  return sum + (a < b) + (c < d) + (e < f) + (g < h) + (a == -1) + (h == -100);
}

int main() {
  int table[8] = { 10, 20, 30, 40, 50, 60, 70, 80 };

  if (sumSigned(bytes, 8) != -3) return 1;
  if (sumUnsigned(ubytes, 8) != 1021) return 2;

  if (bothWidths(bytes, -1) != 3) return 3;
  if (bothWidths(bytes + 2, 127) != 1) return 4;
  if (bothWidths(bytes + 3, 9) != 0) return 5;

  if (throughPhi(ubytes, 1) != 255) return 6;
  if (throughPhi(ubytes, 0) != 7) return 7;
  if (throughPhi(ubytes + 3, 1) != 0) return 8;

  // bytes[3] is 0, bytes[4] is 1, bytes[0] is -1: subscripts 3, 4 and 2.
  if (indexed(table, bytes + 3) != 40) return 9;
  if (indexed(table, bytes + 4) != 50) return 10;
  if (indexed(table, bytes) != 30) return 11;

  // -3 for the eight bytes, plus a<b (0), c<d (0), e<f (0), g<h (0),
  // a==-1 (1), h==-100 (1).
  if (spilled(bytes) != -1) return 12;

  if (halves[0] + halves[1] + halves[2] + halves[3] != 298) return 13;

  {
    short s = halves[1];
    unsigned short u = (unsigned short)halves[1];

    if ((int)s != -32768) return 14;
    if ((int)u != 32768) return 15;
    if (s == (short)u && (int)s == (int)u) return 16;
  }

  return 0;
}
