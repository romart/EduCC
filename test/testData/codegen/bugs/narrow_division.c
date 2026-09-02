// '/=' and '%=' on a char or short.
//
// C promotes both operands to int before dividing, and both backends used to
// divide at the operand's own width instead. That is wrong twice over. At one
// byte there is no divide to emit at all - x86 sign-extends into rdx:rax only
// from 16 bits up, and the byte form leaves its remainder in ah, which neither
// machine model can name - so both aborted outright ("cdq has no byte form" in
// the IR backend, "Unexptected operand size" in the legacy one) and nothing
// below had ever run. At two bytes it assembles and traps: '(short)-32768 / -1'
// overflows a 16-bit divide, where the promoted one is just 32768.
//
// Both now divide at 32 bits and let the store narrow the result.
//
// Three more bugs were behind that abort, all in the legacy backend and all
// only reachable once it stopped aborting: the divisor was materialised at its
// own width ('mov al, 3') and read as 32 bits, the address was left in rax so
// the dividend's own load overwrote it, and a bit-field '%=' stored out of rdx,
// which storeBitField uses as its temporary.

struct B {
  unsigned u : 3;
  int s : 5;
};

static char cd = 3;
static unsigned char ucd = 7;
static short sd = 7;
static int mone = -1;

int main(void) {
  char c;
  unsigned char u;
  short h;
  unsigned short uh;
  struct B b;
  int v;

  c = -100; if ((v = (c /= 3)) != -33 || c != -33) return 1;
  c = -100; if ((v = (c %= 3)) != -1 || c != -1) return 2;
  u = 250;  if ((v = (u /= 7)) != 35 || u != 35) return 3;
  u = 250;  if ((v = (u %= 7)) != 5 || u != 5) return 4;

  h = -3000;  if ((v = (h /= 7)) != -428 || h != -428) return 5;
  uh = 60000; if ((v = (uh %= 7)) != 3 || uh != 3) return 6;

  // A divisor that is an lvalue rather than a literal: it arrives in a
  // register at its own width, and has to be widened like the dividend.
  c = -100; if ((v = (c /= cd)) != -33 || c != -33) return 7;
  u = 250;  if ((v = (u %= ucd)) != 5 || u != 5) return 8;
  h = -3000; if ((v = (h /= sd)) != -428 || h != -428) return 9;

  // Through a pointer, so the address is in a register the divide wants.
  {
    char a[3];
    unsigned char ua[3];

    a[0] = -100; a[1] = 7;
    if ((v = (*a /= a[1])) != -14 || a[0] != -14) return 10;

    ua[0] = 250; ua[1] = 7;
    if ((v = (*ua %= ua[1])) != 5 || ua[0] != 5) return 11;
  }

  // Promoted, so these are 128 and 32768 before the store narrows them - a
  // divide at the operand's own width overflows instead.
  {
    signed char cm = -128;
    short sm = -32768;

    if ((v = (cm /= mone)) != -128 || cm != -128) return 12;
    if ((v = (sm /= mone)) != -32768 || sm != -32768) return 13;
  }

  b.s = -15; if ((v = (b.s /= 2)) != -7 || b.s != -7) return 14;
  b.s = 13;  if ((v = (b.s %= 5)) != 3 || b.s != 3) return 15;
  b.u = 7;   if ((v = (b.u %= 3)) != 1 || b.u != 1) return 16;
  b.u = 6;   if ((v = (b.u /= 2)) != 3 || b.u != 3) return 17;

  return 0;
}
