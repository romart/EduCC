// selectX87Conversion built the store into the x87 staging slot before
// selecting the value it stores, so the widening a narrow unsigned source
// needs landed after the store that read it.

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

union CValue { long double ld; double d; u64 i; };
static union CValue g;

static u8 gu8 = 2;
static u16 gu16 = 3;
static u32 gu32 = 4;
static signed char gs8 = -5;
static short gs16 = -6;

int main() {
  // The shape tcc's gen_cast folds an integer literal to a double with: the
  // source is loaded from the same union the result is stored into.
  g.i = 2;
  g.ld = (u32)g.i;
  if (g.ld != 2.0L) return 1;

  g.i = 2;
  g.ld = (u32)g.i;
  g.d = (double)g.ld;
  if (g.d != 2.0) return 2;

  // Every narrow source that makes selectWidened emit an instruction.
  if ((long double)gu8 != 2.0L) return 3;
  if ((long double)gu16 != 3.0L) return 4;
  if ((long double)gu32 != 4.0L) return 5;
  if ((long double)gs8 != -5.0L) return 6;
  if ((long double)gs16 != -6.0L) return 7;

  {
    u64 i = 2;
    long double ld = (u32)i;
    if (ld != 2.0L) return 8;
  }

  {
    union CValue l;
    l.i = 7;
    l.ld = (u32)l.i;
    if (l.ld != 7.0L) return 9;
  }

  // The other arm of the same rule, a float source, which needs no widening.
  {
    float f = 1.5f;
    double d = 2.5;
    if ((long double)f != 1.5L) return 10;
    if ((long double)d != 2.5L) return 11;
  }

  return 0;
}
