// The other half of long_double.c. Whichever compiler builds this file, the
// one that built the caller has to agree with it about where every one of
// these values lives.

typedef long double LD;

struct One { LD a; };
struct Wide { LD a; LD b; };
struct Mixed { int i; LD v; };

LD p_add(LD a, LD b) { return a + b; }
LD p_sub(LD a, LD b) { return a - b; }
LD p_mul(LD a, LD b) { return a * b; }
LD p_div(LD a, LD b) { return a / b; }
LD p_neg(LD a) { return -a; }

// Four of them fill a0..a7 exactly, so the fifth is the first to reach the
// stack - and the stack slot is sixteen-byte aligned, which is the half of the
// rule a register pair does not show.
LD p_five(LD a, LD b, LD c, LD d, LD e) { return a + b * 2 + c * 3 + d * 4 + e * 5; }

// A long double behind enough integers to have pushed the register file along
// an odd number of places, which is where an aligned pair would differ from
// the next free register. Named arguments take no such alignment; this is what
// says so.
LD p_afterInts(int a, int b, int c, LD x) { return x * (a + b + c); }
LD p_afterFive(long a, long b, long c, long d, long e, LD x) { return x + a + b + c + d + e; }

// Mixed files: the integer and floating registers are counted separately, so
// this is also a check that a long double consumes integer ones.
LD p_mixed(int i, LD a, double d, LD b, long l, float f) {
  return a + b + i + d + l + f;
}

LD p_one(struct One s) { return s.a * 3.0L; }
struct One p_mkone(LD v) { struct One r; r.a = v + 1.0L; return r; }
LD p_wide(struct Wide s) { return s.a - s.b; }
struct Wide p_mkwide(LD a, LD b) { struct Wide r; r.a = a; r.b = b; return r; }
LD p_mixedStruct(struct Mixed m) { return m.v * m.i; }

int p_cmp(LD a, LD b) {
  if (a < b) return -1;
  if (a > b) return 1;
  if (a == b) return 0;
  return 2;  // unordered
}

int p_rel(LD a, LD b, int *out) {
  out[0] = a < b; out[1] = a <= b; out[2] = a > b;
  out[3] = a >= b; out[4] = a == b; out[5] = a != b;
  return 0;
}

double p_toDouble(LD a) { return (double)a; }
float p_toFloat(LD a) { return (float)a; }
long p_toLong(LD a) { return (long)a; }
int p_toInt(LD a) { return (int)a; }
unsigned long p_toULong(LD a) { return (unsigned long)a; }
short p_toShort(LD a) { return (short)a; }
signed char p_toChar(LD a) { return (signed char)a; }

LD p_fromDouble(double a) { return (LD)a; }
LD p_fromFloat(float a) { return (LD)a; }
LD p_fromInt(int a) { return (LD)a; }
LD p_fromLong(long a) { return (LD)a; }
LD p_fromUInt(unsigned a) { return (LD)a; }
LD p_fromULong(unsigned long a) { return (LD)a; }
