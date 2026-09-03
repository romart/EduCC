// An aggregate of one eightbyte made only of floats is SysV class SSE: it
// travels in xmm0, not rdi/rax. The IR backend does that; the legacy backend
// still passes every composite in the integer file, so the two disagree here
// on purpose - see crossabi/sse_aggregate.muted.
//
// Both are self-consistent, so this passes under either. What it guards is the
// IR path not sliding back; that it agrees with the *platform* is what tinycc's
// abitest says, through test/bench/bench_tcc.py.

struct F8 { float x, y; };
struct F4 { float x; };
struct D8 { double x; };
struct FA { float v[2]; };
struct Mixed { float x; int y; };   // not SSE: the int makes the eightbyte INTEGER

static struct F8 mkF8(float x, float y) { struct F8 r; r.x = x; r.y = y; return r; }
static float sumF8(struct F8 s) { return s.x + s.y; }

static struct F4 mkF4(float x) { struct F4 r; r.x = x; return r; }
static float getF4(struct F4 s) { return s.x; }

static struct D8 mkD8(double x) { struct D8 r; r.x = x; return r; }
static double getD8(struct D8 s) { return s.x; }

static struct FA mkFA(float a, float b) { struct FA r; r.v[0] = a; r.v[1] = b; return r; }
static float sumFA(struct FA s) { return s.v[0] + s.v[1]; }

static struct Mixed mkMixed(float x, int y) { struct Mixed r; r.x = x; r.y = y; return r; }
static double sumMixed(struct Mixed s) { return s.x + (double)s.y; }

// An SSE aggregate behind enough scalars to have used the xmm registers up,
// which is where taking the wrong file shows as the wrong register rather than
// as the wrong bytes in the right one.
static float deep(double a, double b, double c, double d,
                  double e, double f, double g, struct F8 s) {
  return (float)(a + b + c + d + e + f + g) + s.x + s.y;
}

int main() {
  struct F8 f8 = mkF8(1.5f, 2.25f);
  if (f8.x != 1.5f) return 1;
  if (f8.y != 2.25f) return 2;
  if (sumF8(f8) != 3.75f) return 3;
  if (sumF8(mkF8(4.0f, 0.5f)) != 4.5f) return 4;

  if (getF4(mkF4(7.5f)) != 7.5f) return 5;
  if (getD8(mkD8(9.25)) != 9.25) return 6;
  if (sumFA(mkFA(1.0f, 2.0f)) != 3.0f) return 7;
  if (sumMixed(mkMixed(1.5f, 2)) != 3.5) return 8;

  if (deep(1, 2, 3, 4, 5, 6, 7, mkF8(0.5f, 0.25f)) != 28.75f) return 9;

  // Through a pointer, so the aggregate is loaded rather than built in place.
  struct F8 *p = &f8;
  if (sumF8(*p) != 3.75f) return 10;

  return 0;
}
