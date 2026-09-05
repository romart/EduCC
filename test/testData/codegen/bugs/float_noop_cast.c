// '(double)d' on a double is a move. Selection emitted CVTF2F for it, which
// reads only the source width and assumes the destination is the other one -
// so the cast converted the value to a float. SQLite's tostringbuff hands
// '(double)obj.value_.n' to snprintf and printed 5.68e-315.

typedef union { void *p; long i; double n; } Value;
typedef struct { Value value_; unsigned char tag; } TValue;

static double viaCast(TValue o) { return (double)o.value_.n; }
static float  narrowCast(double d) { return (float)d; }
static double widenCast(float f) { return (double)f; }

int main(void) {
  double d = 1203.125;
  TValue v;
  float f = 2.5f;

  v.value_.n = 1203.125;
  v.tag = 3;

  if ((double)d != 1203.125) return 1;
  if (viaCast(v) != 1203.125) return 2;
  if ((float)f != 2.5f) return 3;

  /* the real conversions still have to convert */
  if (narrowCast(1.0 / 3.0) == 1.0 / 3.0) return 4;
  if (widenCast(2.5f) != 2.5) return 5;
  return 0;
}
