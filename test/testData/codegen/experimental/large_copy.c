// Struct assignment past the size where unrolling it stops being sensible.
//
// A copy of a constant size is a run of load/store pairs up to
// X86_UNROLLED_COPY_LIMIT bytes and 'rep movsb' beyond it, so what these look
// for is a boundary crossed wrongly: a count off by a chunk, a source and a
// destination the string move has the wrong way round, and the three registers
// it fixes - rdi, rsi and rcx - taken away from something that was still using
// them.
//
// The sizes are chosen around the limit rather than at random. 128 bytes is
// the largest copy still unrolled, 136 the smallest that is not, and 129 is a
// count no chunk width divides, so the two forms have to agree about a tail
// that the unrolled one would copy in four different widths.

struct Under { char b[128]; };
struct Odd { char b[129]; };
struct Over { int a[64]; };

static struct Over global;

static int sumOver(const struct Over *p) {
  int total = 0;
  int i;

  for (i = 0; i < 64; ++i) {
    total += p->a[i];
  }

  return total;
}

static void fillOver(struct Over *p, int base) {
  int i;

  for (i = 0; i < 64; ++i) {
    p->a[i] = base + i;
  }
}

// Local to local, local to global, and back out of the global: three copies of
// the same 256 bytes, none of which the address of the other tells you.
static int throughGlobal(int base) {
  struct Over a;
  struct Over b;

  fillOver(&a, base);
  global = a;
  b = global;

  return sumOver(&b);
}

// The two sizes either side of the limit, byte for byte, so a copy that stops
// or starts one chunk out is a different answer rather than a different shape.
static int bothForms(int seed) {
  struct Under u;
  struct Odd o;
  struct Under u2;
  struct Odd o2;
  int i;
  int total = 0;

  for (i = 0; i < 128; ++i) {
    u.b[i] = (char)(seed + i);
  }
  for (i = 0; i < 129; ++i) {
    o.b[i] = (char)(seed - i);
  }

  u2 = u;
  o2 = o;

  for (i = 0; i < 128; ++i) {
    total += u2.b[i];
  }
  for (i = 0; i < 129; ++i) {
    total += o2.b[i];
  }

  return total;
}

// Values live across the copy, and as many of them as the function has room
// for. rdi, rsi and rcx are ordinary allocatable registers everywhere else, so
// a copy that does not say it destroys them is one the allocator is free to
// put a live value in.
static int liveAcross(int a, int b, int c, int d, int e) {
  struct Over big;
  struct Over copy;
  int f = a + b;
  int g = c + d;
  int h = e + a;

  fillOver(&big, a);
  copy = big;

  return copy.a[0] + copy.a[63] + f + g + h;
}

// A copy of an element of an array of structs, so the addresses the string
// move is handed are computed ones rather than a frame slot's.
static int indexed(int which) {
  struct Over table[3];
  struct Over out;
  int i;

  for (i = 0; i < 3; ++i) {
    fillOver(&table[i], i * 100);
  }

  out = table[which];

  return out.a[0] + out.a[1];
}

// A copy in a loop, and one whose source and destination swap round each time,
// so the same instruction runs with the roles reversed.
static int alternating(int n) {
  struct Over x;
  struct Over y;
  int i;

  fillOver(&x, 1);
  fillOver(&y, 2);

  for (i = 0; i < n; ++i) {
    if (i & 1) {
      x = y;
    } else {
      y = x;
    }
  }

  return x.a[0] * 10 + y.a[0];
}

// Passed by value and returned by value, both of which copy the whole struct
// somewhere the callee or the caller chose.
static struct Over doubled(struct Over in) {
  struct Over out;
  int i;

  for (i = 0; i < 64; ++i) {
    out.a[i] = in.a[i] * 2;
  }

  return out;
}

int main(void) {
  struct Over in;
  struct Over out;

  // 0..63 summed is 2016.
  if (throughGlobal(0) != 2016) return 1;
  if (throughGlobal(1) != 2080) return 2;

  // 128 bytes of seed+i against 129 of seed-i, both as signed chars: the two
  // runs very nearly cancel, so a byte copied wrongly is a visible difference.
  if (bothForms(0) != -128) return 3;
  if (bothForms(1) != -127) return 4;

  if (liveAcross(1, 2, 3, 4, 5) != 1 + 64 + 3 + 7 + 6) return 5;

  if (indexed(0) != 1) return 6;
  if (indexed(2) != 401) return 7;

  // No round leaves the two as they were filled; any number of rounds has the
  // first one - y taking x - decide both.
  if (alternating(0) != 12) return 8;
  if (alternating(1) != 11) return 9;
  if (alternating(2) != 11) return 10;

  fillOver(&in, 10);
  out = doubled(in);
  if (out.a[0] != 20) return 11;
  if (out.a[63] != 146) return 12;

  // The source unchanged: a by-value argument is the callee's own copy.
  if (in.a[63] != 73) return 13;

  return 0;
}
