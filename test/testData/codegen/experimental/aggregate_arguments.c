// Passing a struct too large for one register, which SysV puts on the stack as
// bytes.
//
// This was the last of section 6.10's two halves and the one that stayed open
// longest: the IR copies such an argument into a temporary and names it by the
// temporary's *address*, while the callee reads bytes out of its incoming
// frame. Both halves were right on their own and there is no type that tells
// them apart - an aggregate argument and a pointer argument are the same
// IR_PTR - so the caller now says which is which (IrInstruction.info.call's
// memArgs) and selection pushes the bytes instead of the pointer. Nothing
// noticed before because every function on either side of such a call fell
// back to the legacy backend, which has its own self-consistent convention.
//
// So what is worth pinning here is the *seam*: how many eightbytes an argument
// takes, what that does to the arguments around it, and that the stack pointer
// comes back 16-byte aligned at the call. The sizes are chosen for their
// remainders - 9, 12, 17 and 24 bytes are two, two, three and three
// eightbytes, and the bytes above the last one are padding both sides agree to
// skip. Reading them is deliberate: createAllocaSlot rounds the temporary up,
// so the load never leaves the frame.
//
// 'before'/'after'/'around' and the two 'spill' cases are the ordering claim: a
// memory argument consumes no register of either class, so it neither takes a
// register from the scalars around it nor pushes them along. 'twoAgg' and
// 'mixSmall' put two aggregates next to each other, one of which is small
// enough to travel in a register and so goes the other way entirely.
//
// 'both' is the interaction with composite *returns* (step 12): the hidden
// buffer pointer takes rdi before any declared parameter does, and the
// aggregate argument is on the stack regardless. 'rvalue' passes a struct that
// is not an lvalue - the result of a call - which crashed the legacy backend
// outright; see codegen/bugs/struct_rvalue_argument.c. 'viaPtr' does it
// through a function pointer, where the callee is a register rather than a
// relocation.
//
// Deliberately absent: a struct over 128 bytes, which the block copy into the
// temporary still refuses (see big_copy.c), and a float argument that lands on
// the stack, which has no 'push xmm'. Both are their own rows in section 6.7
// and neither is about aggregates.

struct S9  { char a[9]; };
struct S12 { int a, b, c; };
struct S16 { long a, b; };
struct S17 { char a[17]; };
struct S24 { long a; double b; int c; };
struct S40 { long a[5]; };
struct Small { int x, y; };
struct FP { double a, b, c; };
struct Outer { int pad; struct S16 inner; };
union U20 { char c[20]; int i; };

struct S12 g12 = {1, 2, 3};
struct Outer gouter = {9, {4, 5}};
static struct S12 sg12 = {7, 8, 9};

int s9(struct S9 s) { return s.a[0] + s.a[4] + s.a[8]; }
int s12(struct S12 s) { return s.a * 100 + s.b * 10 + s.c; }
int s16(struct S16 s) { return (int)(s.a * 10 + s.b); }
int s17(struct S17 s) { return s.a[0] + s.a[16]; }
int s24(struct S24 s) { return (int)(s.a + (long)s.b + s.c); }
int s40(struct S40 s) { return (int)(s.a[0] + s.a[2] + s.a[4]); }
double fp(struct FP s) { return s.a * 100 + s.b * 10 + s.c; }
int u20(union U20 u) { return u.c[0] + u.c[19]; }

// The callee owns its copy; the caller's must come back untouched.
int scribble(struct S24 s) { s.a = 999; s.c = -1; return (int)s.a + s.c; }

int before(int p, int q, struct S12 s) { return p * 1000 + q * 100 + s.a + s.b + s.c; }
int after(struct S12 s, int p, int q) { return p * 1000 + q * 100 + s.a + s.b + s.c; }
int around(int p, struct S12 s, int q) { return p * 1000 + q * 100 + s.a + s.b + s.c; }

int twoAgg(struct S12 a, struct S16 b) { return a.a + a.b + a.c + (int)(b.a + b.b); }
int mixSmall(struct Small sm, struct S12 s) { return sm.x + sm.y + s.a + s.b + s.c; }

// The aggregate arrives after the integer registers are gone, so it shares the
// stack with a scalar - once ahead of it and once behind.
int spillBefore(int a, int b, int c, int d, int e, int f, struct S12 s, int g) {
  return a + b + c + d + e + f + s.a + s.b + s.c + g;
}
int spillAfter(int a, int b, int c, int d, int e, int f, int g, struct S12 s) {
  return a + b + c + d + e + f + g + s.a + s.b + s.c;
}

double floaty(double d, struct S12 s, double e) {
  return d * 100 + e * 10 + s.a + s.b + s.c;
}

// A hidden return buffer and a memory argument in the same call.
struct S40 both(struct S24 s, int n) {
  struct S40 r;
  int i;
  for (i = 0; i < 5; ++i)
    r.a[i] = s.a + n + i;
  return r;
}

struct S12 mk12(int n) {
  struct S12 r;
  r.a = n;
  r.b = n + 1;
  r.c = n + 2;
  return r;
}

int rvalue(int n) { return s12(mk12(n)); }
int viaPtr(int (*fn)(struct S12), struct S12 s) { return fn(s); }

long recurse(struct S24 s, int n) {
  if (n == 0)
    return s.a + (long)s.b + s.c;
  s.a += 1;
  return recurse(s, n - 1);
}

int main(void) {
  struct S9 a9;
  struct S12 a12 = {1, 2, 3};
  struct S16 a16 = {50, 8};
  struct S17 a17;
  struct S24 a24 = {5, 2.0, 7};
  struct S40 a40 = {{1, 2, 3, 4, 5}};
  struct FP afp = {1.0, 2.0, 3.0};
  struct Outer o = {9, {6, 7}};
  struct Small sm = {10, 20};
  struct S12 *p = &a12;
  union U20 u;
  int i;
  int rc = 0;

  for (i = 0; i < 9; ++i) a9.a[i] = (char)(i + 1);
  for (i = 0; i < 17; ++i) a17.a[i] = (char)(i + 1);
  for (i = 0; i < 20; ++i) u.c[i] = (char)(i + 1);

  if (s9(a9) != 15) rc |= 1 << 0;
  if (s12(a12) != 123) rc |= 1 << 1;
  if (s16(a16) != 508) rc |= 1 << 2;
  if (s17(a17) != 18) rc |= 1 << 3;
  if (s24(a24) != 14) rc |= 1 << 4;
  if (s40(a40) != 9) rc |= 1 << 5;
  if (fp(afp) != 123.0) rc |= 1 << 6;
  if (u20(u) != 21) rc |= 1 << 7;

  if (scribble(a24) != 998) rc |= 1 << 8;
  if (a24.a != 5 || a24.c != 7) rc |= 1 << 9;

  if (before(4, 5, a12) != 4506) rc |= 1 << 10;
  if (after(a12, 4, 5) != 4506) rc |= 1 << 11;
  if (around(4, a12, 5) != 4506) rc |= 1 << 12;

  if (twoAgg(a12, a16) != 64) rc |= 1 << 13;
  if (mixSmall(sm, a12) != 36) rc |= 1 << 14;
  if (spillBefore(1, 2, 3, 4, 5, 6, a12, 7) != 34) rc |= 1 << 15;
  if (spillAfter(1, 2, 3, 4, 5, 6, 7, a12) != 34) rc |= 1 << 16;
  if (floaty(1.0, a12, 2.0) != 126.0) rc |= 1 << 17;

  {
    struct S40 r = both(a24, 3);
    if (r.a[0] != 8 || r.a[4] != 12) rc |= 1 << 18;
  }

  if (rvalue(10) != 1122) rc |= 1 << 19;
  if (viaPtr(s12, a12) != 123) rc |= 1 << 20;
  if (recurse(a24, 5) != 19) rc |= 1 << 21;

  // The source of a copy need not be a local: a global is rip-relative, a
  // field of one is a displacement off that, and a dereference is a register.
  if (s12(g12) != 123) rc |= 1 << 22;
  if (s12(sg12) != 789) rc |= 1 << 23;
  if (s16(o.inner) != 67) rc |= 1 << 24;
  if (s16(gouter.inner) != 45) rc |= 1 << 25;
  if (s12(*p) != 123) rc |= 1 << 26;

  // Every source above survives being read.
  if (a12.a != 1 || a12.b != 2 || a12.c != 3) rc |= 1 << 27;
  if (g12.a != 1 || g12.c != 3) rc |= 1 << 28;

  return rc;
}
