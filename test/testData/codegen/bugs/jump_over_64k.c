// A forward jump of more than 64K, which is where a rel32 displacement has a
// non-zero third byte and a zero top one. Patching one wrote only the bytes it
// needed over the reserved 0xDEADBEEF and left the 0xDE behind, so the jump
// left the function: Lua's luaV_execute, 39K into itself, jumped 569MB away.
//
// The body is macro-expanded rather than written out, and each unit is one
// store to a volatile - what the bug needs is the distance, and a volatile is
// what stops the code being folded away before it can be measured.

volatile int sink;

#define UNIT      sink = 1;
#define X8(u)     u u u u u u u u
#define X64(u)    X8(X8(u))
#define X512(u)   X8(X64(u))
#define X4096(u)  X8(X512(u))
#define X32768(u) X8(X4096(u))

int big(int x) {
  int acc = 5;

  if (x) {
    X32768(UNIT)
    return acc;
  }

  return 12345;
}

int main(void) {
  if (big(0) != 12345) return 1;
  if (big(1) != 5) return 2;
  return 0;
}
