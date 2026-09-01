// The value of an assignment through a pointer to a narrow object.
//
// '*d = v' is an expression, and its value is what was stored. The legacy
// backend produces that value by reusing the register that held the
// destination address and overwriting as many bytes of it as the store wrote,
// so for a char destination the expression evaluates to the low 32 bits of the
// address with its bottom byte replaced - and for a short, its bottom two.
// Memory is written correctly; only the value handed back is wrong. An int or
// wider destination overwrites the whole register and comes out right, which
// is why this went unnoticed.
//
// The visible form is the copy loop every C program has:
//
//     while ((*d++ = *s++) != 0) { }
//
// which under the legacy backend compares an address against zero, never
// terminates, and runs off the end of the destination. The loops below are
// bounded so the fixture reports a wrong answer instead of a segfault.

static int copy(char *d, const char *s, int limit) {
  int n = 0;
  while ((*d++ = *s++) != 0) {
    if (++n >= limit) return -1;
  }
  return n;
}

int main(void) {
  char buf[8];
  short shorts[4];
  int ints[4];
  char src[4];
  char dst[8];

  char *cp = buf;
  short *sp = shorts;
  int *ip = ints;

  if ((*cp = 0) != 0) return 1;
  if ((*cp = 65) != 65) return 2;
  if (buf[0] != 65) return 3;

  if ((*sp = 0) != 0) return 4;
  if ((*sp = 4660) != 4660) return 5;
  if (shorts[0] != 4660) return 6;

  if ((*ip = 0) != 0) return 7;
  if ((*ip = 305419896) != 305419896) return 8;

  src[0] = 'a';
  src[1] = 'b';
  src[2] = 0;
  if (copy(dst, src, 8) != 2) return 9;
  if (dst[0] != 'a' || dst[1] != 'b' || dst[2] != 0) return 10;

  return 0;
}
