// A struct's alignment is the widest alignment among its members, not the
// widest size. A union member answered with its size, because the check asked
// only about structs - so an enclosing struct got an alignment that need not
// even be a power of two, and its size was rounded up to a multiple of that.
// mbedtls' PSA MAC context is a word followed by such a union, and every HMAC
// through the PSA API failed because callers and callee disagreed about it.

union payload { int i; char c[16]; };
struct headed { unsigned tag; union payload p; };
struct tailed { unsigned tag; union payload p; unsigned end; };

struct inner { int i; char c; };
struct arrayed { char lead; struct inner items[2]; };

int main(void) {
  if (sizeof(union payload) != 16) return 1;
  if (sizeof(struct headed) != 20) return 2;
  if (sizeof(struct tailed) != 24) return 3;
  if (_Alignof(struct headed) != 4) return 4;

  if (sizeof(struct inner) != 8) return 5;
  if (sizeof(struct arrayed) != 20) return 6;
  if (_Alignof(struct arrayed) != 4) return 7;

  struct tailed t;
  if ((char *)&t.p - (char *)&t != 4) return 8;
  if ((char *)&t.end - (char *)&t != 20) return 9;

  return 0;
}
