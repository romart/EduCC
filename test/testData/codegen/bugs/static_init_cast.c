// A static initializer that is a cast is an E_CAST node, and the emitter read
// its constant kind off constExpr.op - a different member of that union - so
// the bytes written were whatever that happened to be. SQLite tags its
// built-in functions with SQLITE_INT_TO_PTR(n) this way, and min() and max()
// came out with the same tag.

#define INT_TO_PTR(X) ((void *)&((char *)0)[X])

struct entry { const char *name; void *tag; };

static const struct entry table[] = {
  { "min", INT_TO_PTR(0) },
  { "max", INT_TO_PTR(1) },
  { "third", INT_TO_PTR(3) },
};

static void *const viaIntptr = (void *)(long)5;
static const long asInteger = (long)&((char *)0)[7];
static void *const scaled = (void *)&((short *)0)[4];
static const double asDouble = (double)3;
static const unsigned char narrowed = (unsigned char)300;

static long asLong(void *p) { return (long)(char *)p; }

int main(void) {
  if (asLong(table[0].tag) != 0) return 1;
  if (asLong(table[1].tag) != 1) return 2;
  if (asLong(table[2].tag) != 3) return 3;
  if (asLong(viaIntptr) != 5) return 4;
  if (asInteger != 7) return 5;
  if (asLong(scaled) != 8) return 6;
  if (asDouble != 3.0) return 7;
  if (narrowed != 44) return 8;
  return 0;
}
