// Where a variable is placed depends on whether the *object* is const, and in
// 'static const char *cached' it is not: the const belongs to what cached
// points at, and cached itself is assignable.
//
// generateVaribale_x86_64() in src/x86_64/codegen_x86_64.c read the const bit
// off the declaration specifiers, which is where 'const' was written - so it
// put every pointer-to-const in .rodata and the first assignment to one was a
// segmentation fault. The shape that found it is the one every cached probe is
// written in:
//
//   static const char *cached = NULL;
//   if (cached != NULL) return cached;
//   cached = candidates[i];
//
// which is how src/link/link.c remembers where the CRT objects live, and which
// crashed a self-hosted compiler while it was linking.
//
// The three cases below are the three answers, and only the middle one is
// read-only: a const element type makes the array read-only, a const pointer
// makes the pointer read-only, and a pointer to const makes neither.

static const char table[] = "abcd";
static char *const fixed = "xy";
static const char *movable = 0;
static const char *const *doubled = 0;

static int lengthOf(const char *s) {
  int n = 0;
  while (*s++) ++n;
  return n;
}

static const char *probe(int which) {
  static const char *cached = 0;
  if (cached) return cached;

  static const char *const candidates[] = { "aa", "bbb", "cccc" };

  cached = candidates[which];
  return cached;
}

int main(void) {
  int total = 0;

  // Assigning through a pointer-to-const object, twice, so that a second call
  // reads back what the first one stored rather than recomputing it.
  movable = table;
  total += lengthOf(movable);

  doubled = &movable;
  total += lengthOf(*doubled);

  total += lengthOf(fixed);
  total += lengthOf(probe(2));
  total += lengthOf(probe(0)); // the cache answers; 'aa' is never reached

  movable = fixed;
  total += lengthOf(movable);

  return total == 4 + 4 + 2 + 4 + 4 + 2 ? 0 : 1;
}
