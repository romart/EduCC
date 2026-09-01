// Byte-at-a-time pointer walks. Sub-register writes, which the allocator has
// to widen correctly when the value it holds is spilled.
#include <stdio.h>

#define SLOTS 512
#define SLOTLEN 96
#define ROUNDS 2800

static char pool[SLOTS][SLOTLEN];
static char scratch[SLOTLEN];

static int len(const char *s) {
  const char *p = s;
  while (*p) ++p;
  return (int)(p - s);
}

static void copy(char *d, const char *s) {
  while ((*d++ = *s++) != 0) { }
}

static int compare(const char *a, const char *b) {
  while (*a && *a == *b) { ++a; ++b; }
  return (int)(unsigned char)*a - (int)(unsigned char)*b;
}

static void reverse(char *s, int n) {
  int i = 0, j = n - 1;
  while (i < j) {
    char t = s[i];
    s[i++] = s[j];
    s[j--] = t;
  }
}

int main() {
  long acc = 0;
  int i, j, round;

  for (i = 0; i < SLOTS; ++i) {
    for (j = 0; j < SLOTLEN - 1; ++j) pool[i][j] = (char)('a' + ((i * 7 + j * 3) % 26));
    pool[i][SLOTLEN - 1] = 0;
  }

  for (round = 0; round < ROUNDS; ++round) {
    for (i = 0; i < SLOTS; ++i) {
      copy(scratch, pool[i]);
      reverse(scratch, len(scratch));
      acc += len(scratch);
      acc += compare(scratch, pool[i]) < 0 ? 1 : -1;
    }
  }

  printf("%ld\n", acc);
  return 0;
}
