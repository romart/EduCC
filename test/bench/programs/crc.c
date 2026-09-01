// Table-driven bit twiddling: two loop-carried integer values, a table load
// and a shift chain, all in registers if there are enough.
#include <stdio.h>

#define BUFSZ (1 << 20)
#define ROUNDS 120

static unsigned int table[256];
static unsigned char buf[BUFSZ];

static void buildTable() {
  unsigned int i, j, c;

  for (i = 0; i < 256; ++i) {
    c = i;
    for (j = 0; j < 8; ++j) c = (c & 1) ? (0xEDB88320u ^ (c >> 1)) : (c >> 1);
    table[i] = c;
  }
}

static unsigned int crc32(const unsigned char *p, int n) {
  unsigned int c = 0xFFFFFFFFu;
  int i;

  for (i = 0; i < n; ++i) c = table[(c ^ p[i]) & 0xFF] ^ (c >> 8);
  return c ^ 0xFFFFFFFFu;
}

int main() {
  unsigned int seed = 12345u, acc = 0;
  int i, round;

  buildTable();
  for (i = 0; i < BUFSZ; ++i) {
    seed = seed * 1103515245u + 12345u;
    buf[i] = (unsigned char)(seed >> 16);
  }

  for (round = 0; round < ROUNDS; ++round) {
    buf[round] = (unsigned char)round;
    acc += crc32(buf, BUFSZ);
  }

  printf("%u\n", acc);
  return 0;
}
