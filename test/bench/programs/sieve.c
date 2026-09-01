// Integer array indexing and a tight inner loop with a strided store.
#include <stdio.h>

#define LIMIT 200000
#define ROUNDS 520

static char flags[LIMIT + 1];

int main() {
  long total = 0;
  int round;

  for (round = 0; round < ROUNDS; ++round) {
    int i, j, count = 0;

    for (i = 0; i <= LIMIT; ++i) flags[i] = 1;
    flags[0] = flags[1] = 0;

    for (i = 2; (long)i * i <= LIMIT; ++i) {
      if (flags[i]) {
        for (j = i * i; j <= LIMIT; j += i) flags[j] = 0;
      }
    }

    for (i = 2; i <= LIMIT; ++i) count += flags[i];
    total += count;
  }

  printf("%ld\n", total);
  return 0;
}
