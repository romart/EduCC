// Struct assignment, which is the only thing that reaches IR_M_COPY. Sizes sit
// on both sides of the 128-byte boundary where selectMemoryCopy stops unrolling
// load/store pairs and emits a string move, so one run measures both forms.
#include <stdio.h>

#define N 64

typedef struct { long a[4]; } Small;   /* 32 bytes - unrolled */
typedef struct { long a[12]; } Mid;    /* 96 bytes - unrolled, near the edge */
typedef struct { long a[48]; } Large;  /* 384 bytes - string move */
typedef struct { long a[192]; } Big;   /* 1536 bytes - string move */

static Small small[N];
static Mid mid[N];
static Large large[N];
static Big big[N];

// Rotating an array by one is N assignments and no arithmetic, so the copy is
// what is being timed. Bytes moved per round are within 20% across the four.
#define ROTATE(array, rounds)                                                  \
  do {                                                                         \
    int r, i;                                                                  \
    for (r = 0; r < rounds; ++r) {                                             \
      tmp = array[0];                                                          \
      for (i = 0; i < N - 1; ++i) array[i] = array[i + 1];                     \
      array[N - 1] = tmp;                                                      \
      acc += array[r & (N - 1)].a[0];                                          \
    }                                                                          \
  } while (0)

// By value, so the copy is the argument-passing one - selectMemoryArgument
// unrolls where selectMemoryCopy would not.
static long sink(Large v) {
  return v.a[0] + v.a[23] + v.a[47];
}

int main() {
  long acc = 0;
  int i, j;

  for (i = 0; i < N; ++i) {
    for (j = 0; j < 4; ++j) small[i].a[j] = i * 4 + j;
    for (j = 0; j < 12; ++j) mid[i].a[j] = i * 12 + j;
    for (j = 0; j < 48; ++j) large[i].a[j] = i * 48 + j;
    for (j = 0; j < 192; ++j) big[i].a[j] = i * 192 + j;
  }

  { Small tmp; ROTATE(small, 750000); }
  { Mid tmp; ROTATE(mid, 240000); }
  { Large tmp; ROTATE(large, 60000); }
  { Big tmp; ROTATE(big, 15000); }

  for (i = 0; i < N; ++i) acc += sink(large[i]);

  printf("%ld\n", acc);
  return 0;
}
