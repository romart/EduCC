// Double-precision loop nest: three live xmm values across an inner loop plus
// two induction variables, which is where an allocator either keeps the
// accumulator in a register or reloads it every iteration.
#include <stdio.h>

#define N 160
#define ROUNDS 36

static double a[N][N], b[N][N], c[N][N];

int main() {
  int i, j, k, round;
  double sum = 0.0;

  for (i = 0; i < N; ++i) {
    for (j = 0; j < N; ++j) {
      a[i][j] = (double)((i * 3 + j * 7) % 13) * 0.5;
      b[i][j] = (double)((i * 5 + j * 11) % 17) * 0.25;
    }
  }

  for (round = 0; round < ROUNDS; ++round) {
    for (i = 0; i < N; ++i) {
      for (j = 0; j < N; ++j) {
        double acc = 0.0;
        for (k = 0; k < N; ++k) acc += a[i][k] * b[k][j];
        c[i][j] = acc;
      }
    }
    sum += c[round][round];
  }

  printf("%.4f\n", sum);
  return 0;
}
