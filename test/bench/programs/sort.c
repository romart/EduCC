// Two sorts over the same data: quicksort's recursion and partition loop, and
// heapsort's sift-down, which keeps four values live across a nested branch.
#include <stdio.h>

#define N 120000
#define ROUNDS 16

static int data[N];
static int work[N];

static void fill(unsigned int seed) {
  int i;
  for (i = 0; i < N; ++i) {
    seed = seed * 1103515245u + 12345u;
    data[i] = (int)(seed >> 8);
  }
}

static void quicksort(int *a, int lo, int hi) {
  while (lo < hi) {
    int pivot = a[(lo + hi) / 2];
    int i = lo, j = hi;

    while (i <= j) {
      while (a[i] < pivot) ++i;
      while (a[j] > pivot) --j;
      if (i <= j) {
        int t = a[i]; a[i] = a[j]; a[j] = t;
        ++i; --j;
      }
    }
    if (j - lo < hi - i) {
      quicksort(a, lo, j);
      lo = i;
    } else {
      quicksort(a, i, hi);
      hi = j;
    }
  }
}

static void siftDown(int *a, int start, int end) {
  int root = start;

  while (root * 2 + 1 <= end) {
    int child = root * 2 + 1;
    if (child + 1 <= end && a[child] < a[child + 1]) ++child;
    if (a[root] >= a[child]) return;
    { int t = a[root]; a[root] = a[child]; a[child] = t; }
    root = child;
  }
}

static void heapsort(int *a, int n) {
  int start, end;

  for (start = n / 2 - 1; start >= 0; --start) siftDown(a, start, n - 1);
  for (end = n - 1; end > 0; --end) {
    int t = a[0]; a[0] = a[end]; a[end] = t;
    siftDown(a, 0, end - 1);
  }
}

int main() {
  long acc = 0;
  int round, i;

  for (round = 0; round < ROUNDS; ++round) {
    fill((unsigned int)(round + 1) * 7919u);
    for (i = 0; i < N; ++i) work[i] = data[i];
    quicksort(work, 0, N - 1);
    acc += work[0] + work[N / 2] + work[N - 1];

    for (i = 0; i < N; ++i) work[i] = data[i];
    heapsort(work, N);
    acc += work[0] + work[N / 2] + work[N - 1];
  }

  printf("%ld\n", acc);
  return 0;
}
