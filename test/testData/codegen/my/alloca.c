#include <stddef.h>

int printf(char *fmt, ...);
void *fn(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9, int x10, int x11, void *p, int x12, int x13, int x14) {
  printf("%d %d %d %d %d %d %d %d\n", x7, x8, x9, x10, x11, x12, x13, x14);
  return p;
}
void *memcpy(void *dest, void *src, long long n);
int memcmp(char *p, char *q, long long n);

int main() {
  int i = 0;

  char *p1 = alloca(16);
  char *p2 = alloca(16);
  char *p3 = 1 + (char *)alloca(3) + 1;
  p3 -= 2;
  char *p4 = fn(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, alloca(16), 12, 13, 14);

  if (16 != p1 - p2) return 1;
  if (16 != p2 - p3) return 2;
  if (16 != p3 - p4) return 3;

  memcpy(p1, "0123456789abcdef", 16);
  memcpy(p2, "ghijklmnopqrstuv", 16);
  memcpy(p3, "wxy", 3);

  if (0 != memcmp(p1, "0123456789abcdef", 16)) return 4;
  if (0 != memcmp(p2, "ghijklmnopqrstuv", 16)) return 5;
  if (0 != memcmp(p3, "wxy", 3)) return 6;

  printf("OK\n");
  return 0;
}
