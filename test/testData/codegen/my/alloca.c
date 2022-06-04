#include <stddef.h>

void *fn(int x, void *p, int y) { return p; }
int printf(char *fmt, ...);
void *memcpy(void *dest, void *src, long long n);
int memcmp(char *p, char *q, long long n);

int main() {
  int i = 0;

  char *p1 = alloca(16);
  char *p2 = alloca(16);
  char *p3 = 1 + (char *)alloca(3) + 1;
  p3 -= 2;
  char *p4 = fn(1, alloca(16), 3);

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
