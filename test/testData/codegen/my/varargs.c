#include <stdarg.h>

int sum1(int x, ...) {
  va_list ap;
  va_start(ap, x);

  for (;;) {
    int y = va_arg(ap, int);
    if (y == 0)
      return x;
    x += y;
  }
}

int sum2(int x, ...) {
  va_list ap;
  va_start(ap, x);

  for (;;) {
    double y = va_arg(ap, double);
    x += y;

    int z = va_arg(ap, int);
    if (z == 0)
      return x;
    x += z;
  }
}

int vsprintf(char *buf, char *fmt, void *ap);
int strcmp(char *p, char *q);

void fmt(char *buf, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  va_list ap2;
  va_copy(ap2, ap);
  vsprintf(buf, fmt, ap2);
  va_end(buf);
}

int main() {
  if (6 != sum1(1, 2, 3, 0)) return 1;
  if (55 != sum1(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0)) return 2;

  if (21 != sum2(1, 2.0, 3, 4.0, 5, 6.0, 0)) return 3;
  if (210 != sum2(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13, 14.0, 15, 16.0, 17, 18.0, 19, 20.0, 0)) return 4;

  char buf[100];
  fmt(buf, "%d %d", 2, 3);

  if (0 != strcmp(buf, "2 3")) return 5;

  return 0;
}
