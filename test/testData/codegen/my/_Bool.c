#include <stdbool.h>

_Bool test(long x) {
  return x;
}


_Bool testf(float x) {
  return x;
}

int main() {
  if (test(0) != false) return 1;
  if (test(1) != true) return 2;

  if (test(0x100) != true) return 3;
  if (test(0x100) != true) return 4;
  if (test(0x11111100) != true) return 5;

  if (test(0.0f) != false) return 6;
  if (test(1.0f) != true) return 7;

  return 0;
}
