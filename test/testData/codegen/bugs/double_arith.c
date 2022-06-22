#include <stdio.h>

double tex(unsigned x0, unsigned x1) {
  double d;
  d = (double)x1 * 4294967296.0 + (double)x0;
  return d;
}

int main() {

  double d = tex(428, 0);
  printf("result: %lf\n", d);

  return 0;
}
