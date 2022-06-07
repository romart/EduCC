int printf(const char *fmt, ...);

unsigned short test2(const char *text, int length) {
  unsigned short r = 0;
  unsigned i;
  for (i = 2; i < length; ++i) {
      unsigned short old = r;
      r <<= 1;
      r += (text[i] - '0');
      if (old > r) {
          // integer overflow
          printf("overflow old = %u, r = %u\n", old, r);
      }
  }

  return r;
}


int main() {
  const char *s = "0b101101010";
  unsigned short int r = test2(s, 12 - 1);
  printf("result = %u\n", r);
  if (0b101101010 != r) return 2;

  const char *s2 = "0b1011010101101101010110110101";

  unsigned short int r2 = test2(s2, 70);

  printf("result2 = %u\n", r2);
  if (62684 != r2) return 2;

  return 0;
}
