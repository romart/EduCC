
char *strcpy(char *dst, char *src) {
  while (*src != 0)
     *dst++ = *src++;

  *dst = '\0';

  return dst;
}


int main() {
  char dst[10] = { 0 };

  strcpy(dst, "hello");

  if (dst[0] != 'h') return 1;
  if (dst[1] != 'e') return 2;
  if (dst[2] != 'l') return 3;
  if (dst[3] != 'l') return 4;
  if (dst[4] != 'o') return 5;
  if (dst[5] != '\0') return 6;

  return 0;
}
