struct S { char a[32]; char *b; };
static struct S global = {"x", "abcdefghijklmnopqrstuv"};

int main(void) {
  struct S local = {"x", "abcdefghijklmnopqrstuv"};
  unsigned char high[4] = "\377";
  char exact[3] = "abc";
  char inferred[] = "abc";
  if (global.a[0] != 'x' || local.a[0] != 'x') return 1;
  for (int i = 1; i < 32; ++i) {
    if (global.a[i] || local.a[i]) return 2;
  }
  if (high[0] != 255 || high[1] || high[2] || high[3]) return 3;
  if (exact[2] != 'c' || sizeof(inferred) != 4 || inferred[3]) return 4;
  return 0;
}
