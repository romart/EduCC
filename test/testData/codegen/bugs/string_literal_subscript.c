// A string literal's value is already an address, so subscripting one has no
// deref to strip - and parentheses do not change that. Canonicalization
// asserted on '("abc")[1]', which is the shape mbedtls' query_config.c builds
// with '(STRINGIFY(macro) "")[0]'.

#define STRINGIFY(x) #x

static int first(void) { return (STRINGIFY(mbedtls) "")[0]; }

int main(void) {
  if (("abc")[1] != 'b') return 1;
  if (("ab" "c")[2] != 'c') return 2;
  if ((("x"))[0] != 'x') return 3;
  if ("abc"[0] != 'a') return 4;
  if (first() != 'm') return 5;
  if ((STRINGIFY() "")[0] != 0) return 6;
  return 0;
}
