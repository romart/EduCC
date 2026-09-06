// C99 6.4.2.2: every function body declares '__func__' holding its own name.
// Only the GNU spelling '__FUNCTION__' existed, and mbedtls' ssl_misc.h reaches
// for the standard one in every buffer-bounds check.

static int same(const char *a, const char *b) {
  while (*a && *a == *b) { ++a; ++b; }
  return *a == *b;
}

static int named(void) { return same(__func__, "named"); }

static int spellings(void) {
  return same(__func__, __FUNCTION__) && same(__func__, __PRETTY_FUNCTION__);
}

static int isArray(void) { return sizeof(__func__) == sizeof("isArray"); }

int main(void) {
  if (!named()) return 1;
  if (!spellings()) return 2;
  if (!isArray()) return 3;
  if (!same(__func__, "main")) return 4;
  return 0;
}
