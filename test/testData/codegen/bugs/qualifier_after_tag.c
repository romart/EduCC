// A struct, union or enum specifier does not end the declaration specifiers:
// 'struct S const x' is as good an order as 'const struct S x'. Parsing stopped
// at the tag, so mbedtls' ssl_tls13_keys.c would not parse.

struct point { int x, y; };
union word { int i; char b[4]; };
enum colour { RED, GREEN };

struct point const origin = { 0, 0 };
union word const zero = { 0 };
enum colour const first = RED;

struct point static counted = { 3, 4 };
enum colour volatile changing = GREEN;

static int sum(struct point const *p) { return p->x + p->y; }

int main(void) {
  if (sum(&origin) != 0) return 1;
  if (sum(&counted) != 7) return 2;
  if (zero.i != 0) return 3;
  if (first != RED) return 4;
  if (changing != GREEN) return 5;
  return 0;
}
