// The other half of 6.2.7's array rule: a header declares 'extern T x[N]' and
// the definition writes 'T x[] = {...}', letting its own initializer count the
// elements. It was refused as a redefinition; mbedtls' psa_util.c does it for
// every error table.

typedef struct { int from, to; } pair;

extern const pair table[3];
extern const char letters[4];

const pair table[] = { { 1, 2 }, { 3, 4 }, { 5, 6 } };
const char letters[] = { 'a', 'b', 'c', 0 };

int main(void) {
  if (table[2].to != 6) return 1;
  if (sizeof(table) != 3 * sizeof(pair)) return 2;
  if (letters[2] != 'c') return 3;
  if (sizeof(letters) != 4) return 4;
  return 0;
}
