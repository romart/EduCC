// A function can be declared through a typedef'd function type, in which case
// the declarator never spells a parameter list out. The parser asserted on the
// missing functional part; mbedtls declares its cookie and ticket callbacks
// this way in ssl_cookie.h and ssl_ticket.h.

typedef int addFn(int a, int b);
typedef int countFn(const char *s, ...);

addFn add;
countFn count;

int add(int a, int b) { return a + b; }

int count(const char *s, ...) {
  int n = 0;
  while (*s++) ++n;
  return n;
}

static int callThrough(addFn *f) { return f(20, 22); }

int main(void) {
  if (add(2, 3) != 5) return 1;
  if (count("abcd", 1, 2) != 4) return 2;
  if (callThrough(add) != 42) return 3;
  return 0;
}
