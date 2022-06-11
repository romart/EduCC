
void testChar() {
  char c1 = 'a';
  char c2 = '\0';
  char c3 = '\a';
  char c4 = '\\';
  char c5 = '\'';
  char c6 = '\"';
  char c055 = '\55';
  char c7f = '\x7f';
}

void testWchar() {
  int w1 = L'a';
  int w2 = L'\0';
  int w3 = L'\a';
  int w4 = L'\\';
  int w5 = L'\'';
  int w6 = L'\"';
  int w0555 = L'\555';
  int w7fff = L'\x7fff';
}

int testInteger() {
  int s = 123;
  unsigned u = 123u;
  unsigned u2 = 123U;

  int o = 07777;
  int b = 0b101101010;
  int BB = 0B1001000101;

  long l = 21142341l;
  long l2 = 21142341L;
  unsigned long ul1 = 21142341ul;
  unsigned long ul2 = 21142341uL;
  unsigned long ul3 = 21142341Ul;
  unsigned long ul4 = 21142341UL;
  unsigned long ul5 = 21142341lu;
  unsigned long ul6 = 21142341Lu;
  unsigned long ul7 = 21142341lU;
  unsigned long ul8 = 21142341LU;

  long long ll = 123222222ll;
  long long ll2 = 123222222LL;

  unsigned long long ull1 = 123222222ull;
  unsigned long long ull2 = 123222222uLL;
  unsigned long long ull3 = 123222222Ull;
  unsigned long long ull4 = 123222222ULL;
  unsigned long long ull5 = 123222222llu;
  unsigned long long ull6 = 123222222LLu;
  unsigned long long ull7 = 123222222llU;
  unsigned long long ull8 = 123222222LLU;
}

int testFloatingPoint() {
  float f1 = 123.0f;
  float f2 = 123.0F;

  double d1 = 431.0;
  double d2 = 431.0l;
  double d3 = 431.0L;
}
