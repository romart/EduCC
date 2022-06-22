long s = 0x12300000456;
unsigned long u = 0x78900000abc;


long test1() {
  return s & 0x0FFFFFFFF;
}

unsigned long test2() {
  return u & 0x0FFFFFFFF;
}

int main() {

  if (test1() != s) return 1;

  if (test2() != 0xabc) return 2;

  return 0;
}
