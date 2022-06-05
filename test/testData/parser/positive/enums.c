enum E1 {
  EC_1 = -1,
  EC_2 = EC_1 + 15,
  EC_3
};

int foo1(void) {
  int xx = EC_1;
  double xxx = EC_2 + EC_3;
  unsigned char xxxx = xx + xxx;

}

enum E2 {
  EC_4 = EC_3 * 2,
  EC_5, EC_6
};
