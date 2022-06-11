

int testeq(unsigned x, unsigned y) {
  if (x == y) {
      return 1;
  }
  return -1;
}


int testne(unsigned x, unsigned y) {
  if (x != y) {
      return 1;
  }
  return -1;
}

int testlt(unsigned x, unsigned y) {
  if (x < y) {
      return 1;
  }
  return -1;
}

int testle(unsigned x, unsigned y) {
  if (x <= y) {
      return 1;
  }
  return -1;
}


int testgt(unsigned x, unsigned y) {
  if (x > y) {
      return 1;
  }
  return -1;
}


int testge(unsigned x, unsigned y) {
  if (x >= y) {
      return 1;
  }
  return -1;
}



int main() {
  if (testeq(42, 42) != 1) return 8;
  if (testeq(42, 24) != -1) return 9;

  if (testne(42, 42) != -1) return 12;
  if (testne(42, 24) != 1) return 13;

  if (testlt(42, 42) != -1) return 16;
  if (testlt(24, 42) != 1) return 18;
  if (testlt(24, 0xFFFFFFFF) != 1) return 19;
  if (testlt(42, 24) != -1) return 19;
  if (testlt(0xFFFFFFFF, 24) != -1) return 17;

  if (testle(42, 42) != 1) return 22;
  if (testle(24, 42) != 1) return 23;
  if (testle(24, 0xFFFFFFFF) != 1) return 24;
  if (testle(42, 24) != -1) return 25;
  if (testle(0xFFFFFFFF, 24) != -1) return 26;

  if (testgt(42, 42) != -1) return 27;
  if (testgt(24, 42) != -1) return 28;
  if (testgt(24, 0xFFFFFFFF) != -1) return 29;
  if (testgt(42, 24) != 1) return 30;
  if (testgt(0xFFFFFFFF, 24) != 1) return 31;

  if (testge(42, 42) != 1) return 34;
  if (testge(24, 42) != -1) return 35;
  if (testge(24, 0xFFFFFFFF) != -1) return 36;
  if (testge(42, 24) != 1) return 37;
  if (testgt(0xFFFFFFFF, 24) != 1) return 38;

  return 0;
}
