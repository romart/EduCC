

int test(int x) {
  if (!x) {
      return 10;
  } else if (x == 20) {
      return 30;
  } else if (x < 5) {
      return 4;
  } else if (7 <= x && x < 11) {
      return 100;
  } else if (x > 100 || x < 6) {
      return 200;
  } else {
      return 1000;
  }
}

int main() {
  if (test(0) != 10) return 1;
  if (test(20) != 30) return 2;
  if (test(4) != 4) return 3;
  if (test(10) != 100) return 4;
  if (test(100) != 1000) return 5;
  if (test(5) != 200) return 6;
  if (test(150) != 200) return 7;
  return 0;
}
