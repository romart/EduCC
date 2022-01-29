
void foo(int x);

void forTest(int a, int b) {
  int i =0;
  for (; i < a; ++i) {
      if (a > b) {
          foo(a - b);
          break;
      }
      foo(b);
      foo(a);
      forTest(b, b - a);
  }
}

void whileTest(int a, int b) {
  int i =0;
  while (i < a) {
      if (a > b) {
          foo(a - b);
          ++i;
          break;
      }
      foo(b);
      foo(a);
      whileTest(b, b - a);
      --a;
  }
}

void doWhileTest(int a, int b) {
  int i =0;
  do {
      if (a > b) {
          foo(a - b);
          ++i;
          break;
      }
      foo(b);
      foo(a);
      whileTest(b, b - a);
      --a;
  } while (i < b);
}

void switchTest(int a) {
  switch (a) {
      case 1: foo(a); break;
      case 2: foo(a + a);
        break;
      case 3: foo(a - a);
      break;
      case 4: foo(a * a);
      break;
    default: foo(-a);
  }
}
