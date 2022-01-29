
int foo(int b);

int testBreakWhile(int a) {
  break;

  while (a) {
      a = foo(a);
  }

  break;
}


int testBreakDoWhile(int a) {
  break;

  do {
      a = foo(a);
  } while (a);

  break;
}

int testBreakFor(int a) {
  break;

  int r = 0;
  int i;
  for (i = 0; i < a; ++i) {
      r += foo(i);
  }

  break;
}

int testBreakSwitch(int a, int b) {
  break;
  int r = 0;

  switch (a) {
  case 1:
        r = foo(b * b);
      break;
  case 2:
        r = foo(b * b);
      break;
  case 3:
        r = foo(b * b);
      break;
  case 4:
        r = foo(b * b);
      break;
  }

  break;

  return r;
}
