
int foo(int b);

int testContinueWhile(int a) {
  continue;

  while (a) {
      a = foo(a);
  }

  continue;
}


int testContinueDoWhile(int a) {
  continue;

  do {
      a = foo(a);
  } while (a);

  continue;
}

int testContinueFor(int a) {
  continue;

  int r = 0;
  int i;
  for (i = 0; i < a; ++i) {
      r += foo(i);
  }

  continue;
}
