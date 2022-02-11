
int bar(const char* s) {}

int foo_fail1(int a, int b) {
  if (a) {
      bar("ss");
      goto bb;
  } else if (b) {
      bar("lll");
      bb:
      bar("lfff");
  } else {
      bar("uuu");
      bb:
      bar("lffppppf");
  }
}

int foo_fail2(int a, int b) {
  if (a) {
      bar("ss");
      goto cc;
  } else {
      bar("uuu");
      bb:
      bar("lffppppf");
  }
}

int foo_fail3(int a, int b) {
  void *lptr = &&cc;
  if (a) {
      bar("ss");
      goto *lptr;
  } else {
      bar("uuu");
      bb:
      bar("lffppppf");
  }
}
