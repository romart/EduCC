
int bar(const char* s) {}

int foo_ok(int a, int b) {
  if (a) {
      bar("ss");
      goto bb;
  } else if(b) {
      bar("lll");
      bb:
      bar("lfff");
  } else {
      bar("JHHHH");
      goto bb;
  }
}

int foo_p(int a, int b) {
  void *lptr = &&bb;
  if (a) {
      bar("ss");
      goto *lptr;
  } else if(b) {
      bar("lll");
      bb:
      bar("lfff");
  } else {
      bar("JHHHH");
      goto *lptr;
  }
}

