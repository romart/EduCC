
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
