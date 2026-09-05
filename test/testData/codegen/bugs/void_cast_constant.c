// '(void)0' discards its operand rather than converting it, and void has no
// width to make a constant of. Folding one aborted the compiler; SQLite's
// mutex stubs are a whole function body of them.

static void enterMutex(void) {
  ((void) (0));;
}

static int notHeld(int x) {
  ((void) (x));
  ((void) (1 + 2));
  return 1;
}

int main(void) {
  enterMutex();
  return notHeld(0) ? 0 : 1;
}
