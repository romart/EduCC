// A switch whose condition folds to a constant during sparse constant
// propagation: the pass replaces the condition with a constant it created
// itself, which has no lattice value, and reading one crashed the compiler.
// zlib's zutil.c does this with switch(sizeof(...)).

int flags(void) {
  int f = 0;

  switch ((int)(sizeof(int))) {
  case 2: break;
  case 4: f += 1; break;
  case 8: f += 2; break;
  default: f += 3;
  }

  switch ((int)(sizeof(void *))) {
  case 2: break;
  case 4: f += 1 << 2; break;
  case 8: f += 2 << 2; break;
  default: f += 3 << 2;
  }

  return f;
}

int main(void) {
  return flags() == (1 + (2 << 2)) ? 0 : 1;
}
