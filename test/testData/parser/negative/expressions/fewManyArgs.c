

int foo(int a, int b);


int many(int a) {
  foo(a, a, a);
}

int few(int a) {
  foo(a);
}
