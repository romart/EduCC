
unsigned char arr[] = { 1, 2, 3, 4, 5 };

int foo(int d, unsigned char p) {
  return p;
}

int test(unsigned char c, int idx) {
  foo(10, c | arr[idx]);
}


int main() { return 0; }
