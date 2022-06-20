




int test(int x[10]) {

    return x[1] + x[3] + sizeof(x);
}

int main() {
  int x[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

  if (test(x) != 14) return 1;

  return 0;
}
