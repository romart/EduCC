

int t(int *a, int i) {
  return a[-i];
}



int main() {
  int arr[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  if (5 != t(&arr[9], 4)) return 1;
  if (3 != t(&arr[5], 2)) return 2;
  if (1 != t(&arr[1], 0)) return 3;

  if (4 != t(&arr[0], -4)) return 4;
  if (6 != t(&arr[4], -2)) return 5;

  return 0;
}
