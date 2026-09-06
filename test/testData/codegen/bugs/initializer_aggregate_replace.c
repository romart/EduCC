struct S { int a, b; };
static struct S global[1] = {[0] = {1, 2}, [0] = {3}};
static int array[1][2] = {[0] = {1, 2}, [0] = {3}};

int main(void) {
  struct S local[1] = {[0] = {1, 2}, [0] = {3}};
  int a[1][2] = {[0] = {1, 2}, [0] = {3}};
  struct S partial[1] = {[0] = {1, 2}, [0].a = 3};
  int b[1][2] = {[0] = {1, 2}, [0][0] = 3};
  if (global[0].a != 3 || global[0].b || local[0].a != 3 || local[0].b) return 1;
  if (array[0][0] != 3 || array[0][1] || a[0][0] != 3 || a[0][1]) return 2;
  if (partial[0].a != 3 || partial[0].b != 2 || b[0][0] != 3 || b[0][1] != 2) return 3;
  return 0;
}
