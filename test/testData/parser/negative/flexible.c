
struct SX {
  int a, b, c, d, e;
  char flex[];
};

struct SX g = { 1, 2, 3, 4, 5, "ABC" };


int test() {
  struct SX l1 = { 1, 2, 3, 4, 5};
  struct SX l2 = { 5, 6, 7, 8, 9, "JJJJ" };
}
