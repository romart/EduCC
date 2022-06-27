
struct SX {
  int a, b, c, d, e;
};

int main() {
  struct SX *pls = &(struct SX) { 1,2, 3, 4, 5 };
}


struct SX *sxp = &(struct SX) { 1, 2, 3, 4, 5 };
