
struct S1 {
  int a;
  int b;
  int c;
};




int intPtrTest(int *p, int x) {
  int *ptr1 = 1 + p;
  int *ptr2 = 2 + p + 2;
  int *ptr3 = (p + 2) + 3;
  int *ptr4 = x + (3 + (2 + p));
}


int voidPtrTest(void *p, int x) {
  void *ptr1 = 1 + p + 1;
  void *ptr2 = 2 + 2 + p;
  void *ptr3 = (p + 2) + 3;
  void *ptr4 = x + (3 + (2 + p));
}

int structPtrTest(struct S1 *p, int x) {
  struct S1 *ptr1 = 1 + p + 1;
  struct S1 *ptr2 = 2 + 2 + p;
  struct S1 *ptr3 = (p + 2) + 3;
  struct S1 *ptr4 = x + (3 + (2 + p));;
}
