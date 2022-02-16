
struct S1 {
  int a;
  int b;
  int c;
  int arr[20];
};


void intPtrTestPlus(int *p, int x) {
  int *ptr1 = 1 + p;
  int *ptr2 = 2 + p + 2;
  int *ptr3 = (p + 2) + 3;
  int *ptr4 = x + (3 + (2 + p));
}


void voidPtrTestPlus(void *p, int x) {
  void *ptr1 = 1 + p + 1;
  void *ptr2 = 2 + 2 + p;
  void *ptr3 = (p + 2) + 3;
  void *ptr4 = x + (3 + (2 + p));
}

void structPtrTestPlus(struct S1 *p, int x) {
  struct S1 *ptr1 = 1 + p + 1;
  struct S1 *ptr2 = 2 + 2 + p;
  struct S1 *ptr3 = (p + 2) + 3;
  struct S1 *ptr4 = x + (3 + (2 + p));
}

void intPtrTestMinus(int *p, int x) {
  int *ptr1 = p - 1;
  int *ptr2 = p - 2 - 2;
  int *ptr3 = (p - 2) + 3;
  int *ptr4 = (p - x) + (3 - 2);
}


void voidPtrTestMinus(void *p, int x) {
  void *ptr1 = p - 1;
  void *ptr2 = p - 2 - 2;
  void *ptr3 = (p + 2) - 3;
  void *ptr4 = p - x - (3 - (2));
}

void structPtrTestMinus(struct S1 *p, struct S2 *p2, int x) {
  struct S1 *ptr1 = p - 1;
  struct S1 *ptr2 = p - 2 - 2;
  struct S1 *ptr3 = (p + 2) - 3;
  struct S1 *ptr4 = p - x + (3 - (2));
}

void intPtrTestDiff(int *p1, const int *p2, int **pp) {
  int a = p1 - p2;

  int arr[10];

  int b = arr - p1;
  int c = p2 - arr;

  int *p_arr[10];

  int d = p_arr - pp;
  int e = pp - p_arr;
}


void voidPtrTestDiff(void *p1, const void *p2, void **pp) {
  int a = p1 - p2;

  void **ppx;

  int d = ppx - pp;
  int e = pp - ppx;
}

void structPtrTestDiff(struct S1 *p1, struct S1 *p2, struct S1 **pp) {
  int a = p1 - p2;

  struct S1 arr[10];

  int b = arr - p1;
  int c = p2 - arr;

  struct S1 *p_arr[10];

  int d = p_arr - pp;
  int e = pp - p_arr;

  struct S3 **ps3_1, **ps3_2;
  struct S3 *arr_s3[10];

  int f = ps3_2 - ps3_1;
  int g = arr_s3 - ps3_1;
  int h= ps3_2 - arr_s3;

  int j = p1 - a - b - p2;
}
