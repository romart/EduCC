struct S1 {
  int a;
  int b;
  int c;
};


struct S2 {
  int a;
  int b;
  int c;
  int d;
};


void incompatiblePtrsDiff() {
  struct S1 *s1;
  struct S2 *s2;
  struct S3 *s3;
  struct S3 *s3_2;

  int a = s1 - s2;
  int b = s3 - s3_2;
  int c = s1 - s3;

  int *ip;
  double *dp;
  int **ipp;

  int d = ip - dp;
  int e = ip - ipp;

  int arr[1];
  int *parr[1];

  int f = dp - arr;

  int g = arr - parr;


  void *vp;


  int h = vp - ip;
}


void ptrPlus(int *ptr1, int *ptr2, struct S3 *p2) {

  float f = 1.0;

  int **pp;

  int *r = ptr1 + ptr2;
  int *a = ptr1 + f;
  int *b = f + ptr1;

  pp + ptr1;

  int x;

  struct S3 *ptr1_2 = 1 + p2 + 1;
  struct S3 *ptr2_2 = 2 + 2 + p2;
  struct S3 *ptr3_2 = (p2 + 2) + 3;
  struct S3 *ptr4_2 = x + (3 + (2 + p2));
}

void ptrMinus(int *ptr1, int *ptr2, struct S3 *p) {

  float f = 1.0;

  int *a = ptr1 - f;
  int *b = f - ptr1;

  int x;

  int *ptr1 = 1 - ptr1;
  int *ptr2 = 2 - ptr1 - 2;
  int *ptr3 = x + (3 - (2 - ptr1));

  struct S3 *ptr4 = 1 - p - 1;
  struct S3 *ptr5 = 2 - 2 - p;
  struct S3 *ptr6 = (p + 2) - 3;
  struct S3 *ptr7 = x + (3 - (2 - p));
}
