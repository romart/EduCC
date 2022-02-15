

struct S1 {
  int a;
  int b;
};

struct S2 {
  struct S1 s1;
  struct S2 *p_s2;
  int c;
};

struct S3 {
  struct S2 s2;
  struct S1 s1;
  struct S1 s1_arr[10];
  struct S2 *p_s2_arr[10];
};

struct S4 {
  unsigned bf: 6;
  unsigned ff: 7;
  struct S3 s3;
};

int testArrayPrimitive() {
  int i = 0;
  int arr[10];
  char carr[10];

  int a = arr[i];
  int b = arr[2];
  int c = arr[4];

  int d = carr[0];
  int e = carr[9];
}

int testStructValued1() {
  struct S1 s1;

  int a = s1.a;
  int b = s1.b;
}

int testStructPointed1(struct S1 *p_s1) {
  int a = p_s1->a;
  int b = p_s1->b;
}


int testStructValued2() {
  struct S2 s2;

  int a = s2.s1.b;
  int c = s2.p_s2->c;
}

int testStructPointed2(struct S2 *p_s2) {
  int a = p_s2->s1.a;
  int b = p_s2->p_s2->c;
  int c = p_s2->c;
}

int testStructValued3(int i) {
  struct S3 s3;

  int b = s3.s2.s1.b;
  int c = s3.s2.p_s2->c;
  int d = s3.s1_arr[i].b;
  int e = s3.p_s2_arr[i + 1]->p_s2->s1.b;
  int e = s3.p_s2_arr[i + 1]->p_s2->c;
}

int testStructPointed3(struct S3 *p_s3, int i) {
  int b = p_s3->s2.s1.b;
  int c = p_s3->s2.p_s2->c;
  int d = p_s3->s1_arr[i].b;
  int e = p_s3->p_s2_arr[i + 1]->p_s2->s1.b;
  int e = p_s3->p_s2_arr[i + 1]->p_s2->c;
}

int testStructPointed4(struct S4 *p_s4, int i) {

  int f1 = p_s4->bf;
  int f2 = p_s4->ff;

  int b = p_s4->s3.s2.s1.b;
  int c = p_s4->s3.s2.p_s2->c;
  int d = p_s4->s3.s1_arr[i].b;
  int e = p_s4->s3.p_s2_arr[i + 15]->p_s2->s1.b;
  int e = p_s4->s3.p_s2_arr[i + 15]->p_s2->c;
}

int testStructValued(int i) {
  struct S4 s4;


  int a = s4.bf;
  int b = s4.ff;
  int c = s4.s3.s2.s1.a;
  int d = s4.s3.s2.s1.b;
  int e = s4.s3.s2.p_s2->c;
  int f = s4.s3.s2.c;
  int g = s4.s3.s1.a;
  int h = s4.s3.s1.b;
  int j = s4.s3.s1_arr[i].b;
  int k = s4.s3.p_s2_arr[i]->p_s2->p_s2->c;
}
