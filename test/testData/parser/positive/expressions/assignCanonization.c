
struct S1 {
  int x;
  int a;
  int arr[20];
};


struct S2 {
  struct S1 as[10];
};



void testStructValue(int i) {
  struct S1 s1;

  s1.a += 10;
  s1.a += s1.a;
  s1.arr[i++] -= 20;
  s1.arr[s1.a] *= s1.a;

  struct S2 s2;

  s2.as[i++].a += 30;
  s2.as[i++].a += s2.as[s1.a].a;
  s2.as[i++].arr[i++] -= 40;
  s2.as[s1.a].arr[i++] *= s2.as[i].arr[s2.as[s1.a].a];
}

void testStructPtr(struct S1 *s1, struct S2 *s2, int i) {
  s1->a += 10;
  s1->a += s1->a;
  s1->arr[i++] -= 20;
  s1->arr[s1->a] *= s1->a;

  s2->as[i++].a += 30;
  s2->as[i++].a += s2->as[s1->a].a;
  s2->as[i++].arr[i++] -= 40;
  s2->as[s1->a].arr[i++] *= s2->as[i].arr[s2->as[s1->a].a];
}

struct S3 {
  double d;
  unsigned f1: 10;
  unsigned f2: 15;
};

struct S4 {
  struct S3 as3[10];
};


void testStructBitField(struct S4 *s4, int x) {
  s4->as3[x++].f1 = 10;
  s4->as3[x++].f2 = s4->as3[--x].f1;
}


void testArrayAsgn(int *ptr, int i) {
  ptr[i++] += 10;
  ptr[10] += --i;
}



