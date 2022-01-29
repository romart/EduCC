
struct S { float x; int *ptr; int *ptr2; };
struct SX { int a; struct S s; struct S as[2]; };

struct S s0;
struct S s1 = { 01.f, (int*)0, (int*)0 };
struct S s2 = { { 01.f, (int*)0, (int*)0 } };
struct S s3 = { { 01.f, { (int*)0, { (int*)0 } } } };
struct S s4 = { { 01.f, { (int*)0, { (int*)0, {} } } } };
struct S s5[] = { { 01.f, (int*)0, (int*)0 }, { 01.f, (int*)0, (int*)0 }, { 01.f, (int*)0, (int*)0 } };
struct S s6 = { { { 01.f }, { (int*)0 }, { (int*)0 } } };


struct S s7 = { { { (void*)0 }, { 01.f }, { } } };
struct S s8[] = { { { (void*)0, 01.f, 5 } , 0 }, { 0, 0.1f, "ccc" } , 0 };

struct S sa[] = { 0 };

struct SX sx1 = { 0, s0, { 0 } };

struct S as1[] = { 0, 0, 0 };
struct S as2[] = { {0}, {0}, {0} };

int a[] = { 0, 1, { 2, 3, { 4, 5, 6 }, 7, {}, 8, }, 9 };
int b[][2] = { {0, 1}, { 2, 3, { 4, 5, 6 }, 7, {}, 8, }, {9} };
int c[] = { 0, 1, 3, 4, 5, 6, 7, 8, 9 };
int d[] = { { 0 } , { 1 } , { 3 } , { 4 } , { 5 } , { 6 } , { 7 } , { 8 } , { 9 } };
int e[] = { { 0 } , 1 , { 3 } , 4 , { 5 } , 6 , { 7 } , 8 , { 9 } };
int f[] = { { 0 } , {{ 1 }} , { 3 } , {{ 4 }} , { 5 } , {{ 6 }}, { 7 } , {{ 8 }} , { 9 } };
int g[4] = { 0, { 1, { 2, { 3 } } } };
int h[4] = { 0, { 1, { 2, { 3, { 4 } } } } };
int i[4] = { 0, { 1, { 2, { 3, { 4, {} } } } } };

int a1[20];
int *pa = a1;
float f = pa;
int a3[];
int a4[20] = 0;
int b2[][20] = {} ;
struct S s22 = { 0 };
int i2 = 10;

float f2 = 1.0f;

void x;
int a5[]={ { { 0, 0}, 0, { 0, 0}}, { 0}, {0, { }}};
int aa = {{{{}, 0}, 0, 0}, 0, 0, {}};

int *ap[] = { 1.0f, 2.0f };
float af[] = { (int*)0, (int*)1 };

int func() {
  int a2[] = a1;
  int *pa = a1;
  float f = pa;
  int i2 = f2;
  int i3 = s2;
  struct SX sx2 = { 0, s0, { 0 } };
}

extern int eei = 20;
