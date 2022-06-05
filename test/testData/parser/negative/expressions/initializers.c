
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

typedef struct { int a,b; short c; char d; } Ty4;
typedef struct { int a; float b; double c; } Ty5;
typedef struct { unsigned char a[3]; } Ty6;
typedef struct { long a, b, c; } Ty7;
typedef struct { unsigned char a[10]; } Ty20;
typedef struct { unsigned char a[20]; } Ty21;


Ty4 r1 = {10, 20, 30, 40};
Ty5 r2 = {10, 20, 30};
Ty6 r3 = {10, 20, 30};
Ty20 r4 = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
Ty21 r5 = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};

extern int eintarr[];

int fox() {}

void *fptr = &fox;

int fvari = 139;
int *pfvari = &fvari;

int axx[29];

int *paxx = axx;

int bbb = axx[0];

static char mon[][4] = {
   "Jan", "Feb", "Mar", "Apr", "May", "Jun",
   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};


