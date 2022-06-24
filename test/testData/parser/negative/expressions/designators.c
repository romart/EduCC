char embraced[] = { "ABCD" };

struct S {

  union {
    int x, y;
  } sections;
} ;

struct S s = { 1, 2, 3 };
struct SX { int a, b, c; };
struct SX ds = { ."aa" = 30 };

int aa[] = { [1] = 10, [2] = 20, [-1] = 30 };
int cc = { [1] = 10, [2] = 20, [-1] = 30 };

struct ST { int a; struct SX s[2][2]; };

int bb[] = { .x = 20, .++ = 30 };
int dd = { .x = 20, .++ = 30 };

struct ST st = { 10, .s = { [1][1] = { .b = 1, .c = 30, .a = 80 } }, 100 };

union { int a; char b[4]; } g50 = {.b[2]=0x12};

int x[3]={1, 2, 3, [0]=4, 5};
int y[2][3]={1,2,3,4,5,6,[0][1]=7,8,[0]=9,[0]=10,11,[1][0]=12};
int z[2][3]={1,2,3,4,5,6,[0]={7,8},9,10};
char a[]={[10-3]=1,2,3};
char b[][2]={[8][1]=1,2};
struct { int a,b; } c={1,2,.b=3,.a=4};

struct { struct { int a,b; } c; } d={.c=1,2};
struct { int a[2]; } e={.a=1,2};
struct { int a[2]; } f={.a[1]=1};

int af[] = { [0] = 31 };

struct { int a,b; } g[]={[1].b=1,2,[0]=3,4,};
union { unsigned short a; char b[2]; } h={.b[0]=0xff};
struct { struct { int a; struct { int b; }; }; int c; } l={1,2,3,.b=4,5};

struct STR { int a; char s[10]; };


struct STR da= { 10, .s = "abcd" };

int x[] = { 1, 2, 9, 10, [0] = 3, 4, [0] = 5, 6, 8 };
