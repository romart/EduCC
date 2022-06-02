#include <stdio.h>


struct {
  char a;
  int b : 5;
  int c : 10;
} g45 = {1, 2, 3}, g46={};

int main() {

  if (1 != sizeof(struct {int x:1; })) return 1;
//  if (8 != sizeof(struct {long x:1; })) return 2;

  struct bit1 {
    short a;
    char b;
    int c : 2;
    int d : 3;
    int e : 3;
  };

  if (4 != sizeof(struct bit1)) return 3;

  struct bit1 x1 ={1,2,3,4,5};

  if (x1.a != 1) return 5;
  if (x1.b != 2) return 6;
  if (x1.c != -1) return 7;
  if (x1.d != -4) return 8;
  if (x1.e != -3) return 9;

  struct bit2 {
    short a;
    char b;
    unsigned int c : 2;
    unsigned int d : 3;
    unsigned int e : 3;
  };

  struct bit2 x2 ={1,2,3,4,5};

  if (x2.a != 1) return 10;
  if (x2.b != 2) return 11;
  if (x2.c != 3) return 12;
  if (x2.d != 4) return 13;
  if (x2.e != 5) return 14;

//  if (1 != g45.a) return 15;
//  if (2 != g45.b) return 16;
//  if (3 != g45.c) return 17;

  if (0 != g46.a) return 18;
  if (0 != g46.b) return 19;
  if (0 != g46.c) return 20;

  typedef struct {
    int a : 10;
    int b : 10;
    int c : 10;
  } T3;

  T3 x3 = {1,2,3};

  if (1 != x3.a++) return 21;
  if (2 != x3.b++) return 22;
  if (3 != x3.c++) return 23;

  T3 x4 = {1,2,3};

  if (2 != ++x4.a) return 24;
  if (3 != ++x4.b) return 25;
  if (4 != ++x4.c) return 26;

//  ASSERT(4, sizeof(struct {int a:3; int c:1; int c:5;}));
//  ASSERT(8, sizeof(struct {int a:3; int:0; int c:5;}));
//  ASSERT(4, sizeof(struct {int a:3; int:0;}));

  printf("OK\n");
  return 0;
}
