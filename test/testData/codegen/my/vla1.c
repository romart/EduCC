#include <stdio.h>

int main() {
  if ((5+1)*(8*2)*4 != ({ int m=5, n=8; int x[m+1][n*2]; sizeof(x); })) return 1;
  if (20 != ({ int n=5; int x[n]; sizeof(x); })) return 2;
  if (8 != ({ char n=10; int (*x)[n][n+2]; sizeof(x); })) return 3;
  if (480 != ({ char n=10; int (*x)[n][n+2]; sizeof(*x); })) return 4;
  if (48 != ({ char n=10; int (*x)[n][n+2]; sizeof(**x); })) return 5;
  if (4 != ({ char n=10; int (*x)[n][n+2]; sizeof(***x); })) return 6;
  if (60 != ({ char n=3; int x[5][n]; sizeof(x); })) return 7;
  if (12 != ({ char n=3; int x[5][n]; sizeof(*x); })) return 8;
  if (60 != ({ char n=3; int x[n][5]; sizeof(x); })) return 9;
  if (20 != ({ char n=3; int x[n][5]; sizeof(*x); })) return 10;
  if (0 != ({ int n=10; int x[n+1][n+6]; int *p=(int *)x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][0]; })) return 11;
  if (5 != ({ int n=10; int x[n+1][n+6]; int *p=(int *)x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][5]; })) return 12;
  if (10 != ({ int n=5; sizeof(char[2][n]); })) return 13;
  if (5*16+2 != ({ int n=10; int x[n+1][n+6]; int *p=(int *)x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[5][2]; })) return 14;

  printf("OK\n");
  return 0;
}
