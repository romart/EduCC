#include <stdio.h>

int main() {
  int x1=3;
  if (3 != *&x1) return 1;
  int x2=3; int *y2=&x2; int **z2=&y2;
  if (3 != **z2) return 2;
  int x3=3; int y3=5;
  if (5 != *(&x3+1)) return 3;
  int x4=3; int y4=5;
  if (3 != *(&y4-1)) return 4;
  int x5=3; int y5=5;
  if (5 != *(&x5-(-1))) return 5;
  int x6=3; int *y6=&x6; *y6=5;
  if (5 != x6) return 6;
  int x7=3; int y7=5; *(&x7+1)=7;
  if (7 != y7) return 7;
  int x8=3; int y8=5; *(&y8-2+1)=7;
  if (7 != x8) return 8;
  int x9=3;
  if (5 != (&x9+2)-&x9+3) return 9;
  int x10, y10; x10=3; y10=5;
  if (8 != x10+y10) return 10;
  int x11=3, y11=5;
  if (8 != x11+y11) return 11;

  int x12[2]; int *y12=&x12; *y12=3;
  if (3 != *x12) return 12;

  int x13[3]; *x13=3; *(x13+1)=4; *(x13+2)=5;
  if (3 != *x13) return 13;
  if (4 != *(x13+1)) return 14;
  if (5 != *(x13+2)) return 15;

  int x16[2][3];
  int *y16=(int*)x16;

  *y16=0;
  if (0 != **x16) return 16;
  *(y16+1)=1;
  if (1 != *(*x16+1)) return 17;
  *(y16+2)=2;
  if (2 != *(*x16+2)) return 18;
  *(y16+3)=3;
  if (3 != **(x16+1)) return 19;
  *(y16+4)=4;
  if (4 != *(*(x16+1)+1)) return 20;
  *(y16+5)=5;
  if (5 != *(*(x16+1)+2)) return 21;

  int x22[3]; *x22=3; x22[1]=4; x22[2]=5;
  if (3 != *x22) return 22;
  if (4 != *(x22+1)) return 23;
  if (5 != *(x22+2)) return 24;
  if (5 != *(x22+2)) return 25;
  2[x22]=6;
  if (6 != *(x22+2)) return 26;

  int x23[2][3];
  int *y23=(int *)x23;

  y23[0]=0;
  if (0 != x23[0][0]) return 23;
  y23[1]=1;
  if (1 != x23[0][1]) return 24;
  y23[2]=2;
  if (2 != x23[0][2]) return 25;
  y23[3]=3;
  if (3 != x23[1][0]) return 26;
  y23[4]=4;
  if (4 != x23[1][1]) return 27;
  y23[5]=5;
  if (5 != x23[1][2]) return 28;

  printf("OK\n");
  return 0;
}
