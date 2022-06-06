#include <stdio.h>

static int ten = 10;

int testUV() {
  unsigned i=0; int j=0;
  for (i=0; i <= ten; i=i+1) j=i+j;

  if (55 != j) return 1;

  i = j = 0;

  while(i<ten) i=i+1;
  if (10 != i) return 2;

  i = 0;
  while(i<ten) i=i+1;
  if (10 != i) return 3;

  i = j = 0;
  while(i<=ten) {
      j=i+j;
      i=i+1;
  }
  if (55 != j) return 4;

  i = j = 0;
  for (i= 0; i <= ten; i = i + 1) j = j + i;
  if (55 != j) return 5;

  i = 0;
  for(;i<ten;i++) { if (i == 3) break; }
  if (3 != i) return 7;

  i = 0;
  while (1) { if (i++ == 3) break; }
  if (4 != i) return 8;

  i = 0;
  for(;i<ten;i++) { for (;;) break; if (i == 3) break; }
  if (3 != i) return 9;

  i = 0;
  while (1) { while(1) break; if (i++ == 3) break; }
  if (4 != i) return 10;

  i = j = 0;
  for (;i<ten;i++) { if (i>5) continue; j++; }
  if (10 != i) return 11;

  i = j = 0;
  for (;i<ten;i++) { if (i>5) continue; j++; }
  if (6 != j) return 12;

  i = j = 0;
  for(;!i;) { for (;j!=ten;j++) continue; break; }
  if (10 != j) return 13;

  i = j = 0;
  while (i++<ten) { if (i>5) continue; j++; }
  if (11 != i) return 14;

  i = j = 0;
  while (i++<ten) { if (i>5) continue; j++; }
  if (5 != j) return 15;

  i = j = 0;
  while(!i) { while (j++!=ten) continue; break; }
  if (11 != j) return 16;

  return 0;
}

int testUC() {
  unsigned i=0; int j=0;
  for (i=0; i <= 10; i=i+1) j=i+j;

  if (55 != j) return 1;

  i = j = 0;

  while(i<10) i=i+1;
  if (10 != i) return 2;

  i = 0;
  while(i<10) i=i+1;
  if (10 != i) return 3;

  i = j = 0;
  while(i<=10) {
      j=i+j;
      i=i+1;
  }
  if (55 != j) return 4;

  i = j = 0;
  for (i= 0; i <= 10; i = i + 1) j = j + i;
  if (55 != j) return 5;

  i = 0;
  for(;i<10;i++) { if (i == 3) break; }
  if (3 != i) return 7;

  i = 0;
  while (1) { if (i++ == 3) break; }
  if (4 != i) return 8;

  i = 0;
  for(;i<ten;i++) { for (;;) break; if (i == 3) break; }
  if (3 != i) return 9;

  i = 0;
  while (1) { while(1) break; if (i++ == 3) break; }
  if (4 != i) return 10;

  i = j = 0;
  for (;i<10;i++) { if (i>5) continue; j++; }
  if (10 != i) return 11;

  i = j = 0;
  for (;i<10;i++) { if (i>5) continue; j++; }
  if (6 != j) return 12;

  i = j = 0;
  for(;!i;) { for (;j!=10;j++) continue; break; }
  if (10 != j) return 13;

  i = j = 0;
  while (i++<10) { if (i>5) continue; j++; }
  if (11 != i) return 14;

  i = j = 0;
  while (i++<10) { if (i>5) continue; j++; }
  if (5 != j) return 15;

  i = j = 0;
  while(!i) { while (j++!=10) continue; break; }
  if (11 != j) return 16;

  return 0;
}

int testSC() {
  int i=0; int j=0;
  for (i=0; i <= 10; i=i+1) j=i+j;

  if (55 != j) return 1;

  i = j = 0;

  while(i<10) i=i+1;
  if (10 != i) return 2;

  i = 0;
  while(i<10) i=i+1;
  if (10 != i) return 3;

  i = j = 0;
  while(i<=10) {
      j=i+j;
      i=i+1;
  }
  if (55 != j) return 4;

  i = j = 0;
  for (i= 0; i <= 10; i = i + 1) j = j + i;
  if (55 != j) return 5;

  i = 0;
  for(;i<10;i++) { if (i == 3) break; }
  if (3 != i) return 7;

  i = 0;
  while (1) { if (i++ == 3) break; }
  if (4 != i) return 8;

  i = 0;
  for(;i<ten;i++) { for (;;) break; if (i == 3) break; }
  if (3 != i) return 9;

  i = 0;
  while (1) { while(1) break; if (i++ == 3) break; }
  if (4 != i) return 10;

  i = j = 0;
  for (;i<10;i++) { if (i>5) continue; j++; }
  if (10 != i) return 11;

  i = j = 0;
  for (;i<10;i++) { if (i>5) continue; j++; }
  if (6 != j) return 12;

  i = j = 0;
  for(;!i;) { for (;j!=10;j++) continue; break; }
  if (10 != j) return 13;

  i = j = 0;
  while (i++<10) { if (i>5) continue; j++; }
  if (11 != i) return 14;

  i = j = 0;
  while (i++<10) { if (i>5) continue; j++; }
  if (5 != j) return 15;

  i = j = 0;
  while(!i) { while (j++!=10) continue; break; }
  if (11 != j) return 16;

  return 0;
}

int testSV() {
  int i=0; int j=0;
  for (i=0; i <= ten; i=i+1) j=i+j;

  if (55 != j) return 1;

  i = j = 0;

  while(i<ten) i=i+1;
  if (10 != i) return 2;

  i = 0;
  while(i<ten) i=i+1;
  if (10 != i) return 3;

  i = j = 0;
  while(i<=ten) {
      j=i+j;
      i=i+1;
  }
  if (55 != j) return 4;

  i = j = 0;
  for (i= 0; i <= ten; i = i + 1) j = j + i;
  if (55 != j) return 5;

  i = 0;
  for(;i<ten;i++) { if (i == 3) break; }
  if (3 != i) return 7;

  i = 0;
  while (1) { if (i++ == 3) break; }
  if (4 != i) return 8;

  i = 0;
  for(;i<ten;i++) { for (;;) break; if (i == 3) break; }
  if (3 != i) return 9;

  i = 0;
  while (1) { while(1) break; if (i++ == 3) break; }
  if (4 != i) return 10;

  i = j = 0;
  for (;i<ten;i++) { if (i>5) continue; j++; }
  if (10 != i) return 11;

  i = j = 0;
  for (;i<ten;i++) { if (i>5) continue; j++; }
  if (6 != j) return 12;

  i = j = 0;
  for(;!i;) { for (;j!=ten;j++) continue; break; }
  if (10 != j) return 13;

  i = j = 0;
  while (i++<ten) { if (i>5) continue; j++; }
  if (11 != i) return 14;

  i = j = 0;
  while (i++<ten) { if (i>5) continue; j++; }
  if (5 != j) return 15;

  i = j = 0;
  while(!i) { while (j++!=ten) continue; break; }
  if (11 != j) return 16;

  return 0;
}

int main() {

  printf("test UV\n");
  int r = testUV();
  if (r != 0) return r;

  printf("test SV\n");
  r = testSV();
  if (r != 0) return r;

  printf("test UC\n");
  r = testUC();
  if (r != 0) return r;

  printf("test SC\n");
  r = testSC();
  if (r != 0) return r;

  printf("OK\n");
  return 0;
}
