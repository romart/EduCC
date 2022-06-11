

struct SUF {
  char s1;
  short s2;
  int s4;
  long s8;

  unsigned char u1;
  unsigned short u2;
  unsigned int u4;
  unsigned long u8;

  float f4;
  double f8;
};


struct USF {
  unsigned char u1;
  unsigned short u2;
  unsigned int u4;
  unsigned long u8;

  char s1;
  short s2;
  int s4;
  long s8;


  float f4;
  double f8;
};


// TODO: add bitfields


int test1() {
  struct SUF suf = { -1, -2, -4, -8L, 0xFF, 0xFFFF, 0xFFFFFFFFU, 0xFFFFFFFFFFFFFFFFUL, 1.0f, 2.2 };

  if (suf.s1 != -1) return 1;
  if (suf.s2 != -2) return 2;
  if (suf.s4 != -4) return 3;
  if (suf.s8 != -8) return 4;

  if (suf.u1 != 0xFF) return 5;
  if (suf.u2 != 0xFFFF) return 6;
  if (suf.u4 != 0xFFFFFFFFU) return 7;
  if (suf.u8 != 0xFFFFFFFFFFFFFFFFUL) return 8;

  if (suf.f4 != 1.0f) return 9;
  if (suf.f8 != 2.2) return 10;

  return 0;
}

int test2(struct SUF *suf) {
  if (suf->s1 != -1) return 1;
  if (suf->s2 != -2) return 2;
  if (suf->s4 != -4) return 3;
  if (suf->s8 != -8) return 4;

  if (suf->u1 != 0xFF) return 5;
  if (suf->u2 != 0xFFFF) return 6;
  if (suf->u4 != 0xFFFFFFFFU) return 7;
  if (suf->u8 != 0xFFFFFFFFFFFFFFFFUL) return 8;

  if (suf->f4 != 1.0f) return 9;
  if (suf->f8 != 2.2) return 10;

  return 0;
}

int test3(struct USF *usf) {

  if (usf->u1 != 0xFFU) return 1;
  if (usf->u2 != 0xFFFEU) return 2;
  if (usf->u4 != 0xFFFFFFFCU) return 3;
  if (usf->u8 != 0xFFFFFFFFFFFFFFF8UL) return 4;

  if (usf->s1 != -1) return 5;
  if (usf->s2 != -1) return 6;
  if (usf->s4 != -1) return 7;
  if (usf->s8 != -1) return 8;

  if (usf->f4 != 1.0f) return 9;
  if (usf->f8 != 2.2) return 10;

  return 0;
}

// TODO: add store tests

int main() {

  int r = test1();

  if (r) return 100 + r;

  struct SUF suf = { -1, -2, -4, -8L, 0xFF, 0xFFFF, 0xFFFFFFFFU, 0xFFFFFFFFFFFFFFFFUL, 1.0f, 2.2 };

  r = test2(&suf);

  if (r) return 200 + r;

  r = test3((struct USF*)&suf);

  if (r) return 300 + r;

  return 0;
}

