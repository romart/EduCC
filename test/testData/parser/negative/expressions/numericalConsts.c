
void testChar() {
  char c1 = 'aa';
  char c2 = 'aas';
  char c3 = '\aa';
  char c4 = '\aas';
  char c5 = '\555';
  char c6 = '\x80';
  char c7 = '\0x44';
  char c9 = '\a';
  char c10 = '\b';
  char c11 = '\c';
  char c12 = '\d';
  char c13 = '\e';
  char c14 = '\f';
  char c8 = '\g';
}

void testWchar() {
  int w1 = L'aa';
  int w2 = L'aas';
  int w3 = L'\aa';
  int w4 = L'\aas';
  int w5 = L'\555555';
  int w6 = L'\x8000';
  int w7 = L'\0x44';
  int w8 = L'\j';
}

int testInteger() {
  int s = 12345465456465;
  unsigned u = 12344545645656u;
  unsigned u2 = 12545646545463U;

  long l = 2114234411646545641l;
  long l2 = 21142344654564461L;
  unsigned long ul1 = 2114235465465441ul;
  unsigned long ul2 = 2114234456465465641uL;
  unsigned long ul3 = 21142348465465446461Ul;
  unsigned long ul4 = 21142345456465465564651UL;
  unsigned long ul5 = 2114234444654564656461lu;
  unsigned long ul6 = 2114246546546546465341Lu;
  unsigned long ul7 = 21142341446546546546lU;
  unsigned long ul8 = 21142356465465465441LU;

  long long ll = 1232222654546444522ll;
  long long ll2 = 12322224546546522LL;

  unsigned long long ull1 = 123224544654654652222ull;
  unsigned long long ull2 = 12322245646545646222uLL;
  unsigned long long ull3 = 12322254445646546222Ull;
  unsigned long long ull4 = 445465465465123222222ULL;
  unsigned long long ull5 = 12322245645665222llu;
  unsigned long long ull6 = 123222546546546222LLu;
  unsigned long long ull7 = 123222464566446222llU;
  unsigned long long ull8 = 12322246546546546546222LLU;
}

//int testFloatingPoint() {
//  float f1 = 123564564654564545465456465.0f;
//  float f2 = 1235444654654654646546465546.0F;

//  double d1 = 431.0;
//  double d2 = 431.0l;
//  double d3 = 431.0L;
//}
