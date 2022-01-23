int i;
float f;
int *pi;
float *pf;

int arr[20];

struct S {};

struct S s, *ps;

void testAmp() {
  register int ri;

  &ri;
  &i;
  &f;
  &pi;
  &pf;
  &arr;
  &s;
  &ps;
}


void testStar() {
  *i;
  *f;
  *pi;
  *pf;
  *arr;
  *s;
  *ps;
}


void testPlus() {
  +i;
  +f;
  +pi;
  +pf;
  +arr;
  +s;
  +ps;
}


void testMinus() {
  -i;
  -f;
  -pi;
  -pf;
  -arr;
  -s;
  -ps;
}


void testExl() {
  !i;
  !f;
  !pi;
  !pf;
  !arr;
  !s;
  !ps;
}


void testTilda() {
  ~i;
  ~f;
  ~pi;
  ~pf;
  ~arr;
  ~s;
  ~ps;
}


void testInc() {
  ++i;
  ++f;
  ++pi;
  ++pf;
  ++arr;
  ++s;
  ++ps;
}


void testDec() {
  --i;
  --f;
  --pi;
  --pf;
  --arr;
  --s;
  --ps;
}
