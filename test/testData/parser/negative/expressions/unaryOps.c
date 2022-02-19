int i;
float f;
int *pi;
float *pf;

int arr[20];

struct S { int x; };

struct S s, *ps;

void *vp;

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
  &vp;
}


void testStar() {
  *i;
  *f;
  *pi;
  *pf;
  *arr;
  *s;
  *ps;
  *vp;
}


void testPlus() {
  +i;
  +f;
  +pi;
  +pf;
  +arr;
  +s;
  +ps;
  +vp;
}


void testMinus() {
  -i;
  -f;
  -pi;
  -pf;
  -arr;
  -s;
  -ps;
  -vp;
}


void testExl() {
  !i;
  !f;
  !pi;
  !pf;
  !arr;
  !s;
  !ps;
  !vp;
}


void testTilda() {
  ~i;
  ~f;
  ~pi;
  ~pf;
  ~arr;
  ~s;
  ~ps;
  ~vp;
}


void testInc() {
  ++i;
  ++f;
  ++pi;
  ++pf;
  ++arr;
  ++s;
  ++ps;
  ++vp;
}


void testDec() {
  --i;
  --f;
  --pi;
  --pf;
  --arr;
  --s;
  --ps;
  --vp;
}
