int i;
float f;
int *pi;
float *pf;

int arr[20];

struct S { int good; };

struct S s, *ps;

void testDot() {
  i.good;
  i.bad;
  f.good;
  f.bad;
  pi.good;
  pi.bad;
  pf.good;
  pf.bad;
  arr.good;
  arr.bad;
  s.good;
  s.bad;
  ps.good;
  ps.bad;
}


void testArrow() {
  i->good;
  i->bad;
  f->good;
  f->bad;
  pi->good;
  pi->bad;
  pf->good;
  pf->bad;
  arr->good;
  arr->bad;
  s->good;
  s->bad;
  ps->good;
  ps->bad;
}
