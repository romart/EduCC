int i;
float f;
int *pi;
float *pf;

int arr[20];

struct S {};

struct S s, *ps;

void testTernary(int cond) {
  cond ? i : i;
  cond ? i : f;
  cond ? i : pi;
  cond ? i : pf;
  cond ? i : arr;
  cond ? i : s;
  cond ? i : ps;
  cond ? f : i;
  cond ? f : f;
  cond ? f : pi;
  cond ? f : pf;
  cond ? f : arr;
  cond ? f : s;
  cond ? f : ps;
  cond ? pi : i;
  cond ? pi : f;
  cond ? pi : pi;
  cond ? pi : pf;
  cond ? pi : arr;
  cond ? pi : s;
  cond ? pi : ps;
  cond ? pf : i;
  cond ? pf : f;
  cond ? pf : pi;
  cond ? pf : pf;
  cond ? pf : arr;
  cond ? pf : s;
  cond ? pf : ps;
  cond ? arr : i;
  cond ? arr : f;
  cond ? arr : pi;
  cond ? arr : pf;
  cond ? arr : arr;
  cond ? arr : s;
  cond ? arr : ps;
  cond ? s : i;
  cond ? s : f;
  cond ? s : pi;
  cond ? s : pf;
  cond ? s : arr;
  cond ? s : s;
  cond ? s : ps;
  cond ? ps : i;
  cond ? ps : f;
  cond ? ps : pi;
  cond ? ps : pf;
  cond ? ps : arr;
  cond ? ps : s;
  cond ? ps : ps;
}
