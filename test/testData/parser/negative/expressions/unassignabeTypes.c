
struct S1 {};
struct S2 {};

int g = 10;
float gf =20.f;

int garr[20];

int main() {

  int i;
  float f;
  struct S1 s1;
  struct S2 s2;
  int arr[20];
  int *pi;
  float *fp;
  struct S1 *ps1;
  struct S2 *ps2;

  i = g;
  i = gf;
  i = garr;
  i = s1;
  i = s2;
  i = pi;
  i = fp;
  i = ps1;
  i = ps2;

  f = g;
  f = gf;
  f = garr;
  f = s1;
  f = s2;
  f = pi;
  f = fp;
  f = ps1;
  f = ps2;

  s1 = g;
  s1 = gf;
  s1 = garr;
  s1 = s1;
  s1 = s2;
  s1 = pi;
  s1 = fp;
  s1 = ps1;
  s1 = ps2;

  s2 = g;
  s2 = gf;
  s2 = garr;
  s2 = s1;
  s2 = s2;
  s2 = pi;
  s2 = fp;
  s2 = ps1;
  s2 = ps2;

  arr = g;
  arr = gf;
  arr = garr;
  arr = s1;
  arr = s2;
  arr = pi;
  arr = fp;
  arr = ps1;
  arr = ps2;

  pi = g;
  pi = gf;
  pi = garr;
  pi = s1;
  pi = s2;
  pi = pi;
  pi = fp;
  pi = ps1;
  pi = ps2;

  fp = g;
  fp = gf;
  fp = garr;
  fp = s1;
  fp = s2;
  fp = pi;
  fp = fp;
  fp = ps1;
  fp = ps2;

  ps1 = g;
  ps1 = gf;
  ps1 = garr;
  ps1 = s1;
  ps1 = s2;
  ps1 = pi;
  ps1 = fp;
  ps1 = ps1;
  ps1 = ps2;

  ps2 = g;
  ps2 = gf;
  ps2 = garr;
  ps2 = s1;
  ps2 = s2;
  ps2 = pi;
  ps2 = fp;
  ps2 = ps1;
  ps2 = ps2;

  return 0;
}
