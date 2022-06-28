


int a[-1];
int b[1.0];
int d;
float f;
int c[d];
int e[f];


int prot(int x[*], int y[static *], int z[static d]);

int neg(int [-2]);

int s1[*];

int def(int s2[*]) {
  int ls[*];

}


int init(int n) {
  int arr[n][n+1][n+2] = { 1, 2, 3, 4, 5, 6 };

  static int vla2[n];

  int s[static];

  int ss[static static];

  int cs[const static 10];

  int vlacs1[const n];
  int vlacs2[volatile n];
  int vlacs3[restrict n];
}

int ss[static];

int foo(int n, int vlacs[const volatile restrict static n]) {

}

int bar(int n, int m, int k, int a1[n], int a2[n][m], int a3[n][m][k]) {
  return 42;
}

struct SX {
  int x;
  int arr[10];
  int vla[d];
};
