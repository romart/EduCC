
int (*(qux(int (******f)(double (*****)(struct S*))))) {}

int *fxx(int p);

int *(***(x)[20][30][40])(int *(*)(int, int **(**)(int, int**)), int);

int *(*xx[10])(int);

typedef int (*f_t)(int, int);

void foo(const char *, ...);

f_t fax(int a, int b) {
  int c = 15;
  for (c = a; c < b; ++c) {
    foo("%d %d %d", a, b, c);
  }
}
