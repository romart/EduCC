

typedef int t_i, *pt_i, **ppt_i;

int foo(int x, t_i xx, t_i **y, pt_i pz, pt_i *ppz, ppt_i a, ppt_i *pa, ppt_i **ppa) {
  yyin = fopen("test.txt", "r");
  yylex();
//  printf("%d %d %d %d\n", x, x * x, num_lines, num_chars);
  return 0;
}

int qux(int (******f)(double (*****)(struct S))) {}

int *(***(x)[20][30][40])(int *(*)(int, int **(**)(int, int**)), int);

int *(*xx[10])(int);

typedef int (*f_t)(int, int);

f_t fax(int a, int b) {
  int c;
  for (c = a; c < b; ++c) {
    printf("%d %d %d", a, b, c);
  }
}

int foo1(void) {

}

int foo2(void) {

}

int f() {
    aa: bar();

    bb = cc + dd ;
    if (ee) {
        printf("1");
    } else printf("2");
}

struct S;

struct SS {
    int a;
    int b, d;
    struct {
        double x;
        struct XXX ****xx;
    } sfx;
};

struct {

};
