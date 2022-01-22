typedef int t_i, *pt_i, **ppt_i;

void yylex();
int fopen(const char *, const char *);

int foo(int x, t_i xx, t_i **y, pt_i pz, pt_i *ppz, ppt_i a, ppt_i *pa, ppt_i **ppa) {
  int yyin = fopen("test.txt", "r");
  yylex();
  int t_t = x + yyin;
  return 0;
}
