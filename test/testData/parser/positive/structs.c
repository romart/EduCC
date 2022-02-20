
int foo2(void) {
  struct SL { int a; } a = { 0 }, b = { 0 }, *c = (void*)0, d;
}

struct S;

struct SS {
    int a;
    int b, d;
    struct {
        double x;
        struct XXX ****xx;
    } ss1, *ss2, **ss3;
};

struct {

};

struct SN {
  int a;
  int b;

  struct {
    int d;
    int e;

    struct {
      int ff;
      int gg;

      struct SSN {
        int x;
        int y;
      } ss, *pss, **ppss;

    };

  };

  struct N {
    int f;
    int g;
  };

  struct NN {
    int h;
    int i;
  } nn;

  struct {
    int j;
    int k;
  } anon;
};
