

union U {
  int a, b, c, d;
  int x;
  struct {
    int aa;
    int bb;
    int cc;
    int dd;
  };
};


union U2 {
  int a, b, c, d;
  int x;
  struct S {
    int aa;
    int bb;
    int cc;
    int dd;
  } s;
};

union U {
  int a, b, c, d;
  int x;
  struct SS {
    int aa;
    int bb;
    int cc;
    int dd;
  };
};
