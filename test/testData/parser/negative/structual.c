

struct S1;
enum S1;
union S1;


struct S2 {
  struct S1 s1;
};

union U2 {
  int a;
  struct S1 s1;
};

struct S3 {
  struct S1 *s1;
  enum S1 *e1;
  union S1 *u1;
};


struct X {
  int a;
  int b;
  struct {};
  int c;
  union {
    struct {};
    struct {
      int a;
      int b;
      int d;
      int e;
    };
    struct {
      struct {};
    };
    int c;
    int d;
  };
  struct {};
  int e;
};
