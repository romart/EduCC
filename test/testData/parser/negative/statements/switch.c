
int foo(int a);
int bar(int b);
int qux(int c);

int testSwitchIntNegative(int v) {
  switch (v) {
  case 1: return foo(v);
  case 2: return bar(v);
  case 3: return qux(v);
  case 4: return foo(v) + bar(v);
  case 4: return bar(v) + foo(v);
  case 5: return bar(v) + qux(v);
  case 5: return qux(v) + bar(v);
  case 6: return qux(v) + foo(v);
  case 6: return foo(v) + qux(v);
  default: return foo(v) + bar(v) + qux(v);
  default: return foo(v) + bar(v) + qux(v);
  }
}

enum E {
  E1 = -100,
  E2,
  E3,
  E4,
  E5,
  E6
};

int testSwitchEnumNegative(enum E v) {
  switch (v) {
  case E1: return foo(v);
  case E1: return foo(v);
  case E2: return bar(v);
  case E3: return qux(v);
  case E3: return qux(v);
  case E4: return foo(v) + bar(v);
  case E5: return bar(v) + qux (v);
  case E6: return qux(v) + foo(v);
  default: return foo(v) + bar(v) + qux(v);
  }
}

int testCaseOutOfSwitch(int v, int b) {
  case 0: return bar(b);
  switch (v) {
    case 1: return foo(b * b);
    case 2: return foo(b * b * b);
  }
  case 4: return qux(b);
}

int testDefaultOutOfSwitch(int v, int b) {
  switch (v) {
    case 1: return foo(b * b);
    case 2: return foo(b * b * b);
  }
  default: return qux(b);
}


int testSwitchArgFloat(float a, struct SX sa) {

  switch (a) {
    default : (int)a;
  }
}

struct SX { int a; };

int testSwitchArgStruct(struct SX sa) {

  switch (sa) {
    default : sa.a;
  }
}

int testSwitchArgPtr(int *pa) {

  switch (pa) {
    default : *pa;
  }
}
