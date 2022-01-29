
struct S { int b; };

int testInt(struct S s) {
  return s;
}

float testFloat(int *ptr) {
  return ptr;
}


int *testPtr(float f) {
  return f;
}

struct S testStruct(int a) {
  return a;
}
