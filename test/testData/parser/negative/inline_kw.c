

inline int foo() {
  return 42;
}

static inline float bar() {
  return 4.2f;
}

inline int x;

int qux() {
  inline int y;
}

int der(inline int z) {
  return 9;
}

inline int zed();

inline int inline dif() {
  return 51;
}

float gyr(int a) {
  return (inline float)a;
}

