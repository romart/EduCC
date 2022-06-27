


int testF(float f) {
  return f ? 1 : 0;
}

int testLD(long double ld) {
  return ld ? 1 : 0;
}

int main() {
  if (testF(0.f) != 0) return 1;
  if (testF(1.f) != 1) return 2;

  if (testLD(0.0L) != 0) return 3;
  if (testLD(1.L) != 1) return 4;

  return 0;
}
