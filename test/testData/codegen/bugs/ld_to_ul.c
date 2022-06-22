long double ldf(long double ld) {
    return ld;
}

unsigned long t(long double ld) {
  return ldf(ld);
}

int main() {

  unsigned long u = t(99.0);

  if (u != 99) return u | 0xF;

  return 0;
}
