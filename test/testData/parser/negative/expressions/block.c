

int main() {
  if (3 != ({ int i=6; i/2; i/=2; })) return 3;

  if (4 != ({ return 2; })) return 10;

  return 0;
}
