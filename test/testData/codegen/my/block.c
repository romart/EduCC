

int main() {
  if (3 != ({ int i=6; i/=2; })) return 3;

  int x = { ({ 10; }) };

  return x - 10;
}
