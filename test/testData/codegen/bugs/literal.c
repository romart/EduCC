char test1() {
  return ":"[0];
}

char test2(int i) {
  return "abc"[i];
}

char test3(int i) {
  return "defghijxyz"[i - 2];
}

int main(int y) {

  if (test1() != ':') return 1;
  if (test2(1) != 'b') return 2;
  if (test3(3) != 'e') return 3;

  return 0;

}

