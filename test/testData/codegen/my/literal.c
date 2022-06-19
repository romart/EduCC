char test1() {
  return ":"[0];
}

char test2(int i) {
  return "abc"[i];
}


int main(int y) {

  if (test1() != ':') return 1;
  if (test2(1) != 'b') return 2;

  return 0;

}

