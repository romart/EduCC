

struct X {
  int a, b;

};

struct X s = { 42, 24 };


int test(struct X *p, struct X **pp) {
  *pp = &s;

  return p->a + p->b;
}

int main() {
  struct X l = { 18, 33 };

  struct X *p = &l;

  int r = test(p, &p);

  if (r != 51) return 1;

  if (p->a != 42) return 2;
  if (p->b != 24) return 3;

  return 0;
}
