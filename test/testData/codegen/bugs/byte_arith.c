
struct S {
    unsigned char v;
};


int tet(struct S *s) {
  ++s->v;
}

int main() {
  struct S s = { 0 };

  tet(&s);
  if (s.v != 1) return s.v | 0xE;

  tet(&s);
  tet(&s);

  if (s.v != 3) return s.v | 0xF;

  return 0;
}
