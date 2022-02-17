
struct N {
  int x;
};

struct S {
  int a[10];
  struct N n;
};


void testIncDec(int *ptr, struct S **pptr, int x) {
  ptr[x]++;
  --*ptr;
  --*(ptr + 15);

  pptr[x]++;
  --*pptr;
  --*(pptr + 10);

  pptr[x]->n.x++;

  struct Incpt **incpt;
  ++incpt;
}
