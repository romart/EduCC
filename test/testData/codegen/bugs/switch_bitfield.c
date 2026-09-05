// The controlling expression of a switch is promoted, so a bit field is a
// legal one. It was refused as 'not of integer type' - SQLite switches on a
// four-bit field to dispatch on a column's declared type.

struct col {
  unsigned kind : 4;
  unsigned flags : 4;
  signed   sk : 3;
};

static int classify(struct col *c) {
  switch (c->kind) {
  case 0: return 100;
  case 1: return 101;
  case 7: return 107;
  default: return 999;
  }
}

static int signedField(struct col *c) {
  switch (c->sk) {
  case -1: return 10;
  case 2: return 20;
  default: return 30;
  }
}

int main(void) {
  struct col c;

  c.kind = 7; c.flags = 0; c.sk = -1;
  if (classify(&c) != 107) return 1;
  if (signedField(&c) != 10) return 2;

  c.kind = 1; c.sk = 2;
  if (classify(&c) != 101) return 3;
  if (signedField(&c) != 20) return 4;

  c.kind = 5;
  if (classify(&c) != 999) return 5;
  return 0;
}
