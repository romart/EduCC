// The usual arithmetic conversions promote a bit field, so it is a legal arm
// of a conditional. Nothing in computeTernaryType counted one as primitive, so
// the type came out as the internal error type - reported to nobody, and met
// by the backend as an abort. SQLite: 'return pStmt ? pVdbe->readOnly : 1;'

typedef unsigned bft;

struct vdbe {
  bft readOnly : 1;
  bft explain : 2;
  signed sk : 4;
};

static int isReadOnly(struct vdbe *p) { return p ? p->readOnly : 1; }
static int explainOf(struct vdbe *p) { return p ? p->explain : 0; }
static long widened(struct vdbe *p) { return p ? p->sk : 100L; }

int main(void) {
  struct vdbe v;

  v.readOnly = 1;
  v.explain = 2;
  v.sk = -3;

  if (isReadOnly(&v) != 1) return 1;
  if (isReadOnly(0) != 1) return 2;
  if (explainOf(&v) != 2) return 3;
  if (explainOf(0) != 0) return 4;
  if (widened(&v) != -3L) return 5;
  if (widened(0) != 100L) return 6;

  v.readOnly = 0;
  if (isReadOnly(&v) != 0) return 7;
  return 0;
}
