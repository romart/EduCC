// Address-mode folding: what a GEP chain collapses into, and what it does not.
//
// The IR spells 'a[i]' as a shift, a GEP and a load, and a field access as
// another GEP on top. Every one of those is a term of a single x86 addressing
// mode, so the whole chain is one instruction - and deciding that is a
// property of the *chain*, which makes it exactly the kind of thing a dump
// pins down better than a runtime check can. What lands in .rodata or in a
// register is invisible from an exit code; '[%v0 + %v1*8 + 4]' is not.
//
// What each function is for:
//
//   scaled       the shift the frontend emitted for the element size becomes
//                the SIB scale, so the shift instruction disappears entirely.
//                One movsx survives, because the index is an int and the
//                addressing mode needs a full-width register.
//   field        a constant offset becomes the displacement. Nothing is left
//                of the GEP at all.
//   scaledField  both at once, and the two GEPs behind them: base, scaled
//                index and displacement in one operand.
//   localSlot    the same, anchored to a frame slot rather than a register.
//                This is the case MAK_FRAME exists for - the frame pointer is
//                the base, so the index still has somewhere to go.
//   shared       one address, two loads. The address folds into both, because
//                an addressing mode costs nothing to repeat, and the widening
//                its index needs is emitted *once* - one 'movsx.8/4' and two
//                address modes. It used to be emitted per fold, the widening
//                being a real instruction that nothing cached; section 6.23
//                gave the index a register of its own and both folds find it
//                already wide. This baseline is what says it stays that way.
//   escaped      the address is also passed to something that wants it in a
//                register, so the 'lea' stays *and* the load still folds. A
//                fold is per use, and only a value every one of whose uses
//                folded is left unselected.
//   indirect     the chain stops at a load: the pointer read out of memory is
//                a value, not a computation, so the second access starts a new
//                address from it.
//   twoIndexes   two variable subscripts of one array, which is where both
//                limits show up at once. A row of 'int m[4][4]' is 16 bytes,
//                and 16 is not a scale x86 can encode, so that shift stays a
//                shift and folds only as an index times one. The element
//                subscript then has nowhere left to go - one addressing mode
//                scales one register - so the row address keeps a 'lea' of its
//                own and the access folds against that.

struct Point {
  int x;
  int y;
};

int scaled(int *a, int i) {
  return a[i];
}

int field(struct Point *p) {
  return p->y;
}

int scaledField(struct Point *p, int i) {
  return p[i].y;
}

int localSlot(int i) {
  int a[8];

  a[i] = 3;
  return a[i];
}

int shared(struct Point *p, int i) {
  return p[i].y + p[i].y;
}

extern int take(int *p);

int escaped(struct Point *p, int i) {
  return take(&p[i].y) + p[i].y;
}

int indirect(struct Point **pp, int i) {
  return pp[i]->x;
}

int twoIndexes(int i, int j) {
  int m[4][4];

  m[i][j] = 5;
  return m[j][i];
}
