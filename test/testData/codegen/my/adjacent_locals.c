// Reading and writing one local through a pointer to the one declared next to
// it. Split out of my/pointers.c, whose other checks are ordinary C.
//
// This is undefined behaviour, not a property of the language: '&x3 + 1' is one
// past the end of a complete object, which C99 6.5.6p8 lets you form and not
// dereference, and nothing at all says the object that follows it in the source
// follows it in the frame. **gcc fails this file exactly where EduCC's IR
// backend does** (both return 3), so the legacy backend is the outlier here
// rather than the one that is right.
//
// What it does pin is a real property of that backend: it emits every declared
// local and lays them out in declaration order, so the adjacency happens to
// hold. The IR backend agrees about the order and disagrees about the
// existence - 'int y3 = 5;' whose address is never taken and whose value is
// never read is a dead store to a dead alloca, and dce removes both, so the
// read lands on whatever is there. Keeping a provably dead local alive so that
// an out-of-bounds read finds it would be a deoptimization in the service of
// undefined behaviour, which is why this is marked as a legacy-only fixture
// rather than treated as a bug in the new backend. See section 10 of
// docs/ir-codegen-design.md.
//
// The return codes are the ones these checks had in my/pointers.c, so a failure
// here means the same thing it used to mean there.

int main() {
  int x3=3; int y3=5;
  if (5 != *(&x3+1)) return 3;
  int x4=3; int y4=5;
  if (3 != *(&y4-1)) return 4;
  int x5=3; int y5=5;
  if (5 != *(&x5-(-1))) return 5;
  int x7=3; int y7=5; *(&x7+1)=7;
  if (7 != y7) return 7;
  int x8=3; int y8=5; *(&y8-2+1)=7;
  if (7 != x8) return 8;

  return 0;
}
