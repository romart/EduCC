// A void expression is not a value, and every one of these tried to use one as
// though it were.
//
// The frontend used to let them all through. What happened next depended on
// where the void landed: an argument became a call the IR backend could not
// select and quietly handed to the legacy one, which then died in
// generateCall; an assignment became a store of a value with no register
// class, which asserted; an initializer of a promoted local vanished into SSA
// and compiled to something. Three different endings for one mistake, none of
// them a diagnostic.
//
// Catching it here is what let the backend's two "no register class" refusals
// become assertions - see selectMemoryStore and section 6.21 of
// docs/ir-codegen-design.md.

void nothing(void);
void sink(int x);
void variadic(int n, ...);

int g;

void assignToLocal(void) {
  int x;
  x = nothing();
}

void assignToGlobal(void) {
  g = nothing();
}

void initialize(void) {
  int x = nothing();
}

void passAsArgument(void) {
  sink(nothing());
}

// A trailing argument has no parameter type to be checked against, so this is
// caught by the default-promotion walk rather than by the argument one.
void passAsTrailingArgument(void) {
  variadic(1, nothing());
}

int returnIt(void) {
  return nothing();
}

// Assigning void to void is not this mistake: neither side is a value, and
// C says an expression statement may have void type.
void statementIsFine(void) {
  nothing();
}
