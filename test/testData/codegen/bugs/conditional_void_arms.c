// '?:' with void arms.
//
// C99 6.5.15p3: if both operands have void type, so does the result. EduCC's
// computeTernaryType had no case for it - 'void' is not primitive there and
// not pointer-like either - so both arms fell through to the error type at the
// end of the function, and no diagnostic was reported to go with it. The
// legacy backend never looked; the IR backend met it as
// 'Unexpected error type in backend' and aborted. 26 of the 30 files of
// EduCC's own source that '-experimental' could not compile ended here.
//
// The value has no value, so what is checked is that both arms run and only
// the selected one does.
//
// The exit code is the number of the first check that failed. gcc returns 0.

int trueRuns = 0;
int falseRuns = 0;

static void g(void) { ++trueRuns; }
static void h(void) { ++falseRuns; }

static void pick(int c) { c ? g() : h(); }

// One arm a call and the other a cast to void, which is how the shape usually
// turns up in real code.
static void half(int c) { c ? g() : (void)0; }

// In a return statement of a void function: nothing goes back, but the
// expression still runs.
static void returned(int c) { return c ? g() : h(); }

int main(void) {
    pick(1);
    if (trueRuns != 1 || falseRuns != 0) return 1;

    pick(0);
    if (trueRuns != 1 || falseRuns != 1) return 2;

    half(1);
    if (trueRuns != 2 || falseRuns != 1) return 3;

    half(0);
    if (trueRuns != 2 || falseRuns != 1) return 4;

    returned(0);
    if (trueRuns != 2 || falseRuns != 2) return 5;

    // As a statement inside a loop, so the merge block is entered repeatedly.
    for (int i = 0; i < 4; ++i) i % 2 ? g() : h();
    if (trueRuns != 4 || falseRuns != 4) return 6;

    return 0;
}
