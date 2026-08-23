// Stack allocations whose size is only known at run time - a VLA, or a call to
// alloca() - run for their answers.
//
// Every other frame object has a fixed displacement from the frame pointer, and
// that is what selection had until step 15: a dynamically sized allocation was
// refused with "its address is not a fixed displacement" and the whole function
// went to the legacy backend. It is now three instructions and a copy - round
// the size up to 16, subtract it from rsp, take rsp as the address - and the
// two things worth checking about that are what this fixture is for.
//
// The first is that the *address* is right. A block has to start where the last
// one ended, stay clear of the fixed frame above it, and survive a call putting
// arguments below it. The second is that a VLA is two objects and not one: the
// block, and the word holding its address that a read of the name loads. That
// word had never been written - nothing noticed while the refusal above kept
// any of this from running - so a VLA read back its own first eight bytes as a
// pointer. Its rows are the other half of the same question: 'm[i]' of a
// multidimensional VLA is an address computed from the size expression, with no
// pointer stored anywhere to load, and loading one anyway is a wild dereference
// rather than a wrong number.
//
// Nothing here allocates in a loop. C99 6.2.4p6 ends a VLA's lifetime at the
// end of its block, but neither backend gives the stack back before the
// function returns, so a VLA in a loop grows the frame every iteration - a real
// gap, recorded in docs/ir-codegen-design.md section 10, and not something a
// test can assert about without depending on how much stack it takes to fail.
//
// The exit code is the number of the first check that failed. gcc returns 0.

#include <alloca.h>

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

// -------- alloca(): consecutive blocks, and alignment --------

// Six named integers and then three on the stack, so the call reaches past the
// argument registers with the stack pointer wherever the allocations left it.
static long nine(long a, long b, long c, long d, long e, long f, long g, long h,
                 long i) {
    return a + b * 2 + c * 4 + d * 8 + e * 16 + f * 32 + g * 64 + h * 128 + i * 256;
}

static void allocaChecks(void) {
    char *p1 = alloca(16);
    char *p2 = alloca(16);

    // Downwards and exactly 16 apart: the block is carved out of the stack, so
    // the second starts where the first ends.
    check(p1 - p2 == 16, 1);

    // Three bytes still costs a whole 16, because rsp has to stay where SysV
    // wants it. Getting this wrong leaves the stack misaligned at the next
    // call rather than returning a wrong pointer.
    char *p3 = alloca(3);
    check(p2 - p3 == 16, 2);
    check(((unsigned long)p3 & 15) == 0, 3);

    for (int i = 0; i < 16; ++i) p1[i] = (char)(i + 1);
    for (int i = 0; i < 16; ++i) p2[i] = (char)(i + 101);
    p3[0] = 7;
    p3[2] = 9;

    // The stores above must not have reached each other.
    check(p1[0] == 1 && p1[15] == 16, 4);
    check(p2[0] == 101 && p2[15] == 116, 5);
    check(p3[0] == 7 && p3[2] == 9, 6);

    // A call with stack arguments after the allocations: the pushes go below
    // the blocks and the 'add rsp' after has to put back exactly what it took.
    check(nine(1, 1, 1, 1, 1, 1, 1, 1, 1) == 511, 7);
    check(p1[0] == 1 && p2[15] == 116 && p3[2] == 9, 8);

    // And one whose argument is itself an allocation, so the block is carved
    // out between the call's arguments being evaluated and being pushed.
    char *p4 = alloca(16);
    check(p3 - p4 == 16, 9);
}

// -------- a VLA, read through its own name --------

static int vlaSum(int n) {
    int v[n];

    for (int i = 0; i < n; ++i) v[i] = i * i;

    int sum = 0;
    for (int i = 0; i < n; ++i) sum += v[i];
    return sum;
}

// sizeof is the size expression, evaluated here rather than folded.
static unsigned long vlaSize(int n) {
    int v[n];
    return sizeof(v);
}

// The name decays to a pointer at a call, which is the same address the
// carve produced and not the word it was stored in.
static int firstOf(const int *p) {
    return p[0];
}

static void vlaChecks(void) {
    check(vlaSum(5) == 0 + 1 + 4 + 9 + 16, 10);
    check(vlaSize(5) == 20, 11);
    check(vlaSize(1) == 4, 12);

    int n = 6;
    int v[n];
    for (int i = 0; i < n; ++i) v[i] = i + 1;

    check(v[0] == 1 && v[5] == 6, 13);
    check(firstOf(v) == 1, 14);

    // A fixed local declared alongside it, which the carve must leave alone.
    int fixed[2];
    fixed[0] = v[5];
    fixed[1] = n;
    check(fixed[0] == 6 && fixed[1] == 6, 15);

    // Two in one function: the second starts below the first, and writing
    // through either must not disturb the other.
    int w[n * 2];
    for (int i = 0; i < n * 2; ++i) w[i] = 100 + i;
    check(v[0] == 1 && v[5] == 6, 16);
    check(w[0] == 100 && w[11] == 111, 17);
}

// -------- a multidimensional VLA, where a row is an address --------

// Both dimensions computed, so 'm[i]' has to scale by the run-time row size.
static int matrix(int rows, int cols) {
    int m[rows][cols];

    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < cols; ++j)
            m[i][j] = i * 100 + j;

    int sum = 0;
    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < cols; ++j)
            sum += m[i][j];

    // The last element, reached through a row that is a long way from the base
    // - this is what a spurious load of a row address gets wrong.
    return sum + m[rows - 1][cols - 1];
}

// The same memory walked flat, which has to agree with the indexing above
// about how wide a row is.
static int matrixFlat(int rows, int cols) {
    int m[rows][cols];
    int *p = (int *)m;

    for (int i = 0; i < rows * cols; ++i) p[i] = i;

    return m[rows - 1][cols - 1];
}

// An inner dimension that is a constant, so only the outer one is dynamic -
// the row size is known and the scaling is an ordinary shift.
static int fixedRows(int rows) {
    int m[rows][3];

    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < 3; ++j)
            m[i][j] = i * 10 + j;

    return m[rows - 1][2] + m[0][0];
}

static void matrixChecks(void) {
    // rows 0..3, cols 0..4: sum of i*100 is 4*(0+100+200+300)... spelled out
    // rather than reduced, because the point is the addressing and not the sum.
    int expected = 0;
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 5; ++j)
            expected += i * 100 + j;
    expected += 3 * 100 + 4;

    check(matrix(4, 5) == expected, 18);
    check(matrixFlat(4, 5) == 4 * 5 - 1, 19);
    check(matrixFlat(11, 16) == 11 * 16 - 1, 20);
    check(fixedRows(4) == 32, 21);
}

int main(void) {
    allocaChecks();
    vlaChecks();
    matrixChecks();

    return failures;
}
