// An aggregate whose single eightbyte is all floats, across a link between the
// two backends. SysV gives it class SSE - xmm0, not rdi/rax.
//
// This fixture is MUTED and expected to fail. The IR backend follows SysV
// here; the legacy backend passes every composite in the integer file, down a
// path that never reaches a register class at all, and is not going to be
// taught otherwise. Both are self-consistent, so each is right when linked
// against itself and both are wrong when linked against each other - which is
// exactly what a crossabi fixture exists to say out loud. The day the legacy
// side learns the rule, this starts passing and the runner flags the mute.
//
// aggregates.c is the fixture that still has to pass: every shape in it is one
// the two backends do agree about.
//
// The exit code is the number of the first check that failed. gcc returns 0.

struct F8 { float x, y; };
struct F4 { float x; };
struct D8 { double x; };

int failures = 0;

// The result is taken into a local before it is compared, and every expected
// value is a global. Written the obvious way - checkF(xTakeF8(s), 3.75f, 1) -
// half of these passed under a backend that gets the ABI wrong: the immediate
// 3.75f was loaded into xmm0 to be the argument of checkF, the call clobbered
// nothing, and the callee read its "argument" out of xmm0 and handed back the
// number the check was about to compare against.
float want1 = 3.75f, want2 = 7.5f, want4 = 4.0f, want5 = 0.5f, want6 = 6.25f;
double want3 = 9.25, want7 = 8.125;

static void checkF(float got, float want, int id) {
    if (got != want && failures == 0) failures = id;
}

// Defined in sse_aggregate.partner.c, i.e. by the other backend.
float xTakeF8(struct F8 s);
float xTakeF4(struct F4 s);
double xTakeD8(struct D8 s);
struct F8 xRetF8(float x, float y);
struct F4 xRetF4(float x);
struct D8 xRetD8(double x);

// Read by the partner, i.e. called across the link the other way.
float dTakeF8(struct F8 s) { return s.x + s.y; }
struct F8 dRetF8(float x, float y) { struct F8 r; r.x = x; r.y = y; return r; }

int main() {
    struct F8 f8; f8.x = 1.5f; f8.y = 2.25f;
    float got1 = xTakeF8(f8);
    checkF(got1, want1, 1);

    struct F4 f4; f4.x = 7.5f;
    float got2 = xTakeF4(f4);
    checkF(got2, want2, 2);

    struct D8 d8; d8.x = 9.25;
    double got3 = xTakeD8(d8);
    if (got3 != want3 && failures == 0) failures = 3;

    struct F8 r8 = xRetF8(4.0f, 0.5f);
    checkF(r8.x, want4, 4);
    checkF(r8.y, want5, 5);

    struct F4 r4 = xRetF4(6.25f);
    checkF(r4.x, want6, 6);

    struct D8 rd = xRetD8(8.125);
    if (rd.x != want7 && failures == 0) failures = 7;

    return failures;
}
