// The other half of sse_aggregate.c, compiled by the other backend. See that
// file, including why the pair is muted.

struct F8 { float x, y; };
struct F4 { float x; };
struct D8 { double x; };

// Defined in sse_aggregate.c, i.e. by the backend this one is linked against.
float dTakeF8(struct F8 s);
struct F8 dRetF8(float x, float y);

float xTakeF8(struct F8 s) { return s.x + s.y; }
float xTakeF4(struct F4 s) { return s.x; }
double xTakeD8(struct D8 s) { return s.x; }

struct F8 xRetF8(float x, float y) { struct F8 r; r.x = x; r.y = y; return r; }
struct F4 xRetF4(float x) { struct F4 r; r.x = x; return r; }
struct D8 xRetD8(double x) { struct D8 r; r.x = x; return r; }

// Back across the link the other way, so the disagreement is caught whichever
// side of it originates the call.
float xRoundTrip(float a, float b) {
    struct F8 s = dRetF8(a, b);
    return dTakeF8(s);
}
