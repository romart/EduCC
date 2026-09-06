// 'extern const struct S x;' reserves no storage, so the type only has to be
// complete where the object is defined. It was rejected as a variable of
// incomplete type, which is what stopped PCRE2's pcre2_internal.h.

struct ctx;
extern const struct ctx defaultContext;
extern struct ctx *const contextSlot;

struct ctx { int a; int b; };

const struct ctx defaultContext = { 7, 11 };
struct ctx *const contextSlot = 0;

int main(void) {
  if (defaultContext.a != 7) return 1;
  if (defaultContext.b != 11) return 2;
  if (contextSlot != 0) return 3;
  if (sizeof(defaultContext) != 2 * sizeof(int)) return 4;
  return 0;
}
