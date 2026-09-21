// LP64D's two aggregate rules that SysV does not have.
//
// The first is flattening. LP64D does not ask which *eightbyte* a member lands
// in; it asks what the struct's fields are, recursively, and a struct of at
// most two floating-point reals travels in floating-point registers one field
// each. SysV asks the eightbyte question and so puts both halves of
// struct { float a, b; } in one register, which is a different place. The two
// agree often enough to hide it - struct { double a, b; } is fa0,fa1 either
// way - and disagree whenever two fields share an eightbyte, or three fields
// are floats, or an array has to be looked through.
//
// The second is the split. When exactly one integer argument register is left,
// LP64D puts the low half of a two-word argument in it and the high half on
// the stack. SysV puts the whole thing on the stack, which is where EduCC's
// all-or-nothing test came from. It needs seven integer arguments ahead of it
// to arise, which is why nothing had hit it.
//
// Both are invisible to a fixture EduCC compiles entirely: the two halves of a
// call agree about the wrong place. So the functions live in the partner file,
// and the harness builds this pair a third time with gcc on both sides.

int printf(const char *, ...);

struct FF { float a, b; };
struct FI { float a; int b; };
struct IF { int a; float b; };
struct DD { double a, b; };
struct DI { double a; int b; };
struct ID { int a; double b; };
struct LD_ { long a; double b; };
struct FFF { float a, b, c; };
struct FA { float a[2]; };
struct Nest { struct { double a; } s; double b; };
struct D1 { double a; };
struct F1 { float a; };
struct LL { long a, b; };
struct Ch { char a; double b; };
struct BF { double a; int b:3; };
struct FB { float a; int b:3; };
struct U { union { float a; int b; } u; };
struct ZA { double a; int z[0]; };
struct Big { long a, b, c, d; };

float p_ff(struct FF);
float p_fi(struct FI);
float p_if(struct IF);
double p_dd(struct DD);
double p_di(struct DI);
double p_id(struct ID);
double p_ld(struct LD_);
float p_fff(struct FFF);
float p_fa(struct FA);
double p_nest(struct Nest);
double p_d1(struct D1);
float p_f1(struct F1);
double p_ch(struct Ch);
double p_bf(struct BF);
float p_fb(struct FB);
float p_u(struct U);
double p_za(struct ZA);

struct FF p_mkff(float, float);
struct FI p_mkfi(float, int);
struct DD p_mkdd(double, double);
struct FFF p_mkfff(float, float, float);
struct FA p_mkfa(float, float);
struct Nest p_mknest(double, double);

// Eight doubles ahead of it: no FP argument register is left, and LP64D then
// passes the struct by the integer convention rather than half of it in a
// register that does not exist.
double p_ddLate(double, double, double, double, double, double, double, double,
                struct DD);
float p_ffLate(double, double, double, double, double, double, double, double,
               struct FF);

// Seven integers ahead of it: exactly one integer argument register is left,
// which is the split.
long p_llSplit(long, long, long, long, long, long, long, struct LL);
double p_ddSplit(long, long, long, long, long, long, long, struct DD);
long double p_ldSplit(long, long, long, long, long, long, long, long double);
long p_llSplit6(long, long, long, long, long, long, struct LL);

// Nine reals with eight floating-point argument registers: LP64D hands the
// ninth over in an *integer* one carrying its bits, where SysV would put it on
// the stack. Nothing else in the tree passes a real that far out.
double p_ninth(double, double, double, double, double, double, double, double,
               double, float);

// Eight integers ahead of an aggregate too wide for two registers: the pointer
// to the caller's copy is itself on the stack, and reading it is a frame
// reference and not arithmetic on the stack pointer.
long p_bigLate(long, long, long, long, long, long, long, long, struct Big);

// The split again, on the variadic side, where it is two rules rather than
// one and they disagree. Six unnamed words ahead of the argument leave
// exactly one integer register:
//
//   an aggregate splits - low word in a7, high word on the stack;
//   a long double does not - it takes an *even* register pair, so the odd one
//   left is skipped and both halves go on the stack instead.
//
// The callee reads both out of the flat save area and cannot tell the
// difference, which is the whole point of the flat va_list - so only a caller
// EduCC compiled against a callee it did not says which rule was applied.
long p_vSplit(int, ...);
long double p_vLdSplit(int, ...);

int main(void) {
  struct FF ff = {1.5f, 2.25f};
  struct FI fi = {3.5f, 7};
  struct IF if_ = {9, 4.5f};
  struct DD dd = {1.125, 2.375};
  struct DI di = {5.5, 11};
  struct ID id = {13, 6.75};
  struct LD_ ld = {17L, 8.125};
  struct FFF fff = {1.0f, 2.0f, 3.0f};
  struct FA fa = {{10.5f, 20.25f}};
  struct Nest nest = {{100.5}, 200.25};
  struct D1 d1 = {42.5};
  struct F1 f1 = {43.5f};
  struct LL ll = {0x1122334455667788L, 0x99aabbccddeeff00L};
  struct Ch ch = {'z', 3.0625};
  struct BF bf = {21.5, 3};
  struct FB fb = {22.5f, 2};
  struct U u;
  struct ZA za = {23.5};
  struct Big big = {1, 2, 3, 4};

  u.u.a = 24.5f;

  printf("ff %g\n", (double)p_ff(ff));
  printf("fi %g\n", (double)p_fi(fi));
  printf("if %g\n", (double)p_if(if_));
  printf("dd %g\n", p_dd(dd));
  printf("di %g\n", p_di(di));
  printf("id %g\n", p_id(id));
  printf("ld %g\n", p_ld(ld));
  printf("fff %g\n", (double)p_fff(fff));
  printf("fa %g\n", (double)p_fa(fa));
  printf("nest %g\n", p_nest(nest));
  printf("d1 %g\n", p_d1(d1));
  printf("f1 %g\n", (double)p_f1(f1));
  printf("ch %g\n", p_ch(ch));
  printf("bf %g\n", p_bf(bf));
  printf("fb %g\n", (double)p_fb(fb));
  printf("u %g\n", (double)p_u(u));
  printf("za %g\n", p_za(za));

  {
    struct FF r = p_mkff(5.5f, 6.5f);
    printf("mkff %g %g\n", (double)r.a, (double)r.b);
  }
  {
    struct FI r = p_mkfi(7.5f, 8);
    printf("mkfi %g %d\n", (double)r.a, r.b);
  }
  {
    struct DD r = p_mkdd(9.5, 10.5);
    printf("mkdd %g %g\n", r.a, r.b);
  }
  {
    struct FFF r = p_mkfff(11.5f, 12.5f, 13.5f);
    printf("mkfff %g %g %g\n", (double)r.a, (double)r.b, (double)r.c);
  }
  {
    struct FA r = p_mkfa(14.5f, 15.5f);
    printf("mkfa %g %g\n", (double)r.a[0], (double)r.a[1]);
  }
  {
    struct Nest r = p_mknest(16.5, 17.5);
    printf("mknest %g %g\n", r.s.a, r.b);
  }

  printf("ddLate %g\n",
         p_ddLate(1, 2, 3, 4, 5, 6, 7, 8, dd));
  printf("ffLate %g\n",
         (double)p_ffLate(1, 2, 3, 4, 5, 6, 7, 8, ff));

  printf("llSplit %lx\n", p_llSplit(1, 2, 3, 4, 5, 6, 7, ll));
  printf("ddSplit %g\n", p_ddSplit(1, 2, 3, 4, 5, 6, 7, dd));
  printf("ldSplit %La\n", p_ldSplit(1, 2, 3, 4, 5, 6, 7, 1.5L));
  printf("llSplit6 %lx\n", p_llSplit6(1, 2, 3, 4, 5, 6, ll));
  printf("ninth %g\n", p_ninth(1, 2, 3, 4, 5, 6, 7, 8, 9, 10.5f));
  printf("bigLate %ld\n", p_bigLate(1, 2, 3, 4, 5, 6, 7, 8, big));
  printf("vSplit %lx\n", p_vSplit(0, 1L, 2L, 3L, 4L, 5L, 6L, ll, 7L));
  printf("vLdSplit %La\n", p_vLdSplit(0, 1L, 2L, 3L, 4L, 5L, 6L, 1.5L, 7L));

  return 0;
}
