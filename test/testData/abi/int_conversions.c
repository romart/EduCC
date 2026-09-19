// Every integer conversion, across a toolchain boundary.
//
// The riscv64 selector had one rule where there are two questions - the
// canonical form a value has in a register, and what C's conversion does to
// it. They agree for every type but 'unsigned int', which lives sign-extended
// like every other 32-bit value on RV64, so widening one to 64 bits sign-
// extended it and (long long)(unsigned)-1 came out -1.
//
// The corpus did not catch it, because the constant folder gets it right: only
// a value the optimizer cannot see through goes wrong. What caught it was a
// riscv64-hosted EduCC refusing to compile Zydis' own
// ((int)-1 >> 1) < (int)((unsigned)-1 >> 1) - which is to say R8, three
// milestones after the bug was written.
//
// So: all sixty-four conversions over twenty values, with the converters in
// the other translation unit and the values in an array, which is what stops
// any of it folding. gcc is the oracle - the harness builds this pair a third
// time with gcc on both sides and diffs what the three print - so nothing here
// is a constant somebody had to work out by hand.
//
// codegen/bugs/unsigned_widening.c is the same bug as a plain fixture, for the
// configurations that have no third party to ask.

int printf(const char *, ...);

typedef unsigned long long U;

signed char c_sc_sc(signed char);
unsigned char c_sc_uc(signed char);
short c_sc_sh(signed char);
unsigned short c_sc_ush(signed char);
int c_sc_i(signed char);
unsigned c_sc_u(signed char);
long long c_sc_ll(signed char);
unsigned long long c_sc_ull(signed char);
signed char c_uc_sc(unsigned char);
unsigned char c_uc_uc(unsigned char);
short c_uc_sh(unsigned char);
unsigned short c_uc_ush(unsigned char);
int c_uc_i(unsigned char);
unsigned c_uc_u(unsigned char);
long long c_uc_ll(unsigned char);
unsigned long long c_uc_ull(unsigned char);
signed char c_sh_sc(short);
unsigned char c_sh_uc(short);
short c_sh_sh(short);
unsigned short c_sh_ush(short);
int c_sh_i(short);
unsigned c_sh_u(short);
long long c_sh_ll(short);
unsigned long long c_sh_ull(short);
signed char c_ush_sc(unsigned short);
unsigned char c_ush_uc(unsigned short);
short c_ush_sh(unsigned short);
unsigned short c_ush_ush(unsigned short);
int c_ush_i(unsigned short);
unsigned c_ush_u(unsigned short);
long long c_ush_ll(unsigned short);
unsigned long long c_ush_ull(unsigned short);
signed char c_i_sc(int);
unsigned char c_i_uc(int);
short c_i_sh(int);
unsigned short c_i_ush(int);
int c_i_i(int);
unsigned c_i_u(int);
long long c_i_ll(int);
unsigned long long c_i_ull(int);
signed char c_u_sc(unsigned);
unsigned char c_u_uc(unsigned);
short c_u_sh(unsigned);
unsigned short c_u_ush(unsigned);
int c_u_i(unsigned);
unsigned c_u_u(unsigned);
long long c_u_ll(unsigned);
unsigned long long c_u_ull(unsigned);
signed char c_ll_sc(long long);
unsigned char c_ll_uc(long long);
short c_ll_sh(long long);
unsigned short c_ll_ush(long long);
int c_ll_i(long long);
unsigned c_ll_u(long long);
long long c_ll_ll(long long);
unsigned long long c_ll_ull(long long);
signed char c_ull_sc(unsigned long long);
unsigned char c_ull_uc(unsigned long long);
short c_ull_sh(unsigned long long);
unsigned short c_ull_ush(unsigned long long);
int c_ull_i(unsigned long long);
unsigned c_ull_u(unsigned long long);
long long c_ull_ll(unsigned long long);
unsigned long long c_ull_ull(unsigned long long);

// Through an array, so the value reaching each conversion is one no pass can
// see through. Both halves of the answer are printed: the number C says the
// conversion produces, and the bits it leaves behind in a register.
static U vals[] = {
  (U)(0),
  (U)(1),
  (U)(-1),
  (U)(127),
  (U)(128),
  (U)(255),
  (U)(-128),
  (U)(32767),
  (U)(65535),
  (U)(-32768),
  (U)(2147483647),
  (U)(2147483648u),
  (U)(4294967295u),
  (U)(-2147483648LL),
  (U)(9223372036854775807LL),
  (U)(-9223372036854775807LL-1),
  (U)(18446744073709551615ULL),
  (U)(305419896),
  (U)(-305419896),
  (U)(3000000000u),
};

int main(void) {
  unsigned k;

  for (k = 0; k < sizeof vals / sizeof *vals; ++k) {
    U v = vals[k];

    printf("%s->%s %lld %llu\n", "sc", "sc",
           (long long)c_sc_sc((signed char)v), (U)c_sc_sc((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "uc",
           (long long)c_sc_uc((signed char)v), (U)c_sc_uc((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "sh",
           (long long)c_sc_sh((signed char)v), (U)c_sc_sh((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "ush",
           (long long)c_sc_ush((signed char)v), (U)c_sc_ush((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "i",
           (long long)c_sc_i((signed char)v), (U)c_sc_i((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "u",
           (long long)c_sc_u((signed char)v), (U)c_sc_u((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "ll",
           (long long)c_sc_ll((signed char)v), (U)c_sc_ll((signed char)v));
    printf("%s->%s %lld %llu\n", "sc", "ull",
           (long long)c_sc_ull((signed char)v), (U)c_sc_ull((signed char)v));
    printf("%s->%s %lld %llu\n", "uc", "sc",
           (long long)c_uc_sc((unsigned char)v), (U)c_uc_sc((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "uc",
           (long long)c_uc_uc((unsigned char)v), (U)c_uc_uc((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "sh",
           (long long)c_uc_sh((unsigned char)v), (U)c_uc_sh((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "ush",
           (long long)c_uc_ush((unsigned char)v), (U)c_uc_ush((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "i",
           (long long)c_uc_i((unsigned char)v), (U)c_uc_i((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "u",
           (long long)c_uc_u((unsigned char)v), (U)c_uc_u((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "ll",
           (long long)c_uc_ll((unsigned char)v), (U)c_uc_ll((unsigned char)v));
    printf("%s->%s %lld %llu\n", "uc", "ull",
           (long long)c_uc_ull((unsigned char)v), (U)c_uc_ull((unsigned char)v));
    printf("%s->%s %lld %llu\n", "sh", "sc",
           (long long)c_sh_sc((short)v), (U)c_sh_sc((short)v));
    printf("%s->%s %lld %llu\n", "sh", "uc",
           (long long)c_sh_uc((short)v), (U)c_sh_uc((short)v));
    printf("%s->%s %lld %llu\n", "sh", "sh",
           (long long)c_sh_sh((short)v), (U)c_sh_sh((short)v));
    printf("%s->%s %lld %llu\n", "sh", "ush",
           (long long)c_sh_ush((short)v), (U)c_sh_ush((short)v));
    printf("%s->%s %lld %llu\n", "sh", "i",
           (long long)c_sh_i((short)v), (U)c_sh_i((short)v));
    printf("%s->%s %lld %llu\n", "sh", "u",
           (long long)c_sh_u((short)v), (U)c_sh_u((short)v));
    printf("%s->%s %lld %llu\n", "sh", "ll",
           (long long)c_sh_ll((short)v), (U)c_sh_ll((short)v));
    printf("%s->%s %lld %llu\n", "sh", "ull",
           (long long)c_sh_ull((short)v), (U)c_sh_ull((short)v));
    printf("%s->%s %lld %llu\n", "ush", "sc",
           (long long)c_ush_sc((unsigned short)v), (U)c_ush_sc((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "uc",
           (long long)c_ush_uc((unsigned short)v), (U)c_ush_uc((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "sh",
           (long long)c_ush_sh((unsigned short)v), (U)c_ush_sh((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "ush",
           (long long)c_ush_ush((unsigned short)v), (U)c_ush_ush((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "i",
           (long long)c_ush_i((unsigned short)v), (U)c_ush_i((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "u",
           (long long)c_ush_u((unsigned short)v), (U)c_ush_u((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "ll",
           (long long)c_ush_ll((unsigned short)v), (U)c_ush_ll((unsigned short)v));
    printf("%s->%s %lld %llu\n", "ush", "ull",
           (long long)c_ush_ull((unsigned short)v), (U)c_ush_ull((unsigned short)v));
    printf("%s->%s %lld %llu\n", "i", "sc",
           (long long)c_i_sc((int)v), (U)c_i_sc((int)v));
    printf("%s->%s %lld %llu\n", "i", "uc",
           (long long)c_i_uc((int)v), (U)c_i_uc((int)v));
    printf("%s->%s %lld %llu\n", "i", "sh",
           (long long)c_i_sh((int)v), (U)c_i_sh((int)v));
    printf("%s->%s %lld %llu\n", "i", "ush",
           (long long)c_i_ush((int)v), (U)c_i_ush((int)v));
    printf("%s->%s %lld %llu\n", "i", "i",
           (long long)c_i_i((int)v), (U)c_i_i((int)v));
    printf("%s->%s %lld %llu\n", "i", "u",
           (long long)c_i_u((int)v), (U)c_i_u((int)v));
    printf("%s->%s %lld %llu\n", "i", "ll",
           (long long)c_i_ll((int)v), (U)c_i_ll((int)v));
    printf("%s->%s %lld %llu\n", "i", "ull",
           (long long)c_i_ull((int)v), (U)c_i_ull((int)v));
    printf("%s->%s %lld %llu\n", "u", "sc",
           (long long)c_u_sc((unsigned)v), (U)c_u_sc((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "uc",
           (long long)c_u_uc((unsigned)v), (U)c_u_uc((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "sh",
           (long long)c_u_sh((unsigned)v), (U)c_u_sh((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "ush",
           (long long)c_u_ush((unsigned)v), (U)c_u_ush((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "i",
           (long long)c_u_i((unsigned)v), (U)c_u_i((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "u",
           (long long)c_u_u((unsigned)v), (U)c_u_u((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "ll",
           (long long)c_u_ll((unsigned)v), (U)c_u_ll((unsigned)v));
    printf("%s->%s %lld %llu\n", "u", "ull",
           (long long)c_u_ull((unsigned)v), (U)c_u_ull((unsigned)v));
    printf("%s->%s %lld %llu\n", "ll", "sc",
           (long long)c_ll_sc((long long)v), (U)c_ll_sc((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "uc",
           (long long)c_ll_uc((long long)v), (U)c_ll_uc((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "sh",
           (long long)c_ll_sh((long long)v), (U)c_ll_sh((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "ush",
           (long long)c_ll_ush((long long)v), (U)c_ll_ush((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "i",
           (long long)c_ll_i((long long)v), (U)c_ll_i((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "u",
           (long long)c_ll_u((long long)v), (U)c_ll_u((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "ll",
           (long long)c_ll_ll((long long)v), (U)c_ll_ll((long long)v));
    printf("%s->%s %lld %llu\n", "ll", "ull",
           (long long)c_ll_ull((long long)v), (U)c_ll_ull((long long)v));
    printf("%s->%s %lld %llu\n", "ull", "sc",
           (long long)c_ull_sc((unsigned long long)v), (U)c_ull_sc((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "uc",
           (long long)c_ull_uc((unsigned long long)v), (U)c_ull_uc((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "sh",
           (long long)c_ull_sh((unsigned long long)v), (U)c_ull_sh((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "ush",
           (long long)c_ull_ush((unsigned long long)v), (U)c_ull_ush((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "i",
           (long long)c_ull_i((unsigned long long)v), (U)c_ull_i((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "u",
           (long long)c_ull_u((unsigned long long)v), (U)c_ull_u((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "ll",
           (long long)c_ull_ll((unsigned long long)v), (U)c_ull_ll((unsigned long long)v));
    printf("%s->%s %lld %llu\n", "ull", "ull",
           (long long)c_ull_ull((unsigned long long)v), (U)c_ull_ull((unsigned long long)v));
  }

  return 0;
}
