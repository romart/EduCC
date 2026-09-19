// The other half of int_conversions.c: one function per conversion, so that
// every one of them is a real call whose argument and result cross the ABI as
// well as the conversion itself.

signed char c_sc_sc(signed char x) { return (signed char)x; }
unsigned char c_sc_uc(signed char x) { return (unsigned char)x; }
short c_sc_sh(signed char x) { return (short)x; }
unsigned short c_sc_ush(signed char x) { return (unsigned short)x; }
int c_sc_i(signed char x) { return (int)x; }
unsigned c_sc_u(signed char x) { return (unsigned)x; }
long long c_sc_ll(signed char x) { return (long long)x; }
unsigned long long c_sc_ull(signed char x) { return (unsigned long long)x; }
signed char c_uc_sc(unsigned char x) { return (signed char)x; }
unsigned char c_uc_uc(unsigned char x) { return (unsigned char)x; }
short c_uc_sh(unsigned char x) { return (short)x; }
unsigned short c_uc_ush(unsigned char x) { return (unsigned short)x; }
int c_uc_i(unsigned char x) { return (int)x; }
unsigned c_uc_u(unsigned char x) { return (unsigned)x; }
long long c_uc_ll(unsigned char x) { return (long long)x; }
unsigned long long c_uc_ull(unsigned char x) { return (unsigned long long)x; }
signed char c_sh_sc(short x) { return (signed char)x; }
unsigned char c_sh_uc(short x) { return (unsigned char)x; }
short c_sh_sh(short x) { return (short)x; }
unsigned short c_sh_ush(short x) { return (unsigned short)x; }
int c_sh_i(short x) { return (int)x; }
unsigned c_sh_u(short x) { return (unsigned)x; }
long long c_sh_ll(short x) { return (long long)x; }
unsigned long long c_sh_ull(short x) { return (unsigned long long)x; }
signed char c_ush_sc(unsigned short x) { return (signed char)x; }
unsigned char c_ush_uc(unsigned short x) { return (unsigned char)x; }
short c_ush_sh(unsigned short x) { return (short)x; }
unsigned short c_ush_ush(unsigned short x) { return (unsigned short)x; }
int c_ush_i(unsigned short x) { return (int)x; }
unsigned c_ush_u(unsigned short x) { return (unsigned)x; }
long long c_ush_ll(unsigned short x) { return (long long)x; }
unsigned long long c_ush_ull(unsigned short x) { return (unsigned long long)x; }
signed char c_i_sc(int x) { return (signed char)x; }
unsigned char c_i_uc(int x) { return (unsigned char)x; }
short c_i_sh(int x) { return (short)x; }
unsigned short c_i_ush(int x) { return (unsigned short)x; }
int c_i_i(int x) { return (int)x; }
unsigned c_i_u(int x) { return (unsigned)x; }
long long c_i_ll(int x) { return (long long)x; }
unsigned long long c_i_ull(int x) { return (unsigned long long)x; }
signed char c_u_sc(unsigned x) { return (signed char)x; }
unsigned char c_u_uc(unsigned x) { return (unsigned char)x; }
short c_u_sh(unsigned x) { return (short)x; }
unsigned short c_u_ush(unsigned x) { return (unsigned short)x; }
int c_u_i(unsigned x) { return (int)x; }
unsigned c_u_u(unsigned x) { return (unsigned)x; }
long long c_u_ll(unsigned x) { return (long long)x; }
unsigned long long c_u_ull(unsigned x) { return (unsigned long long)x; }
signed char c_ll_sc(long long x) { return (signed char)x; }
unsigned char c_ll_uc(long long x) { return (unsigned char)x; }
short c_ll_sh(long long x) { return (short)x; }
unsigned short c_ll_ush(long long x) { return (unsigned short)x; }
int c_ll_i(long long x) { return (int)x; }
unsigned c_ll_u(long long x) { return (unsigned)x; }
long long c_ll_ll(long long x) { return (long long)x; }
unsigned long long c_ll_ull(long long x) { return (unsigned long long)x; }
signed char c_ull_sc(unsigned long long x) { return (signed char)x; }
unsigned char c_ull_uc(unsigned long long x) { return (unsigned char)x; }
short c_ull_sh(unsigned long long x) { return (short)x; }
unsigned short c_ull_ush(unsigned long long x) { return (unsigned short)x; }
int c_ull_i(unsigned long long x) { return (int)x; }
unsigned c_ull_u(unsigned long long x) { return (unsigned)x; }
long long c_ull_ll(unsigned long long x) { return (long long)x; }
unsigned long long c_ull_ull(unsigned long long x) { return (unsigned long long)x; }
