/* Test some common transformations. See:
 *   https://gcc.gnu.org/wiki/FloatingPointMath
 *
 * Note: the NaN tests may not be valid on all machines.
 * Tested only on x86_64.
 */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>
#include <fenv.h>

unsigned long int strtoul(const char *nptr, char **endptr, int base);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

typedef union { double d; uint64_t i; } ieee_double_t;

double x_add_zero_0 (double x) { return x + 0.0; }
double x_add_zero_1 (double x) { volatile double c = 0.0; return x + c; }

double x_sub_zero_0 (double x) { return x - 0.0; }
double x_sub_zero_1 (double x) { volatile double c = 0.0; return x - c; }

double zero_sub_x_0 (double x) { return 0.0 - x; }
double zero_sub_x_1 (double x) { volatile double c = 0.0; return c - x; }

double zero_mul_x_0 (double x) { return 0.0 * x; }
double zero_mul_x_1 (double x) { volatile double c = 0.0; return c * x; }

double zero_div_x_0 (double x) { return 0.0 / x; }
double zero_div_x_1 (double x) { volatile double c = 0.0; return c / x; }

double x_mul_pone_0 (double x) { return x * 1.0; }
double x_mul_pone_1 (double x) { volatile double c = 1.0; return x * c; }

double x_mul_mone_0 (double x) { return x * -1.0; }
double x_mul_mone_1 (double x) { volatile double c = -1.0; return x * c; }

double x_div_pone_0 (double x) { return x / 1.0; }
double x_div_pone_1 (double x) { volatile double c = 1.0; return x / c; }

double x_div_mone_0 (double x) { return x / -1.0; }
double x_div_mone_1 (double x) { volatile double c = -1.0; return x / c; }

/* NaN canonicalization */
uint64_t canon (uint64_t i)
{
  return ((i & 0x7ff0000000000000) != 0x7ff0000000000000 ||
          (i & 0x000fffffffffffff) == 0) ? i /* not NaN */ :
    0x7ff0000000000001 | (i & 0x0008000000000000);
}

void vout (char *s, double d)
{
  ieee_double_t v;
  v.d = d;
  printf ("%s = [0x%016" PRIx64 "] = %g%s\n", s, v.i, d,
    canon (v.i) != 0x7ff0000000000001 ? "" : " (signaling)");
}

int cmp (char *s, char *r, double y0, double y1)
{
  volatile double z0 = y0, z1 = y1;
  volatile ieee_double_t v0, v1;

  v0.d = y0;
  v1.d = y1;

  if (canon (v0.i) == canon (v1.i))
    return 0;

#if 0
  if ((isnan (z0) && isnan (z1)) ||
      (z0 == z1 && !signbit (z0) == !signbit (z1)))
    return 0;
#endif

  printf ("Error for %s in %s:\n", s, r);
  vout ("  y0", z0);
  vout ("  y1", z1);
  return 1;
}

int test (char *r, double x)
{
  volatile double y0, y1;
  int err = 0;

  y0 = x_add_zero_0 (x);
  y1 = x_add_zero_1 (x);
  err |= cmp ("x_add_zero", r, y0, y1);

  y0 = x_sub_zero_0 (x);
  y1 = x_sub_zero_1 (x);
  err |= cmp ("x_sub_zero", r, y0, y1);

  y0 = zero_sub_x_0 (x);
  y1 = zero_sub_x_1 (x);
  err |= cmp ("zero_sub_x", r, y0, y1);

  y0 = zero_mul_x_0 (x);
  y1 = zero_mul_x_1 (x);
  err |= cmp ("zero_mul_x", r, y0, y1);

  y0 = zero_div_x_0 (x);
  y1 = zero_div_x_1 (x);
  err |= cmp ("zero_div_x", r, y0, y1);

  y0 = x_mul_pone_0 (x);
  y1 = x_mul_pone_1 (x);
  err |= cmp ("x_mul_pone", r, y0, y1);

  y0 = x_mul_mone_0 (x);
  y1 = x_mul_mone_1 (x);
  err |= cmp ("x_mul_mone", r, y0, y1);

  y0 = x_div_pone_0 (x);
  y1 = x_div_pone_1 (x);
  err |= cmp ("x_div_pone", r, y0, y1);

  y0 = x_div_mone_0 (x);
  y1 = x_div_mone_1 (x);
  err |= cmp ("x_div_mone", r, y0, y1);

  return err;
}

int main (int argc, char *argv[])
{
  char *end;
  double x;
  int err = 0;

  if (argc != 2)
    {
      exit (1);
    }

  if (strncmp (argv[1], "0x", 2) == 0)
    {
      ieee_double_t v;
      v.i = strtoull (argv[1], &end, 0);
      if (*end != '\0')
        exit (1);
      x = v.d;
    }
  else
    {
      x = strtod (argv[1], &end);
      if (*end != '\0')
        exit (1);
    }

  vout ("x", x);

#ifdef FE_TONEAREST
  if (fesetround (FE_TONEAREST))
    printf ("fesetround failed on FE_TONEAREST\n");
  else
    err |= test ("FE_TONEAREST", x);
#else
  printf ("FE_TONEAREST is not supported\n");
#endif

#ifdef FE_TOWARDZERO
  if (fesetround (FE_TOWARDZERO))
    printf ("fesetround failed on FE_TOWARDZERO\n");
  else
    err |= test ("FE_TOWARDZERO", x);
#else
  printf ("FE_TOWARDZERO is not supported\n");
#endif

#ifdef FE_DOWNWARD
  if (fesetround (FE_DOWNWARD))
    printf ("fesetround failed on FE_DOWNWARD\n");
  else
    err |= test ("FE_DOWNWARD", x);
#else
  printf ("FE_DOWNWARD is not supported\n");
#endif

#ifdef FE_UPWARD
  if (fesetround (FE_UPWARD))
    printf ("fesetround failed on FE_UPWARD\n");
  else
    err |= test ("FE_UPWARD", x);
#else
  printf ("FE_UPWARD is not supported\n");
#endif

  return err;
}

/* $Id: transf-generated.c 70083 2014-06-04 14:24:10Z vinc17/xvii $ */
