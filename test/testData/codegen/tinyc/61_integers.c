

#include <stdio.h>

static int s18 = 18;
static int s21 = 21;
static int s41 = 41;
static int s42 = 42;
static int s43 = 43;
static int s126 = 126;
static int s131 = 131;



int main()
{

    unsigned UINT_MAX = ((unsigned) -1);
    printf("18/21=%u\n", s18/s21);
    printf("18%%21=%u\n", s18%s21);
    printf("41/21=%u\n", s41/s21);
    printf("41%%21=%u\n", s41%s21);
    printf("42/21=%u\n", s42/s21);
    printf("42%%21=%u\n", s42%s21);
    printf("43/21=%u\n", s43/s21);
    printf("43%%21=%u\n", s43%s21);
    printf("126/21=%u\n", s126/s21);
    printf("126%%21=%u\n", s126%s21);
    printf("131/21=%u\n", s131/s21);
    printf("131%%21=%u\n", s131%s21);
    printf("(UINT_MAX/2+3)/2=%u\n", (UINT_MAX/2+3)/2);
    printf("(UINT_MAX/2+3)%%2=%u\n", (UINT_MAX/2+3)%2);

    printf("18/-21=%u\n", s18/-s21);
    printf("18%%-21=%u\n", s18%-s21);
    printf("41/-21=%u\n", s41/-s21);
    printf("41%%-21=%u\n", s41%-s21);
    printf("42/-21=%u\n", s42/-s21);
    printf("42%%-21=%u\n", s42%-s21);
    printf("43/-21=%u\n", s43/-s21);
    printf("43%%-21=%u\n", s43%-s21);
    printf("126/-21=%u\n", s126/-s21);
    printf("126%%-21=%u\n", s126%-s21);
    printf("131/-21=%u\n", s131/-s21);
    printf("131%%-21=%u\n", s131%-s21);
    printf("(UINT_MAX/2+3)/-2=%u\n", (UINT_MAX/2+3)/-2);
    printf("(UINT_MAX/2+3)%%-2=%u\n", (UINT_MAX/2+3)%-2);

    printf("-18/21=%u\n", -s18/s21);
    printf("-18%%21=%u\n", -s18%s21);
    printf("-41/21=%u\n", -s41/s21);
    printf("-41%%21=%u\n", -s41%s21);
    printf("-42/21=%u\n", -s42/s21);
    printf("-42%%21=%u\n", -s42%s21);
    printf("-43/21=%u\n", -s43/s21);
    printf("-43%%21=%u\n", -s43%s21);
    printf("-126/21=%u\n", -s126/s21);
    printf("-126%%21=%u\n", -s126%s21);
    printf("-131/21=%u\n", -s131/s21);
    printf("-131%%21=%u\n", -s131%s21);
    printf("-(UINT_MAX/2+3)/2=%u\n", (0-(UINT_MAX/2+3))/2);
    printf("-(UINT_MAX/2+3)%%2=%u\n", (0-(UINT_MAX/2+3))%2);

    printf("-18/-21=%u\n", -s18/-s21);
    printf("-18%%-21=%u\n", -s18%-s21);
    printf("-41/-21=%u\n", -s41/-s21);
    printf("-41%%-21=%u\n", -s41%-s21);
    printf("-42/-21=%u\n", -s42/-s21);
    printf("-42%%-21=%u\n", -s42%-s21);
    printf("-43/-21=%u\n", -s43/-s21);
    printf("-43%%-21=%u\n", -s43%-s21);
    printf("-126/-21=%u\n", -s126/-s21);
    printf("-126%%-21=%u\n", -s126%-s21);
    printf("-131/-21=%u\n", -s131/-s21);
    printf("-131%%-21=%u\n", -s131%-s21);
    printf("-(UINT_MAX/2+3)/-2=%u\n", (0-(UINT_MAX/2+3))/-2);
    printf("-(UINT_MAX/2+3)%%-2=%u\n", (0-(UINT_MAX/2+3))%-2);

    return 0;
}
