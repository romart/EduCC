

#include <stdio.h>


int main()
{
    unsigned UINT_MAX = ((unsigned) -1);
    printf("18/21=%u\n", 18/21);
    printf("18%%21=%u\n", 18%21);
    printf("41/21=%u\n", 41/21);
    printf("41%%21=%u\n", 41%21);
    printf("42/21=%u\n", 42/21);
    printf("42%%21=%u\n", 42%21);
    printf("43/21=%u\n", 43/21);
    printf("43%%21=%u\n", 43%21);
    printf("126/21=%u\n", 126/21);
    printf("126%%21=%u\n", 126%21);
    printf("131/21=%u\n", 131/21);
    printf("131%%21=%u\n", 131%21);
    printf("(UINT_MAX/2+3)/2=%u\n", (UINT_MAX/2+3)/2);
    printf("(UINT_MAX/2+3)%%2=%u\n", (UINT_MAX/2+3)%2);

    printf("18/-21=%u\n", 18/-21);
    printf("18%%-21=%u\n", 18%-21);
    printf("41/-21=%u\n", 41/-21);
    printf("41%%-21=%u\n", 41%-21);
    printf("42/-21=%u\n", 42/-21);
    printf("42%%-21=%u\n", 42%-21);
    printf("43/-21=%u\n", 43/-21);
    printf("43%%-21=%u\n", 43%-21);
    printf("126/-21=%u\n", 126/-21);
    printf("126%%-21=%u\n", 126%-21);
    printf("131/-21=%u\n", 131/-21);
    printf("131%%-21=%u\n", 131%-21);
    printf("(UINT_MAX/2+3)/-2=%u\n", (UINT_MAX/2+3)/-2);
    printf("(UINT_MAX/2+3)%%-2=%u\n", (UINT_MAX/2+3)%-2);

    printf("-18/21=%u\n", -18/21);
    printf("-18%%21=%u\n", -18%21);
    printf("-41/21=%u\n", -41/21);
    printf("-41%%21=%u\n", -41%21);
    printf("-42/21=%u\n", -42/21);
    printf("-42%%21=%u\n", -42%21);
    printf("-43/21=%u\n", -43/21);
    printf("-43%%21=%u\n", -43%21);
    printf("-126/21=%u\n", -126/21);
    printf("-126%%21=%u\n", -126%21);
    printf("-131/21=%u\n", -131/21);
    printf("-131%%21=%u\n", -131%21);
    printf("-(UINT_MAX/2+3)/2=%u\n", (0-(UINT_MAX/2+3))/2);
    printf("-(UINT_MAX/2+3)%%2=%u\n", (0-(UINT_MAX/2+3))%2);

    printf("-18/-21=%u\n", -18/-21);
    printf("-18%%-21=%u\n", -18%-21);
    printf("-41/-21=%u\n", -41/-21);
    printf("-41%%-21=%u\n", -41%-21);
    printf("-42/-21=%u\n", -42/-21);
    printf("-42%%-21=%u\n", -42%-21);
    printf("-43/-21=%u\n", -43/-21);
    printf("-43%%-21=%u\n", -43%-21);
    printf("-126/-21=%u\n", -126/-21);
    printf("-126%%-21=%u\n", -126%-21);
    printf("-131/-21=%u\n", -131/-21);
    printf("-131%%-21=%u\n", -131%-21);
    printf("-(UINT_MAX/2+3)/-2=%u\n", (0-(UINT_MAX/2+3))/-2);
    printf("-(UINT_MAX/2+3)%%-2=%u\n", (0-(UINT_MAX/2+3))%-2);

    return 0;
}
