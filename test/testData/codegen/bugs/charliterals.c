
#include <stdio.h>

#define ANSI_COLOR_RESET   "\x1b[0m"
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_PURPLE  "\x1b[95m"
#define ANSI_COLOR_BOLD    "\x1b[1m"




int main() {
  printf("%s%s", ANSI_COLOR_BOLD, ANSI_COLOR_PURPLE);
  printf("TEST\n");
  printf(ANSI_COLOR_RESET);

  return 0;
}
