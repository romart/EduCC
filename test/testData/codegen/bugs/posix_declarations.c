// __STRICT_ANSI__ used to be predefined unconditionally, and glibc's features.h
// reads it as "hide everything that is not ISO C". None of these declarations
// were visible, which is why src/main.c carried hand-written externs.

#include <string.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
  char *copy = strdup("abc");
  if (copy == 0) return 1;
  if (strlen(copy) != 3) return 2;
  if (strcmp(copy, "abc") != 0) return 3;
  free(copy);

  char buffer[8];
  if (strncpy(buffer, "xy", sizeof buffer) != buffer) return 4;

  // Declared, not called: what the fixture is checking is that the compiler
  // sees a prototype for each of them at all.
  if ((void *)&readlink == 0) return 5;
  if ((void *)&usleep == 0) return 6;
  if ((void *)&mkdtemp == 0) return 7;
  if ((void *)&strndup == 0) return 8;

  return 0;
}
