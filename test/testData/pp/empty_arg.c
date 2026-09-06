// An argument that is tokens but expands to none of them. The parameter
// contributes nothing, and the expansion used to dereference the empty result
// - libpng's pngconf.h passes PNGARG-style empty macros this way.

#define EMPTY
#define ID(x) x
#define WRAP(a, b) a b
#define STR(x) #x
#define XSTR(x) STR(x)

int one = 1 ID(EMPTY) + 2;
int two = WRAP(EMPTY, 3);
int three = WRAP(4, EMPTY);
char *s = XSTR(ID(EMPTY));
