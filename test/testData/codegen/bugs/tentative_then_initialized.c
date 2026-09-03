// A file-scope object may be declared any number of times and defined once. The
// parser keeps a node per declaration, and every one of them used to be given
// its own storage: the tentative declaration landed in .bss, the definition in
// .data, and every reference used the first - so the object read as zero.
//
// tinycc is where this came from. tcc.h declares 'reg_classes' and
// x86_64-gen.c defines it with an initializer; in a ONE_SOURCE build both are
// one translation unit, and get_reg() read an all-zero register class table.

int gInt;
int gInt = 11;

const int gArr[3];
const int gArr[3] = { 1, 2, 3 };

extern int gExtern;
int gExtern = 22;

static int sInt;
static int sInt = 33;

static const char sStr[4];
static const char sStr[4] = "abc";

// Declared, defined, then declared again: the definition still wins.
int gAgain;
int gAgain = 44;
int gAgain;

// The definition first and the tentative declaration after it.
int gFirst = 55;
int gFirst;

struct P { int a; int b; };
static struct P sStruct;
static struct P sStruct = { 66, 77 };

// A reference from before the definition is textually reached has to end up at
// the same object as one from after it.
static int beforeInt(void) { return sInt; }
static const char *beforeStr(void) { return sStr; }

static int check(int cond, int code) { return cond ? 0 : code; }

int main(void) {
  int rc = 0;

  rc += check(gInt == 11, 1);
  rc += check(gArr[0] == 1 && gArr[1] == 2 && gArr[2] == 3, 2);
  rc += check(gExtern == 22, 4);
  rc += check(sInt == 33, 8);
  rc += check(sStr[0] == 'a' && sStr[1] == 'b' && sStr[2] == 'c' && sStr[3] == 0, 16);
  rc += check(gAgain == 44, 32);
  rc += check(gFirst == 55, 64);
  rc += check(sStruct.a == 66 && sStruct.b == 77, 128);
  rc += check(beforeInt() == 33, 256);
  rc += check(beforeStr() == sStr, 512);

  return rc;
}
