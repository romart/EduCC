
register int tlReg = 10;

void testF(float f) {
  int a = (static int)f;
  int b = (extern int)f;
  int c = (typedef int)f;
  int d = (register int)f;
}

void testP(static float f, extern int i) {}
