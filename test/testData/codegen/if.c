

int test(int x) {
  if (!x) {
      return 10;
  } else if (x == 20) {
      return 30;
  } else if (x < 5) {
      return 4;
  } else if (7 <= x && x < 11) {
      return 100;
  } else if (x > 100 || x < 6) {
      return 200;
  } else {
      return 1000;
  }
}


int testeq(int x, int y) {
  if (x == y) {
      return 1;
  }
  return -1;
}

int testeq_else(int x, int y) {
  if (x == y) {
    return 1;
  } else {
    return -1;
  }
}

int testne(int x, int y) {
  if (x != y) {
      return 1;
  }
  return -1;
}

int testne_else(int x, int y) {
  if (x != y) {
    return 1;
  } else {
    return -1;
  }
}

int testlt(int x, int y) {
  if (x < y) {
      return 1;
  }
  return -1;
}

int testlt_else(int x, int y) {
  if (x < y) {
    return 1;
  } else {
    return -1;
  }
}

int testle(int x, int y) {
  if (x <= y) {
      return 1;
  }
  return -1;
}

int testle_else(int x, int y) {
  if (x <= y) {
    return 1;
  } else {
    return -1;
  }
}

int testgt(int x, int y) {
  if (x > y) {
      return 1;
  }
  return -1;
}

int testgt_else(int x, int y) {
  if (x > y) {
    return 1;
  } else {
    return -1;
  }
}

int testge(int x, int y) {
  if (x >= y) {
      return 1;
  }
  return -1;
}

int testge_else(int x, int y) {
  if (x >= y) {
    return 1;
  } else {
    return -1;
  }
}

int vflag = 0;

int testCascade(int m) {
  if ((m && !vflag) || (!m && vflag)) {
      return 1;
  } else return -1;
}

enum DEFINED {

 LMAX    = 512,
 PMAX    = 256,
 CHAR    = 1,
 BOL     = 2,
 EOL     = 3,
 ANY     = 4,
 CLASS   = 5,
 NCLASS  = 6,
 STAR    = 7,
 PLUS    = 8,
 MINUS   = 9,
 ALPHA   = 10,
 DIGIT   = 11,
 NALPHA  = 12,
 PUNCT   = 13,
 RANGE   = 14,
 ENDPAT  = 15,
  NULL = 0
};

char *pp, lbuf[LMAX], pbuf[PMAX];

int testCascade2() {
  char o;
  if (pp == pbuf ||
        (o=pp[-1]) == BOL ||
        o == EOL ||
        o == STAR ||
        o == PLUS ||
        o == MINUS)
     return 1;
  else return -1;
}

int main() {
  if (test(0) != 10) return 1;
  if (test(20) != 30) return 2;
  if (test(4) != 4) return 3;
  if (test(10) != 100) return 4;
  if (test(100) != 1000) return 5;
  if (test(5) != 200) return 6;
  if (test(150) != 200) return 7;

  if (testeq(42, 42) != 1) return 8;
  if (testeq(42, 24) != -1) return 9;
  if (testeq_else(42, 42) != 1) return 10;
  if (testeq_else(42, 24) != -1) return 11;

  if (testne(42, 42) != -1) return 12;
  if (testne(42, 24) != 1) return 13;
  if (testne_else(42, 42) != -1) return 14;
  if (testne_else(42, 24) != 1) return 15;

  if (testlt(42, 42) != -1) return 16;
  if (testlt(24, 42) != 1) return 17;
  if (testlt(42, 24) != -1) return 18;
  if (testlt_else(42, 42) != -1) return 19;
  if (testlt_else(24, 42) != 1) return 20;
  if (testlt_else(42, 24) != -1) return 21;

  if (testle(42, 42) != 1) return 22;
  if (testle(24, 42) != 1) return 23;
  if (testle(42, 24) != -1) return 24;
  if (testle_else(42, 42) != 1) return 25;
  if (testle_else(24, 42) != 1) return 26;
  if (testle_else(42, 24) != -1) return 27;

  if (testgt(42, 42) != -1) return 28;
  if (testgt(24, 42) != -1) return 29;
  if (testgt(42, 24) != 1) return 30;
  if (testgt_else(42, 42) != -1) return 31;
  if (testgt_else(24, 42) != -1) return 32;
  if (testgt_else(42, 24) != 1) return 33;

  if (testge(42, 42) != 1) return 34;
  if (testge(24, 42) != -1) return 35;
  if (testge(42, 24) != 1) return 36;
  if (testge_else(42, 42) != 1) return 37;
  if (testge_else(24, 42) != -1) return 38;
  if (testge_else(42, 24) != 1) return 39;

  if (testCascade(1) != 1) return 40;
  if (testCascade(0) != -1) return 41;

  vflag = 1;

  if (testCascade(1) != -1) return 42;
  if (testCascade(0) != 1) return 43;

  pp = pbuf;
  if (testCascade2() != 1) return 44;

  pp += 1;
  if (testCascade2() != -1) return 45;

  pbuf[0] = BOL;
  if (testCascade2() != 1) return 46;

  pbuf[0] = EOL;
  if (testCascade2() != 1) return 47;

  pbuf[0] = STAR;
  if (testCascade2() != 1) return 48;

  pbuf[0] = PLUS;
  if (testCascade2() != 1) return 49;

  pbuf[0] = MINUS;
  if (testCascade2() != 1) return 50;

  return 0;
}
