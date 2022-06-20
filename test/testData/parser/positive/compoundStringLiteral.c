
int test() {
  char s0[] = "";
  const char *s1 = "1";
  char s2[] = "1" "22";
  char s3[] = "1"
       "222"
           "333333";

  char s4[] = "1" "2" "3" "4" "5" "" "" "" "9";
  const char *tx = "a\0b\0c\0d";
  char s6[] = "a\0b\0c\0d" "" "\0e" "" "" "" "\0f";

  int l0 = sizeof s0;
  int l1 = sizeof s1;
  int l2 = sizeof s2;
  int l3 = sizeof s3;
  int l4 = sizeof s4;
}
