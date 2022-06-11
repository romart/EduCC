

int strlen(const char *s) {
  int r = 0;

  while (s[r]) r++;

  return r;
}


int main() {
  if (strlen("") != 0) return 1;
  if (strlen("1") != 1) return 2;
  if (strlen("ab") != 2) return 3;
  if (strlen("cde") != 3) return 4;
  if (strlen("xxxxxx") != 6) return 5;
  if (strlen("edc\n") != 4) return 6;
  if (strlen("\n") != 1) return 7;
  return 0;
}
