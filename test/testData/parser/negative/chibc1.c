


struct Type {
  int kind;
};

static void pop(const char *c) {}

static void store(Type *ty) {
  pop("%rdi");

  switch (ty->kind) {
    case 1: return ;
    case 2: pop(";;");
  }
}
