
struct;


struct NegativeBFSize {
  unsigned negative : -5;

  unsigned : -6;
};


struct ExceedBFSize {
  unsigned not_exceed : 32;
  unsigned long long not_exceed_l : 64;
  unsigned exceed : 33;
  unsigned long long exceed_l : 65;
  unsigned exceed2 : 33333;

  unsigned : 32;
  unsigned : 33;
  unsigned : 33333;
};

struct F { int a; };

struct NotIntType {
  float f_bf : 10;

  struct F : 20;
};
