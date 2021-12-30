



enum Tokens {
  LAST_SIMEPL_TOKEN=0xff,
 
  IDENTIFIER,

  // key words
  BREAK,
  CASE,
  CHAR,
  CONTINUE,
  DEFAULT,
  DO,
  DOUBLE,
  ELSE,
  ENUM,
  EXTERN, 
  FLOAT,
  FOR,
  GOTO,
  IF, 
  INT,
  LONG,
  REGISTER,
  RESTRICT,
  RETURN,
  SHORT,
  SIGNED,
  SIZEOF,
  STATIC,
  STRUCT,
  SWITCH,
  TYPEDEF,
  UNION, 
  UNSIGNED,
  VOID, 
  WHILE, 
  
  // extra key words 

  // constants
  I_CONSTANT,
  F_CONSTANT,
  STRING_LITERAL,

  // operations
  ELLIPSIS,
  
  // op= operations
  RIGHT_ASSIGN, // >>=
  LEFT_ASSIGN, // <<=
  ADD_ASSIGN, // +=
  SUB_ASSIGN, // -=
  MUL_ASSIGN, // *=
  DIV_ASSIGN, // /=
  MOD_ASSIGN, // %=
  AND_ASSIGN, // &=
  XOR_ASSIGN, // ^=
  OR_ASSIGN, // |=
  
  // ops
  RIGHT_OP, // >>
  LEFT_OP, // <<
  INC_OP, // ++
  DEC_OP, // --
  PTR_OP, // ->
  AND_OP, // &&
  OR_OP, // ||
  LE_OP, // <=
  GE_OP, // >=
  EQ_OP, // ==
  NE_OP, // !=   

  NUMBER_OF_TOKENS 
};


