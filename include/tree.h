

#ifndef __TREE_H__
#define __TREE_H__ 1

#include "utils.h"
#include "types.h"

typedef struct _ParserContext ParserContext;

typedef struct _Coordinates {
  int startOffset;
  int endOffset;
} Coordinates;

enum NodeType {
  N_DEFINITION,
  N_DECLARATION,

  N_EXPRESSION,
  N_STATEMENT,

  N_NAME_REFERENCE,
  N_INVOCATION,

  N_BLOCK_STATEMENT,

  N_TYPE_REF
};

// TODO: make AstNodes be allocated in Arena


enum ExpressionType {
    E_CONST,
    E_TERNARY,
    E_CAST,
    E_NAMEREF,
    E_CALL,


    EC_INT_CONST,
    EC_FLOAT_CONST,
    EC_STRING_LITERAL,
    EC_SIZEOF,


    EU_PRE_INC,   /** --a */
    EU_POST_INC,  /** a++ */
    EU_PRE_DEC,   /** --a */
    EU_POST_DEC,  /** a-- */
    EU_DEREF,     /** *a */
    EU_REF,       /** &a */
    EU_PLUS,      /** +a */
    EU_MINUS,     /** -a */
    EU_TILDA,     /** ~a */
    EU_EXL,       /** !a */
    EU_SIZEOF,    /** sizeof a */

    EB_ADD,
    EB_SUB,
    EB_MUL,
    EB_DIV,
    EB_MOD,
    EB_LHS, /** << */
    EB_RHS, /** >> */
    EB_AND,
    EB_OR,
    EB_XOR,
    EB_ANDAND,
    EB_OROR,
    EB_EQ,
    EB_NE,
    EB_LT,
    EB_LE,
    EB_GT,
    EB_GE,
    EB_A_ACC, /** a[b] */
    EB_COMMA, /** a, b = c, d */


    EF_DOT, /** a.b */
    EF_ARROW, /** a->b */


    EB_ASSIGN,
    EB_RIGHT_ASSIGN,
    EB_LEFT_ASSIGN,
    EB_ADD_ASSIGN,
    EB_SUB_ASSIGN,
    EB_MUL_ASSIGN,
    EB_DIV_ASSIGN,
    EB_MOD_ASSIGN,
    EB_AND_ASSIGN,
    EB_XOR_ASSIGN,
    EB_OR_ASSIGN
};

typedef signed long long sint64_const_t;
typedef unsigned long long int64_const_t;
typedef double float64_const_t;
typedef const char *literal_const_t;

typedef struct ConstOp {
  int op;
  union {
    // TODO: is that OK to distinguish signed and unsinged consts
      int64_const_t i;
      float64_const_t f;
      literal_const_t l;
      struct _TypeDesc* t;
  };
} AstConst;

typedef struct _AstUnaryExpression {
  struct _AstExpression* argument;
} AstUnaryExpression;

typedef struct _AstBinaryExpression {
  struct _AstExpression *left, *right;
} AstBinaryExpression;

typedef struct _AstTernaryExpression {
  struct _AstExpression* condition;
  struct _AstExpression* ifTrue;
  struct _AstExpression* ifFalse;
} AstTernaryExpression;

typedef struct _AstCastExpression {
  struct _TypeRef* type;
  struct _AstExpression* argument;
} AstCastExpression;

typedef struct _AstNameRef {
  const char* name;
  struct _Symbol *s;
} AstNameRef;

typedef struct _AstCallExpression {
  struct _AstExpression* callee;
  Vector *arguments;
} AstCallExpression;

typedef struct _AstFieldExpression {
    int op;
    struct _AstExpression* recevier;
    const char* member;
} AstFieldExpression;


typedef struct _AstExpression {
  Coordinates coordinates;
  int op;
  TypeRef *type;
  union {
    AstConst constExpr;
    AstUnaryExpression unaryExpr;
    AstBinaryExpression binaryExpr;
    AstTernaryExpression ternaryExpr;
    AstCastExpression castExpr;
    AstNameRef nameRefExpr;
    AstCallExpression callExpr;
    AstFieldExpression fieldExpr;
  };
} AstExpression;

// statements

enum StatementKind {
    SK_BLOCK,
    SK_EXPR_STMT,
    SK_LABEL,
    SK_DECLARATION,
    SK_EMPTY,

    SK_IF,
    SK_SWITCH,

    SK_WHILE,
    SK_DO_WHILE,
    SK_FOR,

    SK_GOTO,
    SK_CONTINUE,
    SK_BREAK,
    SK_RETURN
};

typedef struct _AstBlock {
  struct _Scope *scope;
  Vector * stmts; // Vector<AstStatement*>
} AstBlock;

typedef struct _AstExpressionStatement {
    AstExpression* expression;
} AstExpressionStatement;

enum LabelKind {
    LK_LABEL,
    LK_CASE,
    LK_DEFAULT
};

typedef struct _AstLabelStatement {
    int kind;
    union {
        const char* label;
        int caseConst;
    };
    struct _AstStatement* body;
} AstLabelStatement;

typedef struct _AstDeclarationStatement {
    struct _AstDeclaration* declaration;
} AstDeclarationStatement;

typedef struct _AstIfStatement {
    AstExpression* condition;
    struct _AstStatement* thenBranch;
    struct _AstStatement* elseBranch;
} AstIfStatement;

typedef struct _AstSwitchStatement {
    AstExpression* condition;
    struct _AstStatement* body;
} AstSwitchStatement;

typedef struct _AstLoopStatement {
    AstExpression* condition;
    struct _AstStatement* body;
} AstLoopStatement;

typedef struct _AstForStatement {
//    struct _AstDeclaration* declaration;
    AstExpression* initial;
    AstExpression *condition;
    AstExpression *modifier;
    struct _AstStatement* body;
} AstForStatement;

typedef struct _AstJumpStatement {
    union {
        const char *label;
        AstExpression *expression;
    };
} AstJumpStatement;

typedef struct _AstStatement {
    Coordinates coordinates;
    int statementKind;
    union {
      AstBlock block; /** SK_BLOCK */
      AstExpressionStatement exprStmt; /** SK_EXPR_STMT */
      AstLabelStatement labelStmt; /** SK_LABEL */
      AstDeclarationStatement declStmt; /** SK_DECLARATION */
      AstIfStatement ifStmt; /** SK_IF */
      AstSwitchStatement switchStmt; /** SK_SWITCH */
      AstLoopStatement loopStmt; /** SK_WHILE | SK_DO_WHILE */
      AstForStatement forStmt; /** SK_FOR */
      AstJumpStatement jumpStmt; /** SK_GOTO | SK_CONTINUE | SK_BREAK | SK_RETURN */
    };
} AstStatement;

struct _AstDeclaration;

typedef struct _AstIdentifierList {
    const char* name;
    struct _AstIdentifierList* next;
} AstIdentifierList;

enum InitializerKind {
  IK_EXPRESSION,
  IK_LIST
};

typedef struct _AstInitializer {
    Coordinates coordinates;
    int kind; // expression | list of initializers
    union {
        AstExpression* expression;
        Vector* initializers; // Vector<AstInitializer>
    };
} AstInitializer;

enum DeclaratorPartKind {
    DPK_NONE = 0,
    DPK_POINTER,
    DPK_ARRAY,
    DPK_FUNCTION
};

typedef struct _FunctionParams {
    Vector *parameters;
    struct _Scope *scope;
    unsigned isVariadic : 1; // (int, int, ...)
} FunctionParams;

typedef struct _DeclaratorPart {
    int kind;
    union {
        SpecifierFlags flags;
        int arraySize;
        FunctionParams parameters; // Vector<ParameterDeclaration>
    };
} DeclaratorPart;

typedef struct _Declarator {
    const char* identificator;
    unsigned partsCounter;
    DeclaratorPart declaratorParts[256];
} Declarator;

typedef struct _EnumConstant {
    Coordinates coordinates;
    const char* name;
    int value;
} EnumConstant;

typedef struct _AstEnumDeclaration {
    Coordinates coordinates;
    const char* name;
    Vector* enumerators; //Vector<EnumConstant>
} AstEnumDeclaration;

typedef struct _AstStructDeclarator {
    Coordinates coordinates;
    TypeRef *typeRef;
    const char *name;

    int f_width; // -1 if not specified;
} AstStructDeclarator;

enum StructMember {
  SM_DECLARATOR,
  SM_DECLARATION,
};

typedef struct _AstStructMember {
  int kind; // SM_DECLARATOR | SM_DECLARATION
  union {
    AstStructDeclarator *declarator;
    struct _AstDeclaration *declaration;
  };
} AstStructMember;

typedef struct _AstSUEDeclaration { // Struct | Union | Enum declaration
    Coordinates coordinates;
    int kind; // DKX_UNION | DKX_STRUCT | DKX_ENUM
    const char *name;
    Vector *members; // Vector<AstStructMember> | Vector<EnumConstant>
} AstSUEDeclaration;

typedef struct _DeclarationSpecifiers {
  SpecifierFlags flags;
  TypeRef *basicType;

  int kind; // DKX_UNION | DKX_STRUCT | DKX_ENUM

  AstSUEDeclaration *defined;

} DeclarationSpecifiers;

enum DeclarationKind {
    DECLK_FUNCTION_DEFINITION,
    DECLK_VARIABLE_DECLARATION
};

enum {
  VD_PARAMETER,
  VD_VARIABLE
};

typedef struct _AstValueDeclaration {
  Coordinates coordinates;

  int kind; // VD_PARAMETER | VD_VARIABLE
  const char *name;
  TypeRef *type;
  union {
    unsigned index; // VD_PARAMETER
    struct {
      SpecifierFlags flags;
      AstInitializer *initializer; // VD_VARIABLE
    };
  };
} AstValueDeclaration;

typedef struct _AstFunctionDeclaration {
  Coordinates coordinates;
  SpecifierFlags flags;
  const char *name;
  TypeRef *returnType;
  unsigned parameterCount;
  AstValueDeclaration **parameters;
  unsigned isVariadic : 1;
} AstFunctionDeclaration;

enum {
  DKX_ENUM,
  DKX_STRUCT,
  DKX_UNION,
  DKX_TYPEDEF,
  DKX_VAR,
  DKX_PROTOTYPE
};


typedef struct _AstDeclaration {
  int kind; // DKX
  const char *name;
  union {
    AstSUEDeclaration *structDeclaration; // DKX_ENUM | DKX_STRUCT | DKX_UNION
    struct {
      TypeRef *definedType; // DKX_TYPEDEF
      Coordinates coordinates;
    } typeDefinition;
    AstValueDeclaration *variableDeclaration; // DKX_VAR
    AstFunctionDeclaration *functionProrotype; // DKX_PROTOTYPE
  };
} AstDeclaration;

typedef struct _AstFunctionDefinition { // _AstFunctionDefinition
  AstFunctionDeclaration *declaration;
  AstStatement *body;
  struct _Scope *scope;
} AstFunctionDefinition;

enum {
  TU_DECLARATION,
  TU_FUNCTION_DEFINITION
};

typedef struct _AstTranslationUnit {
    int kind; // TU_DECLARATION | TU_FUNCTION_DEFINITION
    union {
      AstDeclaration *declaration;
      AstFunctionDefinition *definition;
    };
} AstTranslationUnit;

typedef struct _AstFile {
  const char* fileName;
  Vector* declarations; // Vector<AstTranslationUnit*>
  struct _AstFile *next;
} AstFile;


//Factories

// types

TypeDesc *createTypeDescriptor(ParserContext *ctx, int typeId, const char *name, int size);

// declarations

EnumConstant *createEnumConst(ParserContext *ctx, int startOffset, int endOffset, const char* name, int64_const_t value);

AstInitializer *createAstInitializer(ParserContext *ctx, int startOffset, int endOffset, AstExpression *expr, Vector *initializers);
AstStructDeclarator *createStructDeclarator(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, int width);
AstStructMember* createStructMember(ParserContext *ctx, AstDeclaration *declaration, AstStructDeclarator *declarator);
AstSUEDeclaration *createSUEDeclaration(ParserContext *ctx, int startOffset, int endOffset, int kind, const char *name, Vector *members);
AstFunctionDeclaration *createFunctionDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *returnType, const char *name, unsigned flags, unsigned parameterCount, AstValueDeclaration **parameters, int isVariadic);
AstValueDeclaration *createAstValueDeclaration(ParserContext *ctx, int startOffset, int endOffset, int kind, TypeRef *type, const char *name, unsigned index, unsigned flags, AstInitializer *initializer);

AstDeclaration *createAstDeclaration(ParserContext *ctx, int kind, const char *name);
AstFunctionDefinition *createFunctionDefinition(ParserContext *ctx, AstFunctionDeclaration *declaration, struct _Scope *scope, AstStatement *body);

AstTranslationUnit *createTranslationUnit(ParserContext *ctx, AstDeclaration *declaration, AstFunctionDefinition *definition);

AstFile *createAstFile(ParserContext *ctx, int capacity);

// expressions

AstExpression* createAstConst(ParserContext *ctx, int startOffset, int endOffset, int type, void* value);
AstExpression *createCastExpression(ParserContext *ctx, int startOffset, int endOffset, TypeRef *typeRef, AstExpression *argument);
AstExpression *createTernaryExpression(ParserContext *ctx, AstExpression *cond, AstExpression *t, AstExpression* f);
AstExpression *createBinaryExpression(ParserContext *ctx, int op, AstExpression *left, AstExpression *right);
AstExpression *createUnaryExpression(ParserContext *ctx, int startOffset, int endOffset, int op, AstExpression *argument);
AstExpression *createNameRef(ParserContext *ctx, int startOffset, int endOffset, const char *name);
AstExpression *createCallExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *callee, Vector *arguments);
AstExpression *createFieldExpression(ParserContext *ctx, int startOffset, int endOffset, int op, AstExpression *receiver, const char *member);


// statemetns

AstStatement *createBlockStatement(ParserContext *ctx, int startOffset, int endOffset, struct _Scope *scope, Vector *stmts);
AstStatement *createExprStatement(ParserContext *ctx, AstExpression* expression);
AstStatement *createLabelStatement(ParserContext *ctx, int startOffset, int endOffset, int labelKind, AstStatement *body, const char *label, int c);
AstStatement *createDeclStatement(ParserContext *ctx, int startOffset, int endOffset, AstDeclaration *decl);
AstStatement *createIfStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *thenB, AstStatement *elseB);
AstStatement *createSwitchStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *body);
AstStatement *createLoopStatement(ParserContext *ctx, int startOffset, int endOffset, int kind, AstExpression *cond, AstStatement *body);
AstStatement *createForStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body);
AstStatement *createJumpStatement(ParserContext *ctx, int startOffset, int endOffset, int jumpKind);
AstStatement *createEmptyStatement(ParserContext *ctx, int startOffset, int endOffset);

#endif // __TREE_H__
