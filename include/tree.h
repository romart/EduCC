

#ifndef __TREE_H__
#define __TREE_H__ 1

#include "utils.h"

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

#define BIT(n) (1U << (n))

enum TypeId {
  T_VOID,

  T_S1,
  T_S2,
  T_S4,
  T_S8,

  T_F4,
  T_F8,

  T_U1,
  T_U2,
  T_U4,
  T_U8,

  T_BUILT_IN_TYPES,

  T_STRUCT,
  T_UNION,

  T_ENUM,
};

typedef union {
    unsigned storage;
    struct {
        unsigned isConst : 1;
        unsigned isVolatile : 1;

        unsigned isStatic : 1;
        unsigned isExternal : 1;
        unsigned isRegister : 1;
        unsigned isTypedef : 1;
    } bits;
} SpecifierFlags;

typedef struct _FunctionTypeDescriptor {
    int parameterCount;
    struct _ParameterDeclaration *parameters;
    struct _TypeRef *returnType;
    unsigned isVariadic : 1;
} FunctionTypeDescriptor;

typedef struct _ArrayTypeDescriptor {
    struct _TypeRef *elementType;
    int size;
} ArrayTypeDescriptor;

typedef struct _TypeDesc {
  int typeId;
  const char *name;
  int size;
  union {
    struct _AstStructDeclaration *structInfo;
    struct _AstEnumDeclaration *enumInfo;
  };
} TypeDesc;

enum TypeRefKind {
    TR_VALUE,
    TR_POINTED,
    TR_ARRAY,
    TR_FUNCTION
};

typedef struct _TypeRef {
    int kind; /** VALUE | POINTED | TR_ARRAY | TR_FUNCTION */
    SpecifierFlags flags;
    union {
        TypeDesc *descriptorDesc; // aka TypeConstructor
        FunctionTypeDescriptor functionTypeDesc;
        struct _TypeRef *pointedTo; // aka UnderlyingType
        ArrayTypeDescriptor arrayTypeDesc;
    };
} TypeRef;

// TODO: make AstNodes be allocated in Arena


enum ExpressionType {
    E_CONST,
    E_TERNARY,
    E_CAST,
    E_NAMEREF,
    E_CALL,


    EC_S_INT_CONST,
    EC_U_INT_CONST,
    EC_FLOAT_CONST,
    EC_DOUBLE_CONST,
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

typedef struct ConstOp {
  int op;
  union {
    // TODO: is that OK to distinguish signed and unsinged consts
      signed long long s;
      unsigned long long u;
      float f;
      double d;
      const char* l;
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
  Vector* stmts; // Vector<AstStatement*>
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

typedef struct _ParameterDeclaration {
    Coordinates coordinates;
    const char *name;
    TypeRef* type;
    int index;
} ParameterDeclaration;

enum DeclaratorPartKind {
    DPK_NONE = 0,
    DPK_POINTER,
    DPK_ARRAY,
    DPK_FUNCTION
};

typedef struct _FunctionParams {
    Vector *parameters;
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
    int partsCounter;
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

typedef struct _AstStructDeclaration {
    Coordinates coordinates;
    int token; // struct or union
    const char *name;
    Vector *members;
} AstStructDeclaration;

typedef struct _DeclarationSpecifiers {
  SpecifierFlags flags;
  TypeDesc * typeSpecifier;
  TypeRef *typeRef;
} DeclarationSpecifiers;

typedef struct _AstFunctionDefinition {
    struct _Scope* scope;
    AstBlock *body;
    const char* name;
    TypeRef *returnType;
    Vector *parameters;
} AstFunctionDefinition;

typedef struct _AstDeclaration {
    Coordinates coordinates;
    TypeRef *type;
    SpecifierFlags flags;
    const char *name;
    AstInitializer *initializer;
} AstDeclaration;

enum DeclarationKind {
    DECLK_FUNCTION_DEFINITION,
    DECLK_VARIABLE_DECLARATION
};

typedef struct _Declaration {
    Coordinates coordinates;
    int kind;

    const char *name;

    unsigned isStatic : 1;
    unsigned isExternal : 1;

    union {
        struct {
            FunctionTypeDescriptor *declaration;
            AstStatement *body;
        } functionDefinition;

        struct {
            TypeRef *variableType;
            AstInitializer *initializer;
        } variableDeclaration;
    };
} Declaration;

typedef struct _AstFile {
  const char* fileName;
  Vector* declarations; // Vector<Declaration*>
  struct _AstFile *next;
} AstFile;


//Factories

// types

TypeDesc *createTypeDescriptor(int typeId, const char *name, int size);

// declarations

EnumConstant *createEnumConst(const char* name, int value);

AstInitializer *createAstInitializer(AstExpression *expr, Vector *initializers);
AstStructDeclarator *createStructDeclarator(DeclarationSpecifiers *specifiers, Declarator* declarator, int width);
AstStructDeclaration *createStructDeclaration(int token, const char *name, Vector *members);
AstEnumDeclaration *createEnumDeclaration(const char *name, Vector *enumerators);
AstDeclaration *createAstDeclaration(TypeRef *type, const char *name, AstInitializer *initializer, unsigned flags);
AstFile *createAstFile(int capacity);

Declaration *createVariableDeclaration(ParserContext *ctx, TypeRef *type, const char *name, AstInitializer *initializer, SpecifierFlags flags);
Declaration *createFunctionDefinition(ParserContext *ctx, const char *name, FunctionTypeDescriptor *descriptor, SpecifierFlags flags);

ParameterDeclaration *createParameterDeclaration(ParserContext *ctx, TypeRef *type, const char *name, int index);

// expressions

AstExpression* createAstConst(int type, void* value);
AstExpression *createCastExpression(TypeRef *typeRef, AstExpression *argument);
AstExpression *createTernaryExpression(AstExpression *cond, AstExpression *t, AstExpression* f);
AstExpression *createBinaryExpression(int op, AstExpression *left, AstExpression *right);
AstExpression *createUnaryExpression(int op, AstExpression *argument);
AstExpression *createNameRef(const char *name);
AstExpression *createCallExpression(AstExpression *callee, Vector *arguments);
AstExpression *createFieldExpression(int op, AstExpression *receiver, const char *member);


// statemetns

AstStatement *createBlockStatement(Vector *stmts);
AstStatement *createExprStatement(AstExpression* expression);
AstStatement *createLabelStatement(int labelKind, AstStatement *body, const char *label, int c);
AstStatement *createDeclStatement(AstDeclaration *decl);
AstStatement *createIfStatement(AstExpression *cond, AstStatement *thenB, AstStatement *elseB);
AstStatement *createSwitchStatement(AstExpression *cond, AstStatement *body);
AstStatement *createLoopStatement(int kind, AstExpression *cond, AstStatement *body);
AstStatement *createForStatement(AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body);
AstStatement *createJumpStatement(int jumpKind);
AstStatement *createEmptyStatement();

#endif // __TREE_H__
