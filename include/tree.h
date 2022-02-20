

#ifndef __TREE_H__
#define __TREE_H__ 1

#include "utils.h"
#include "types.h"

typedef struct _ParserContext ParserContext;

typedef struct _Coordinates {
  int startOffset;
  int endOffset;
} Coordinates;

#define EXPR_TYPES \
  DEF_EXPRESSION_OP(E_CONST, 17), \
  DEF_EXPRESSION_OP(E_TERNARY, 3), \
  DEF_EXPRESSION_OP(E_CAST, 14), \
  DEF_EXPRESSION_OP(E_NAMEREF, 17), \
  DEF_EXPRESSION_OP(E_CALL, 16), \
  DEF_EXPRESSION_OP(E_PAREN, 17), \
  DEF_EXPRESSION_OP(E_LABEL_REF, 15), \
  DEF_EXPRESSION_OP(E_ERROR, 17), \
  DEF_EXPRESSION_OP(EU_PRE_INC, 15), \
  DEF_EXPRESSION_OP(EU_POST_INC, 16), \
  DEF_EXPRESSION_OP(EU_PRE_DEC, 15), \
  DEF_EXPRESSION_OP(EU_POST_DEC, 16), \
  DEF_EXPRESSION_OP(EU_DEREF, 15), \
  DEF_EXPRESSION_OP(EU_REF, 15), \
  DEF_EXPRESSION_OP(EU_PLUS, 15), \
  DEF_EXPRESSION_OP(EU_MINUS, 15), \
  DEF_EXPRESSION_OP(EU_TILDA, 15), \
  DEF_EXPRESSION_OP(EU_EXL, 15), \
  DEF_EXPRESSION_OP(EB_ADD, 12), \
  DEF_EXPRESSION_OP(EB_SUB, 12), \
  DEF_EXPRESSION_OP(EB_MUL, 13), \
  DEF_EXPRESSION_OP(EB_DIV, 13), \
  DEF_EXPRESSION_OP(EB_MOD, 13), \
  DEF_EXPRESSION_OP(EB_LHS, 11), \
  DEF_EXPRESSION_OP(EB_RHS, 11), \
  DEF_EXPRESSION_OP(EB_AND, 8), \
  DEF_EXPRESSION_OP(EB_XOR, 7), \
  DEF_EXPRESSION_OP(EB_OR, 6), \
  DEF_EXPRESSION_OP(EB_ANDAND, 5), \
  DEF_EXPRESSION_OP(EB_OROR, 4), \
  DEF_EXPRESSION_OP(EB_EQ, 9), \
  DEF_EXPRESSION_OP(EB_NE, 9), \
  DEF_EXPRESSION_OP(EB_LT, 10), \
  DEF_EXPRESSION_OP(EB_LE, 10), \
  DEF_EXPRESSION_OP(EB_GT, 10), \
  DEF_EXPRESSION_OP(EB_GE, 10), \
  DEF_EXPRESSION_OP(EB_A_ACC, 16), \
  DEF_EXPRESSION_OP(EB_COMMA, 1), \
  DEF_EXPRESSION_OP(EF_DOT, 16), \
  DEF_EXPRESSION_OP(EF_ARROW, 16), \
  DEF_EXPRESSION_OP(EB_ASSIGN, 2), \
  DEF_EXPRESSION_OP(EB_ASG_ADD, 2), \
  DEF_EXPRESSION_OP(EB_ASG_SUB, 2), \
  DEF_EXPRESSION_OP(EB_ASG_MUL, 2), \
  DEF_EXPRESSION_OP(EB_ASG_DIV, 2), \
  DEF_EXPRESSION_OP(EB_ASG_MOD, 2), \
  DEF_EXPRESSION_OP(EB_ASG_SHL, 2), \
  DEF_EXPRESSION_OP(EB_ASG_SHR, 2), \
  DEF_EXPRESSION_OP(EB_ASG_AND, 2), \
  DEF_EXPRESSION_OP(EB_ASG_XOR, 2), \
  DEF_EXPRESSION_OP(EB_ASG_OR, 2)

typedef enum _ExpressionType {
#define DEF_EXPRESSION_OP(ENUM, PRIORITY) ENUM
  EXPR_TYPES,
#undef DEF_EXPRESSION_OP
  E_NUM_OF_OPS
} ExpressionType;

unsigned opPriority(ExpressionType op);

Boolean isCommute(ExpressionType op);
Boolean isBinary(ExpressionType op);
Boolean isAdditiveOp(ExpressionType op);
Boolean isAssignmentOp(ExpressionType op);
Boolean isMultiplicative(ExpressionType op);
Boolean isShiftOp(ExpressionType op);

typedef signed long long sint64_const_t;
typedef unsigned long long int64_const_t;
typedef double float64_const_t;
typedef const char *literal_const_t;

typedef enum _ConstKind {
  CK_INT_CONST,
  CK_FLOAT_CONST,
  CK_STRING_LITERAL
} ConstKind;

typedef struct ConstOp {
  ConstKind op;
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

typedef struct _AstExpressionList {
  struct _AstExpression *expression;
  struct _AstExpressionList *next;
} AstExpressionList;

typedef struct _AstCallExpression {
  struct _AstExpression* callee;
  AstExpressionList *arguments; // linked list
} AstCallExpression;

typedef struct _AstFieldExpression {
    struct _AstExpression* recevier;
    struct _AstStructDeclarator *member;
} AstFieldExpression;


typedef struct _AstExpression {
  Coordinates coordinates;
  ExpressionType op;
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
    struct _AstExpression *parened;
    const char* label;
  };
} AstExpression;

// statements

typedef enum _StatementKind {
    SK_BLOCK,
    SK_EXPR_STMT,
    SK_LABEL,
    SK_DECLARATION,
    SK_EMPTY,
    SK_ERROR,

    SK_IF,
    SK_SWITCH,

    SK_WHILE,
    SK_DO_WHILE,
    SK_FOR,

    SK_GOTO_L,
    SK_GOTO_P,
    SK_CONTINUE,
    SK_BREAK,
    SK_RETURN
} StatementKind;

typedef struct _AstStatementList {
  struct _AstStatement *stmt;
  struct _AstStatementList *next;
} AstStatementList;

typedef struct _AstBlock {
  struct _Scope *scope;
  AstStatementList *stmts; // LinkedList<AstStatement*>
} AstBlock;

typedef struct _AstExpressionStatement {
    AstExpression* expression;
} AstExpressionStatement;

typedef enum _LabelKind {
    LK_LABEL,
    LK_CASE,
    LK_DEFAULT
} LabelKind;

typedef struct _AstLabelStatement {
    LabelKind kind;
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
    StatementKind statementKind;
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

typedef enum _InitializerKind {
  IK_EXPRESSION,
  IK_LIST
} InitializerKind;

typedef struct _AstInitializerList {
  struct _AstInitializer *initializer;
  struct _AstInitializerList *next;
} AstInitializerList;

typedef struct _AstInitializer {
    Coordinates coordinates;
    InitializerKind kind; // expression | list of initializers
    union {
      AstExpression *expression;
      struct {
        AstInitializerList *initializerList;
        int numOfInitializers;
      };
    };
} AstInitializer;

typedef enum _DeclaratorPartKind {
    DPK_NONE = 0,
    DPK_POINTER,
    DPK_ARRAY,
    DPK_FUNCTION
} DeclaratorPartKind;

typedef struct _FunctionParams {
    struct _AstValueDeclaration *parameters; // linked list
    struct _Scope *scope;
    unsigned isVariadic : 1; // (int, int, ...)
} FunctionParams;

typedef struct _DeclaratorPart {
    Coordinates coordinates;
    DeclaratorPartKind kind;
    struct _DeclaratorPart *next;
    union {
      SpecifierFlags flags;
      int arraySize;
      FunctionParams parameters;
    };
} DeclaratorPart;

typedef enum _DeclaratorScope {
  DS_FILE,
  DS_PARAMETERS,
  DS_STRUCT,
  DS_STATEMENT,
  DS_CAST
} DeclaratorScope;

typedef struct _Declarator {
    Coordinates coordinates;
    Coordinates idCoordinates;
    const char* identificator;
    DeclaratorPart *declaratorParts;
    DeclaratorPart *functionDeclarator;
} Declarator;

typedef struct _EnumConstant {
    Coordinates coordinates;
    const char* name;
    int value;
} EnumConstant;

typedef struct _AstStructDeclarator {
    Coordinates coordinates;
    TypeRef *typeRef;
    const char *name;
    unsigned offset;
} AstStructDeclarator;

typedef enum _StructMember {
  SM_DECLARATOR,
  SM_DECLARATION,
  SM_ENUMERATOR
} StructMember;

typedef struct _AstStructMember {
  StructMember kind; // SM_DECLARATOR | SM_DECLARATION
  union {
    AstStructDeclarator *declarator;
    struct _AstDeclaration *declaration;
    EnumConstant *enumerator;
  };
  struct _AstStructMember *next;
} AstStructMember;

typedef enum _DeclarationKind {
  DK_ENUM,
  DK_STRUCT,
  DK_UNION,
  DK_TYPEDEF,
  DK_VAR,
  DK_PROTOTYPE
} DeclarationKind;

typedef struct _AstSUEDeclaration { // Struct | Union | Enum declaration
    Coordinates coordinates;
    DeclarationKind kind; // DK_UNION | DK_STRUCT | DK_ENUM
    const char *name;
    AstStructMember *members; // linked list
    unsigned isDefinition : 1;
} AstSUEDeclaration;

typedef struct _DeclarationSpecifiers {
  Coordinates coordinates;
  SpecifierFlags flags;
  TypeRef *basicType;
  AstSUEDeclaration *defined;
} DeclarationSpecifiers;

typedef enum _ValueKind {
  VD_PARAMETER,
  VD_VARIABLE
} ValueKind;

typedef struct _AstValueDeclaration {
  Coordinates coordinates;

  ValueKind kind;
  const char *name;
  TypeRef *type;
  SpecifierFlags flags;
  union {
    struct {
      unsigned index; // VD_PARAMETER
      struct _AstValueDeclaration* next; // Parameters are linked into list
    };
    AstInitializer *initializer; // VD_VARIABLE
  };
} AstValueDeclaration;

typedef struct _AstFunctionDeclaration {
  Coordinates coordinates;
  SpecifierFlags flags;
  const char *name;
  TypeRef *returnType;
  unsigned parameterCount;
  AstValueDeclaration *parameters; // linkedList
  unsigned isVariadic : 1;
} AstFunctionDeclaration;


typedef struct _AstDeclaration {
  DeclarationKind kind; // DK
  const char *name;
  union {
    AstSUEDeclaration *structDeclaration; // DK_ENUM | DK_STRUCT | DK_UNION
    struct {
      TypeRef *definedType; // DK_TYPEDEF
      Coordinates coordinates;
    } typeDefinition;
    AstValueDeclaration *variableDeclaration; // DK_VAR
    AstFunctionDeclaration *functionProrotype; // DK_PROTOTYPE
  };
} AstDeclaration;

typedef struct _AstFunctionDefinition { // _AstFunctionDefinition
  AstFunctionDeclaration *declaration;
  AstStatement *body;
  struct _Scope *scope;
} AstFunctionDefinition;

typedef enum _TranslationUnitKind {
  TU_DECLARATION,
  TU_FUNCTION_DEFINITION
} TranslationUnitKind;

typedef struct _AstTranslationUnit {
    TranslationUnitKind kind; // TU_DECLARATION | TU_FUNCTION_DEFINITION
    union {
      AstDeclaration *declaration;
      AstFunctionDefinition *definition;
    };
    struct _AstTranslationUnit *next; // linked list
} AstTranslationUnit;

typedef struct _AstFile {
  const char* fileName;
  AstTranslationUnit *units; // LinkedList<AstTranslationUnit>
  AstTranslationUnit *last; // LinkedList<AstTranslationUnit>
  struct _AstFile *next;
} AstFile;

AstExpression *deparen(AstExpression *expr);

//Factories

// types

TypeDesc *createTypeDescriptor(ParserContext *ctx, TypeId typeId, const char *name, int size);

// declarations

DeclaratorPart *allocateDeclaratorPart(ParserContext *ctx);

EnumConstant *createEnumConst(ParserContext *ctx, Coordinates *coords, const char* name, int64_const_t value);

AstInitializerList *createAstInitializerList(ParserContext *ctx);
AstInitializer *createAstInitializer(ParserContext *ctx, Coordinates *coords, InitializerKind kind);
AstStructDeclarator *createStructDeclarator(ParserContext *ctx, Coordinates *coords, TypeRef *type, const char *name, unsigned offset);
AstStructMember* createStructMember(ParserContext *ctx, AstDeclaration *declaration, AstStructDeclarator *declarator, EnumConstant *enumerator);
AstSUEDeclaration *createSUEDeclaration(ParserContext *ctx, Coordinates *coords, DeclarationKind kind, unsigned isDefinition, const char *name, AstStructMember *members);
AstFunctionDeclaration *createFunctionDeclaration(ParserContext *ctx, Coordinates *coords, TypeRef *returnType, const char *name, unsigned flags, AstValueDeclaration *parameters, Boolean isVariadic);
AstValueDeclaration *createAstValueDeclaration(ParserContext *ctx, Coordinates *coords, ValueKind kind, TypeRef *type, const char *name, unsigned index, unsigned flags, AstInitializer *initializer);

AstDeclaration *createAstDeclaration(ParserContext *ctx, DeclarationKind kind, const char *name);
AstFunctionDefinition *createFunctionDefinition(ParserContext *ctx, AstFunctionDeclaration *declaration, struct _Scope *scope, AstStatement *body);

AstTranslationUnit *createTranslationUnit(ParserContext *ctx, AstDeclaration *declaration, AstFunctionDefinition *definition);

AstFile *createAstFile(ParserContext *ctx);

// expressions

AstExpression* createAstConst(ParserContext *ctx, Coordinates *coords, ConstKind type, void* value);
AstExpression* createAstConst2(ParserContext *ctx, Coordinates *coords, TypeRef *type, AstConst *cnst);
AstExpression *createCastExpression(ParserContext *ctx, Coordinates *coords, TypeRef *typeRef, AstExpression *argument);
AstExpression *createTernaryExpression(ParserContext *ctx, TypeRef *type, AstExpression *cond, AstExpression *t, AstExpression* f);
AstExpression *createBinaryExpression(ParserContext *ctx, ExpressionType op, TypeRef *type, AstExpression *left, AstExpression *right);
AstExpression *createUnaryExpression(ParserContext *ctx, Coordinates *coords, ExpressionType op, AstExpression *argument);
AstExpression *createNameRef(ParserContext *ctx, Coordinates *coords, const char *name, struct _Symbol *s);
AstExpression *createCallExpression(ParserContext *ctx, Coordinates *coords, AstExpression *callee, AstExpressionList *arguments);
AstExpression *createFieldExpression(ParserContext *ctx, Coordinates *coords, ExpressionType op, AstExpression *receiver, AstStructDeclarator *member);
AstExpression *createParenExpression(ParserContext *ctx, Coordinates *coords, AstExpression *parened);
AstExpression *createLabelRefExpression(ParserContext *ctx, Coordinates *coords, const char *label);
AstExpression *createErrorExpression(ParserContext *ctx, Coordinates *coords);

// statemetns

AstStatement *createBlockStatement(ParserContext *ctx, Coordinates *coords, struct _Scope *scope, AstStatementList *stmts);
AstStatement *createExprStatement(ParserContext *ctx, AstExpression* expression);
AstStatement *createLabelStatement(ParserContext *ctx, Coordinates *coords, LabelKind labelKind, AstStatement *body, const char *label, int c);
AstStatement *createDeclStatement(ParserContext *ctx, Coordinates *coords, AstDeclaration *decl);
AstStatement *createIfStatement(ParserContext *ctx, Coordinates *coords, AstExpression *cond, AstStatement *thenB, AstStatement *elseB);
AstStatement *createSwitchStatement(ParserContext *ctx, Coordinates *coords, AstExpression *cond, AstStatement *body);
AstStatement *createLoopStatement(ParserContext *ctx, Coordinates *coords, StatementKind kind, AstExpression *cond, AstStatement *body);
AstStatement *createForStatement(ParserContext *ctx, Coordinates *coords, AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body);
AstStatement *createJumpStatement(ParserContext *ctx, Coordinates *coords, StatementKind jumpKind);
AstStatement *createEmptyStatement(ParserContext *ctx, Coordinates *coords);
AstStatement *createErrorStatement(ParserContext *ctx, Coordinates *coords);

#endif // __TREE_H__
