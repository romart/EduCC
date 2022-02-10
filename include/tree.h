

#ifndef __TREE_H__
#define __TREE_H__ 1

#include "utils.h"
#include "types.h"

typedef struct _ParserContext ParserContext;

typedef struct _Coordinates {
  int startOffset;
  int endOffset;
} Coordinates;

typedef enum _ExpressionType {
    E_CONST,
    E_TERNARY,
    E_CAST,
    E_NAMEREF,
    E_CALL,
    E_PAREN,      /** (expr) */
    E_ERROR,

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

    EB_ASSIGN
} ExpressionType;

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
    const char* member;
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

    SK_GOTO,
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
    DeclaratorPartKind kind;
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
    const char* identificator;
    unsigned partsCounter;
    DeclaratorPart declaratorParts[256];
    unsigned identificatorCounter;
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

    int f_width; // -1 if not specified;
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

EnumConstant *createEnumConst(ParserContext *ctx, int startOffset, int endOffset, const char* name, int64_const_t value);

AstInitializerList *createAstInitializerList(ParserContext *ctx);
AstInitializer *createAstInitializer(ParserContext *ctx, int startOffset, int endOffset, InitializerKind kind);
AstStructDeclarator *createStructDeclarator(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, int width);
AstStructMember* createStructMember(ParserContext *ctx, AstDeclaration *declaration, AstStructDeclarator *declarator, EnumConstant *enumerator);
AstSUEDeclaration *createSUEDeclaration(ParserContext *ctx, int startOffset, int endOffset, DeclarationKind kind, unsigned isDefinition, const char *name, AstStructMember *members);
AstFunctionDeclaration *createFunctionDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *returnType, const char *name, unsigned flags, AstValueDeclaration *parameters, Boolean isVariadic);
AstValueDeclaration *createAstValueDeclaration(ParserContext *ctx, int startOffset, int endOffset, ValueKind kind, TypeRef *type, const char *name, unsigned index, unsigned flags, AstInitializer *initializer);

AstDeclaration *createAstDeclaration(ParserContext *ctx, DeclarationKind kind, const char *name);
AstFunctionDefinition *createFunctionDefinition(ParserContext *ctx, AstFunctionDeclaration *declaration, struct _Scope *scope, AstStatement *body);

AstTranslationUnit *createTranslationUnit(ParserContext *ctx, AstDeclaration *declaration, AstFunctionDefinition *definition);

AstFile *createAstFile(ParserContext *ctx);

// expressions

AstExpression* createAstConst(ParserContext *ctx, int startOffset, int endOffset, ConstKind type, void* value);
AstExpression *createCastExpression(ParserContext *ctx, int startOffset, int endOffset, TypeRef *typeRef, AstExpression *argument);
AstExpression *createTernaryExpression(ParserContext *ctx, AstExpression *cond, AstExpression *t, AstExpression* f);
AstExpression *createBinaryExpression(ParserContext *ctx, ExpressionType op, AstExpression *left, AstExpression *right);
AstExpression *createUnaryExpression(ParserContext *ctx, int startOffset, int endOffset, ExpressionType op, AstExpression *argument);
AstExpression *createNameRef(ParserContext *ctx, int startOffset, int endOffset, const char *name, struct _Symbol *s);
AstExpression *createCallExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *callee, AstExpressionList *arguments);
AstExpression *createFieldExpression(ParserContext *ctx, int startOffset, int endOffset, ExpressionType op, AstExpression *receiver, const char *member);
AstExpression *createParenExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *parened);
AstExpression *createErrorExpression(ParserContext *ctx, int startOffset, int endOffset);

// statemetns

AstStatement *createBlockStatement(ParserContext *ctx, int startOffset, int endOffset, struct _Scope *scope, AstStatementList *stmts);
AstStatement *createExprStatement(ParserContext *ctx, AstExpression* expression);
AstStatement *createLabelStatement(ParserContext *ctx, int startOffset, int endOffset, LabelKind labelKind, AstStatement *body, const char *label, int c);
AstStatement *createDeclStatement(ParserContext *ctx, int startOffset, int endOffset, AstDeclaration *decl);
AstStatement *createIfStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *thenB, AstStatement *elseB);
AstStatement *createSwitchStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *body);
AstStatement *createLoopStatement(ParserContext *ctx, int startOffset, int endOffset, StatementKind kind, AstExpression *cond, AstStatement *body);
AstStatement *createForStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body);
AstStatement *createJumpStatement(ParserContext *ctx, int startOffset, int endOffset, StatementKind jumpKind);
AstStatement *createEmptyStatement(ParserContext *ctx, int startOffset, int endOffset);
AstStatement *createErrorStatement(ParserContext *ctx, int startOffset, int endOffset);

#endif // __TREE_H__
