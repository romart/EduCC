
#include <malloc.h>
#include <memory.h>
#include <stdlib.h>
#include <assert.h>

#include "tree.h"
#include "parser.h"

// types

TypeDesc *createTypeDescriptor(int typeId, const char *name, int size) {
  TypeDesc *result = (TypeDesc*)malloc(sizeof(TypeDesc));
  if (result == NULL) exit(-5);

  memset(result, 0, sizeof(TypeDesc));
  result->typeId = typeId;
  result->name = name;
  result->size = size;

  return result;
}

// declarations

EnumConstant *createEnumConst(const char* name, int value) {
    EnumConstant* result = (EnumConstant*)malloc(sizeof(EnumConstant));
    if (result == NULL) exit(-5);

    result->name = name;
    result->value = value;

    return result;
}

AstStructDeclarator* createStructDeclarator(DeclarationSpecifiers *specifiers, Declarator* declarator, int width) {
    AstStructDeclarator* result = (AstStructDeclarator*)malloc(sizeof(AstStructDeclarator));
    if (result == NULL) exit(-5);
    memset(result, 0, sizeof(AstStructDeclarator));

    result->f_width = width;

    return result;
}

AstStructDeclaration *createStructDeclaration(int token, const char *name, Vector *members) {
    AstStructDeclaration *result = (AstStructDeclaration*)malloc(sizeof(AstStructDeclaration));
    if (result == NULL) exit(-5);
    memset(result, 0, sizeof(AstStructDeclaration));

    result->token = token;
    result->name = name;
    result->members = members;

    return result;
}

AstEnumDeclaration *createEnumDeclaration(const char *name, Vector *enumerators) {
    AstEnumDeclaration *result = (AstEnumDeclaration*)malloc(sizeof(AstEnumDeclaration));
    if (result == NULL) exit(-5);
    memset(result, 0, sizeof(AstStructDeclaration));

    result->name = name;
    result->enumerators = enumerators;

    return result;
}

ParameterDeclaration *createParameterDeclaration(ParserContext *ctx, TypeRef *type, const char *name, int index) {
    ParameterDeclaration *result = (ParameterDeclaration *)malloc(sizeof (ParameterDeclaration));
    if (result == NULL) exit(-5);
    memset(result, 0, sizeof(AstStructDeclaration));

    result->name = name;
    result->index = index;
    result->type = type;

    return result;
}

AstDeclaration *createAstDeclaration(TypeRef *type, const char *name, AstInitializer *initializer, unsigned flags) {
    AstDeclaration* result = (AstDeclaration*)malloc(sizeof(AstDeclaration));
    if (result == NULL) exit(-5);

    result->flags.storage = flags;
    result->name = name;
    result->type = type;
    result->initializer = initializer;

    return result;
}

AstInitializer *createAstInitializer(AstExpression *expr, Vector *initializers) {
    AstInitializer* result = (AstInitializer*)malloc(sizeof(AstInitializer));
    if (result == NULL) exit(-5);

    if (expr) {
      result->kind = IK_EXPRESSION;
      result->expression = expr;
    } else {
      result->kind = IK_LIST;
      result->initializers = initializers;
    }
    return result;
}

AstFile *createAstFile(int capacity) {

    AstFile* astFile = (AstFile*)malloc(sizeof(AstFile));
    if (astFile == NULL) exit(-5);

    astFile->fileName = NULL;
    astFile->declarations = createVector(capacity);

    return astFile;
}


Declaration *createVariableDeclaration(ParserContext *ctx, TypeRef *type, const char *name, AstInitializer *initializer, SpecifierFlags flags) {
    Declaration *result = (Declaration *)malloc(sizeof(Declaration));
    if (result == NULL) exit(5);
    memset(result, 0, sizeof(Declaration));

    result->isExternal = flags.bits.isExternal;
    result->isStatic = flags.bits.isStatic;
    result->kind = DECLK_VARIABLE_DECLARATION;
    result->variableDeclaration.variableType = type;
    result->variableDeclaration.initializer = initializer;
    result->name = name;

    return result;
}

Declaration *createFunctionDefinition(ParserContext *ctx, const char *name, FunctionTypeDescriptor *descriptor, SpecifierFlags flags) {
    Declaration *result = (Declaration *)malloc(sizeof(Declaration));
    if (result == NULL) exit(5);
    memset(result, 0, sizeof(Declaration));

    result->isExternal = flags.bits.isExternal;
    result->isStatic = flags.bits.isStatic;
    result->kind = DECLK_FUNCTION_DEFINITION;

    result->functionDefinition.declaration = descriptor;
    result->functionDefinition.body = NULL;

    return result;
}


static AstExpression *allocAstExpression() {
  AstExpression *result = (AstExpression *)malloc(sizeof(AstExpression));
  if (result == NULL) exit(-4);
  memset(result, 0, sizeof(AstExpression));
  return result;
}

static AstStatement *allocAstStatement() {
  AstStatement *result = (AstStatement *)malloc(sizeof(AstStatement));
  if (result == NULL) exit(-4);
  memset(result, 0, sizeof(AstStatement));
  return result;
}

AstExpression* createAstConst(int type, void* value) {
    AstExpression* result = allocAstExpression();
    result->op = E_CONST;
    result->constExpr.op = type;
    switch (type) {
        case EC_S_INT_CONST: result->constExpr.s = *(signed long long*)value; break;
        case EC_U_INT_CONST: result->constExpr.u = *(unsigned long long*)value; break;
        case EC_FLOAT_CONST: result->constExpr.f = *(float*)value; break;
        case EC_DOUBLE_CONST: result->constExpr.d = *(double*)value; break;
        case EC_STRING_LITERAL: result->constExpr.l = *(const char**)value; break;
    default: assert(0);
    }

    return result;
}



AstExpression *createCastExpression(TypeRef *typeRef, AstExpression *argument) {
    AstExpression *result = allocAstExpression();
    result->op = E_CAST;
    result->castExpr.type= typeRef;
    result->castExpr.argument = argument;
    return result;
}

AstExpression *createTernaryExpression(AstExpression *cond, AstExpression *t, AstExpression* f) {
    AstExpression *result = allocAstExpression();
    result->op = E_TERNARY;
    result->ternaryExpr.condition = cond;
    result->ternaryExpr.ifTrue = t;
    result->ternaryExpr.ifFalse = f;
    return result;
}

AstExpression *createBinaryExpression(int op, AstExpression *left, AstExpression *right) {
    AstExpression *result = allocAstExpression();
    result->op = op;
    result->binaryExpr.left = left;
    result->binaryExpr.right = right;
    return result;
}

AstExpression *createUnaryExpression(int op, AstExpression *argument) {
    AstExpression *result = allocAstExpression();
    result->op = op;
    result->unaryExpr.argument = argument;
    return result;
}

AstExpression *createNameRef(const char *name) {
    AstExpression *result = allocAstExpression();
    result->op = E_NAMEREF;
    result->nameRefExpr.name = name;
    return result;
}

AstExpression *createCallExpression(AstExpression *callee, Vector *arguments) {
    AstExpression *result = allocAstExpression();
    result->op = E_CALL;
    result->callExpr.callee = callee;
    result->callExpr.arguments = arguments;
    return result;
}

AstExpression *createFieldExpression(int op, AstExpression *receiver, const char *member) {
    AstExpression *result = allocAstExpression();
    result->op = op;
    result->fieldExpr.recevier = receiver;
    result->fieldExpr.member = member;
    return result;
}

// statements

AstStatement *createBlockStatement(Vector *stmts) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_BLOCK;
    result->block.stmts = stmts;

    return result;
}

AstStatement *createExprStatement(AstExpression* expression) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_EXPR_STMT;
    result->exprStmt.expression = expression;

    return result;
}

AstStatement *createLabelStatement(int labelKind, AstStatement *body, const char *label, int c) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_LABEL;
    result->labelStmt.kind = labelKind;
    if (label) {
      result->labelStmt.label = label;
    } else {
      result->labelStmt.caseConst = c;
    }
    result->labelStmt.body = body;

    return result;
}

AstStatement *createDeclStatement(AstDeclaration *decl) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_DECLARATION;
    result->declStmt.declaration = decl;

    return result;
}

AstStatement *createIfStatement(AstExpression *cond, AstStatement *thenB, AstStatement *elseB) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_IF;
    result->ifStmt.condition = cond;
    result->ifStmt.thenBranch = thenB;
    result->ifStmt.elseBranch = elseB;

    return result;
}

AstStatement *createSwitchStatement(AstExpression *cond, AstStatement *body) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_SWITCH;
    result->switchStmt.condition = cond;
    result->switchStmt.body = body;

    return result;
}

AstStatement *createLoopStatement(int kind, AstExpression *cond, AstStatement *body) {
    AstStatement *result = allocAstStatement();

    result->statementKind = kind;
    result->loopStmt.condition = cond;
    result->loopStmt.body = body;

    return result;
}

AstStatement *createForStatement(AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body) {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_FOR;
    result->forStmt.initial = init;
    result->forStmt.condition = cond;
    result->forStmt.modifier = modifier;
    result->forStmt.body = body;

    return result;
}

AstStatement *createJumpStatement(int jumpKind) {
    AstStatement *result = allocAstStatement();

    result->statementKind = jumpKind;

    return result;
}

AstStatement *createEmptyStatement() {
    AstStatement *result = allocAstStatement();

    result->statementKind = SK_EMPTY;

    return result;
}

