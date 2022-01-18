
#include <malloc.h>
#include <memory.h>
#include <stdlib.h>
#include <assert.h>

#include "tree.h"
#include "parser.h"

// types

TypeDesc *createTypeDescriptor(ParserContext *ctx, int typeId, const char *name, int size) {
  TypeDesc *result = (TypeDesc*)areanAllocate(ctx->astArena, sizeof(TypeDesc));

  result->typeId = typeId;
  result->name = name;
  result->size = size;

  return result;
}

// declarations

EnumConstant *createEnumConst(ParserContext *ctx, int startOffset, int endOffset, const char* name, int value) {
    EnumConstant* result = (EnumConstant*)areanAllocate(ctx->astArena, sizeof(EnumConstant));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;
    result->name = name;
    result->value = value;

    return result;
}

AstStructDeclarator* createStructDeclarator(ParserContext *ctx, int startOffset, int endOffset, DeclarationSpecifiers *specifiers, Declarator* declarator, int width) {
    AstStructDeclarator* result = (AstStructDeclarator*)areanAllocate(ctx->astArena, sizeof(AstStructDeclarator));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;
    result->f_width = width;

    return result;
}

AstStructDeclaration *createStructDeclaration(ParserContext *ctx, int startOffset, int endOffset, int token, const char *name, Vector *members) {
    AstStructDeclaration *result = (AstStructDeclaration*)areanAllocate(ctx->astArena, sizeof(AstStructDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->token = token;
    result->name = name;
    result->members = members;

    return result;
}

AstEnumDeclaration *createEnumDeclaration(ParserContext *ctx, int startOffset, int endOffset, const char *name, Vector *enumerators) {
    AstEnumDeclaration *result = (AstEnumDeclaration*)areanAllocate(ctx->astArena, sizeof(AstEnumDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->name = name;
    result->enumerators = enumerators;

    return result;
}

ParameterDeclaration *createParameterDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, int index) {
    ParameterDeclaration *result = (ParameterDeclaration *)areanAllocate(ctx->astArena, sizeof (ParameterDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->name = name;
    result->index = index;
    result->type = type;

    return result;
}

AstDeclaration *createAstDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, AstInitializer *initializer, unsigned flags) {
    AstDeclaration* result = (AstDeclaration*)areanAllocate(ctx->astArena, sizeof(AstDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->flags.storage = flags;
    result->name = name;
    result->type = type;
    result->initializer = initializer;

    return result;
}

AstInitializer *createAstInitializer(ParserContext *ctx, int startOffset, int endOffset, AstExpression *expr, Vector *initializers) {
    AstInitializer* result = (AstInitializer*)areanAllocate(ctx->astArena, sizeof(AstInitializer));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    if (expr) {
      result->kind = IK_EXPRESSION;
      result->expression = expr;
    } else {
      result->kind = IK_LIST;
      result->initializers = initializers;
    }
    return result;
}

AstFile *createAstFile(ParserContext *ctx, int capacity) {

    AstFile* astFile = (AstFile*)areanAllocate(ctx->astArena, sizeof(AstFile));

    astFile->fileName = NULL;
    astFile->declarations = createVector(capacity);

    return astFile;
}


Declaration *createVariableDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, AstInitializer *initializer, SpecifierFlags flags) {
    Declaration *result = (Declaration *)areanAllocate(ctx->astArena, sizeof(Declaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->isExternal = flags.bits.isExternal;
    result->isStatic = flags.bits.isStatic;
    result->kind = DECLK_VARIABLE_DECLARATION;
    result->variableDeclaration.variableType = type;
    result->variableDeclaration.initializer = initializer;
    result->name = name;

    return result;
}

Declaration *createFunctionDefinition(ParserContext *ctx, const char *name, FunctionTypeDescriptor *descriptor, SpecifierFlags flags) {
    Declaration *result = (Declaration *)areanAllocate(ctx->astArena, sizeof(Declaration));

    result->isExternal = flags.bits.isExternal;
    result->isStatic = flags.bits.isStatic;
    result->kind = DECLK_FUNCTION_DEFINITION;

    result->functionDefinition.declaration = descriptor;
    result->functionDefinition.body = NULL;

    return result;
}


static AstExpression *allocAstExpression(ParserContext *ctx, int startOffset, int endOffset) {
  AstExpression *result = (AstExpression *)areanAllocate(ctx->astArena, sizeof(AstExpression));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = startOffset;

  return result;
}

static AstStatement *allocAstStatement(ParserContext *ctx, int startOffset, int endOffset) {
  AstStatement *result = (AstStatement *)areanAllocate(ctx->astArena, sizeof(AstStatement));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = startOffset;

  return result;
}

AstExpression* createAstConst(ParserContext *ctx, int startOffset, int endOffset, int type, void* value) {
    AstExpression* result = allocAstExpression(ctx, startOffset, endOffset);
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



AstExpression *createCastExpression(ParserContext *ctx, int startOffset, int endOffset, TypeRef *typeRef, AstExpression *argument) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_CAST;
    result->castExpr.type= typeRef;
    result->castExpr.argument = argument;
    return result;
}

AstExpression *createTernaryExpression(ParserContext *ctx, AstExpression *cond, AstExpression *t, AstExpression* f) {
    AstExpression *result = allocAstExpression(ctx, cond->coordinates.startOffset, f->coordinates.endOffset);
    result->op = E_TERNARY;
    result->ternaryExpr.condition = cond;
    result->ternaryExpr.ifTrue = t;
    result->ternaryExpr.ifFalse = f;
    return result;
}

AstExpression *createBinaryExpression(ParserContext *ctx, int op, AstExpression *left, AstExpression *right) {
    AstExpression *result = allocAstExpression(ctx, left->coordinates.startOffset, right->coordinates.endOffset);
    result->op = op;
    result->binaryExpr.left = left;
    result->binaryExpr.right = right;
    return result;
}

AstExpression *createUnaryExpression(ParserContext *ctx, int startOffset, int endOffset, int op, AstExpression *argument) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = op;
    result->unaryExpr.argument = argument;
    return result;
}

AstExpression *createNameRef(ParserContext *ctx, int startOffset, int endOffset, const char *name) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_NAMEREF;
    result->nameRefExpr.name = name;
    return result;
}

AstExpression *createCallExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *callee, Vector *arguments) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_CALL;
    result->callExpr.callee = callee;
    result->callExpr.arguments = arguments;
    return result;
}

AstExpression *createFieldExpression(ParserContext *ctx, int startOffset, int endOffset, int op, AstExpression *receiver, const char *member) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = op;
    result->fieldExpr.recevier = receiver;
    result->fieldExpr.member = member;
    return result;
}

// statements

AstStatement *createBlockStatement(ParserContext *ctx, int startOffset, int endOffset, Vector *stmts) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_BLOCK;
    result->block.stmts = stmts;

    return result;
}

AstStatement *createExprStatement(ParserContext *ctx, AstExpression* expression) {
    AstStatement *result = allocAstStatement(ctx, expression->coordinates.startOffset, expression->coordinates.endOffset);

    result->statementKind = SK_EXPR_STMT;
    result->exprStmt.expression = expression;

    return result;
}

AstStatement *createLabelStatement(ParserContext *ctx, int startOffset, int endOffset, int labelKind, AstStatement *body, const char *label, int c) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

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

AstStatement *createDeclStatement(ParserContext *ctx, AstDeclaration *decl) {
    AstStatement *result = allocAstStatement(ctx, decl->coordinates.startOffset, decl->coordinates.endOffset);

    result->statementKind = SK_DECLARATION;
    result->declStmt.declaration = decl;

    return result;
}

AstStatement *createIfStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *thenB, AstStatement *elseB) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_IF;
    result->ifStmt.condition = cond;
    result->ifStmt.thenBranch = thenB;
    result->ifStmt.elseBranch = elseB;

    return result;
}

AstStatement *createSwitchStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression *cond, AstStatement *body) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_SWITCH;
    result->switchStmt.condition = cond;
    result->switchStmt.body = body;

    return result;
}

AstStatement *createLoopStatement(ParserContext *ctx, int startOffset, int endOffset, int kind, AstExpression *cond, AstStatement *body) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = kind;
    result->loopStmt.condition = cond;
    result->loopStmt.body = body;

    return result;
}

AstStatement *createForStatement(ParserContext *ctx, int startOffset, int endOffset, AstExpression* init, AstExpression *cond, AstExpression *modifier, AstStatement *body) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_FOR;
    result->forStmt.initial = init;
    result->forStmt.condition = cond;
    result->forStmt.modifier = modifier;
    result->forStmt.body = body;

    return result;
}

AstStatement *createJumpStatement(ParserContext *ctx, int startOffset, int endOffset, int jumpKind) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = jumpKind;

    return result;
}

AstStatement *createEmptyStatement(ParserContext *ctx, int startOffset, int endOffset) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_EMPTY;

    return result;
}

