
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

EnumConstant *createEnumConst(ParserContext *ctx, int startOffset, int endOffset, const char* name, int64_const_t value) {
    EnumConstant* result = (EnumConstant*)areanAllocate(ctx->astArena, sizeof(EnumConstant));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;
    result->name = name;
    result->value = value;

    return result;
}

AstStructMember* createStructMember(ParserContext *ctx, AstDeclaration *declaration, AstStructDeclarator *declarator) {
    AstStructMember* result = (AstStructMember*)areanAllocate(ctx->astArena, sizeof(AstStructMember));

    if (declaration) {
        assert(declarator == NULL);
        result->kind = SM_DECLARATION;
        result->declaration = declaration;
    } else {
        result->kind = SM_DECLARATOR;
        result->declarator = declarator;
    }

    return result;
}


AstStructDeclarator* createStructDeclarator(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, int width) {
    AstStructDeclarator* result = (AstStructDeclarator*)areanAllocate(ctx->astArena, sizeof(AstStructDeclarator));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;
    result->f_width = width;

    result->name = name;
    result->typeRef = type;

    return result;
}

AstSUEDeclaration *createSUEDeclaration(ParserContext *ctx, int startOffset, int endOffset, int kind, const char *name, Vector *members) {
    AstSUEDeclaration *result = (AstSUEDeclaration*)areanAllocate(ctx->astArena, sizeof(AstSUEDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->kind = kind;
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

AstValueDeclaration *createAstValueDeclaration(ParserContext *ctx, int startOffset, int endOffset, int kind, TypeRef *type, const char *name, unsigned index, unsigned flags, AstInitializer *initializer) {
    AstValueDeclaration *result = (AstValueDeclaration *)areanAllocate(ctx->astArena, sizeof (AstValueDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = startOffset;

    result->kind = kind;
    result->name = name;
    result->type = type;
    if (kind == VD_PARAMETER) {
      result->index = index;
    } else {
        result->flags.storage = flags;
        result->initializer = initializer;
    }

    return result;
}

AstDeclaration *createAstDeclaration(ParserContext *ctx, int kind, const char *name) {
    AstDeclaration* result = (AstDeclaration*)areanAllocate(ctx->astArena, sizeof(AstDeclaration));

    result->name = name;
    result->kind = kind;

    return result;
}

AstTranslationUnit *createTranslationUnit(ParserContext *ctx, AstDeclaration *declaration, AstFunctionDefinition *definition) {
   AstTranslationUnit* result = (AstTranslationUnit*)areanAllocate(ctx->astArena, sizeof(AstTranslationUnit));

   if (definition) {
       assert(declaration == NULL);
       result->kind = TU_FUNCTION_DEFINITION;
       result->definition = definition;
   } else {
       result->kind = TU_DECLARATION;
       result->declaration = declaration;
   }
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

AstFunctionDeclaration *createFunctionDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *returnType, const char *name, unsigned flags, unsigned parameterCount, AstValueDeclaration **parameters, int isVariadic) {
  AstFunctionDeclaration *result = (AstFunctionDeclaration *)areanAllocate(ctx->astArena, sizeof(AstFunctionDeclaration));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = endOffset;

  result->flags.storage = flags;
  result->name = name;

  result->returnType = returnType;
  result->parameterCount = parameterCount;
  result->parameters = parameters;
  result->isVariadic = isVariadic != 0;

  return result;
}

AstFunctionDefinition *createFunctionDefinition(ParserContext *ctx, AstFunctionDeclaration *declaration, AstStatement *body) {
    AstFunctionDefinition *result = (AstFunctionDefinition *)areanAllocate(ctx->astArena, sizeof(AstFunctionDefinition));

    result->declaration = declaration;
    result->body = body;

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
        case EC_INT_CONST: result->constExpr.i = *(int64_const_t*)value; break;
        case EC_FLOAT_CONST: result->constExpr.f = *(float64_const_t*)value; break;
        case EC_STRING_LITERAL: result->constExpr.l = *(literal_const_t*)value; break;
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

AstStatement *createDeclStatement(ParserContext *ctx, int startOffset, int endOffset, AstDeclaration *decl) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

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

