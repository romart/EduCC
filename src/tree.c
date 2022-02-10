
#include <memory.h>
#include <stdlib.h>
#include <assert.h>

#include "tree.h"
#include "parser.h"
#include "sema.h"
// types

TypeDesc *createTypeDescriptor(ParserContext *ctx, TypeId typeId, const char *name, int size) {
  TypeDesc *result = (TypeDesc*)areanAllocate(ctx->memory.typeArena, sizeof(TypeDesc));

  result->typeId = typeId;
  result->name = name;
  result->size = size;

  return result;
}

// declarations

EnumConstant *createEnumConst(ParserContext *ctx, int startOffset, int endOffset, const char* name, int64_const_t value) {
    EnumConstant* result = (EnumConstant*)areanAllocate(ctx->memory.astArena, sizeof(EnumConstant));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = endOffset;
    result->name = name;
    result->value = value;

    return result;
}

AstStructMember* createStructMember(ParserContext *ctx, AstDeclaration *declaration, AstStructDeclarator *declarator, EnumConstant *enumerator) {
    AstStructMember* result = (AstStructMember*)areanAllocate(ctx->memory.astArena, sizeof(AstStructMember));

    if (declaration) {
        assert(declarator == NULL);
        assert(enumerator == NULL);
        result->kind = SM_DECLARATION;
        result->declaration = declaration;
    } else if (declarator) {
        assert(enumerator == NULL);
        result->kind = SM_DECLARATOR;
        result->declarator = declarator;
    } else {
        result->kind = SM_ENUMERATOR;
        result->enumerator = enumerator;
    }

    return result;
}


AstStructDeclarator* createStructDeclarator(ParserContext *ctx, int startOffset, int endOffset, TypeRef *type, const char *name, int width) {
    AstStructDeclarator* result = (AstStructDeclarator*)areanAllocate(ctx->memory.astArena, sizeof(AstStructDeclarator));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = endOffset;
    result->f_width = width;

    result->name = name;
    result->typeRef = type;

    return result;
}

AstSUEDeclaration *createSUEDeclaration(ParserContext *ctx, int startOffset, int endOffset, DeclarationKind kind, Boolean isDefinition, const char *name, AstStructMember *members) {
    AstSUEDeclaration *result = (AstSUEDeclaration*)areanAllocate(ctx->memory.astArena, sizeof(AstSUEDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = endOffset;

    result->kind = kind;
    result->name = name;
    result->members = members;
    result->isDefinition = isDefinition;

    return result;
}

AstValueDeclaration *createAstValueDeclaration(ParserContext *ctx, int startOffset, int endOffset, ValueKind kind, TypeRef *type, const char *name, unsigned index, unsigned flags, AstInitializer *initializer) {
    AstValueDeclaration *result = (AstValueDeclaration *)areanAllocate(ctx->memory.astArena, sizeof (AstValueDeclaration));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = endOffset;

    result->kind = kind;
    result->name = name;
    result->type = type;
    result->flags.storage = flags;
    if (kind == VD_PARAMETER) {
      result->index = index;
    } else {
      result->initializer = initializer;
    }

    return result;
}

AstDeclaration *createAstDeclaration(ParserContext *ctx, DeclarationKind kind, const char *name) {
    AstDeclaration* result = (AstDeclaration*)areanAllocate(ctx->memory.astArena, sizeof(AstDeclaration));

    result->name = name;
    result->kind = kind;

    return result;
}

AstTranslationUnit *createTranslationUnit(ParserContext *ctx, AstDeclaration *declaration, AstFunctionDefinition *definition) {
   AstTranslationUnit* result = (AstTranslationUnit*)areanAllocate(ctx->memory.astArena, sizeof(AstTranslationUnit));

   if (definition) {
       assert(declaration == NULL);
       result->kind = TU_FUNCTION_DEFINITION;
       result->definition = definition;
   } else {
       result->kind = TU_DECLARATION;
       result->declaration = declaration;
   }
}

AstInitializerList *createAstInitializerList(ParserContext *ctx) {
    return (AstInitializerList*)areanAllocate(ctx->memory.astArena, sizeof(AstInitializerList));
}

AstInitializer *createAstInitializer(ParserContext *ctx, int startOffset, int endOffset, InitializerKind kind) {
    AstInitializer* result = (AstInitializer*)areanAllocate(ctx->memory.astArena, sizeof(AstInitializer));

    result->coordinates.startOffset = startOffset;
    result->coordinates.endOffset = endOffset;

    result->kind = kind;

    return result;
}

AstFile *createAstFile(ParserContext *ctx) {
    AstFile* astFile = (AstFile*)areanAllocate(ctx->memory.astArena, sizeof(AstFile));

    astFile->fileName = NULL;

    return astFile;
}

AstFunctionDeclaration *createFunctionDeclaration(ParserContext *ctx, int startOffset, int endOffset, TypeRef *returnType, const char *name, unsigned flags, AstValueDeclaration *parameters, Boolean isVariadic) {
  AstFunctionDeclaration *result = (AstFunctionDeclaration *)areanAllocate(ctx->memory.astArena, sizeof(AstFunctionDeclaration));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = endOffset;

  result->flags.storage = flags;
  result->name = name;

  result->returnType = returnType;
  result->parameters = parameters;
  result->isVariadic = isVariadic != FALSE;

  return result;
}

AstFunctionDefinition *createFunctionDefinition(ParserContext *ctx, AstFunctionDeclaration *declaration, struct _Scope *scope, AstStatement *body) {
    AstFunctionDefinition *result = (AstFunctionDefinition *)areanAllocate(ctx->memory.astArena, sizeof(AstFunctionDefinition));

    result->declaration = declaration;
    result->body = body;
    result->scope = scope;

    return result;
}


static AstExpression *allocAstExpression(ParserContext *ctx, int startOffset, int endOffset) {
  AstExpression *result = (AstExpression *)areanAllocate(ctx->memory.astArena, sizeof(AstExpression));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = endOffset;

  return result;
}

static AstStatement *allocAstStatement(ParserContext *ctx, int startOffset, int endOffset) {
  AstStatement *result = (AstStatement *)areanAllocate(ctx->memory.astArena, sizeof(AstStatement));

  result->coordinates.startOffset = startOffset;
  result->coordinates.endOffset = endOffset;

  return result;
}

AstExpression* createAstConst(ParserContext *ctx, int startOffset, int endOffset, ConstKind type, void* value) {
    AstExpression* result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_CONST;
    result->constExpr.op = type;
    switch (type) {
        case CK_INT_CONST: result->constExpr.i = *(int64_const_t*)value; break;
        case CK_FLOAT_CONST: result->constExpr.f = *(float64_const_t*)value; break;
        case CK_STRING_LITERAL: result->constExpr.l = *(literal_const_t*)value; break;
      default: unreachable("sizeof is not for here");
    }

    return result;
}

AstExpression *createParenExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *parened) {
  AstExpression* result = allocAstExpression(ctx, startOffset, endOffset);

  result->op = E_PAREN;
  result->parened = parened;
  result->type = parened->type;

  return result;
}

AstExpression *createErrorExpression(ParserContext *ctx, int startOffset, int endOffset) {
  AstExpression* result = allocAstExpression(ctx, startOffset, endOffset);

  result->op = E_ERROR;
  result->type = makeErrorRef(ctx);

  return result;
}

AstExpression *createCastExpression(ParserContext *ctx, int startOffset, int endOffset, TypeRef *typeRef, AstExpression *argument) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_CAST;
    result->type = result->castExpr.type = typeRef;
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

AstExpression *createBinaryExpression(ParserContext *ctx, ExpressionType op, AstExpression *left, AstExpression *right) {
    AstExpression *result = allocAstExpression(ctx, left->coordinates.startOffset, right->coordinates.endOffset);
    result->op = op;
    result->binaryExpr.left = left;
    result->binaryExpr.right = right;
    return result;
}

AstExpression *createUnaryExpression(ParserContext *ctx, int startOffset, int endOffset, ExpressionType op, AstExpression *argument) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = op;
    result->unaryExpr.argument = argument;
    return result;
}

AstExpression *createNameRef(ParserContext *ctx, int startOffset, int endOffset, const char *name, Symbol* s) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_NAMEREF;
    result->nameRefExpr.name = name;
    result->nameRefExpr.s = s;
    return result;
}

AstExpression *createCallExpression(ParserContext *ctx, int startOffset, int endOffset, AstExpression *callee, AstExpressionList *arguments) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = E_CALL;
    result->callExpr.callee = callee;
    result->callExpr.arguments = arguments;
    return result;
}

AstExpression *createFieldExpression(ParserContext *ctx, int startOffset, int endOffset, ExpressionType op, AstExpression *receiver, const char *member) {
    AstExpression *result = allocAstExpression(ctx, startOffset, endOffset);
    result->op = op;
    result->fieldExpr.recevier = receiver;
    result->fieldExpr.member = member;
    return result;
}

// statements

AstStatement *createBlockStatement(ParserContext *ctx, int startOffset, int endOffset, struct _Scope *scope, AstStatementList *stmts) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_BLOCK;
    result->block.stmts = stmts;
    result->block.scope = scope;

    return result;
}

AstStatement *createExprStatement(ParserContext *ctx, AstExpression* expression) {
    AstStatement *result = allocAstStatement(ctx, expression->coordinates.startOffset, expression->coordinates.endOffset);

    result->statementKind = SK_EXPR_STMT;
    result->exprStmt.expression = expression;

    return result;
}

AstStatement *createLabelStatement(ParserContext *ctx, int startOffset, int endOffset, LabelKind labelKind, AstStatement *body, const char *label, int c) {
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

AstStatement *createLoopStatement(ParserContext *ctx, int startOffset, int endOffset, StatementKind kind, AstExpression *cond, AstStatement *body) {
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

AstStatement *createJumpStatement(ParserContext *ctx, int startOffset, int endOffset, StatementKind jumpKind) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = jumpKind;

    return result;
}

AstStatement *createEmptyStatement(ParserContext *ctx, int startOffset, int endOffset) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_EMPTY;

    return result;
}

AstStatement *createErrorStatement(ParserContext *ctx, int startOffset, int endOffset) {
    AstStatement *result = allocAstStatement(ctx, startOffset, endOffset);

    result->statementKind = SK_ERROR;

    return result;
}

AstExpression *deparen(AstExpression *expr) {
  if (expr->op == E_PAREN) return deparen(expr->parened);
  return expr;
}
