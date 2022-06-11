

#include <assert.h>

#include "tokens.h"
#include "tree.h"
#include "pp.h"
#include "parser.h"
#include "mem.h"
#include "sema.h"
#include "codegen.h"

#include "treeDump.h"
#include "diagnostics.h"

#define INITIAL_FILE_CAPACITY 30

extern TypeDesc *errorTypeDescriptor;
extern TypeDesc builtInTypeDescriptors[];

static Boolean nextTokenIf(ParserContext *ctx, int nextIf) {
    if (ctx->token->code == nextIf) {
        nextToken(ctx);
        return TRUE;
    }
    return FALSE;
}

static void reportUnexpectedToken(ParserContext *ctx, int expected) {

  Token *t = ctx->token;

  int actual = t->code;

  Coordinates coords = { t, t };
  char *b = strndup(t->pos, t->length);
  reportDiagnostic(ctx, DIAG_UNEXPECTED_TOKEN, &coords, actual, b, expected);
  free(b);
}

static void expect(ParserContext *ctx, int token) {
    int next = nextToken(ctx)->code;
    if (next && token != next) {
        reportUnexpectedToken(ctx, token);
    }
}

static void consume(ParserContext *ctx, int expected) {
    int token = ctx->token->code;
    if (token && token != expected) {
        reportUnexpectedToken(ctx, expected);
    }
    nextToken(ctx);
}

static void consumeRaw(ParserContext *ctx, int expected) {
    int token = ctx->token->rawCode;
    if (token && token != expected) {
        reportUnexpectedToken(ctx, expected);
    }
    nextToken(ctx);
}

static void parseDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, DeclaratorScope scope);
static void parseDeclarator(ParserContext *ctx, Declarator *declarator);

static void verifyDeclarator(ParserContext *ctx, Declarator *declarator, DeclaratorScope scope);
static Boolean verifyDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, DeclaratorScope scope);

/**
assignment_operator
    : '='
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | LEFT_ASSIGN
    | RIGHT_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
 */
static Boolean isAssignmentOperator(int token) {
    return token == '=' || token == MUL_ASSIGN || token == DIV_ASSIGN || token == MOD_ASSIGN || token == ADD_ASSIGN ||
            token == SUB_ASSIGN || token == LEFT_ASSIGN || token == RIGHT_ASSIGN || token == AND_ASSIGN ||
            token == XOR_ASSIGN || token == OR_ASSIGN ? TRUE : FALSE;
}

static ExpressionType assignOpTokenToEB(int token) {
    switch (token) {
        case '=': return EB_ASSIGN;
        case MUL_ASSIGN: return EB_ASG_MUL;
        case DIV_ASSIGN: return EB_ASG_DIV;
        case MOD_ASSIGN: return EB_ASG_MOD;
        case ADD_ASSIGN: return EB_ASG_ADD;
        case SUB_ASSIGN: return EB_ASG_SUB;
        case LEFT_ASSIGN: return EB_ASG_SHL;
        case RIGHT_ASSIGN: return EB_ASG_SHR;
        case AND_ASSIGN: return EB_ASG_AND;
        case XOR_ASSIGN: return EB_ASG_XOR;
        case OR_ASSIGN: return EB_ASG_OR;
    }

    unreachable("Unepxected token");
    return (ExpressionType)-1;
}

static ExpressionType assignOpTokenToOp(int token) {
    switch (token) {
        case MUL_ASSIGN: return EB_MUL;
        case DIV_ASSIGN: return EB_DIV;
        case MOD_ASSIGN: return EB_MOD;
        case ADD_ASSIGN: return EB_ADD;
        case SUB_ASSIGN: return EB_SUB;
        case LEFT_ASSIGN: return EB_LHS;
        case RIGHT_ASSIGN: return EB_RHS;
        case AND_ASSIGN: return EB_AND;
        case XOR_ASSIGN: return EB_XOR;
        case OR_ASSIGN: return EB_OR;
    }

    unreachable("Unepxected token");
    return (ExpressionType)-1;
}

/**
type_qualifier
    : CONST
    | VOLATILE
    ;
 */
static int isTypeQualifierToken(int token) {
    return token == CONST || token == VOLATILE;
}

/**
storage_class_specifier
    : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    ;

 */
static int isStorageClassToken(int token) {
    return token == TYPEDEF || token == EXTERN || token == STATIC || token == REGISTER;
}

/**
type_specifier
    : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | SIGNED
    | UNSIGNED
    | struct_or_union_specifier
    | enum_specifier
    | TYPE_NAME
    ;
 */
static int isTypeSpecifierToken(int token) {
    return token == VOID || token == CHAR || token == SHORT || token == INT || token == LONG ||
           token == FLOAT || token == DOUBLE || token == SIGNED || token == UNSIGNED ||
           token == STRUCT || token == UNION || token == ENUM || token == TYPE_NAME;
}


/**

declaration_specifiers
    : storage_class_specifier
    | storage_class_specifier declaration_specifiers
    | type_specifier
    | type_specifier declaration_specifiers
    | type_qualifier
    | type_qualifier declaration_specifiers
    ;
 */
static int isDeclarationSpecifierToken(int token) {
    return isTypeQualifierToken(token) || isStorageClassToken(token) || isTypeSpecifierToken(token);
}

/**
specifier_qualifier_list
    : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    ;
 */
static int isSpecifierQualifierList(int token) {
    return isTypeQualifierToken(token) || isTypeSpecifierToken(token);
}

// Expression parser

static AstExpression* parseExpression(ParserContext *ctx, struct _Scope* scope);
static AstExpression* parseCastExpression(ParserContext *ctx, struct _Scope* scope);
static AstExpression* parseAssignmentExpression(ParserContext *ctx, struct _Scope* scope);

static AstConst* parseConstExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* expression = parseConditionalExpression(ctx, scope);
    AstConst *constExpr = eval(ctx, expression);

    if (constExpr == NULL) {
        reportDiagnostic(ctx, DIAG_EXPECTED_CONST_EXPR, &expression->coordinates);
    }

    return constExpr;
}

static Boolean parseAsIntConst(ParserContext *ctx, int64_t *result) {
    AstExpression* expression = parseConditionalExpression(ctx, NULL);
    AstConst *constExpr = eval(ctx, expression);
    if (constExpr == NULL) {
        reportDiagnostic(ctx, DIAG_EXPECTED_CONST_EXPR, &expression->coordinates);
        return FALSE;
    }
    if (constExpr->op != CK_INT_CONST) {
        reportDiagnostic(ctx, DIAG_EXPECTED_INTEGER_CONST_EXPR, &expression->coordinates);
        return FALSE;
    }

    *result = (int)constExpr->i;
    return TRUE;
}

static AstExpression *resolveNameRef(ParserContext *ctx) {
  Token *t = ctx->token;
  Coordinates coords = { t, t };
  Symbol *s = findSymbol(ctx, t->id);

  if (s) {
    assert(s->kind == FunctionSymbol || s->kind == ValueSymbol);
    AstExpression *result = createNameRef(ctx, &coords, t->id, s);

    SpecifierFlags flags = { 0 };

    if (s->kind == ValueSymbol) {
        TypeRef *type = s->variableDesc->type;

        if (type->kind == TR_ARRAY) {
            flags.bits.isConst = 1;
            result->type = makePointedType(ctx, flags.storage, type->arrayTypeDesc.elementType);
            result->type->pointedTo.arrayType = type;
        } else {
            result->type = makePointedType(ctx, flags.storage, type);
            result = createUnaryExpression(ctx, &coords, EU_DEREF, result);
            result->type = type;
        }
    } else {
        assert(s->kind == FunctionSymbol);
        flags.bits.isConst = 1;
        result->type = makePointedType(ctx, flags.storage, computeFunctionType(ctx, &coords, s->function));
    }

    return result;
  }

  reportDiagnostic(ctx, DIAG_UNDECLARED_ID_USE, &coords, t->id);
  return createErrorExpression(ctx, &coords);
}

static TypeRef* parseTypeName(ParserContext *ctx, struct _Scope *scope, DeclaratorScope ds_scope);

static AstExpression *va_arg_expression(ParserContext *ctx, Coordinates *coords, AstExpression *va_list_Arg, TypeRef *typeArg) {
  TypeRef *va_list_Type = va_list_Arg->type;

  if (!is_va_list_Type(va_list_Type)) {
      reportDiagnostic(ctx, DIAG_FIRST_VA_ARG_NOT_VA_LIST, &va_list_Arg->coordinates, va_list_Type);
      return createErrorExpression(ctx, coords);
  }

  if (isErrorType(typeArg)) {
      return createErrorExpression(ctx, coords);
  }

  AstExpression *vaarg = createVaArgExpression(ctx, coords, va_list_Arg, typeArg);
  vaarg->type = makePointedType(ctx, 0U, typeArg);

  AstExpression *result = createUnaryExpression(ctx, coords, EU_DEREF, vaarg);
  result->type = typeArg;

  return result;
}

/**
primary_expression
    : IDENTIFIER
    | CONSTANT
    | STRING_LITERAL
--    | '__builtin_va_start' '(' IDENTIFIER ',' IDENTIFIER ')'
    | '__builtin_va_arg' '(' IDENTIFIER ',' type_name ')'
    | '(' expression ')'
    ;
 */
static AstExpression* parsePrimaryExpression(ParserContext *ctx, struct _Scope *scope) {
    AstExpression *result = NULL;
    SpecifierFlags flags = { 0 };
    flags.bits.isConst = 1;
    Coordinates coords = { ctx->token, ctx->token };
    TypeId typeId = T_ERROR;
    switch (ctx->token->code) {
        case IDENTIFIER:
            if (strcmp("__builtin_va_arg", ctx->token->id) == 0) {
              nextToken(ctx);
              consume(ctx, '(');
              AstExpression *valist = parseAssignmentExpression(ctx, scope);
              consume(ctx, ',');
              TypeRef* vatype = parseTypeName(ctx, scope, DS_VA_ARG);
              coords.right = ctx->token;
              consume(ctx, ')');
              return va_arg_expression(ctx, &coords, valist, vatype);
            } else {
              result = resolveNameRef(ctx);
            }
            break;
        case TYPE_NAME: {
            reportDiagnostic(ctx, DIAG_UNEXPECTED_TYPE_NAME_EXPR, &coords, ctx->token->id);
            result = createErrorExpression(ctx, &coords);
          }
          break;
        case C_CONSTANT: typeId = T_S1; goto iconst;
        case C16_CONSTANT: typeId = T_S2; goto iconst;
        case ENUM_CONST: //enum constant is int32_t aka T_S4
        case I_CONSTANT: typeId = T_S4; goto iconst;
        case U_CONSTANT: typeId = T_U4; goto iconst;
        case L_CONSTANT: typeId = T_S8; goto iconst;
        case UL_CONSTANT: typeId = T_U8; goto iconst;
        iconst: {
            int64_t l = ctx->token->value.iv;
            result = createAstConst(ctx, &coords, CK_INT_CONST, &l);
            result->type = makePrimitiveType(ctx, typeId, flags.storage);
            break;
        }
        case F_CONSTANT: typeId = T_F4; goto fconst;
        case D_CONSTANT: typeId = T_F8; goto fconst;
        fconst: {
            float64_const_t f = ctx->token->value.dv;
            result = createAstConst(ctx, &coords, CK_FLOAT_CONST, &f);
            result->type = makePrimitiveType(ctx, typeId, flags.storage);
            break;
        }
        case STRING_LITERAL: {
            // compound string literal
            Token *first = ctx->token;
            int code = -1;
            unsigned length = 0;
            Token *last = NULL;

            while (ctx->token->code == STRING_LITERAL) {
                last = ctx->token;
                length += strlen(ctx->token->value.text);
                nextToken(ctx);
            }

            coords.right = last;

            char *buffer = allocateString(ctx, length + 1);
            const char *literal = buffer;

            while (first != last->next) {
                unsigned l = strlen(first->value.text);
                strncpy(buffer, first->value.text, l);
                buffer += l;
                first = first->next;
            }

            result = createAstConst(ctx, &coords, CK_STRING_LITERAL, &literal);
            result->type = makeArrayType(ctx, length + 1, makePrimitiveType(ctx, T_S1, 0));
            return result;
        }
        case 0:
          return createErrorExpression(ctx, &coords);
        default: {
            consume(ctx, '(');
            AstExpression* expr = parseExpression(ctx, scope);
            coords.right = ctx->token;
            consume(ctx, ')');

            return createParenExpression(ctx, &coords, expr);
        }
    }

    nextToken(ctx);

    return result;
}

/**
argument_expression_list
    : assignment_expression (',' assignment_expression)*
    ;
 */
static AstExpressionList *parseArgumentExpressionList(ParserContext *ctx, struct _Scope *scope) {
  AstExpressionList head = { 0 } , *tail = &head;

    do {
      AstExpression *expr = parseAssignmentExpression(ctx, scope);
      AstExpressionList *node = (AstExpressionList*)areanAllocate(ctx->memory.astArena, sizeof(AstExpressionList));
      node->prev = tail;
      node->expression = expr;
      tail = tail->next = node;
    } while (nextTokenIf(ctx, ','));

    head.next->prev = NULL;

    return head.next;
}


/**
type_name
    : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    ;
 */
static TypeRef* parseTypeName(ParserContext *ctx, struct _Scope *scope, DeclaratorScope ds_scope) {
    DeclarationSpecifiers specifiers = { 0 };
    Declarator declarator= { 0 };
    specifiers.coordinates.left = specifiers.coordinates.right = ctx->token;
    parseDeclarationSpecifiers(ctx, &specifiers, ds_scope);

    if (ctx->token->code != ')') {
        declarator.coordinates.left = declarator.coordinates.right = ctx->token;
        parseDeclarator(ctx, &declarator);
        verifyDeclarator(ctx, &declarator, ds_scope);
    }
    if (isErrorType(specifiers.basicType)) {
        reportDiagnostic(ctx, DIAG_UNKNOWN_TYPE_NAME, &specifiers.coordinates, declarator.identificator);
    }

    return makeTypeRef(ctx, &specifiers, &declarator);
}

/**
postfix_expression
    : primary_expression
      ('[' expression ']' | '(' argument_expression_list? ')' | '.' IDENTIFIER | PTR_OP IDENTIFIER | INC_OP | DEC_OP)*
    ;
 */
static AstExpression* parsePostfixExpression(ParserContext *ctx, struct _Scope *scope) {
    AstExpression *left = parsePrimaryExpression(ctx, scope);
    AstExpression *right = NULL, *tmp = NULL;

    Coordinates coords = { 0 };

    ExpressionType op;

    for (;;) {
        AstExpressionList *arguments = NULL;
        coords = left->coordinates;
        switch (ctx->token->code) {
        case '[': // '[' expression ']'
            nextToken(ctx);
            right = parseExpression(ctx, scope);
            coords.right = ctx->token;
            consume(ctx, ']');
            TypeRef *arrayType = left->type;
            TypeRef *indexType = right->type;
            TypeRef *exprType = computeArrayAccessExpressionType(ctx, &coords, arrayType, indexType);
            left = createBinaryExpression(ctx, EB_A_ACC, exprType, left, right);
            break;
        case '(': // '(' argument_expression_list? ')'
            nextToken(ctx);
            TypeRef *calleeType = left->type;
            coords.right  = ctx->token;
            if (ctx->token->code != ')') {
                arguments = parseArgumentExpressionList(ctx, scope);
                coords.right  = ctx->token;
                verifyAndTransformCallAruments(ctx, &coords, calleeType, arguments);
            }
            coords.right  = ctx->token;
            consume(ctx, ')');
            left = createCallExpression(ctx, &coords, left, arguments);
            left->type = computeFunctionReturnType(ctx, &coords, calleeType);
            break;
        case '.':    op = EF_DOT; goto acc;// '.' IDENTIFIER
        case PTR_OP: op = EF_ARROW; // PTR_OP IDENTIFIER
        acc:
            nextToken(ctx);
            const char *id = ctx->token->id;
            consumeRaw(ctx, IDENTIFIER);
            coords.right  = ctx->token;
            TypeRef *receiverType = left->type;
            AstStructDeclarator *declarator = computeMemberDeclarator(ctx, &coords, receiverType, id, op);
            if (declarator) {
              left = createFieldExpression(ctx, &coords, op, left, declarator);
            } else {
              left = createErrorExpression(ctx, &coords);
            }
            break;
        case INC_OP: op = EU_POST_INC; goto incdec;
        case DEC_OP: op = EU_POST_DEC;
        incdec:
            coords.right  = ctx->token;
            TypeRef *argType = left->type;
            tmp = createUnaryExpression(ctx, &coords, op, left);
            tmp->type = computeIncDecType(ctx, &coords, argType, op == EU_POST_DEC);
            coords.left = coords.right;
            if (!isErrorType(tmp->type)) checkExpressionIsAssignable(ctx, &coords, left, TRUE);
            nextToken(ctx);
            return tmp;
        default: return left;
        }
    }
}

static AstExpression *createUnaryIncDecExpression(ParserContext *ctx, Coordinates *coords, AstExpression *arg, TypeRef *type, ExpressionType op) {
  if (isErrorType(type)) return createErrorExpression(ctx, coords);

  AstExpression *offset = NULL;

  if (isRealType(type)) {
    double d = 1.0;
    offset = createAstConst(ctx, coords, CK_FLOAT_CONST, &d);
    offset->type = type;
  } else if (isPointerLikeType(type)) {
    assert(type->kind == TR_POINTED);
    TypeRef *ptr= type->pointedTo.toType;
    int64_t typeSize = isVoidType(ptr) ? 1 : computeTypeSize(type->pointedTo.toType);
    assert(typeSize != UNKNOWN_SIZE);
    offset = createAstConst(ctx, coords, CK_INT_CONST, &typeSize);
    offset->type = makePrimitiveType(ctx, T_S8, 0);
  } else {
    int64_t i = 1LL;
    offset = createAstConst(ctx, coords, CK_INT_CONST, &i);
    offset->type = type;
  }

  return createBinaryExpression(ctx, op, type, arg, offset);
}

static void useLabelExpr(ParserContext *ctx, AstExpression *expr, AstStatement *stmt, const char *label) {
  DefinedLabel *l = ctx->labels.definedLabels;

  while (l) {
      if (strcmp(l->label->label, label) == 0) {
          return;
      }
      l = l->next;
  }

  UsedLabel *used = heapAllocate(sizeof (UsedLabel));
  if (expr) {
      assert(stmt == 0);
      used->kind = LU_REF_USE;
      used->labelRef = expr;
  } else {
      assert(stmt != 0);
      used->kind = LU_GOTO_USE;
      used->gotoStatement = stmt;
  }
  used->label = label;
  used->next = ctx->labels.usedLabels;
  ctx->labels.usedLabels = used;
}

/**
unary_expression
    : postfix_expression
    | (INC_OP | DEC_OP) unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')'
    | ALIGNOF '(' type_name ')'
    | ALIGNOF unary_expression
    | AND_OP ID
    ;
 */
static AstExpression* parseUnaryExpression(ParserContext *ctx, struct _Scope* scope) {
    ExpressionType op;
    AstExpression* argument = NULL;
    AstExpression* result = NULL;
    const char *label = NULL;
    Coordinates coords = { ctx->token, ctx->token };
    int32_t code = ctx->token->code;

    switch (code) {
        case AND_OP: // &&label
            consume(ctx, AND_OP);
            label = ctx->token->id;
            coords.right = ctx->token;
            // TODO actually we should consume either TYPE_NAME or IDENTIFIER
            consumeRaw(ctx, IDENTIFIER);
            result = createLabelRefExpression(ctx, &coords, label);
            useLabelExpr(ctx, result, NULL, label);
            return result;
        case INC_OP: op = EB_ASG_ADD; goto ue1;
        case DEC_OP: op = EB_ASG_SUB;
        ue1:
            nextToken(ctx);
            argument = parseUnaryExpression(ctx, scope);
            TypeRef *type = computeIncDecType(ctx, &coords, argument->type, op == EB_ASG_SUB);
            if (!isErrorType(type)) checkExpressionIsAssignable(ctx, &coords, argument, TRUE);
            coords.right = argument->coordinates.right;
            return createUnaryIncDecExpression(ctx, &coords, argument, type, op);
        case '&': op = EU_REF; goto ue2;
        case '*': op = EU_DEREF; goto ue2;
        case '+': op = EU_PLUS; goto ue2;
        case '-': op = EU_MINUS; goto ue2;
        case '~': op = EU_TILDA; goto ue2;
        case '!': op = EU_EXL;
        ue2:
            nextToken(ctx);
            argument = parseCastExpression(ctx, scope);
            coords.right = argument->coordinates.right;
            if (op == EU_REF) {
                if (argument->op == EU_DEREF) {
                  argument = argument->unaryExpr.argument;
                }
                if (argument->op == E_NAMEREF) {
                    Symbol *s = argument->nameRefExpr.s;
                    if (s) {
                        if (s->kind == ValueSymbol) {
                            if (s->variableDesc->flags.bits.isRegister) {
                                // register int x;
                                // int *y = &x;
                                reportDiagnostic(ctx, DIAG_REGISTER_ADDRESS, &coords);
                            }
                            return argument;
                        } else if (s->kind == FunctionSymbol) {
                            if (argument->type->kind == TR_POINTED) {
                                assert(argument->type->pointedTo.toType->kind == TR_FUNCTION);
                                // we are done here
                                return argument;
                            }
                        }
                    } else {
                        unreachable("Very suspissios, symbol is NULL");
                    }
                } else if (argument->op == EF_ARROW || argument->op == EF_DOT) {
                    TypeRef *fieldType = argument->fieldExpr.member->typeRef;
                    if (fieldType->kind == TR_BITFIELD) {
                        reportDiagnostic(ctx, DIAG_BIT_FIELD_ADDRESS, &coords);
                    }
                }
                checkRefArgument(ctx, &coords, argument, TRUE);
            } else if (op == EU_DEREF && argument->type->kind == TR_FUNCTION) {
                return argument;
            }
            result = createUnaryExpression(ctx, &coords, op, argument);
            result->type = computeTypeForUnaryOperator(ctx, &coords, argument->type, op);
            return result;
        case ALIGNOF:
        case SIZEOF: {
            Token *saved = nextToken(ctx);
            int token = saved->code;
            TypeRef *sizeType = NULL;
            if (token == '(') {
                token = nextToken(ctx)->code;
                if (isDeclarationSpecifierToken(token)) {
                    argument = NULL;
                    sizeType = parseTypeName(ctx, scope, DS_SIZEOF);
                    coords.right = ctx->token;
                    consume(ctx, ')');
                } else {
                    ctx->token = saved;
                    argument = parseUnaryExpression(ctx, scope);
                    sizeType = argument->type;
                    coords.right = argument->coordinates.right;
                }
            } else {
                argument = parseUnaryExpression(ctx, scope);
                coords.right = argument->coordinates.right;
                sizeType = argument->type;
            }

            if (isErrorType(sizeType)) {
                return argument;
            } else {
                long long c = code == SIZEOF ? computeTypeSize(sizeType) : typeAlignment(sizeType);
                AstExpression *constVal = NULL;
                if (c >= 0) {
                    constVal = createAstConst(ctx, &coords, CK_INT_CONST, &c);
                    constVal->type = makePrimitiveType(ctx, T_U8, 0);
                } else {
                    reportDiagnostic(ctx, DIAG_SIZEOF_INCOMPLETE_TYPE, &coords, sizeType);
                    constVal = createErrorExpression(ctx, &coords);
                }

                if (argument) {
                  result = createBinaryExpression(ctx, EB_COMMA, constVal->type, argument, constVal);
                  result->type = constVal->type;
                  return result;
                } else {
                  return constVal;
                }
            }
        }
        default:
            return parsePostfixExpression(ctx, scope);
    }
}


/**
cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression
    ;
 */
static AstExpression* parseCastExpression(ParserContext *ctx, struct _Scope* scope) {

    if (ctx->token->code == '(') {
        Token * saved = ctx->token;
        Coordinates coords = { ctx->token, ctx->token };
        nextToken(ctx);
        if (isDeclarationSpecifierToken(ctx->token->code)) {
            TypeRef* typeRef = parseTypeName(ctx, scope, DS_CAST);
            coords.right = ctx->token;
            consume(ctx, ')');
            AstExpression* argument = parseCastExpression(ctx, scope);
            checkTypeIsCastable(ctx, &coords, typeRef, argument->type, TRUE);
            return createCastExpression(ctx, &coords, typeRef, argument);
        } else {
            ctx->token = saved;
            AstExpression *result = parseUnaryExpression(ctx, scope);
            return result;
        }
    } else {
        return parseUnaryExpression(ctx, scope);
    }
}

/**
multiplicative_expression
    : cast_expression ( ( '*' | '/' | '%' ) cast_expression )*
    ;
 */
static AstExpression* parseMultiplicativeExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseCastExpression(ctx, scope);

    int tokenCode = ctx->token->code;

    while (tokenCode == '*' || tokenCode == '/' || tokenCode == '%') {
        ExpressionType op = tokenCode == '*' ? EB_MUL : tokenCode == '/' ? EB_DIV : EB_MOD;
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseCastExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, op);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, op, resultType, result, tmp));
        tokenCode = ctx->token->code;
    }

    return result;
}

/**
additive_expression
    : multiplicative_expression ( ( '+' | '-' ) multiplicative_expression )*
    ;
 */
static AstExpression* parseAdditiveExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseMultiplicativeExpression(ctx, scope);

    int tokenCode = ctx->token->code;

    while (tokenCode == '+' || tokenCode == '-') {
        ExpressionType op = tokenCode == '+' ? EB_ADD : EB_SUB;
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseMultiplicativeExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, op);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, op, resultType, result, tmp));
        tokenCode = ctx->token->code;
    }

    return result;
}

/**
shift_expression
    : additive_expression ( ( LEFT_OP | RIGHT_OP ) additive_expression )*
    ;
 */
static AstExpression* parseShiftExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseAdditiveExpression(ctx, scope);
    int tokenCode = ctx->token->code;

    while (tokenCode == LEFT_OP || tokenCode == RIGHT_OP) {
        ExpressionType op = tokenCode == LEFT_OP ? EB_LHS : EB_RHS;
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseAdditiveExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, op);
        result = createBinaryExpression(ctx, op, resultType, result, tmp);
        tokenCode = ctx->token->code;
    }

    return result;
}

/**
relational_expression
    : shift_expression ( ( '<' | '>' | LE_OP | GE_OP ) shift_expression )*
    ;
 */

static Boolean isRelationalOperator(int token) {
    return token == '>' || token == '<' || token == LE_OP || token == GE_OP ? TRUE : FALSE;
}

static ExpressionType relationalTokenToOp(int token) {
    switch (token) {
        case '>': return EB_GT;
        case '<': return EB_LT;
        case LE_OP: return EB_LE;
        case GE_OP: return EB_GE;
    }

    return -1;
}

static AstExpression* parseRelationalExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseShiftExpression(ctx, scope);

    while (isRelationalOperator(ctx->token->code)) {
        ExpressionType op = relationalTokenToOp(ctx->token->code);
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseShiftExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, op);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, op, resultType, result, tmp));
    }

    return result;
}

/**
equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NE_OP relational_expression
    ;
 */
static Boolean isEqualityOperator(int token) {
    return token == EQ_OP || token == NE_OP ? TRUE : FALSE;
}

static ExpressionType equalityTokenToOp(int token) {
    switch (token) {
        case EQ_OP: return EB_EQ;
        case NE_OP: return EB_NE;
    }
    return -1;
}

static AstExpression* parseEqualityExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseRelationalExpression(ctx, scope);

    while (isEqualityOperator(ctx->token->code)) {
        int op = equalityTokenToOp(ctx->token->code);
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseRelationalExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, op);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, op, resultType, result, tmp));
    }

    return result;
}

/**
and_expression
    : equality_expression ('&' equality_expression)*
    ;
 */
static AstExpression* parseAndExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseEqualityExpression(ctx, scope);

    while (ctx->token->code == '&') {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseEqualityExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, EB_AND);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, EB_AND, resultType, result, tmp));
    }

    return result;
}

/**
exclusive_or_expression
    : and_expression ('^' and_expression)*
    ;
 */
static AstExpression* parseExcOrExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseAndExpression(ctx, scope);

    while (ctx->token->code == '^') {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseAndExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, EB_XOR);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, EB_XOR, resultType, result, tmp));
    }

    return result;
}

/**
inclusive_or_expression
    : exclusive_or_expression ('|' exclusive_or_expression)*
    ;
 */
static AstExpression* parseIncOrExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseExcOrExpression(ctx, scope);

    while (ctx->token->code == '|') {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseExcOrExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, EB_OR);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, EB_OR, resultType, result, tmp));
    }

    return result;
}

/**
logical_and_expression
    : inclusive_or_expression (AND_OP inclusive_or_expression)*
    ;
 */
static AstExpression* parseLogicalAndExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseIncOrExpression(ctx, scope);

    while (ctx->token->code == AND_OP) {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseIncOrExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, EB_ANDAND);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, EB_ANDAND, resultType, result, tmp));
    }

    return result;
}

/**
logical_or_expression
    : logical_and_expression (OR_OP logical_and_expression)*
    ;
 */
static AstExpression* parseLogicalOrExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* result = parseLogicalAndExpression(ctx, scope);

    while (ctx->token->code == OR_OP) {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* tmp = parseLogicalAndExpression(ctx, scope);
        coords.right = tmp->coordinates.right;
        TypeRef *resultType = computeBinaryType(ctx, &coords, result, tmp, EB_OROR);
        result = transformBinaryExpression(ctx, createBinaryExpression(ctx, EB_OROR, resultType, result, tmp));
    }

    return result;
}

/**
conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
 */
AstExpression* parseConditionalExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* left = parseLogicalOrExpression(ctx, scope);

    if (ctx->token->code == '?') {
        Coordinates coords = { ctx->token };
        nextToken(ctx);
        AstExpression* ifTrue = parseExpression(ctx, scope);
        consume(ctx, ':');
        AstExpression* ifFalse = parseConditionalExpression(ctx, scope);
        coords.right = ifFalse->coordinates.right;
        TypeRef *resultType = computeTernaryType(ctx, &coords, left->type, ifTrue->type, ifFalse->type, E_TERNARY);
        AstExpression *result = transformTernaryExpression(ctx, createTernaryExpression(ctx, resultType, left, ifTrue, ifFalse));
        return result;
    }

    return left;
}

/**
assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
    ;
 */
static AstExpression* parseAssignmentExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* left = parseConditionalExpression(ctx, scope);
    int tokenCode = ctx->token->code;
    if (isAssignmentOperator(tokenCode)) {
        Coordinates coords = { ctx->token, ctx->token };
        checkExpressionIsAssignable(ctx, &coords, left, FALSE);
        nextToken(ctx);
        AstExpression* right = parseAssignmentExpression(ctx, scope);
        ExpressionType op = assignOpTokenToEB(tokenCode);
        TypeRef *resultType = computeAssignmentTypes(ctx, &coords, op, left, right);
        AstExpression *result = createBinaryExpression(ctx, op, resultType, left, right);
        return transformAssignExpression(ctx, result);
    }

    return left;
}

/**

expression
    : assignment_expression (',' assignment_expression)*
    ;
 */
static AstExpression* parseExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* expression = parseAssignmentExpression(ctx, scope);

    if (ctx->token->code != ',') return expression;

    while (ctx->token->code == ',') {
        AstExpression *right = parseAssignmentExpression(ctx, scope);
        expression = createBinaryExpression(ctx, EB_COMMA, right->type, expression, right);
    }

    return expression;
}

/**
 [GNU] attributes:
         attribute
         attributes attribute

 [GNU]  attribute:
          '__attribute__' '(' '(' attribute-list ')' ')'

 [GNU]  attribute-list:
          attrib
          attribute_list ',' attrib

 [GNU]  attrib:
          empty
          attrib-name
          attrib-name '(' identifier ')'

 [GNU]  attrib-name:
          identifier
          typespec
          typequal
          storageclass
*/

static Boolean isAttributeName(Token *token) {
  if (token->code == IDENTIFIER) return TRUE;
  if (isTypeSpecifierToken(token->code)) return TRUE;
  if (isTypeQualifierToken(token->code)) return TRUE;
  if (isStorageClassToken(token->code)) return TRUE;

  return FALSE;
}

static AstAttribute *parseAttributes(ParserContext *ctx) {
  AstAttribute head = { 0 }, *current = &head;

  Coordinates coords = { ctx->token };

  while (nextTokenIf(ctx, ATTRIBUTE)) {

      consume(ctx, '(');
      consume(ctx, '(');

      Boolean first = TRUE;

      AstAttributeList idHead = { 0 }, *idcur  = &idHead;

      while (ctx->token->code != ')') {
          Coordinates coords2 = { ctx->token };

          if (!first) {
              consume(ctx, ',');
          }

          first = FALSE;

          if (isAttributeName(ctx->token)) {
              const char *attribName = ctx->token->id;
              nextToken(ctx);
              const char *idArg = NULL;
              if (nextTokenIf(ctx, '(')) {
                  idArg = ctx->token->id;
                  consume(ctx, IDENTIFIER);
                  consume(ctx, ')');
              }

              coords2.right = ctx->token;

              idcur = idcur->next = createAttributeList(ctx, &coords2, attribName, idArg);
          }
      }

      consume(ctx, ')');
      coords.right = ctx->token;
      consume(ctx, ')');
      current = current->next = createAttribute(ctx, &coords, idHead.next);
  }

  return head.next;
}


/**
enumerator_list
    : enumerator ( ',' enumerator)*
    ;

enumerator
    : IDENTIFIER ('=' constant_expression)?
    ;
*/
static AstStructMember *parseEnumeratorList(ParserContext *ctx, struct _Scope* scope) {
    AstStructMember *head = NULL, *tail = NULL;
    int64_const_t idx = 0;
    do {
        int token = ctx->token->code;
        if (token == '}') break;
        Coordinates coords = { ctx->token, ctx->token };
        const char* name = NULL;
        if (token == IDENTIFIER) {
            name = ctx->token->id;
            token = nextToken(ctx)->code;
        } else {
            reportDiagnostic(ctx, DIAG_ENUM_LIST_ID_EXPECT, &coords, token);
        }

        int64_t v = idx;
        if (nextTokenIf(ctx, '=')) {
            coords.right = ctx->token; // TODO: fix
            parseAsIntConst(ctx, &v);
            idx = v + 1;
        } else {
            v = idx++;
        }

        if (name) {
          EnumConstant *enumerator = createEnumConst(ctx, &coords, name, v);
          declareEnumConstantSymbol(ctx, enumerator);
          AstStructMember *member = createStructMember(ctx, NULL, NULL, enumerator);
          if (tail) {
              tail->next = member;
          } else {
              head = member;
          }
          tail = member;
        }
    } while (nextTokenIf(ctx, ','));

    return head;
}

/**

enum_specifier
    : ENUM IDENTIFIER? ('{' enumerator_list ','? '}')?
    ;
 */
static AstSUEDeclaration* parseEnumDeclaration(ParserContext *ctx, struct _Scope* scope) {
    const char *name = NULL;

    AstStructMember *members = NULL;
    Coordinates coords = { ctx->token, ctx->token };
    consume(ctx, ENUM);
    int token = ctx->token->code;
    coords.right = ctx->token;

    if (token == IDENTIFIER) {
        name = ctx->token->id;
        token = nextToken(ctx)->code;
        coords.right = ctx->token;
    }

    if (token == '{') {
      token = nextToken(ctx)->code;

      if (token == '}') {
          coords.left = coords.right = ctx->token;
          reportDiagnostic(ctx, DIAG_EMPTY_ENUM, &coords);
      }

      members = parseEnumeratorList(ctx, scope);
      coords.right = ctx->token;
      consume(ctx, '}');
    }

    return createSUEDeclaration(ctx, &coords, DK_ENUM, TRUE, name, members, sizeof(int32_t));
}

int32_t alignMemberOffset(TypeRef *memberType, int32_t offset) {
  return ALIGN_SIZE(offset, typeAlignment(memberType));
}

static int32_t adjustBitFieldStorage(ParserContext *ctx, AstStructMember *chain, unsigned chainWidth, unsigned *offset) {
  TypeId sid, uid;
  unsigned align;

  if (chainWidth <= 8) {
      sid = T_S1; uid = T_U1; align = 1;
  } else if (chainWidth <= 16) {
      sid = T_S2; uid = T_U2; align = 2;
  } else if (chainWidth <= 32) {
      sid = T_S4; uid = T_U4; align = 4;
  } else if (chainWidth <= 64) {
      sid = T_S8; uid = T_U8; align = 8;
  } else {
      return 0;
  }

  TypeRef *sType = makePrimitiveType(ctx, sid, 0);
  TypeRef *uType = makePrimitiveType(ctx, uid, 0);

  *offset = ALIGN_SIZE(*offset, align);

  for (;chain; chain = chain->next) {
      if (chain->kind != SM_DECLARATOR) continue;

      TypeRef *bfType = chain->declarator->typeRef;
      if (bfType->kind == TR_BITFIELD) {
        TypeRef *storageType = bfType->bitFieldDesc.storageType;
        bfType->bitFieldDesc.storageType = isUnsignedType(storageType) ? uType : sType;
      }
      chain->declarator->offset = *offset;
  }

  return align;
}

/**
struct_declaration_list
    : struct_declaration*
    ;

struct_declaration
    : specifier_qualifier_list struct_declarator_list ';'
    ;

specifier_qualifier_list
    : type_specifier specifier_qualifier_list
    | type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    ;

struct_declarator_list
    : struct_declarator ( ',' struct_declarator )*
    ;

struct_declarator
    : declarator
    | ':' constant_expression
    | declarator ':' constant_expression
    ;
*/
static AstStructMember *parseStructDeclarationList(ParserContext *ctx, unsigned factor, struct _Scope* scope) {
  AstStructMember head = { 0 }, *current = &head;
    int token = ctx->token->code;
    unsigned offset = 0;
    unsigned bitOffset = 0;
    AstStructMember *bitfieldChain = NULL;
    unsigned bfChainWidth = 0;
    do {
        DeclarationSpecifiers specifiers = { 0 };
        specifiers.coordinates.left = specifiers.coordinates.right = ctx->token;
        parseDeclarationSpecifiers(ctx, &specifiers, DS_STRUCT);
        Coordinates coords = specifiers.coordinates;

        if (specifiers.defined) {
            // TODO: it doesn't work like this
            AstSUEDeclaration *definition = specifiers.defined;
            Boolean isAnon = strstr(definition->name, "<anon") == definition->name;
            if (isAnon && nextTokenIf(ctx, ';')) {
                /** Handle that case
                 *
                 * struct S {
                 *   int a;
                 *
                 *   struct {
                 *     int b;
                 *   };
                 */

                AstStructMember *members = definition->members;

                unsigned size = 0;

                offset = ALIGN_SIZE(offset, definition->align);

                for (; members; members = members->next) {
                    if (members->kind == SM_DECLARATOR) {
                      AstStructDeclarator *declarator = members->declarator;
                      int32_t memberOffset = declarator->offset;
                      TypeRef *memberType = declarator->typeRef;
                      int32_t typeSize = computeTypeSize(memberType);
                      // TODO: flexible struct
                      if (typeSize != UNKNOWN_SIZE) {
                        members->declarator->offset += offset;
                        if (definition->kind == DK_STRUCT) {
                          size = memberOffset + typeSize;
                        } else {
                          size = max(size, memberOffset + typeSize);
                        }
                      }
                    }
                    current = current->next = members;
                }

                offset += size * factor;
                continue;
            } else {
                AstDeclaration *declaration = createAstDeclaration(ctx, definition->kind, definition->name);
                declaration->structDeclaration = definition;
                current = current->next = createStructMember(ctx, declaration, NULL, NULL);
            }
        }

        for (;;) {
            Declarator declarator = { 0 };
            declarator.coordinates.left = declarator.coordinates.right = ctx->token;
            if (ctx->token->code != ':') {
                parseDeclarator(ctx, &declarator);
                verifyDeclarator(ctx, &declarator, DS_STRUCT);
            }
            int64_t width = -1;
            Boolean hasWidth = FALSE;
            if (ctx->token->code == ':') {
                nextToken(ctx);
                hasWidth = parseAsIntConst(ctx, &width);
            }

            const char *name = declarator.identificator;

            if (name == NULL && specifiers.defined) {
                /** Handle that case
                 *
                 * struct S {
                 *   int a;
                 *
                 *   struct N {
                 *     int b;
                 *   };
                 */
                ;
            } else {
              coords.right = declarator.coordinates.right;
              TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
              if (hasWidth) {
                  // TODO: coordinates
                  if (isIntegerType(type)) {
                    if (width < 0) {
                        if (name) {
                          reportDiagnostic(ctx, DIAG_BIT_FIELD_NEGATIVE_WIDTH, &declarator.coordinates, name, width);
                        } else {
                          reportDiagnostic(ctx, DIAG_ANON_BIT_FIELD_NEGATIVE_WIDTH, &specifiers.coordinates, width);
                        }
                    } else {
                      int typeSize = type->descriptorDesc->size;
                      int typeWidth = typeSize * BYTE_BIT_SIZE;
                      if (width > typeWidth) {
                        if (name) {
                          reportDiagnostic(ctx, DIAG_EXCEED_BIT_FIELD_TYPE_WIDTH, &declarator.coordinates, name, width, typeWidth);
                        } else {
                          reportDiagnostic(ctx, DIAG_EXCEED_ANON_BIT_FIELD_TYPE_WIDTH, &specifiers.coordinates, width, typeWidth);
                        }
                      }

                      const static unsigned maxWidth = sizeof(uint64_t) * BYTE_BIT_SIZE;
                      if (bitfieldChain) {
                        if (width > 0 && (maxWidth - bitOffset) <= width) {
                          int32_t storageSize = adjustBitFieldStorage(ctx, bitfieldChain, bfChainWidth, &offset);
                          bitOffset = 0;
                          bfChainWidth = 0;
                          bitfieldChain = NULL;
                          offset += storageSize * factor;
                        }
                      }

                      type = makeBitFieldType(ctx, type, bitOffset, width);

                      bitOffset += width * factor;

                      if (factor) {
                        bfChainWidth += width;
                      } else {
                        bfChainWidth = max(bfChainWidth, width);
                      }

                      if (width == 0) {
                        if (name) {
                           reportDiagnostic(ctx, DIAG_ZERO_NAMED_BIT_FIELD, &declarator.idCoordinates, name);
                        }

                        int32_t storageSize = adjustBitFieldStorage(ctx, bitfieldChain, bfChainWidth, &offset);
                        bitfieldChain = NULL;
                        bitOffset = bfChainWidth = 0;
                        offset += storageSize * factor;
                        goto end;
                      }
                    }
                  } else {
                      if (name) {
                        reportDiagnostic(ctx, DIAG_BIT_FIELD_TYPE_NON_INT, &declarator.coordinates, name, type);
                      } else {
                        reportDiagnostic(ctx, DIAG_ANON_BIT_FIELD_TYPE_NON_INT, &declarator.coordinates, type);
                      }
                  }
              } else if (bitfieldChain) {
                  int32_t storageSize = adjustBitFieldStorage(ctx, bitfieldChain, bfChainWidth, &offset);
                  offset += storageSize * factor;
              }

              int32_t typeSize = computeTypeSize(type);

              if (!hasWidth) {
                offset = alignMemberOffset(type, offset);
              }

              AstStructDeclarator *structDeclarator = createStructDeclarator(ctx, &coords, type, name, offset);
              current = current->next = createStructMember(ctx, NULL, structDeclarator, NULL);

              if (hasWidth && bitfieldChain == NULL) {
                  bitfieldChain = current;
              }

              if (!hasWidth) {
                bitOffset = 0;
                bitfieldChain = NULL;
                bfChainWidth = 0;
                offset += typeSize * factor;
              }
            }
            end:
            if (!nextTokenIf(ctx, ',')) break;
        }

        consume(ctx, ';');
    } while (ctx->token->code != '}');

    if (bitfieldChain)
      adjustBitFieldStorage(ctx, bitfieldChain, bfChainWidth, &offset);

    return head.next;
}

static int32_t computeStructAlignment(AstStructMember *members) {
  int32_t biggestSize = 1;

  for (; members; members = members->next) {
      if (members->kind != SM_DECLARATOR) continue;

      TypeRef *memberType = members->declarator->typeRef;
      if (isStructualType(memberType)) {
          biggestSize = max(biggestSize, memberType->descriptorDesc->structInfo->align);
      } else if (memberType->kind == TR_ARRAY) {
          biggestSize = max(biggestSize, computeTypeSize(memberType->arrayTypeDesc.elementType));
      } else {
          biggestSize = max(biggestSize, computeTypeSize(memberType));
      }
  }

  return biggestSize;
}

/**
struct_or_union_specifier
    : struct_or_union attributes? '{' struct_declaration_list '}'
    | struct_or_union attributes? IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union attributes? IDENTIFIER
    ;

struct_or_union
    : STRUCT
    | UNION
    ;
 */
static AstSUEDeclaration* parseStructOrUnionDeclaration(ParserContext *ctx, DeclarationKind kind, struct _Scope* scope) {
    const char *name = NULL;
    AstStructMember *members = NULL;
    Boolean isDefinition = FALSE;

    Coordinates coords = { ctx->token };
    unsigned factor = ctx->token->code == STRUCT ? 1 : 0;

    AstAttribute *attributes = parseAttributes(ctx);

    int token = nextToken(ctx)->rawCode;
    coords.right = ctx->token;

    if (token == IDENTIFIER) { // typedef'ed typename is valid struct name
        name = ctx->token->id;
        token = nextToken(ctx)->code;
    }

    if (token != '{') {
        goto done;
    }
    token = nextToken(ctx)->code;
    isDefinition = TRUE;

    coords.right = ctx->token;
    if (nextTokenIf(ctx, '}')) {
        goto done;
    }

    members = parseStructDeclarationList(ctx, factor, scope);

    coords.right = ctx->token;
    consume(ctx, '}');

done:

    return createSUEDeclaration(ctx, &coords, kind, isDefinition, name, members, computeStructAlignment(members));
}

/**
declaration_specifiers
    : storage_class_specifier declaration_specifiers
    | storage_class_specifier
    | type_specifier declaration_specifiers
    | type_specifier
    | type_qualifier declaration_specifiers
    | type_qualifier
    ;

type_specifier
    : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | SIGNED
    | UNSIGNED
    | struct_or_union_specifier
    | enum_specifier
    | TYPEDEF_NAME
    ;

type_qualifier
    : CONST
    | VOLATILE
    ;


storage_class_specifier
    : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    ;
*/

typedef enum _SCS {
  SCS_NONE,
  SCS_REGISTER,
  SCS_STATIC,
  SCS_EXTERN,
  SCS_TYPEDEF,
  SCS_ERROR
} SCS;

typedef enum _TSW {
  TSW_NONE,
  TSW_LONG,
  TSW_LONGLONG,
  TSW_SHORT,
  TSW_ERROR
} TSW;

typedef enum _TSS {
  TSS_NONE,
  TSS_SIGNED,
  TSS_UNSIGNED,
  TSS_ERROR
} TSS;

typedef enum _TST {
  TST_NONE,
  TST_VOID,
  TST_CHAR,
  TST_INT,
  TST_FLOAT,
  TST_DOUBLE,
  TST_ERROR
} TST;

typedef enum _TQT {
  TQT_NONE,
  TQT_CONST,
  TQT_VOLATILE,
  TQT_ERROR
} TQT;

static TypeDesc *computePrimitiveTypeDescriptor(ParserContext *ctx, TSW tsw, const char *tsw_s, TSS tss, const char *tss_s, TST tst, const char *tst_s) {
  if (tsw == TST_ERROR || tss == TSS_ERROR || tst == TST_ERROR) {
      return errorTypeDescriptor; // TODO: return special errorType
  }

  Coordinates coords = { ctx->token, ctx->token };

  if (tsw != TSW_NONE) {
//      short int x; - OK
//      long int x; - OK
//      long long int x; - OK
//      long double x; - OK

//      short char x; - 'short char' is invalid
//      short float x; - 'short float' is invalid
//      short double x; - 'short double' is invalid
//      short void x; 'short void' is invalid
//      long char x; 'long char' is invalid
//      long float x; 'long float' is invalid
//      long long double x; - 'long long double' is invalid

//      unsigned long double x; - 'long double' cannot be signed or unsigned
//      signed long double x; - 'double' cannot be signed or unsigned

      if (tsw == TSW_LONG && tst == TST_DOUBLE) {
          if (tss == TSS_NONE) {
            return &builtInTypeDescriptors[T_F10];
          } else {
            // TODO: coordinates
            reportDiagnostic(ctx, DIAG_ILL_TYPE_SIGN, &coords, "long double");
            return errorTypeDescriptor;
          }
      }

      if (tst == TST_NONE || tst == TST_INT) {
        if (tsw == TSW_SHORT) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U2 : T_S2];
        if (tsw == TSW_LONG) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U8 : T_S8];
        if (tsw == TSW_LONGLONG) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U8 : T_S8];
      }

      assert(tss_s || tsw_s || tst_s);

      const char *s1 = tss_s == NULL ? "" : tss_s;
      const char *s2 = tss_s == NULL ? "" : " ";
      const char *s3 = tsw_s == NULL ? "" : tsw_s;
      const char *s4 = tsw_s == NULL ? "" : " ";
      const char *s5 = tst_s == NULL ? "" : tst_s;
      reportDiagnostic(ctx, DIAG_INVALID_TYPE, &coords, s1, s2, s3, s4, s5);
      return errorTypeDescriptor;
  } // tsw != TSW_NONE

  // tsw == TSW_NONE

  if (tss != TSS_NONE) {
//     unsigned char x; - OK
//     unsigned int x; - OK
//     unsigned float x; - 'float' cannot be signed or unsigned
//     unsigned double x; - 'double' cannot be signed or unsigned
//     unsigned void x; - 'void' cannot be signed or unsigned

      if (tst == TST_CHAR) {
          return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U1 : T_S1];
      }
      if (tst == TST_NONE || tst == TST_INT) {
          return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U4 : T_S4];
      }

      reportDiagnostic(ctx, DIAG_ILL_TYPE_SIGN, &coords, tst_s);
      return errorTypeDescriptor;
  }


  if (tst == TST_VOID) return &builtInTypeDescriptors[T_VOID];
  if (tst == TST_CHAR) return &builtInTypeDescriptors[T_S1];
  if (tst == TST_INT) return &builtInTypeDescriptors[T_S4];
  if (tst == TST_FLOAT) return &builtInTypeDescriptors[T_F4];
  if (tst == TST_DOUBLE) return &builtInTypeDescriptors[T_F8];

  unreachable("Type has to be specicied by this point");
}


enum StructSpecifierKind {
  SSK_NONE,
  SSK_DECLARATION,
  SSK_REFERENCE,
  SSK_DEFINITION,
  SSK_ERROR
};

static enum StructSpecifierKind guessStructualMode(ParserContext *ctx) {
//   struct S;         -- SSK_DECLARATION
//   struct S s;       -- SSK_REFERENCE
//   struct S? { .. }; -- SSK_DEFINITION

  Token *kwToken = ctx->token;

  enum StructSpecifierKind ssk = SSK_NONE;

  Token *nToken = nextToken(ctx);

  if (nToken->code == '{') {
      ssk = SSK_DEFINITION;
  } else if (nToken->code == IDENTIFIER || nToken->code == TYPE_NAME) {
      int nnTokenCode = nextToken(ctx)->code;
      if (nnTokenCode == ';') {
          ssk = SSK_DECLARATION;
      } else if (nnTokenCode == '{') {
          ssk = SSK_DEFINITION;
      } else {
          ssk = SSK_REFERENCE;
      }
  } else {
      ssk = SSK_ERROR;
  }

  ctx->token = kwToken;

  return ssk;
}

static const char *duplicateMsgFormater = "duplicate '%s' declaration specifier";
static const char *nonCombineMsgFormater = "cannot combine with previous '%s' declaration specifier";

static void parseDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, DeclaratorScope scope) {
    SCS scs = SCS_NONE;
    const char *scs_s = NULL;
    TSW tsw = TSW_NONE;
    TSS tss = TSS_NONE;
    TST tst = TST_NONE;
    const char *tsw_s = NULL, *tss_s = NULL, *tst_s = NULL;

    unsigned tmp = 0;
    const char *tmp_s = NULL;

    const char *prefix;
    TypeId typeId;
    SymbolKind symbolId;

    Boolean seenTypeSpecifier = FALSE;

    Coordinates coords = { ctx->token };

    do {
        coords.right = ctx->token;
        Coordinates c2 = { ctx->token, ctx->token };
        switch (ctx->token->code) {
        // storage class specifier
        case REGISTER: tmp = SCS_REGISTER; tmp_s = "register"; goto scs_label;
        case STATIC: tmp = SCS_STATIC; tmp_s = "static"; goto scs_label;
        case EXTERN: tmp = SCS_EXTERN; tmp_s = "extern"; goto scs_label;
        case TYPEDEF: tmp = SCS_TYPEDEF; tmp_s = "typedef"; goto scs_label;
        scs_label:
            if (scs != SCS_ERROR) {
              if (scs == SCS_NONE) {
                  scs = tmp;
                  scs_s  = tmp_s;
              } else {
                  enum DiagnosticId diag = scs == tmp ? DIAG_E_DUPLICATE_DECL_SPEC : DIAG_CANNOT_COMBINE_DECL_SPEC;
                  reportDiagnostic(ctx, diag, &c2, scs_s);
                  scs = SCS_ERROR;
              }
            }
            break;
        // type qualifiers
        case CONST:    tmp = TQT_CONST; goto tq_label;
        case VOLATILE: tmp = TQT_VOLATILE; goto tq_label;
        tq_label:
            if (specifiers->flags.bits.isConst && tmp == TQT_CONST || specifiers->flags.bits.isVolatile && tmp == TQT_VOLATILE) {
                reportDiagnostic(ctx, DIAG_W_DUPLICATE_DECL_SPEC, &c2, tokenName(ctx->token->code));
            }
            specifiers->flags.bits.isConst |= tmp == TQT_CONST;
            specifiers->flags.bits.isVolatile |= tmp == TQT_VOLATILE;
            break;

       case SIGNED: tmp = TSS_SIGNED; tmp_s = "signed"; goto tss_label;
       case UNSIGNED: tmp = TSS_UNSIGNED; tmp_s = "unsigned"; goto tss_label;
       tss_label:
            seenTypeSpecifier = TRUE;
            if (tss != TSS_ERROR) {
              if (tss == TSS_NONE) {
                  tss = tmp;
                  tss_s = tmp_s;
              } else {
                  enum DiagnosticId diag = tss == tmp ? DIAG_W_DUPLICATE_DECL_SPEC : DIAG_CANNOT_COMBINE_DECL_SPEC;
                  reportDiagnostic(ctx, DIAG_CANNOT_COMBINE_DECL_SPEC, &c2, tss_s);
                  tss = TSS_ERROR;
              }
            }
            break;
        case SHORT: tmp = TSW_SHORT; tmp_s = "short"; goto tsw_label;
        case LONG:  tmp = TSW_LONG; tmp_s = "long"; goto tsw_label;
        tsw_label:
            seenTypeSpecifier = TRUE;
            if (tsw != TSW_ERROR) {
              if (tsw == TSW_NONE) {
                  tsw = tmp;
                  tsw_s = tmp_s;
              } else if (tsw == tmp) {
                  if (tsw == TSW_SHORT) {
                      reportDiagnostic(ctx, DIAG_W_DUPLICATE_DECL_SPEC, &c2, tmp_s);
                  } else {
                      tsw = TSW_LONGLONG;
                      tsw_s = "long long";
                  }
              } else {
                  tsw = TSW_ERROR;
                  reportDiagnostic(ctx, DIAG_CANNOT_COMBINE_DECL_SPEC, &c2, tsw_s);
              }
            }
            break;
        case VOID: tmp = TST_VOID; tmp_s = "void"; goto tst_label;
        case CHAR: tmp = TST_CHAR; tmp_s = "char"; goto tst_label;
        case INT: tmp = TST_INT; tmp_s = "int"; goto tst_label;
        case FLOAT: tmp = TST_FLOAT; tmp_s = "float"; goto tst_label;
        case DOUBLE: tmp = TST_DOUBLE; tmp_s = "double"; goto tst_label;
        tst_label:
            seenTypeSpecifier = TRUE;
            if (tst != TST_ERROR) {
                if (tst == TST_NONE) {
                    tst = tmp;
                    tst_s = tmp_s;
                } else {
                    tst = TST_ERROR;
                    reportDiagnostic(ctx, DIAG_CANNOT_COMBINE_DECL_SPEC, &c2, tst_s);
                }
            }
            break;
        case STRUCT: typeId = T_STRUCT; symbolId = StructSymbol; prefix = "$"; goto sue;
        case UNION:  typeId = T_UNION; symbolId = UnionSymbol; prefix = "|"; goto sue;
        case ENUM:   typeId = T_ENUM; symbolId = EnumSymbol; prefix = "#"; goto sue;
        sue:
        seenTypeSpecifier = TRUE;
        {
            enum StructSpecifierKind ssk = guessStructualMode(ctx);

            AstSUEDeclaration *declaration = typeId == T_ENUM
                ? parseEnumDeclaration(ctx, NULL)
                : parseStructOrUnionDeclaration(ctx, typeId == T_STRUCT ? DK_STRUCT : DK_UNION, NULL);

            if (ssk == SSK_DEFINITION)
              specifiers->defined = declaration;

            coords.right = declaration->coordinates.right;
            const char* name = declaration->name;
            char tmpBuf[1024];
            int size = 0;

            prefix = "$";
            TypeDesc *typeDescriptor = NULL;
            if (name) { // TODO: should not be done here
                int len = strlen(name);
                char *symbolName = allocateString(ctx, len + 1 + 1);
                size = sprintf(symbolName, "%s%s", prefix, name);

                Symbol *s = NULL;
                if (ssk == SSK_REFERENCE) {
                    s = findSymbol(ctx, symbolName);
                    if (s && s->kind != symbolId) {
                        reportDiagnostic(ctx, DIAG_USE_WITH_DIFFERENT_TAG, &declaration->coordinates, name);
                    }
                }

                if (s == NULL) {
                    s = declareSUESymbol(ctx, symbolId, typeId, symbolName, declaration);
                }

                typeDescriptor = s ? s->typeDescriptor : NULL;
            } else {
                if (ssk == SSK_DEFINITION) {
                  size = sprintf(tmpBuf, "<anon$%d>", ctx->anonSymbolsCounter++);
                  name = allocateString(ctx, size + 1);
                  memcpy((char *)name, tmpBuf, size + 1);
                  declaration->name = name;
                  int typeSize = computeSUETypeSize(ctx, declaration);
                  if (typeSize < 0) {
                      reportDiagnostic(ctx, DIAG_NON_COMPUTE_DECL_SIZE, &declaration->coordinates);
                  }
                  typeDescriptor = createTypeDescriptor(ctx, typeId, name, typeSize);
                  typeDescriptor->structInfo = declaration;
                } else {
                  reportDiagnostic(ctx, DIAG_ANON_STRUCT_IS_DEFINITION, &declaration->coordinates);
                }
            }

            specifiers->basicType = typeDescriptor != NULL ? makeBasicType(ctx, typeDescriptor, specifiers->flags.storage) : makeErrorRef(ctx);

            goto almost_done;
        }
        case TYPE_NAME:
        {
            if (!seenTypeSpecifier) {
              const char *name = ctx->token->id;
              Symbol *s = findSymbol(ctx, name);
              if (s == NULL || s->kind != TypedefSymbol) {
                  // TODO: probably should be replaced with assert
                  reportDiagnostic(ctx, DIAG_UNKNOWN_TYPE_NAME, &c2, name);
              } else {
                  specifiers->basicType = s->typeref;
              }

              nextToken(ctx);
              goto almost_done;
            } else {
              // IDENTIFICATOR in declarator position should be treaten as an ID
              ctx->token->code = IDENTIFIER;
            }
        }
        default: {
            Boolean isError = FALSE;
            if (!(tss || tsw || tst)) {
              if (scope != DS_CAST && scope != DS_VA_ARG && scope != DS_STRUCT && scope != DS_SIZEOF) {
                reportDiagnostic(ctx, DIAG_MISSING_TYPE_SPECIFIER, &c2);
                tst = TST_INT;
                tst_s = "int";
              } else {
                isError = TRUE;
              }
            }
            specifiers->basicType = isError ? makeErrorRef(ctx) : makeBasicType(ctx, computePrimitiveTypeDescriptor(ctx, tsw, tsw_s, tss, tss_s, tst, tst_s), specifiers->flags.storage);

        almost_done:
            specifiers->flags.bits.isExternal = scs == SCS_EXTERN;
            specifiers->flags.bits.isStatic = scs == SCS_STATIC;
            specifiers->flags.bits.isRegister = scs == SCS_REGISTER;
            specifiers->flags.bits.isTypedef = scs == SCS_TYPEDEF;
            specifiers->coordinates.right = ctx->token;

            verifyDeclarationSpecifiers(ctx, specifiers, scope);
            return;
        }
        }
    } while (nextToken(ctx));
}

/**
type_qualifier
    : CONST
    | RESTRICT
    | VOLATILE
    | ATOMIC
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;

 */
static SpecifierFlags parseTypeQualifierList(ParserContext *ctx) {
    SpecifierFlags result = { 0 };
    TQT tmp = TQT_NONE;
    do {

        switch (ctx->token->code) {
          case CONST:    tmp = TQT_CONST; goto tq_label;
          case VOLATILE: tmp = TQT_VOLATILE; goto tq_label;
          tq_label:
              if (result.bits.isConst || result.bits.isVolatile) {
                  Coordinates coords = { ctx->token, ctx->token };
                  reportDiagnostic(ctx, DIAG_W_DUPLICATE_DECL_SPEC, &coords, tokenName(ctx->token->code));
              }
              result.bits.isConst |= tmp == TQT_CONST;
              result.bits.isVolatile |= tmp == TQT_VOLATILE;
              break;

          default: {
              return result;
          }
        }
    } while (nextToken(ctx));
}



/**
initializer
    : '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    | assignment_expression
    ;
 */
static AstInitializer* parseInitializer(ParserContext *ctx, struct _Scope* scope) {
    AstExpression *expr = NULL;
    Coordinates coords = { ctx->token };

    AstInitializer *result;

    if (nextTokenIf(ctx, '{')) {
        AstInitializerList *head = NULL, *tail = NULL;
        int numOfInits = 0;
        while (ctx->token->code != '}') {
            AstInitializerList *next = createAstInitializerList(ctx);
            AstInitializer* initializer = parseInitializer(ctx, scope);
            next->initializer = initializer;
            nextTokenIf(ctx, ',');
            if (tail) {
                tail->next = next;
            } else {
                head = next;
            }
            tail = next;
            ++numOfInits;
        }
        coords.right = ctx->token;
        consume(ctx, '}');
        result = createAstInitializer(ctx, &coords, IK_LIST);
        result->initializerList = head;
        result->numOfInitializers = numOfInits;
    } else {
        expr = parseAssignmentExpression(ctx, scope);
        coords.right = expr->coordinates.right;
        result = createAstInitializer(ctx, &coords, IK_EXPRESSION);
        result->expression = expr;
    }

    return result;
}

/**
parameter_declaration
    : declaration_specifiers (declarator | abstract_declarator)?
    ;

abstract_declarator
    : pointer | direct_abstract_declarator | pointer direct_abstract_declarator
    ;

declarator
    : pointer? direct_declarator
    ;

direct_abstract_declarator
    : ( '(' (abstract_declarator | parameter_type_list)? ')' | '[' constant_expression? ']' )
      ( '[' constant_expression? ']' | '(' parameter_type_list? ')' )*
    ;

direct_declarator
    : ( IDENTIFIER | '(' declarator ')' )
      ( '[' constant_expression? ']' | '(' (parameter_type_list [| identifier_list])? ')' )*
    ;

*/

static void parseDirectDeclarator(ParserContext *ctx, Declarator *declarator);

/**
parameter_list
    : parameter_declaration ( ',' parameter_declaration )* ( ',' ELLIPSIS )?
    ;

 */
static void parseParameterList(ParserContext *ctx, FunctionParams *params, struct _Scope* scope) {
    int idx = 0;
    AstValueDeclaration *head = NULL, *tail = NULL;

    int ellipsisIdx = -1;

    do {
        Coordinates c2 = { ctx->token, ctx->token };
        if (nextTokenIf(ctx, ELLIPSIS)) {
            if (idx == 0) {
                // foo(...)
                reportDiagnostic(ctx, DIAG_PARAM_BEFORE_ELLIPSIS, &c2);
            } else if (!params->isVariadic) {
                ellipsisIdx = idx++;
                params->isVariadic = 1;
            }
            if (ctx->token->code != ')') {
                c2.left = c2.right = ctx->token;
                reportDiagnostic(ctx, DIAG_EXPECTED_TOKEN, &c2, ')', ctx->token->code);
            }
        } else {
          DeclarationSpecifiers specifiers = { 0 };
          Coordinates coords = { ctx->token, ctx->token };
          specifiers.coordinates = coords;
          parseDeclarationSpecifiers(ctx, &specifiers, DS_PARAMETERS);
          coords.right = specifiers.coordinates.right;


          TypeRef *type = specifiers.basicType;
          TypeDesc *typeDesc = type->kind == TR_VALUE ? type->descriptorDesc : NULL;
          if (typeDesc && typeDesc->typeId == T_VOID) {
              c2.left = c2.right = coords.right = ctx->token;
              if (ctx->token->code == ')' && idx == 0) {
                  // it's a that case foo(void), we are done
                  return;
              } else if (ctx->token->code == ')' || ctx->token->code == ',') {
                  // foo(int x, void) or foo(void, int x)
                  reportDiagnostic(ctx, DIAG_VOID_SINGLE, &coords);
              } else if (ctx->token->code == IDENTIFIER) {
                  // foo(void x)
                  reportDiagnostic(ctx, DIAG_VOID_PARAMTER_TYPE, &c2);
              }
          }

          // pointer | direct_abstract_declarator | pointer direct_abstract_declarator | pointer? direct_declarator
          Declarator declarator = { 0 };
          declarator.coordinates.left = declarator.coordinates.right = ctx->token;
          parseDeclarator(ctx, &declarator);
          verifyDeclarator(ctx, &declarator, DS_PARAMETERS);

          coords.right = declarator.coordinates.right;
          const char *name = declarator.identificator;

          type = makeTypeRef(ctx, &specifiers, &declarator);
          if (type->kind == TR_FUNCTION) {
              type = makePointedType(ctx, 0U, type);
          } else if (type->kind == TR_ARRAY) {
              if (type->arrayTypeDesc.size < 0) {
                  // int foo(int a[]) -> int foo(const int *a)
                  TypeRef *arrayType = type;
                  type = makePointedType(ctx, 0U, type->arrayTypeDesc.elementType);
              }
          }
          AstValueDeclaration *parameter =
              createAstValueDeclaration(ctx, &coords, VD_PARAMETER, type, name, idx++, specifiers.flags.storage, NULL);
          parameter->symbol = declareValueSymbol(ctx, name, parameter);
          parameter->flags.bits.isLocal = 1;

          if (tail) {
            tail->next = parameter;
          } else {
            head = parameter;
          }
          tail = parameter;
        }
    } while (nextTokenIf(ctx, ','));

    params->parameters = head;
}

/**
identifier_list
    : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    ;
 */
static AstIdentifierList* parseIdentifierList(ParserContext *ctx, struct _Scope* scope) {

  // K&R parameters are not yet supported
  return NULL;
//    AstIdentifierList* result = (AstIdentifierList*)malloc(sizeof(AstIdentifierList));
//    char* storage = (char*)malloc(yyget_leng(ctx->scanner) + 1);
//    strncpy(storage, yyget_text(ctx->scanner), yyget_leng(ctx->scanner));
//    result->name = storage;

//    AstIdentifierList *prev = result;

//    while(nextToken(ctx) == ',') {
//        if (nextToken(ctx) != IDENTIFIER) {
//            reportUnexpectedToken(ctx, IDENTIFIER);
//        }
//        AstIdentifierList *tmp = (AstIdentifierList*)malloc(sizeof(AstIdentifierList));
//        tmp->name = copyLiteralString(ctx);
//        prev->next = tmp;
//        prev = tmp;
//    }

//    return result;
}

static void parseFunctionDeclaratorPart(ParserContext *ctx, Declarator *declarator) {
  Token *l = ctx->token;
  consume(ctx, '(');

  Scope *paramScope = newScope(ctx, ctx->currentScope);
  DeclaratorPart *part = allocateDeclaratorPart(ctx);

  ctx->currentScope = paramScope;
  if (ctx->token->code != ')') {
      parseParameterList(ctx, &part->parameters, NULL);
  }

  part->coordinates.left = l;
  part->coordinates.right = declarator->coordinates.right = ctx->token;

  part->kind = DPK_FUNCTION;
  part->parameters.scope = paramScope;

  part->next = declarator->declaratorParts;

  if (declarator->functionDeclarator == NULL)
    declarator->functionDeclarator = declarator->declaratorParts ? NULL : part;
  declarator->declaratorParts = part;

  ctx->currentScope = paramScope->parent;

  consume(ctx, ')');
}

static void parseArrayDeclaratorPart(ParserContext *ctx, Declarator *declarator) {
  Token *l = ctx->token;
  consume(ctx, '[');
  int64_t size = UNKNOWN_SIZE;
  if (ctx->token->code != ']') {
      parseAsIntConst(ctx, &size);
  }

  DeclaratorPart *part = allocateDeclaratorPart(ctx);
  part->coordinates.left = l;
  part->coordinates.right = declarator->coordinates.right = ctx->token;
  part->kind = DPK_ARRAY;
  part->arraySize = size;

  part->next = declarator->declaratorParts;
  declarator->declaratorParts = part;

  consume(ctx, ']');
}

/**

direct_declarator
    : (IDENTIFIER | '(' declarator ')')
      ( '[' constant_expression? ']' | '(' parameter_type_list? ')' )*
    ;

direct_abstract_declarator
    : ('(' (abstract_declarator | parameter_type_list)? ')' | '[' constant_expression? ']')
      ( '[' constant_expression? ']' | '(' parameter_type_list? ')' )*
    ;
 */
static void parseDirectDeclarator(ParserContext *ctx, Declarator *declarator) {

  Token *t = ctx->token;
    if (t->code == IDENTIFIER || t->code == TYPE_NAME) { // rawCode, rly?
        if (declarator->identificator) {
            Coordinates c2 = { t, t };
            reportDiagnostic(ctx, DIAG_ID_ALREADY_SPECIFIED, &c2);
        } else {
            declarator->idCoordinates.left = declarator->idCoordinates.right = t;
            declarator->identificator = t->id;
        }
        declarator->coordinates.right = t;
        nextToken(ctx);
    } else if (ctx->token->code == '[') {
        parseArrayDeclaratorPart(ctx, declarator);
    } else if (ctx->token->code == '(') {
        if (ctx->token->code != ')') {
            if (isDeclarationSpecifierToken(ctx->token->code)) {
                parseFunctionDeclaratorPart(ctx, declarator);
            } else {
                consume(ctx, '(');
                parseDeclarator(ctx, declarator);
                declarator->coordinates.right = ctx->token;
                consume(ctx, ')');
            }
        }
    } else {
        return;
    }


    while (ctx->token) {
        if (ctx->token->code == '[') {
            parseArrayDeclaratorPart(ctx, declarator);
        } else if (ctx->token->code == '(') {
            parseFunctionDeclaratorPart(ctx, declarator);
        } else {
            return;
        }
    }
}

/**
declarator
    : pointer* direct_declarator
    ;
 */
static void parseDeclarator(ParserContext *ctx, Declarator *declarator) {
  Token *l = ctx->token;

  if (nextTokenIf(ctx, '*')) {
      SpecifierFlags qualifiers = parseTypeQualifierList(ctx);

      Token *r = ctx->token;
      parseDeclarator(ctx, declarator);


      DeclaratorPart *part = allocateDeclaratorPart(ctx);
      part->coordinates.left = l;
      part->coordinates.right = r;
      part->kind = DPK_POINTER;
      part->flags.storage = qualifiers.storage;
      part->next = declarator->declaratorParts;
      declarator->declaratorParts = part;
  } else {
      parseDirectDeclarator(ctx, declarator);
  }
}

static AstStatement *parseCompoundStatement(ParserContext *ctx);
static AstStatement *parseStatement(ParserContext *ctx, struct _Scope* scope);

static AstStatement *parseIfStatement(ParserContext *ctx, struct _Scope* scope) {
  Coordinates coords = { ctx->token };
  consume(ctx, IF);
  consume(ctx, '(');
  AstExpression *cond = parseExpression(ctx, scope);
  consume(ctx, ')');
  AstStatement *thenB = parseStatement(ctx, scope);
  coords.right = thenB->coordinates.right;
  AstStatement *elseB = NULL;
  if (ctx->token->code == ELSE) {
      nextToken(ctx);
      elseB = parseStatement(ctx, scope);
      coords.right = elseB->coordinates.right;
  }

  return createIfStatement(ctx, &coords, cond, thenB, elseB);
}

static void defineLabel(ParserContext *ctx, const char *label, AstStatement *lblStmt) {
  DefinedLabel *defined = ctx->labels.definedLabels;
  Boolean redefinition = FALSE;

  while (defined) {
      assert(defined->label->kind == LK_LABEL);
      if (strcmp(defined->label->label, label) == 0) {
          redefinition = TRUE;
      }
      defined = defined->next;
  }

  if (redefinition) {
    reportDiagnostic(ctx, DIAG_LABEL_REDEFINITION, &lblStmt->coordinates, label);
  }

  DefinedLabel *newLabel = heapAllocate(sizeof (DefinedLabel));
  assert(lblStmt->statementKind == SK_LABEL);
  assert(lblStmt->labelStmt.kind == LK_LABEL);
  newLabel->label = &lblStmt->labelStmt;
  newLabel->next = ctx->labels.definedLabels;
  ctx->labels.definedLabels = newLabel;

  UsedLabel **prev = &ctx->labels.usedLabels;
  UsedLabel *used = ctx->labels.usedLabels;

  while (used) {
      if (strcmp(used->label, label) == 0) {
        *prev = used->next;
        UsedLabel *t = used;
        used = used->next;
        releaseHeap(t);
        // TOOD: link use with its def?
      } else {
        prev = &used->next;
        used = *prev;
      }
  }
}

static AstStatement *parseStatement(ParserContext *ctx, struct _Scope* scope) {
    AstExpression *expr, *expr2, *expr3;
    AstStatement *stmt;
    int64_t c = 0;
    unsigned oldFlag = 0;
    unsigned oldCaseCount = 0;
    unsigned oldHasDefault = 0;
    Coordinates coords = { ctx->token, ctx->token };
    switch (ctx->token->code) {
    case CASE:
        if (!ctx->stateFlags.inSwitch) {
            reportDiagnostic(ctx, DIAG_SWITCH_LABEL_NOT_IN_SWITCH, &coords, "case");
        } else {
            ctx->stateFlags.caseCount += 1;
        }
        consume(ctx, CASE);
        parseAsIntConst(ctx, &c);
        consume(ctx, ':');
        stmt = parseStatement(ctx, scope);
        coords.right = stmt->coordinates.right;
        return createLabelStatement(ctx, &coords, LK_CASE,stmt, NULL, c);
    case DEFAULT:
        if (!ctx->stateFlags.inSwitch) {
            reportDiagnostic(ctx, DIAG_SWITCH_LABEL_NOT_IN_SWITCH, &coords, "default");
        } else {
            ctx->stateFlags.hasDefault = 1;
        }
        consume(ctx, DEFAULT);
        consume(ctx, ':');
        stmt = parseStatement(ctx, scope);
        coords.right = stmt->coordinates.right;
        return createLabelStatement(ctx, &coords, LK_DEFAULT, stmt, NULL, c);
    case '{': return parseCompoundStatement(ctx);
    case IF: return parseIfStatement(ctx, scope);
    case SWITCH:
        consume(ctx, SWITCH);
        consume(ctx, '(');
        expr = parseExpression(ctx, scope);
        if (!isIntegerType(expr->type)) {
            reportDiagnostic(ctx, DIAG_SWITCH_ARG_NOT_INTEGER, &expr->coordinates, expr->type);
        }
        consume(ctx, ')');
        oldCaseCount = ctx->stateFlags.caseCount;
        ctx->stateFlags.caseCount = 0;
        oldFlag = ctx->stateFlags.inSwitch;
        ctx->stateFlags.inSwitch = 1;
        oldHasDefault = ctx->stateFlags.hasDefault;
        ctx->stateFlags.hasDefault = 0;
        stmt = parseStatement(ctx, scope);
        ctx->stateFlags.inSwitch = oldFlag;
        verifySwitchCases(ctx, stmt, ctx->stateFlags.caseCount);
        unsigned caseCount = ctx->stateFlags.caseCount;
        unsigned hasDefault = ctx->stateFlags.hasDefault;
        ctx->stateFlags.caseCount = oldCaseCount;
        ctx->stateFlags.hasDefault = oldHasDefault;
        coords.right = stmt->coordinates.right;
        return createSwitchStatement(ctx, &coords, expr, stmt, caseCount, hasDefault);
    case WHILE:
        consume(ctx, WHILE);
        consume(ctx, '(');
        oldFlag = ctx->stateFlags.inLoop;
        expr = parseExpression(ctx, scope);
        consume(ctx, ')');
        ctx->stateFlags.inLoop = 1;
        stmt = parseStatement(ctx, scope);
        ctx->stateFlags.inLoop = oldFlag;
        coords.right = stmt->coordinates.right;
        return createLoopStatement(ctx, &coords, SK_WHILE, expr, stmt);
    case DO:
        consume(ctx, DO);
        oldFlag = ctx->stateFlags.inLoop;
        ctx->stateFlags.inLoop = 1;
        stmt = parseStatement(ctx, scope);
        ctx->stateFlags.inLoop = oldFlag;
        coords.right = ctx->token;
        consume(ctx, WHILE);
        consume(ctx, '(');
        expr = parseExpression(ctx, scope);
        consume(ctx, ')');
        consume(ctx, ';');
        return createLoopStatement(ctx, &coords, SK_DO_WHILE, expr, stmt);
    case FOR:
        consume(ctx, FOR); // for
        consume(ctx, '('); // for(

        expr = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ';'); // for( ...;

        expr2 = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ';'); // for( ...; ...;

        expr3 = ctx->token->code != ')' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ')'); // for( ...; ...; ...)

        oldFlag = ctx->stateFlags.inLoop;
        ctx->stateFlags.inLoop = 1;
        stmt = parseStatement(ctx, scope); // for( ...; ...; ...) ...
        ctx->stateFlags.inLoop = oldFlag;

        coords.right = stmt->coordinates.right;
        return createForStatement(ctx, &coords, expr, expr2, expr3, stmt);
    case GOTO:
        consume(ctx, GOTO);
        if (nextTokenIf(ctx, '*')) {
          AstExpression *expr = parseExpression(ctx, scope);
          verifyGotoExpression(ctx, expr);
          stmt = createJumpStatement(ctx, &coords, SK_GOTO_P);
          stmt->jumpStmt.expression = expr;
        } else {
          const char* label = ctx->token->id;
          // TODO: consume either IDENTIFIER OR TYPE_NAME
          consumeRaw(ctx, IDENTIFIER);
          coords.right = ctx->token;
          consume(ctx, ';');
          if (label) {
            stmt = createJumpStatement(ctx, &coords, SK_GOTO_L);
            stmt->jumpStmt.label = label;
            useLabelExpr(ctx, NULL, stmt, label);
          } else {
            stmt = createErrorStatement(ctx, &coords);
          }
        }
        return stmt;
    case CONTINUE:
        if (!ctx->stateFlags.inLoop) {
            reportDiagnostic(ctx, DIAG_CONTINUE_NOT_IN_LOOP, &coords);
        }
        consume(ctx, CONTINUE);
        consume(ctx, ';');
        return createJumpStatement(ctx, &coords, SK_CONTINUE);
    case BREAK:
        if (!(ctx->stateFlags.inLoop || ctx->stateFlags.inSwitch)) {
            reportDiagnostic(ctx, DIAG_BRAEK_NOT_IN_LOOP_OR_SWITCH, &coords);
        }
        consume(ctx, BREAK);
        consume(ctx, ';');
        return createJumpStatement(ctx, &coords, SK_BREAK);
    case RETURN:
        consume(ctx, RETURN);
        expr = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        coords.right = ctx->token;
        consume(ctx, ';');
        stmt = createJumpStatement(ctx, &coords, SK_RETURN);
        if (expr) {
          if (isAssignableTypes(ctx, &coords, ctx->functionReturnType, expr->type, expr, FALSE)) {
            if (!typesEquals(ctx->functionReturnType, expr->type)) {
                expr = createCastExpression(ctx, &coords, ctx->functionReturnType, expr);
            }
          }
        }
        stmt->jumpStmt.expression = expr;
        return stmt;
    case ';':
        consume(ctx, ';');
        return createEmptyStatement(ctx, &coords);
    case IDENTIFIER: { // IDENTIFIER ':' statement
        Token *savedToken = ctx->token;
        nextToken(ctx);
        if (nextTokenIf(ctx, ':')) {
            stmt = parseStatement(ctx, scope);
            coords.right = stmt->coordinates.right;
            AstStatement *lbl = createLabelStatement(ctx, &coords, LK_LABEL, stmt, savedToken->id, -1);
            defineLabel(ctx, savedToken->id, lbl);
            return lbl;
        } else {
            ctx->token = savedToken;
        }
    }
    default:
        expr = parseExpression(ctx, scope);
        verifyStatementLevelExpression(ctx, expr);
        consume(ctx, ';');
        return createExprStatement(ctx, expr);
    }
}

static unsigned processDeclarationPart(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator) {
  return 0;
}

static AstStatementList *allocateStmtList(ParserContext *ctx, AstStatement *stmt) {
  AstStatementList* result = (AstStatementList*)areanAllocate(ctx->memory.astArena, sizeof(AstStatementList));
  result->stmt = stmt;
  return result;
}

/**
compound_statement
    : '{'  block_item_list? '}'
    ;

block_item_list
    : block_item+
    ;

block_item
    : declaration | statement
    ;
 */
static AstStatement *parseCompoundStatementImpl(ParserContext *ctx) {

    Coordinates coords = { ctx->token };
    consume(ctx, '{');

    Scope *blockScope = ctx->currentScope;
    AstStatementList *head = NULL, *tail = NULL;

    while (ctx->token->code && ctx->token->code != '}') {
        if (isDeclarationSpecifierToken(ctx->token->code)) {
            DeclarationSpecifiers specifiers = { 0 };
            specifiers.coordinates.left = specifiers.coordinates.right = ctx->token;
            parseDeclarationSpecifiers(ctx, &specifiers, DS_STATEMENT);
            Coordinates coords2 = specifiers.coordinates;

            if (specifiers.defined) {
                AstDeclaration *declaration = createAstDeclaration(ctx, specifiers.defined->kind, specifiers.defined->name);
                declaration->structDeclaration = specifiers.defined;
                AstStatement *declStmt = createDeclStatement(ctx, &coords2, declaration);
                AstStatementList *node = allocateStmtList(ctx, declStmt);
                if (tail) tail->next = node;
                else head = node;
                tail = node;
            }

            if (ctx->token->code != ';') {
                do {
                    Declarator declarator = { 0 };
                    declarator.coordinates.left = declarator.coordinates.right = ctx->token;
                    parseDeclarator(ctx, &declarator);
                    verifyDeclarator(ctx, &declarator, DS_STATEMENT);

                    const char *name = declarator.identificator;
                    coords2.right = declarator.coordinates.right;
                    TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
                    AstInitializer* initializer = NULL;
                    Coordinates eqCoords = { ctx->token, ctx->token };
                    if (nextTokenIf(ctx, '=')) {
                        Boolean isTypeOk = verifyValueType(ctx, &coords2, type);
                        if (!isTypeOk) type = makeErrorRef(ctx);
                        if (specifiers.flags.bits.isExternal) {
                            reportDiagnostic(ctx, DIAG_EXTERN_VAR_INIT, &declarator.coordinates);
                        }

                        initializer = parseInitializer(ctx, NULL);
                        initializer = finalizeInitializer(ctx, type, initializer, specifiers.flags.bits.isStatic);
                        coords2.right = initializer->coordinates.right;
                    } else {
                        if (type->kind == TR_ARRAY && type->arrayTypeDesc.size == UNKNOWN_SIZE && !specifiers.flags.bits.isExternal) {
                            reportDiagnostic(ctx, DIAG_ARRAY_EXPLICIT_SIZE_OR_INIT, &coords2);
                        }
                    }

                    AstDeclaration *declaration = NULL;
                    if (specifiers.flags.bits.isTypedef) {
                        if (initializer != NULL) {
                            eqCoords.right = coords2.right;
                            reportDiagnostic(ctx, DIAG_ILLEGAL_INIT_ONLY_VARS, &eqCoords);
                        }
                        declareTypeDef(ctx, name, type);
                        declaration = createAstDeclaration(ctx, DK_TYPEDEF, name);
                        declaration->typeDefinition.coordinates = coords2;
                        declaration->typeDefinition.definedType = type;
                    } else {
                        Boolean isTypeOk = verifyValueType(ctx, &coords2, type);
                        if (!isTypeOk) type = makeErrorRef(ctx);
                        declaration = createAstDeclaration(ctx, DK_VAR, name);
                        AstValueDeclaration *valueDeclaration =
                            createAstValueDeclaration(ctx, &coords2, VD_VARIABLE, type, name, 0, specifiers.flags.storage, initializer);
                        declaration->variableDeclaration = valueDeclaration;
                        if (!valueDeclaration->flags.bits.isStatic) {
                            valueDeclaration->flags.bits.isLocal = 1;
                            valueDeclaration->next = ctx->locals;
                            ctx->locals = valueDeclaration;

                        }
                        valueDeclaration->symbol = declareValueSymbol(ctx, name, valueDeclaration);
                    }

                    if (declaration) {
                      AstStatement *declStmt = createDeclStatement(ctx, &coords2, declaration);
                      AstStatementList *node = allocateStmtList(ctx, declStmt);
                      if (tail) tail->next = node;
                      else head = node;
                      tail = node;
                    }
                } while (nextTokenIf(ctx, ','));
            } else {
                // TODO: typedef int;
                if (specifiers.defined == NULL)  {
                  reportDiagnostic(ctx, DIAG_DECLARES_NOTHING, &specifiers.coordinates);
                }
            }

            consume(ctx, ';');
        } else {
            AstStatement *statement = parseStatement(ctx, NULL);
            AstStatementList *node = allocateStmtList(ctx, statement);
            if (tail) tail->next = node;
            else head = node;
            tail = node;
        }
    }

    coords.right = ctx->token;
    consume(ctx, '}');

    return createBlockStatement(ctx, &coords, ctx->currentScope, head);
}

static AstStatement *parseCompoundStatement(ParserContext *ctx) {
  ctx->currentScope = newScope(ctx, ctx->currentScope);
  AstStatement *result = parseCompoundStatementImpl(ctx);
  assert(result->statementKind == SK_BLOCK);
  ctx->currentScope = ctx->currentScope->parent;
  return result;
}

static AstStatement *parseFunctionBody(ParserContext *ctx) {
  return parseCompoundStatementImpl(ctx);
}

// return FALSE if no errors found
static Boolean verifyDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, DeclaratorScope scope) {
  SpecifierFlags flags = specifiers->flags;
  flags.bits.isConst = 0;
  flags.bits.isVolatile = 0;
  switch (scope) {
    case DS_FILE:
      if (flags.bits.isRegister) {
          reportDiagnostic(ctx, DIAG_ILLEGAL_STORAGE_ON_FILE_SCOPE, &specifiers->coordinates);
          return TRUE;
      }
      break;
    case DS_STRUCT:
    case DS_CAST:
    case DS_SIZEOF:
    case DS_VA_ARG:
      if (flags.storage) {
          reportDiagnostic(ctx, DIAG_STORAGE_NOT_ALLOWED, &specifiers->coordinates);
          return TRUE;
      }
      break;
    case DS_PARAMETERS:
      flags.bits.isRegister = 0;
      if (flags.storage) {
          reportDiagnostic(ctx, DIAG_INVALID_STORAGE_ON_PARAM, &specifiers->coordinates);
      }
      break;
    default:
      break;
  }

  return FALSE;
}

static void verifyDeclarator(ParserContext *ctx, Declarator *declarator, DeclaratorScope scope) {

  switch (scope) {
  case DS_FILE:
  case DS_STATEMENT:
  case DS_STRUCT:
      if (declarator->identificator == NULL) {
          reportDiagnostic(ctx, DIAG_DECLARES_NOTHING, &declarator->coordinates);
      }
      break;
  default:
      break;
  }
}

/**
declaration_list
    : declaration
    | declaration_list declaration
    ;
 */
static void* parseDeclarationList(ParserContext *ctx, struct _Scope* scope) {
    return NULL;
}

static void addToFile(AstFile *file, AstTranslationUnit *newUnit) {
  AstTranslationUnit *tail = file->last;
  if (tail) {
    tail->next = newUnit;
    file->last = newUnit;
  } else {
    file->units = file->last = newUnit;
  }
}

static void verifyLabels(ParserContext *ctx) {
  DefinedLabel *def = ctx->labels.definedLabels;
  ctx->labels.definedLabels = NULL;

  while (def) {
      DefinedLabel *next = def->next;
      // TODO: warning - unused label?
      releaseHeap(def);
      def = next;
  }

  UsedLabel *used = ctx->labels.usedLabels;
  ctx->labels.usedLabels = NULL;

  while (used) {
    Coordinates *coords = NULL;
    const char *label = NULL;

    if (used->kind == LU_GOTO_USE) {
        assert(used->gotoStatement->statementKind == SK_GOTO_L);
        coords = &used->gotoStatement->coordinates;
        label = used->gotoStatement->jumpStmt.label;
    } else {
        assert(used->kind == LU_REF_USE);
        assert(used->labelRef->op == E_LABEL_REF);
        coords = &used->labelRef->coordinates;
        label = used->labelRef->label;
    }

    reportDiagnostic(ctx, DIAG_UNDECLARED_LABEL, coords, label);

    UsedLabel *next = used->next;
    releaseHeap(used);
    used = next;
  }
}

/**
  external_declaration
    : function_definition
    | declaration

declaration
    : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';'
    ;

function_definition
    --: declaration_specifiers declarator declaration_list compound_statement
    | declaration_specifiers declarator compound_statement
    ;

declaration_list
    : declaration
    | declaration_list declaration
    ;

init_declarator_list
    : init_declarator
    | init_declarator_list ',' init_declarator
    ;

init_declarator
    : declarator '=' initializer
    | declarator
    ;
*/
static void parseExternalDeclaration(ParserContext *ctx, AstFile *file) {
  DeclarationSpecifiers specifiers = { 0 };
  specifiers.coordinates.left = specifiers.coordinates.right = ctx->token;
  parseDeclarationSpecifiers(ctx, &specifiers, DS_FILE);
  Coordinates coords = specifiers.coordinates;

  Boolean isTypeDefDeclaration = specifiers.flags.bits.isTypedef != 0 ? TRUE : FALSE;

  if (specifiers.defined) {
    AstSUEDeclaration *defined = specifiers.defined;
    AstDeclaration *declaration = createAstDeclaration(ctx, defined->kind, defined->name);
    declaration->structDeclaration = defined;
    addToFile(file, createTranslationUnit(ctx, declaration, NULL));
  }

  if (nextTokenIf(ctx, ';')) {
      if (isTypeDefDeclaration) {
          reportDiagnostic(ctx, DIAG_TYPEDEF_WITHOUT_NAME, &specifiers.coordinates);
      }
      // TODO: declares nothing
      return;
  }

  int id_idx = 0;
  const char *funName = NULL;
  AstFunctionDeclaration *functionDeclaration = NULL;
  Scope *functionScope = NULL;
  do {
    Declarator declarator = { 0 };
    AstInitializer *initializer = NULL;
    functionDeclaration = NULL;
    declarator.coordinates.left = declarator.coordinates.right = ctx->token;
    parseDeclarator(ctx, &declarator);

    DeclaratorPart *funDeclarator = declarator.functionDeclarator;
    Coordinates eqCoords = { ctx->token };
    if (nextTokenIf(ctx, '=')) {
        if (specifiers.flags.bits.isExternal) {
            reportDiagnostic(ctx, DIAG_EXTERN_VAR_INIT, &declarator.coordinates);
        }
        initializer = parseInitializer(ctx, NULL);
    };

    if (initializer != NULL) {
        if (isTypeDefDeclaration || funDeclarator) {
            eqCoords.right = initializer->coordinates.right;
            reportDiagnostic(ctx, DIAG_ILLEGAL_INIT_ONLY_VARS, &eqCoords);
        }
    }

    verifyDeclarator(ctx, &declarator, DS_FILE);

    const char *name = declarator.identificator;
    coords.right = declarator.coordinates.right;

    AstDeclaration *declaration = NULL;

    if (isTypeDefDeclaration) {
      TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
      declareTypeDef(ctx, name, type);
      declaration = createAstDeclaration(ctx, DK_TYPEDEF, name);
      declaration->typeDefinition.definedType = type;
      declaration->typeDefinition.coordinates = coords;
    } else if (funDeclarator) {
      assert(funDeclarator->kind == DPK_FUNCTION);
      TypeRef *returnType = makeFunctionReturnType(ctx, &specifiers, &declarator);
      verifyFunctionReturnType(ctx, &declarator, returnType);
      unsigned i;
      funName = name;

      AstValueDeclaration *params = funDeclarator->parameters.parameters;
      functionDeclaration = createFunctionDeclaration(ctx, &coords, returnType, funName, specifiers.flags.storage, params, funDeclarator->parameters.isVariadic);
      functionDeclaration->symbol = declareFunctionSymbol(ctx, name, functionDeclaration);

      if (ctx->token->code == '{') {
          if (id_idx != 0) {
              Coordinates coords3 = { ctx->token, ctx->token };
              reportDiagnostic(ctx, DIAG_EXPECTED_SEMI_AFTER_TL_DECLARATOR, &coords3);
          }
          functionScope = funDeclarator->parameters.scope;
          break;
      } else {
          declaration = createAstDeclaration(ctx, DK_PROTOTYPE, name);
          declaration->functionProrotype = functionDeclaration;
      }
    } else {
        TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
        Boolean isTypeOk = verifyValueType(ctx, &coords, type);
        if (!isTypeOk) type = makeErrorRef(ctx);
        if (initializer) {
          initializer = finalizeInitializer(ctx, type, initializer, TRUE);
        } else {
          if (type->kind == TR_ARRAY && type->arrayTypeDesc.size == UNKNOWN_SIZE && !specifiers.flags.bits.isExternal) {
              reportDiagnostic(ctx, DIAG_ARRAY_EXPLICIT_SIZE_OR_INIT, &declarator.coordinates);
          }
        }
        AstValueDeclaration *valueDeclaration = createAstValueDeclaration(ctx, &coords, VD_VARIABLE, type, name, 0, specifiers.flags.storage, initializer);
        valueDeclaration->symbol = declareValueSymbol(ctx, name, valueDeclaration);
        declaration = createAstDeclaration(ctx, DK_VAR, name);
        declaration->variableDeclaration = valueDeclaration;
    }
    if (declaration) {
      addToFile(file, createTranslationUnit(ctx, declaration, NULL));
    }
    ++id_idx;
  } while (nextTokenIf(ctx, ','));


  // it's function definition

  // K&R param syntax is not supported yet

  if (nextTokenIf(ctx, ';')) return;

  if (functionScope == NULL) {
      // some error ocured
      functionScope = newScope(ctx, ctx->currentScope);
  }

  AstValueDeclaration *va_area_var = NULL;
  ctx->locals = NULL;
  ctx->functionReturnType = functionDeclaration ? functionDeclaration->returnType : makeErrorRef(ctx);
  ctx->stateFlags.hasSmallStructs = 0;
  ctx->currentScope = functionScope;

  if (functionDeclaration && functionDeclaration->isVariadic) {
      TypeRef *vatype = makeArrayType(ctx, 4 + 6 + 8, makePrimitiveType(ctx, T_U8, 0));
      Coordinates vacoords = { ctx->token, ctx->token };
      va_area_var = createAstValueDeclaration(ctx, &vacoords, VD_VARIABLE, vatype, "__va_area__", 0, 0, NULL);
      va_area_var->flags.bits.isLocal = 1;
      va_area_var->symbol = declareValueSymbol(ctx, va_area_var->name, va_area_var);
  }

  AstStatement *body = parseFunctionBody(ctx);
  verifyLabels(ctx);
  ctx->functionReturnType = NULL;
  ctx->currentScope = functionScope->parent;

  if (functionDeclaration) {
    AstFunctionDefinition *definition = createFunctionDefinition(ctx, functionDeclaration, functionScope, body);
    definition->scope = functionScope;
    definition->locals = ctx->locals;
    definition->va_area = va_area_var;
    definition->hasSmallStructs = ctx->stateFlags.hasSmallStructs;

    AstTranslationUnit *newUnit = createTranslationUnit(ctx, NULL, definition);
    addToFile(file, newUnit);
  }
}

/**
ast_file
    : declaration
    | ast_file declaration
    ;
*/

static void initializeContext(ParserContext *ctx) {
  ctx->anonSymbolsCounter = 0;

  ctx->memory.tokenArena = createArena("Tokens Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.macroArena = createArena("Macros Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.astArena = createArena("AST Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.typeArena = createArena("Types Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.stringArena = createArena("String Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.diagnosticsArena = createArena("Diagnostic Arena", DEFAULT_CHUNCK_SIZE);
  ctx->memory.codegenArena = createArena("Codegen Arena", DEFAULT_CHUNCK_SIZE);

  ctx->rootScope = ctx->currentScope = newScope(ctx, NULL);

  ctx->macroMap = createHashMap(DEFAULT_MAP_CAPACITY, stringHashCode, stringCmp);

  initializeProprocessor(ctx);
}

static void releaseContext(ParserContext *ctx) {

  Scope *scope = ctx->scopeList;

  while (scope) {
      releaseHashMap(scope->symbols);
      scope = scope->next;
  }

  releaseArena(ctx->memory.tokenArena);
  releaseArena(ctx->memory.macroArena);
  releaseArena(ctx->memory.typeArena);
  releaseArena(ctx->memory.astArena);
  releaseArena(ctx->memory.stringArena);
  releaseArena(ctx->memory.diagnosticsArena);
  releaseArena(ctx->memory.codegenArena);

  LocationInfo *locInfo = ctx->locationInfo;

  while (locInfo) {
      LocationInfo *next = locInfo->next;
      if (locInfo->kind != LIK_CONST_MACRO) {
        releaseHeap((void*)locInfo->buffer);
      }
      if (locInfo->kind == LIK_FILE) {
        releaseHeap(locInfo->fileInfo.linesPos);
      }
      releaseHeap(locInfo);

      locInfo = next;
  }

  releaseHashMap(ctx->macroMap);
}

static Boolean printDiagnostics(Diagnostics *diagnostics, Boolean verbose) {
  Diagnostic *diagnostic = diagnostics->head;

  Boolean hasError = FALSE;

  while (diagnostic) {
      FILE *output = stderr;
      printDiagnostic(output, diagnostic, verbose);
      fputc('\n', output);
      if (getSeverity(diagnostic->descriptor->severityKind)->isError) {
          hasError = TRUE;
      }
      diagnostic = diagnostic->next;
  }

  return hasError;
}

/**
translation_unit
    : external_declaration+
 */
static AstFile *parseFile(ParserContext *ctx) {
  AstFile *astFile = createAstFile(ctx);
  ctx->parsedFile = astFile;
  astFile->fileName = ctx->config->fileToCompile;
  nextToken(ctx);

  while (ctx->token->code) {
      parseExternalDeclaration(ctx, astFile);
  }

  return astFile;
}

static void dumpFile(AstFile *file, const char* dumpFile) {
  remove(dumpFile);
  FILE* toDump = fopen(dumpFile, "w");
  dumpAstFile(toDump, file);
  fclose(toDump);
}

static void printMemoryStatistics(ParserContext *ctx) {
  extern size_t heapBytesAllocated;
  const size_t kb = 1024;

  printf("Heap bytes allocated: %lu bytes (%lu kb)\n", heapBytesAllocated, heapBytesAllocated / kb);
  printArenaStatistic(stdout, ctx->memory.tokenArena);
  printArenaStatistic(stdout, ctx->memory.stringArena);
  printArenaStatistic(stdout, ctx->memory.astArena);
  printArenaStatistic(stdout, ctx->memory.typeArena);
  printArenaStatistic(stdout, ctx->memory.diagnosticsArena);
  printArenaStatistic(stdout, ctx->memory.codegenArena);
  fflush(stdout);
}

static unsigned needSpace(char c, const Token *p, const Token *t) {
  if (t == NULL || p == NULL) return 0;
  if (t->length == 0 || t->startOfLine) return 0;

  char nc = t->pos[0];

  int pcode = p->rawCode;
  int ncode = t->rawCode;

  return 'E' == c ? '+' == ncode || '-' == ncode
      : '+' == pcode ? INC_OP == ncode || '+' == ncode
      : '-' == pcode ? DEC_OP == ncode || '-' == ncode
      : pcode == IDENTIFIER ? ncode == IDENTIFIER
      : pcode == I_CONSTANT_RAW || pcode == F_CONSTANT_RAW ? ncode == IDENTIFIER
      : 0;
}

const char *joinToStringTokenSequence(ParserContext *ctx, Token *s) {

  StringBuffer sb = { 0 };

  Token *t = s, *p = NULL;

  while (t && t->rawCode) {
      if (sb.idx && t->startOfLine) {
          putSymbol(&sb, '\n');
      }

      if (t->hasLeadingSpace || sb.ptr && needSpace(sb.ptr[sb.idx - 1], p, t)) {
          putSymbol(&sb, ' ');
      }

      unsigned idx;
      for (idx = 0; idx < t->length; ++idx) {
          putSymbol(&sb, t->pos[idx]);
      }

      p = t;
      t = t->next;
  }

  return sb.ptr;
}



static void printPPOutput(ParserContext *ctx) {
  const char *r = joinToStringTokenSequence(ctx, ctx->firstToken);
  if (r) {
    fprintf(stdout, "%s\n", r);
    releaseHeap((char*)r);
  }
}

void compileFile(Configuration * config) {
  unsigned lineNum = 0;
  ParserContext context = { 0 };
  context.config = config;

  initializeContext(&context);

  context.stateFlags.inPP = 1;

  Token eof = { 0 };
  Token *startToken = tokenizeFile(&context, config->fileToCompile, &eof);

  if (!startToken) {
      fprintf(stderr, "Cannot open file %s\n", config->fileToCompile);
      return;
  }

  context.stateFlags.inPP = 0;
  context.firstToken = startToken;

  if (config->ppOutput) {
      printDiagnostics(&context.diagnostics, config->verbose);
      printPPOutput(&context);
      return;
  }


  AstFile *astFile = parseFile(&context);

  Boolean hasError = printDiagnostics(&context.diagnostics, config->verbose);

  if (config->memoryStatistics) {
      printMemoryStatistics(&context);
  }

  if (config->dumpFileName) {
      dumpFile(astFile, config->dumpFileName);
  }

  if (!hasError) {
    cannonizeAstFile(&context, astFile);
    if (config->canonDumpFileName) {
      dumpFile(astFile, config->canonDumpFileName);
    }

    if (!config->skipCodegen) {
      GeneratedFile *genFile = generateCodeForFile(&context, astFile);
    }
  }

  releaseContext(&context);
}
