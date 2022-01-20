

#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

#include <signal.h>

#include "tokens.h"
#include "lex.h"
#include "tree.h"
#include "parser.h"
#include "mem.h"
#include "sema.h"

#include "treeDump.h"

#define INITIAL_FILE_CAPACITY 30

extern TypeDesc *errorTypeDescriptor;
extern TypeDesc builtInTypeDescriptors[];

static Token* nextToken(ParserContext *ctx);

static int nextTokenIf(ParserContext *ctx, int nextIf) {
    if (ctx->token->code == nextIf) {
        nextToken(ctx);
        return 1;
    }
    return 0;
}

void parseError(ParserContext *ctx, const char* fmt, ...) {

    char buffer[1024] = {0};

    unsigned ln = ctx->locationInfo.lineno;
    unsigned cn = ctx->locationInfo.position;
    if (ln) {
      cn -= ctx->locationInfo.linesPos[ln - 1];
    }

    va_list myargs;
    va_start(myargs, fmt);
    vsprintf(buffer, fmt, myargs);
    va_end(myargs);

    fprintf(stderr, "%s:%d:%d: error: %s\n", ctx->parsedFile->fileName, cn + 1, ln + 1, buffer);
    fflush(stdout);
    fflush(stderr);
}

void parseWarning(ParserContext *ctx, const char* fmt, ...) {

    char buffer[1024] = {0};

    unsigned ln = ctx->locationInfo.lineno;
    unsigned cn = ctx->locationInfo.position;
    if (ln) {
      cn -= ctx->locationInfo.linesPos[ln - 1];
    }

    va_list myargs;
    va_start(myargs, fmt);
    vsprintf(buffer, fmt, myargs);
    va_end(myargs);

    fprintf(stdout, "%s:%d:%d: warning: %s\n", ctx->parsedFile->fileName, cn + 1, ln + 1, buffer);
    fflush(stdout);
}

static void reportUnexpectedToken(ParserContext *ctx, int expected) {

    int actual = ctx->token->code;
    char eb[2], ab[2];
    const char* actToken = tokenNameInBuffer(actual, ab);
    const char* yytext = ctx->token->text;
    const char* expToken = tokenNameInBuffer(expected, eb);
    parseError(ctx, "unexpected token %s '%s' instead of %s", actToken, yytext, expToken);
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


static const char* copyLiteralString(ParserContext *ctx) {

    int yyleng = yyget_leng(ctx->scanner);
    const char* yytext = yyget_text(ctx->scanner);

    char* r = (char*)heapAllocate(yyleng + 1);
    strncpy(r, yytext, yyleng + 1);

    return r;
}

static Token *allocToken(ParserContext *ctx) {
  return (Token *)areanAllocate(ctx->tokenArena, sizeof(Token));
}

static EnumConstant *enumConstant(ParserContext *ctx, const char* name) {
  Symbol *s = findSymbol(ctx, name);
  if (s && s->kind == EnumConstSymbol) return s->enumerator;
  return NULL;
}

static void dumpToken(char *buffer, size_t bsize, Token *token) {

  char *bf = buffer;
  size_t l = snprintf(bf, bsize, "Token '%s':%d", tokenName(token->code), token->code);
  bf += l; bsize -= l;

  if (token->code != token->rawCode) {
    l = snprintf(bf, bsize, " (raw '%s': %d)", tokenName(token->rawCode), token->rawCode);
    bf += l; bsize -= l;
  }

  l = snprintf(bf, bsize, ", coordinates [%d, %d]", token->coordinates.startOffset, token->coordinates.endOffset);
  bf += l; bsize -= l;

  if (token->text) {
    l = snprintf(bf, bsize, ", text \"%s\"", token->text);
    bf += l; bsize -= l;
  }

  if (token->code == I_CONSTANT) {
      l = snprintf(bf, bsize, ", integer value '%lld'", token->value.iv);
      bf += l; bsize -= l;
  }

  if (token->code == F_CONSTANT) {
      l = snprintf(bf, bsize, ", float value '%f'", token->value.dv);
      bf += l; bsize -= l;
  }
}

static Token* nextToken(ParserContext *ctx) {
    Token *cur = ctx->token;

    if (cur) {
        if (cur->next) {
            ctx->token = cur->next;
            return cur->next;
        }
    }

    YYSTYPE dummy = 0;
    int rawToken;
    int endOffset;
    for (;;) {
      rawToken = yylex(&dummy, &ctx->locationInfo.position, ctx->scanner);
      endOffset = ctx->locationInfo.position;
      if (rawToken == DANGLING_NEWLINE || rawToken == NEWLINE) {
          unsigned thisLine = ctx->locationInfo.lineno++;
          assert(thisLine < ctx->locationInfo.lineCount);
          ctx->locationInfo.linesPos[thisLine] = endOffset;
      } else break;
    }

    cur = allocToken(ctx);

    if (ctx->firstToken == NULL)
      ctx->firstToken = cur;

    size_t tokenLength = yyget_leng(ctx->scanner);

    cur->code = cur->rawCode = rawToken;
    cur->coordinates.endOffset = endOffset;
    cur->coordinates.startOffset = endOffset - tokenLength;

    if (rawToken == IDENTIFIER)  { // aka "Lexer hack"
        cur->text = copyLiteralString(ctx);

        if (isTypeName(ctx, cur->text, ctx->currentScope)) {
            cur->code = TYPE_NAME;
        } else {
          EnumConstant *enumerator = enumConstant(ctx, cur->text);
          if (enumerator) {
            cur->code = ENUM_CONST;
            cur->value.iv = enumerator->value;
          }
        }
    } else if (rawToken == I_CONSTANT) {
        cur->value.iv = atoll(yyget_text(ctx->scanner));
    } else if (rawToken == F_CONSTANT) {
        cur->value.dv = atof(yyget_text(ctx->scanner));
    } else if (rawToken == STRING_LITERAL) {
        cur->text = copyLiteralString(ctx);
    }

    if (ctx->token) {
        ctx->token->next = cur;
    }
    ctx->token = cur;


    char buffer[1024];
    dumpToken(buffer, sizeof buffer, cur);
    printf("%s\n", buffer); fflush(stdout);

    return cur;
}

enum DeclaratorKind {
    DK_ABSTRACT = BIT(0),
    DK_NON_ABSTRACT = BIT(1),
    DK_ANY = DK_ABSTRACT | DK_NON_ABSTRACT
};


static void parseDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, int isDeclaration, struct _Scope* scope);

static void parseDeclarator(ParserContext *ctx, Declarator *declarator, int kind);

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
static int isAssignmentOperator(int token) {
    return token == '=' || token == MUL_ASSIGN || token == DIV_ASSIGN || token == MOD_ASSIGN || token == ADD_ASSIGN ||
            token == SUB_ASSIGN || token == LEFT_ASSIGN || token == RIGHT_ASSIGN || token == AND_ASSIGN ||
            token == XOR_ASSIGN || token == OR_ASSIGN;
}

static int assignOpTokenToEB(int token) {
    switch (token) {
        case '=': return EB_ASSIGN;
        case MUL_ASSIGN: return EB_MUL_ASSIGN;
        case DIV_ASSIGN: return EB_DIV_ASSIGN;
        case MOD_ASSIGN: return EB_MOD_ASSIGN;
        case ADD_ASSIGN: return EB_ADD_ASSIGN;
        case SUB_ASSIGN: return EB_SUB_ASSIGN;
        case LEFT_ASSIGN: return EB_LEFT_ASSIGN;
        case RIGHT_ASSIGN: return EB_RIGHT_ASSIGN;
        case AND_ASSIGN: return EB_AND_ASSIGN;
        case XOR_ASSIGN: return EB_XOR_ASSIGN;
        case OR_ASSIGN: return EB_OR_ASSIGN;
    }

    assert(0);
    return -1;
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
static AstExpression* parseConditionalExpression(ParserContext *ctx, struct _Scope* scope);
static AstExpression* parseAssignmentExpression(ParserContext *ctx, struct _Scope* scope);


static AstConst* parseConstExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* expression = parseConditionalExpression(ctx, scope);
    return eval(ctx, expression);
}

static int parseAsIntConst(ParserContext *ctx, struct _Scope* scope) {
    AstConst* expr = parseConstExpression(ctx, scope);
    if (!expr) return 42;
    if (expr->op != EC_INT_CONST) {
        parseError(ctx, "Expected integer const, not %d", expr->op);
    }
    return (int)expr->i;
}

/**
primary_expression
    : IDENTIFIER
    | CONSTANT
    | STRING_LITERAL
    | '(' expression ')'
    ;
 */
static AstExpression* parsePrimaryExpression(ParserContext *ctx, struct _Scope *scope) {
    AstExpression *result = NULL;
    int so = ctx->token->coordinates.startOffset;
    int eo = ctx->token->coordinates.endOffset;
    switch (ctx->token->code) {
        case IDENTIFIER:
            result = createNameRef(ctx, so, eo, ctx->token->text);
            break;
        case ENUM_CONST:
        case I_CONSTANT: {
            int64_const_t l = ctx->token->value.iv;
            result = createAstConst(ctx, so, eo, EC_INT_CONST, &l);
            break;
        }
        case F_CONSTANT: {
            float64_const_t f = ctx->token->value.dv;
            result = createAstConst(ctx, so, eo, EC_FLOAT_CONST, &f);
            break;
        }
        case STRING_LITERAL: {
            const char* s = ctx->token->text;
            result = createAstConst(ctx, so, eo, EC_STRING_LITERAL, &s);
            break;
        }
        case 0:
          return createErrorExpression(ctx, so, eo);
        default: {
            consume(ctx, '(');
            AstExpression* expr = parseExpression(ctx, scope);
            consume(ctx, ')');
            return expr;
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
    AstExpressionList *head = NULL, *tail = NULL;

    do {
      AstExpression *expr = parseAssignmentExpression(ctx, scope);
      AstExpressionList *node = (AstExpressionList*)areanAllocate(ctx->astArena, sizeof(AstExpressionList));
      node->expression = expr;
      if (tail) {
          tail->next = node;
      } else {
          head = node;
      }
      tail = node;
    } while (nextTokenIf(ctx, ','));

    return head;
}


/**
type_name
    : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    ;
 */
static TypeRef* parseTypeName(ParserContext *ctx, struct _Scope *scope) {
    DeclarationSpecifiers specifiers = { 0 };
    parseDeclarationSpecifiers(ctx, &specifiers, FALSE, scope);
    Declarator declarator= { 0 };
    if (ctx->token->code != ')') {
        parseDeclarator(ctx, &declarator, DK_ABSTRACT);
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
    AstExpression *right = NULL;

    int op, so, eo;

    for (;;) {
        AstExpressionList *arguments = NULL;
        so = left->coordinates.startOffset;
        switch (ctx->token->code) {
        case '[': // '[' expression ']'
            nextToken(ctx);
            right = parseExpression(ctx, scope);
            eo = ctx->token->coordinates.endOffset;
            consume(ctx, ']');
            left = createBinaryExpression(ctx, EB_A_ACC, left, right);
            left->coordinates.endOffset = eo; // more precise
            break;
        case '(': // '(' argument_expression_list? ')'
            nextToken(ctx);
            if (ctx->token->code != ')') {
                arguments = parseArgumentExpressionList(ctx, scope);
            }
            eo = ctx->token->coordinates.endOffset;
            consume(ctx, ')');
            left = createCallExpression(ctx, so, eo, left, arguments);
            break;
        case '.':    op = EF_DOT; goto acc;// '.' IDENTIFIER
        case PTR_OP: op = EF_ARROW; // PTR_OP IDENTIFIER
        acc:
            expect(ctx, IDENTIFIER);
            eo = ctx->token->coordinates.endOffset;
            left = createFieldExpression(ctx, so, eo, op, left, ctx->token->text);
            nextToken(ctx);
            break;
        case INC_OP: op = EU_POST_INC; goto incdec;
        case DEC_OP: op = EU_POST_DEC;
        incdec:
            eo = ctx->token->coordinates.endOffset;
            left = createUnaryExpression(ctx, so, eo, op, left);
            nextToken(ctx);
            break;
        default: return left;
        }
    }
}

/**
unary_expression
    : postfix_expression
    | (INC_OP | DEC_OP) unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')'
    ;
 */
static AstExpression* parseUnaryExpression(ParserContext *ctx, struct _Scope* scope) {
    int op;
    AstExpression* argument;
    int so = ctx->token->coordinates.startOffset, eo = -1;
    switch (ctx->token->code) {
        case INC_OP: op = EU_PRE_INC; goto ue1;
        case DEC_OP: op = EU_PRE_DEC;
        ue1:
            nextToken(ctx);
            argument = parseUnaryExpression(ctx, scope);
            eo = argument->coordinates.endOffset;
            return createUnaryExpression(ctx, so, eo, op, argument);
        case '&': op = EU_REF; goto ue2;
        case '*': op = EU_DEREF; goto ue2;
        case '+': op = EU_PLUS; goto ue2;
        case '-': op = EU_MINUS; goto ue2;
        case '~': op = EU_TILDA; goto ue2;
        case '!': op = EU_EXL;
        ue2:
            nextToken(ctx);
            argument = parseCastExpression(ctx, scope);
            eo = argument->coordinates.endOffset;
            return createUnaryExpression(ctx, so, eo, op, argument);
        case SIZEOF: {
            int token = nextToken(ctx)->code;
            if (token == '(') {
                token = nextToken(ctx)->code;
                if (isSpecifierQualifierList(token)) {
                    TypeRef* typeRef = parseTypeName(ctx, scope);
                    eo = ctx->token->coordinates.endOffset;
                    consume(ctx, ')');
                    unsigned long long c = 42; // TODO: compute size of type
                    return createAstConst(ctx, so, eo, T_U4, &c);
                } else {
                    // TODO: should be const too
                    argument = parseExpression(ctx, scope);
                    eo = ctx->token->coordinates.endOffset;
                    consume(ctx, ')');
                    return createUnaryExpression(ctx, so, eo, EU_SIZEOF, argument);
                }
            } else {
                // TODO: should be const too
                argument = parseUnaryExpression(ctx, scope);
                eo = argument->coordinates.endOffset;
                return createUnaryExpression(ctx, so, eo, EU_SIZEOF, argument);
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
        int so = ctx->token->coordinates.startOffset;
        nextToken(ctx);
        if (isSpecifierQualifierList(ctx->token->code)) {
            TypeRef* typeRef = parseTypeName(ctx, scope);
            consume(ctx, ')');
            AstExpression* argument = parseCastExpression(ctx, scope);
            int eo = argument->coordinates.endOffset;
            return createCastExpression(ctx, so, eo, typeRef, argument);
        } else {
            AstExpression *result = parseExpression(ctx, scope);
            consume(ctx, ')');
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
        int op = tokenCode == '*' ? EB_MUL : tokenCode == '/' ? EB_DIV : EB_MOD;
        nextToken(ctx);
        AstExpression* tmp = parseCastExpression(ctx, scope);
        result = createBinaryExpression(ctx, op, result, tmp);
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
        int op = tokenCode == '+' ? EB_ADD : EB_SUB;
        nextToken(ctx);
        AstExpression* tmp = parseMultiplicativeExpression(ctx, scope);
        result = createBinaryExpression(ctx, op, result, tmp);
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
        int op = tokenCode == LEFT_OP ? EB_LHS : EB_RHS;
        nextToken(ctx);
        AstExpression* tmp = parseAdditiveExpression(ctx, scope);
        result = createBinaryExpression(ctx, op, result, tmp);
        tokenCode = ctx->token->code;
    }

    return result;
}

/**
relational_expression
    : shift_expression ( ( '<' | '>' | LE_OP | GE_OP ) shift_expression )*
    ;
 */

static int isRelationalOperator(int token) {
    return token == '>' || token == '<' || token == LE_OP || token == GE_OP;
}

static int relationalTokenToOp(int token) {
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
        int op = relationalTokenToOp(ctx->token->code);
        nextToken(ctx);
        AstExpression* tmp = parseShiftExpression(ctx, scope);
        result = createBinaryExpression(ctx, op, result, tmp);
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
static int isEqualityOperator(int token) {
    return token == EQ_OP || token == NE_OP;
}

static int equalityTokenToOp(int token) {
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
        nextToken(ctx);
        AstExpression* tmp = parseRelationalExpression(ctx, scope);
        result = createBinaryExpression(ctx, op, result, tmp);
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
        nextToken(ctx);
        AstExpression* tmp = parseEqualityExpression(ctx, scope);
        result = createBinaryExpression(ctx, EB_AND, result, tmp);
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
        nextToken(ctx);
        AstExpression* tmp = parseAndExpression(ctx, scope);
        result = createBinaryExpression(ctx, EB_XOR, result, tmp);
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
        nextToken(ctx);
        AstExpression* tmp = parseExcOrExpression(ctx, scope);
        result = createBinaryExpression(ctx, EB_OR, result, tmp);
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
        nextToken(ctx);
        AstExpression* tmp = parseIncOrExpression(ctx, scope);
        result = createBinaryExpression(ctx, EB_ANDAND, result, tmp);
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
        nextToken(ctx);
        AstExpression* tmp = parseLogicalAndExpression(ctx, scope);
        result = createBinaryExpression(ctx, EB_OROR, result, tmp);
    }

    return result;
}

/**
conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
 */
static AstExpression* parseConditionalExpression(ParserContext *ctx, struct _Scope* scope) {
    AstExpression* left = parseLogicalOrExpression(ctx, scope);

    if (ctx->token->code == '?') {
        nextToken(ctx);
        AstExpression* ifTrue = parseExpression(ctx, scope);
        consume(ctx, ':');
        AstExpression* ifFalse = parseConditionalExpression(ctx, scope);
        return createTernaryExpression(ctx, left, ifTrue, ifFalse);
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
    if (isAssignmentOperator(ctx->token->code)) {
        int op = assignOpTokenToEB(ctx->token->code);
        // check if left is valid
        nextToken(ctx);
        AstExpression* right = parseAssignmentExpression(ctx, scope);
        return createBinaryExpression(ctx, op, left, right);
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
        expression = createBinaryExpression(ctx, EB_COMMA, expression, right);
    }

    return expression;
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
        int so = ctx->token->coordinates.startOffset;
        int eo = eo = ctx->token->coordinates.endOffset;
        const char* name = NULL;
        if (token == IDENTIFIER) {
            name = ctx->token->text;
            token = nextToken(ctx)->code;
        } else {
           parseError(ctx, "Expecting IDENTIFIER in enum list but found %s", tokenName(token));
        }

        int64_const_t v;
        if (nextTokenIf(ctx, '=')) {
            eo = ctx->token->coordinates.endOffset; // TODO: fix
            v = parseAsIntConst(ctx, scope);
            idx = v + 1;
        } else {
            v = idx++;
        }
        token = ctx->token->code;
        EnumConstant *enumerator = createEnumConst(ctx, so, eo, name, v);
        declareEnumConstantSymbol(ctx, enumerator);
        AstStructMember *member = createStructMember(ctx, NULL, NULL, enumerator);
        if (tail) {
            tail->next = member;
        } else {
            head = member;
        }
        tail = member;
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
    int so = ctx->token->coordinates.startOffset;
    int token = nextToken(ctx)->code;
    int eo = ctx->token->coordinates.endOffset;

    if (token == IDENTIFIER) {
        name = ctx->token->text;
        token = nextToken(ctx)->code;
    }

    if (token == '{') {
      token = nextToken(ctx)->code;

      if (token == '}') {
          parseError(ctx, "use of empty enum");
      }

      members = parseEnumeratorList(ctx, scope);
      eo = ctx->token->coordinates.endOffset;
      consume(ctx, '}');
    }

    return createSUEDeclaration(ctx, so, eo, DKX_ENUM, TRUE, name, members);
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
static AstStructMember *parseStructDeclarationList(ParserContext *ctx, struct _Scope* scope) {
    AstStructMember *head = NULL, *tail = NULL;
    int token = ctx->token->code;
    do {
        int so = ctx->token->coordinates.startOffset;
        DeclarationSpecifiers specifiers = { 0 };
        parseDeclarationSpecifiers(ctx, &specifiers, TRUE, scope);

        if (specifiers.defined) {
            AstSUEDeclaration *definition = specifiers.defined;
            AstDeclaration *declaration = createAstDeclaration(ctx, definition->kind, definition->name);
            declaration->structDeclaration = definition;
            AstStructMember *m1 = createStructMember(ctx, declaration, NULL, NULL);
            if (tail) {
                tail->next = m1;
            } else {
                head = m1;
            }
            tail = m1;
        }

        for (;;) {
            Declarator declarator = { 0 };
            if (ctx->token->code != ':') {
                parseDeclarator(ctx, &declarator, DK_NON_ABSTRACT);
            }
            int width = -1;
            if (ctx->token->code == ':') {
                nextToken(ctx);
                width = parseAsIntConst(ctx, scope);
                // check width (size and sign)
            }

            int eo = ctx->token->coordinates.endOffset;
            TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
            const char *name = declarator.identificator;
            AstDeclaration *declaration = NULL;
            AstStructDeclarator *structDeclarator = NULL;
            if (specifiers.flags.bits.isTypedef) {
              declareTypeDef(ctx, name, type);
              declaration = createAstDeclaration(ctx, DKX_TYPEDEF, name);
              declaration->typeDefinition.definedType = type;
              declaration->typeDefinition.coordinates.startOffset = so;
              declaration->typeDefinition.coordinates.endOffset = eo;

            } else {
              structDeclarator = createStructDeclarator(ctx, so, eo, type, name, width);
            }
            AstStructMember *member = createStructMember(ctx, declaration, structDeclarator, NULL);
            if (tail) {
                tail->next = member;
            } else {
                head = member;
            }
            tail = member;
            if (ctx->token->code == ',') nextToken(ctx);
            else break;
        }

        consume(ctx, ';');
    } while (ctx->token->code != '}');

    return head;
}

/**
struct_or_union_specifier
    : struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;

struct_or_union
    : STRUCT
    | UNION
    ;
 */
static AstSUEDeclaration* parseStructOrUnionDeclaration(ParserContext *ctx, int kind, struct _Scope* scope) {
    const char *name = NULL;
    AstStructMember *members = NULL;
    unsigned isDefinition = FALSE;

    int so = ctx->token->coordinates.startOffset;
    int token = nextToken(ctx)->rawCode;
    int eo = ctx->token->coordinates.endOffset;

    if (token == IDENTIFIER) { // typedef'ed typename is valid struct name
        name = ctx->token->text;
        token = nextToken(ctx)->code;
    }

    if (token != '{') {
        goto done;
    }
    token = nextToken(ctx)->code;
    isDefinition = TRUE;

    eo = ctx->token->coordinates.endOffset;
    if (nextTokenIf(ctx, '}')) {
        goto done;
    }

    members = parseStructDeclarationList(ctx, scope);

    eo = ctx->token->coordinates.endOffset;
    consume(ctx, '}');

done:
    return createSUEDeclaration(ctx, so, eo, kind, isDefinition, name, members);
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

enum {
  SCS_NONE,
  SCS_REGISTER,
  SCS_STATIC,
  SCS_EXTERN,
  SCS_TYPEDEF,
  SCS_ERROR
};

enum {
  TSW_NONE,
  TSW_LONG,
  TSW_LONGLONG,
  TSW_SHORT,
  TSW_ERROR
};

enum {
  TSS_NONE,
  TSS_SIGNED,
  TSS_UNSIGNED,
  TSS_ERROR
};

enum {
  TST_NONE,
  TST_VOID,
  TST_CHAR,
  TST_INT,
  TST_FLOAT,
  TST_DOUBLE,
  TST_ERROR
};

enum {
  TQT_NONE,
  TQT_CONST,
  TQT_VOLATILE,
  TQT_ERROR
};

static TypeDesc *computePrimitiveTypeDescriptor(ParserContext *ctx, unsigned tsw, const char *tsw_s, unsigned tss, const char *tss_s, unsigned tst, const char *tst_s) {
  if (tsw == TST_ERROR || tss == TSS_ERROR || tst == TST_ERROR) {
      return errorTypeDescriptor; // TODO: return special errorType
  }

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
            return &builtInTypeDescriptors[T_F8];
          } else {
            parseError(ctx, "'long double' cannot be signed or unsigned");
            return errorTypeDescriptor;
          }
      }

      if (tst == TST_NONE || tst == TST_INT) {
        if (tsw == TSW_SHORT) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U2 : T_S2];
        if (tsw == TSW_LONG) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U4 : T_S4];
        if (tsw == TSW_LONGLONG) return &builtInTypeDescriptors[tss == TSS_UNSIGNED ? T_U8 : T_S8];
      }

      assert(tss_s || tsw_s || tst_s);

      const char *s1 = tss_s == NULL ? "" : tss_s;
      const char *s2 = tss_s == NULL ? "" : " ";
      const char *s3 = tsw_s == NULL ? "" : tsw_s;
      const char *s4 = tsw_s == NULL ? "" : " ";
      const char *s5 = tst_s == NULL ? "" : tst_s;
      parseError(ctx, "'%s%s%s%s%s' is invalid", s1, s2, s3, s4, s5);
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

      parseError(ctx, "'%s' cannot be signed or unsigned", tst_s);
      return errorTypeDescriptor;
  }


  if (tst == TST_VOID) return &builtInTypeDescriptors[T_VOID];
  if (tst == TST_CHAR) return &builtInTypeDescriptors[T_S1];
  if (tst == TST_INT) return &builtInTypeDescriptors[T_S4];
  if (tst == TST_FLOAT) return &builtInTypeDescriptors[T_F4];
  if (tst == TST_DOUBLE) return &builtInTypeDescriptors[T_F8];

  unreachable("Type has to be specicied by this point");
}

static const char *duplicateMsgFormater = "duplicate '%s' declaration specifier";
static const char *nonCombineMsgFormater = "cannot combine with previous '%s' declaration specifier";

static void parseDeclarationSpecifiers(ParserContext *ctx, DeclarationSpecifiers *specifiers, int isDeclaration, struct _Scope* scope) {
    unsigned scs = SCS_NONE;
    const char *scs_s = NULL;
    unsigned tsw = TSW_NONE, tss = TSS_NONE, tst = TST_NONE;
    const char *tsw_s = NULL, *tss_s = NULL, *tst_s = NULL;

    unsigned tmp = 0;
    const char *tmp_s = NULL;

    const char *prefix;
    int typeId, symbolId;

    int seenTypeSpecifier = FALSE;

    do {
        switch (ctx->token->code) {
        // storage class specifier
        case REGISTER: tmp = SCS_REGISTER; tmp_s = "register"; goto scs_label;
        case STATIC: tmp = SCS_STATIC; tmp_s = "static"; goto scs_label;
        case EXTERN: tmp = SCS_EXTERN; tmp_s = "extern"; goto scs_label;
        case TYPEDEF: tmp = SCS_TYPEDEF; tmp_s = "typedef"; goto scs_label;
        scs_label:
            if (scs != SCS_ERROR) {
              if (scs == SCS_NONE && isDeclaration) {
                  scs = tmp;
                  scs_s  = tmp_s;
              } else {
                  scs = SCS_ERROR;
                  if (isDeclaration) {
                    parseError(ctx, duplicateMsgFormater, scs_s);
                  } else {
                    parseError(ctx, "Unexpected storage type %s", tmp_s);
                  }
              }
            }
            break;
        // type qualifiers
        case CONST:    tmp = TQT_CONST; goto tq_label;
        case VOLATILE: tmp = TQT_VOLATILE; goto tq_label;
        tq_label:
            if (specifiers->flags.bits.isConst && tmp == TQT_CONST || specifiers->flags.bits.isVolatile && tmp == TQT_VOLATILE) {
                parseWarning(ctx, duplicateMsgFormater, tokenName(ctx->token->code));
            }
            specifiers->flags.bits.isConst |= tmp == TQT_CONST;
            specifiers->flags.bits.isVolatile |= tmp == TQT_VOLATILE;
            break;

       case SIGNED: tmp = TSS_SIGNED; tmp_s = "signed"; goto tss_label;
       case UNSIGNED: tmp = TSS_UNSIGNED; tmp_s = "unsigned"; goto tss_label;
       tss_label:
            seenTypeSpecifier = TRUE;
            if (tss != TSS_ERROR) {
              if (tss == tmp) {
                  parseWarning(ctx, duplicateMsgFormater, tmp_s);
              } else if (tss == TSS_NONE) {
                  tss = tmp;
                  tss_s = tmp_s;
              } else {
                  tss = TSS_ERROR;
                  parseError(ctx, duplicateMsgFormater, tss_s);
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
                      parseWarning(ctx, duplicateMsgFormater, tmp_s);
                  } else {
                      tsw = TSW_LONGLONG;
                      tsw_s = "long long";
                  }
              } else {
                  tsw = TSW_ERROR;
                  parseError(ctx, nonCombineMsgFormater, tsw_s);
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
                    parseError(ctx, nonCombineMsgFormater, tst_s);
                }
            }
            break;
        case STRUCT: typeId = T_STRUCT; symbolId = StructSymbol; prefix = "$"; goto sue;
        case UNION:  typeId = T_UNION; symbolId = UnionSymbol; prefix = "|"; goto sue;
        case ENUM:   typeId = T_ENUM; symbolId = EnumSymbol; prefix = "#"; goto sue;
        sue:
        seenTypeSpecifier = TRUE;
        {
            if (typeId != T_ENUM) {
              ctx->currentScope = newScope(ctx, ctx->currentScope);
            }
            AstSUEDeclaration *declaration = typeId == T_ENUM
                ? parseEnumDeclaration(ctx, scope)
                : parseStructOrUnionDeclaration(ctx, typeId == T_STRUCT ? DKX_STRUCT : DKX_UNION, scope);

            if (typeId != T_ENUM) {
              ctx->currentScope = ctx->currentScope->parent;
            }
            if (declaration->isDefinition)
              specifiers->defined = declaration;

            const char* name = declaration->name;
            char tmpBuf[1024];
            int size = 0;

            TypeDesc *typeDescriptor = NULL;
            if (name) {
                int len = strlen(name);
                char *symbolName = (char *)heapAllocate(len + 1 + 1);
                size = sprintf(symbolName, "%s%s", prefix, name);
                Symbol *s;

                if (declareSUESymbol(ctx, symbolId, typeId, symbolName, declaration, &s)) {
                    releaseHeap(symbolName);
                    symbolName = NULL;
                }

                typeDescriptor = s->typeDescriptor;
            } else {
                if (declaration->isDefinition) {
                  size = sprintf(tmpBuf, "<anon$%d>", ctx->anonSymbolsCounter++);
                  name = (char *)heapAllocate(size + 1);
                  memcpy((char *)name, tmpBuf, size + 1);
                  declaration->name = name;
                  typeDescriptor = createTypeDescriptor(ctx, typeId, name, -1);
                  typeDescriptor->structInfo = declaration;
                } else {
                  parseError(ctx, "declaration of anonymous struct must be a definition");
                }
            }

            specifiers->basicType = makeBasicType(ctx, typeDescriptor, specifiers->flags.storage);

            goto almost_done;
        }
        case TYPE_NAME:
        {
            if (!seenTypeSpecifier) {
              const char *name = ctx->token->text;
              Symbol *s = findSymbol(ctx, name);
              if (s == NULL || s->kind != TypedefSymbol) {
                  parseError(ctx, "unknown type name '%s'", name);
              } else {
                  specifiers->basicType = s->typeref;
              }

              nextToken(ctx);
              goto almost_done;
            } else {
              ctx->token->code = IDENTIFIER;
            }
        }
        default: {
            if (!(tss || tsw || tst)) {
              parseWarning(ctx, "type specifier missing, defaults to 'int'");
              tst = TST_INT;
              tst_s = "int";
            }
            specifiers->basicType = makeBasicType(ctx, computePrimitiveTypeDescriptor(ctx, tsw, tsw_s, tss, tss_s, tst, tst_s), specifiers->flags.storage);

        almost_done:

            specifiers->flags.bits.isExternal = scs == SCS_EXTERN;
            specifiers->flags.bits.isStatic = scs == SCS_STATIC;
            specifiers->flags.bits.isRegister = scs == SCS_REGISTER;
            specifiers->flags.bits.isTypedef = scs == SCS_TYPEDEF;

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
    unsigned tmp;
    do {

        switch (ctx->token->code) {
          case CONST:    tmp = TQT_CONST; goto tq_label;
          case VOLATILE: tmp = TQT_VOLATILE; goto tq_label;
          tq_label:
              if (result.bits.isConst || result.bits.isVolatile) {
                  parseWarning(ctx, duplicateMsgFormater, tokenName(ctx->token->code));
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
    int kind = -1;
    int so = ctx->token->coordinates.startOffset, eo = -1;

    AstInitializer *head = NULL, *tail = NULL;

    if (nextTokenIf(ctx, '{')) {
        kind = IK_LIST;
        do {
            AstInitializer* initializer = parseInitializer(ctx, scope);
            nextTokenIf(ctx, ',');
            eo = ctx->token->coordinates.endOffset;
            if (tail) {
                tail->initializers = initializer;
            } else {
                head = initializer;
            }
            tail = initializer;
        } while (ctx->token->code != '}');
        nextToken(ctx); // eat '}'
    } else {
        kind = IK_EXPRESSION;
        expr = parseAssignmentExpression(ctx, scope);
        eo = expr->coordinates.endOffset;
    }

    return createAstInitializer(ctx, so, eo, kind, expr, head);
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

static void parseDirectDeclarator(ParserContext *ctx, Declarator *declarator, int kind);


static int isFunctionDeclarator(Declarator *declarator) {
  if (declarator->partsCounter) {
      return declarator->declaratorParts[0].kind == DPK_FUNCTION;
  }

  return FALSE;
}

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
        if (nextTokenIf(ctx, ELLIPSIS)) {
            if (idx == 0) {
                // foo(...)
                parseError(ctx, "ISO C requires a named parameter before '...'");
            } else if (!params->isVariadic) {
                ellipsisIdx = idx++;
                params->isVariadic = 1;
            }
            if (ctx->token->code != ')') {
                parseError(ctx, "expected ')'");
            }
        } else {
          int so = ctx->token->coordinates.startOffset;
          DeclarationSpecifiers specifiers = { 0 };
          parseDeclarationSpecifiers(ctx, &specifiers, FALSE, scope);
          // TODO: handle foo(void) case

          TypeRef *type = specifiers.basicType;
          TypeDesc *typeDesc = type->kind == TR_VALUE ? type->descriptorDesc : NULL;
          if (typeDesc && typeDesc->typeId == T_VOID) {
              if (ctx->token->code == ')' && idx == 0) {
                  // it's a that case foo(void), we are done
                  return;
              } else if (ctx->token->code == ')' || ctx->token->code == ',') {
                  // foo(int x, void) or foo(void, int x)
                  parseError(ctx, "'void' must be the first and only parameter if specified");
              } else if (ctx->token->code == IDENTIFIER) {
                  // foo(void x)
                  parseError(ctx, "argument may not have 'void' type");
              }
          }

          // pointer | direct_abstract_declarator | pointer direct_abstract_declarator | pointer? direct_declarator
          Declarator declarator = { 0 };
          parseDeclarator(ctx, &declarator, DK_ANY);
          int eo = ctx->token->coordinates.endOffset;
          const char *name = declarator.identificator;

          type = makeTypeRef(ctx, &specifiers, &declarator);
          AstValueDeclaration *parameter =
              createAstValueDeclaration(ctx, so, eo, VD_PARAMETER, type, name, idx++, specifiers.flags.storage, NULL);

          if (name) {
              declareValueSymbol(ctx, name, parameter);
          }

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

static void parseFunctionDeclaratorPart(ParserContext *ctx, Declarator *declarator, int kind) {
  if (isFunctionDeclarator(declarator)) {
      parseError(ctx, "function cannot return function type");
  }

  Scope *paramScope = newScope(ctx, ctx->currentScope);
  ctx->currentScope = paramScope;
  FunctionParams params = { 0 };
  if (ctx->token->code != ')') {
      parseParameterList(ctx, &params, NULL);
  }
  DeclaratorPart *part = &declarator->declaratorParts[declarator->partsCounter++];
  part->kind = DPK_FUNCTION;
  part->parameters.isVariadic = params.isVariadic;
  part->parameters.parameters = params.parameters;
  part->parameters.scope = paramScope;

  ctx->currentScope = paramScope->parent;
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
static void parseDirectDeclarator(ParserContext *ctx, Declarator *declarator, int kind) {

    if (ctx->token->rawCode == IDENTIFIER) {
        if (declarator->identificator) {
            parseError(ctx, "Identificator is already specified");
        }
        declarator->identificator= ctx->token->text;
        nextToken(ctx);
    } else if (nextTokenIf(ctx, '[')) {
        int size = ctx->token->code != ']' ? parseAsIntConst(ctx, NULL) : 0;
        consume(ctx, ']');
        DeclaratorPart *part = &declarator->declaratorParts[declarator->partsCounter++];
        part->kind = DPK_ARRAY;
        part->arraySize = size;
    } else if (nextTokenIf(ctx, '(')) {
        if (ctx->token->code != '(') {
            if (isDeclarationSpecifierToken(ctx->token->code)) {
                parseFunctionDeclaratorPart(ctx, declarator, kind);
            } else {
                parseDeclarator(ctx, declarator, kind);
            }
            consume(ctx, ')');
        }
    } else {
        return;
    }


    while (ctx->token) {
        if (nextTokenIf(ctx, '[')) {
            int size = 0;
            if (ctx->token->code != ']') {
                size = parseAsIntConst(ctx, NULL);
            }
            consume(ctx, ']');
            DeclaratorPart *part = &declarator->declaratorParts[declarator->partsCounter++];
            part->kind = DPK_ARRAY;
            part->arraySize = size;
        } else if (nextTokenIf(ctx, '(')) {
            parseFunctionDeclaratorPart(ctx, declarator, kind);
            consume(ctx, ')');
        } else {
            return;
        }
    }
}

/**
declarator
    : pointer? direct_declarator
    ;
 */
static void parseDeclarator(ParserContext *ctx, Declarator *declarator, int kind) {

    if (nextTokenIf(ctx, '*')) {
        SpecifierFlags qualifiers = parseTypeQualifierList(ctx);
        parseDeclarator(ctx, declarator, kind);

        DeclaratorPart *part = &declarator->declaratorParts[declarator->partsCounter++];
        part->kind = DPK_POINTER;
        part->flags.storage = qualifiers.storage;
    } else {
        parseDirectDeclarator(ctx, declarator, kind);
    }

    if (!(kind & DK_ABSTRACT) && declarator->identificator == NULL) {
        parseError(ctx, "expected identifier or '('");
    }
}

static AstStatement *parseCompoundStatement(ParserContext *ctx);
static AstStatement *parseStatement(ParserContext *ctx, struct _Scope* scope);

static AstStatement *parseIfStatement(ParserContext *ctx, struct _Scope* scope) {
    int so = ctx->token->coordinates.startOffset;
    consume(ctx, IF);
    consume(ctx, '(');
    AstExpression *cond = parseExpression(ctx, scope);
    consume(ctx, ')');
    AstStatement *thenB = parseStatement(ctx, scope);
    int eo = thenB->coordinates.endOffset;
    AstStatement *elseB = NULL;
    if (ctx->token->code == ELSE) {
        nextToken(ctx);
        elseB = parseStatement(ctx, scope);
        eo = elseB->coordinates.endOffset;
    }

    return createIfStatement(ctx, so, eo, cond, thenB, elseB);
}

static AstStatement *parseStatement(ParserContext *ctx, struct _Scope* scope) {
    AstExpression *expr, *expr2, *expr3;
    AstStatement *stmt;
    int op;
    int c = 0;
    int so = ctx->token->coordinates.startOffset;
    int eo = ctx->token->coordinates.endOffset;
    switch (ctx->token->rawCode) {
    case CASE:
        nextToken(ctx);
        c = parseAsIntConst(ctx, scope);
    case DEFAULT:
        expect(ctx, ':');
        nextToken(ctx);
        stmt = parseStatement(ctx, scope);
        eo = stmt->coordinates.endOffset;
        return createLabelStatement(ctx, so, eo, LK_DEFAULT, stmt, NULL, c);
    case '{': return parseCompoundStatement(ctx);
    case IF: return parseIfStatement(ctx, scope);
    case SWITCH:
        consume(ctx, SWITCH);
        consume(ctx, '(');
        expr = parseExpression(ctx, scope);
        consume(ctx, ')');
        stmt = parseStatement(ctx, scope);
        eo = stmt->coordinates.endOffset;
        return createSwitchStatement(ctx, so, eo, expr, stmt);
    case WHILE:
        consume(ctx, WHILE);
        consume(ctx, '(');
        expr = parseExpression(ctx, scope);
        consume(ctx, ')');
        stmt = parseStatement(ctx, scope);
        eo = stmt->coordinates.endOffset;
        return createLoopStatement(ctx, so, eo, SK_WHILE, expr, stmt);
    case DO:
        consume(ctx, DO);
        stmt = parseStatement(ctx, scope);
        consume(ctx, WHILE);
        consume(ctx, '(');
        expr = parseExpression(ctx, scope);
        consume(ctx, ')');
        eo = ctx->token->coordinates.endOffset;
        consume(ctx, ';');
        return createLoopStatement(ctx, so, eo, SK_DO_WHILE, expr, stmt);
    case FOR:
        consume(ctx, FOR); // for
        consume(ctx, '('); // for(

        expr = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ';'); // for( ...;

        expr2 = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ';'); // for( ...; ...;

        expr3 = ctx->token->code != ')' ? parseExpression(ctx, scope) : NULL;
        consume(ctx, ')'); // for( ...; ...; ...)

        stmt = parseStatement(ctx, scope); // for( ...; ...; ...) ...

        eo = stmt->coordinates.endOffset;
        return createForStatement(ctx, so, eo, expr, expr2, expr3, stmt);
    case GOTO:
        consume(ctx, GOTO);
        const char* label = ctx->token->text;
        consumeRaw(ctx, IDENTIFIER);
        eo = ctx->token->coordinates.endOffset;
        consume(ctx, ';');
        stmt = createJumpStatement(ctx, so, eo, SK_GOTO);
        stmt->labelStmt.label = label;
        return stmt;
    case CONTINUE:
        consume(ctx, CONTINUE);
        consume(ctx, ';');
        return createJumpStatement(ctx, so, eo, SK_CONTINUE);
    case BREAK:
        consume(ctx, BREAK);
        consume(ctx, ';');
        return createJumpStatement(ctx, so, eo, SK_BREAK);
    case RETURN:
        consume(ctx, RETURN);
        expr = ctx->token->code != ';' ? parseExpression(ctx, scope) : NULL;
        eo = ctx->token->coordinates.endOffset;
        consume(ctx, ';');
        stmt = createJumpStatement(ctx, so, eo, SK_RETURN);
        stmt->jumpStmt.expression = expr;
        return stmt;
    case ';':
        consume(ctx, ';');
        return createEmptyStatement(ctx, so, eo);
    case IDENTIFIER: { // IDENTIFIER ':' statement
        Token *savedToken = ctx->token;
        nextToken(ctx);
        if (nextTokenIf(ctx, ':')) {
            stmt = parseStatement(ctx, scope);
            eo = stmt->coordinates.endOffset;
            return createLabelStatement(ctx, so, eo, LK_LABEL, stmt, savedToken->text, 0);
        } else {
            ctx->token = savedToken;
        }
    }
    default:
        expr = parseExpression(ctx, scope);
        consume(ctx, ';');
        return createExprStatement(ctx, expr);
    }
}

static unsigned processDeclarationPart(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator) {
  return 0;
}

static AstStatementList *allocateStmtList(ParserContext *ctx, AstStatement *stmt) {
  AstStatementList* result = (AstStatementList*)areanAllocate(ctx->astArena, sizeof(AstStatementList));
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

    int so = ctx->token->coordinates.startOffset;
    consume(ctx, '{');

    Scope *blockScope = ctx->currentScope;
    AstStatementList *head = NULL, *tail = NULL;

    while (ctx->token->code && ctx->token->code != '}') {
        if (isDeclarationSpecifierToken(ctx->token->code)) {
            int sod = ctx->token->coordinates.startOffset;
            DeclarationSpecifiers specifiers = { 0 };
            parseDeclarationSpecifiers(ctx, &specifiers, TRUE, NULL);
            int eod = ctx->token->coordinates.startOffset;

            if (specifiers.defined) {
                AstDeclaration *declaration = createAstDeclaration(ctx, specifiers.defined->kind, specifiers.defined->name);
                declaration->structDeclaration = specifiers.defined;
                AstStatement *declStmt = createDeclStatement(ctx, sod, eod, declaration);
                AstStatementList *node = allocateStmtList(ctx, declStmt);
                if (tail) tail->next = node;
                else head = node;
                tail = node;
            }

            if (ctx->token->code != ';') {
                do {
                    Declarator declarator = { 0 };
                    parseDeclarator(ctx, &declarator, DK_NON_ABSTRACT);
                    const char *name = declarator.identificator;
                    if (name == NULL) {
                        parseError(ctx, "expected identifier or '('");
                    }
                    TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
                    AstInitializer* initializer = nextTokenIf(ctx, '=') ? parseInitializer(ctx, NULL) : NULL;
                    int eod = ctx->token->coordinates.startOffset;

                    AstDeclaration *declaration;
                    if (specifiers.flags.bits.isTypedef) {
                        if (initializer != NULL) {
                            parseError(ctx, "illegal initializer (only variables can be initialized)");
                        }
                        declareTypeDef(ctx, name, type);
                        declaration = createAstDeclaration(ctx, DKX_TYPEDEF, name);
                        declaration->typeDefinition.coordinates.startOffset = sod;
                        declaration->typeDefinition.coordinates.endOffset = eod;
                        declaration->typeDefinition.definedType = type;
                    } else {
                        declaration = createAstDeclaration(ctx, DKX_VAR, name);
                        AstValueDeclaration *valueDeclaration =
                            createAstValueDeclaration(ctx, sod, eod, VD_VARIABLE, type, name, 0, specifiers.flags.storage, initializer);
                        declaration->variableDeclaration = valueDeclaration;
                        declareValueSymbol(ctx, name, valueDeclaration);
                        // TODO: declare value symbol here (needs Scope support)
                    }

                    AstStatement *declStmt = createDeclStatement(ctx, sod, eod, declaration);
                    AstStatementList *node = allocateStmtList(ctx, declStmt);
                    if (tail) tail->next = node;
                    else head = node;
                    tail = node;
                } while (nextTokenIf(ctx, ','));
            } else {
                // TODO: warning: declaration does not declare anything
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

    int eo = ctx->token->coordinates.endOffset;
    consume(ctx, '}');

    return createBlockStatement(ctx, so, eo, ctx->currentScope, head);
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

static int isDeclaratorCorrect(Declarator *declarator, int kind) {
  if (!(kind & DK_ABSTRACT) && declarator->identificator == NULL) return FALSE;

  return TRUE;
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
  int so = ctx->token->coordinates.startOffset;
  parseDeclarationSpecifiers(ctx, &specifiers, TRUE, NULL);

  unsigned isTypeDefDeclaration = specifiers.flags.bits.isTypedef;

  if (specifiers.defined) {
    AstSUEDeclaration *defined = specifiers.defined;
    AstDeclaration *declaration = createAstDeclaration(ctx, defined->kind, defined->name);
    declaration->structDeclaration = defined;
    addToFile(file, createTranslationUnit(ctx, declaration, NULL));
  }

  if (nextTokenIf(ctx, ';')) {
      if (isTypeDefDeclaration) {
          // warning
          // TODO: check other SCS to be off
          parseWarning(ctx, "typedef requires a name");
      }
      return;
  }

  int hasErrors = FALSE;
  int id_idx = 0;
  const char *funName = NULL;
  AstFunctionDeclaration *functionDeclaration = NULL;
  Scope *functionScope = NULL;
  do {
    Declarator declarator = { 0 };
    AstInitializer *initializer = NULL;
    functionDeclaration = NULL;
    parseDeclarator(ctx, &declarator, DK_NON_ABSTRACT);

    int isFunDeclarator = isFunctionDeclarator(&declarator);
    if (nextTokenIf(ctx, '=')) {
        initializer = parseInitializer(ctx, NULL);
    };

    if (initializer != NULL) {
        if (isTypeDefDeclaration || isFunDeclarator) {
            parseError(ctx, "illegal initializer (only variables can be initialized)");
        }
    }

    if (!isDeclaratorCorrect(&declarator, DK_NON_ABSTRACT)) {
        hasErrors = TRUE;
        continue;
    }

    const char *name = declarator.identificator;

    int eo = ctx->token->coordinates.startOffset;

    AstDeclaration *declaration = NULL;

    if (isTypeDefDeclaration) {
        if (declarator.identificator != NULL) {
            TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
            declareTypeDef(ctx, name, type);
            declaration = createAstDeclaration(ctx, DKX_TYPEDEF, name);
            declaration->typeDefinition.definedType = type;
            declaration->typeDefinition.coordinates.startOffset = so;
            declaration->typeDefinition.coordinates.endOffset = ctx->token->coordinates.startOffset;
        }
    } else if (isFunDeclarator) {
      TypeRef *returnType = makeFunctionReturnType(ctx, &specifiers, &declarator);
      FunctionParams *params_dpk = NULL;
      unsigned i;

      for (i = 0; i < declarator.partsCounter; ++i) {
          DeclaratorPart *dp = &declarator.declaratorParts[i];
          if (dp->kind == DPK_FUNCTION) {
              params_dpk = &dp->parameters;
              break;
          }
      }

      assert(params_dpk != NULL);
      AstValueDeclaration *params = params_dpk->parameters;
      functionDeclaration = createFunctionDeclaration(ctx, so, eo, returnType, name, specifiers.flags.storage, params, params_dpk->isVariadic);
      Symbol *s = declareFunctionSymbol(ctx, name, functionDeclaration);
      if (ctx->token->code == '{') {
          funName = name;
          if (id_idx == 0) {
              functionScope = params_dpk->scope;
              break;
          } else {
              parseError(ctx, "expected ';' after top level declarator");
          }
      } else {
          declaration = createAstDeclaration(ctx, DKX_PROTOTYPE, name);
          declaration->functionProrotype = functionDeclaration;
      }
    } else {
        TypeRef *type = makeTypeRef(ctx, &specifiers, &declarator);
        AstValueDeclaration *valueDeclaration = createAstValueDeclaration(ctx, so, eo, VD_VARIABLE, type, name, 0, specifiers.flags.storage, initializer);
        Symbol *s = declareValueSymbol(ctx, name, valueDeclaration);
        declaration = createAstDeclaration(ctx, DKX_VAR, name);
        declaration->variableDeclaration = valueDeclaration;
    }
    addToFile(file, createTranslationUnit(ctx, declaration, NULL));
    ++id_idx;
  } while (nextTokenIf(ctx, ','));


  // it's function definition

  // K&R param syntax is not supported yet

  // TODO: check types

  if (nextTokenIf(ctx, ';')) return;
  if (hasErrors) {
    nextToken(ctx); // avoid infinite loops in parser
    return;
  }

  if (functionDeclaration == NULL) {
      parseError(ctx, "Expected function type");
      return;
  }

  assert(functionScope != NULL);

  ctx->currentScope = functionScope;
  AstStatement *body = parseFunctionBody(ctx);
  if (functionDeclaration)
    functionDeclaration->coordinates.endOffset = body->coordinates.endOffset;
  AstFunctionDefinition *definition = createFunctionDefinition(ctx, functionDeclaration, functionScope, body);
  definition->scope = functionScope;
  ctx->currentScope = functionScope->parent;

  AstTranslationUnit *newUnit = createTranslationUnit(ctx, NULL, definition);
  addToFile(file, newUnit);
}

/**
ast_file
    : declaration
    | ast_file declaration
    ;
*/


static void initializeContext(ParserContext *ctx, unsigned lineNum) {
  size_t as = sizeof(unsigned) * lineNum;
  unsigned *linePos = (unsigned *)heapAllocate(as);

  memset(linePos, 0, as);
  ctx->locationInfo.linesPos = linePos;
  ctx->locationInfo.lineCount = lineNum;
  ctx->anonSymbolsCounter = 0;

  yylex_init(&ctx->scanner);

  ctx->tokenArena = createArena("Tokens Arena", DEFAULT_CHUNCK_SIZE);
  ctx->astArena = createArena("AST Arena", DEFAULT_CHUNCK_SIZE);
  ctx->typeArena = createArena("Types Arena", DEFAULT_CHUNCK_SIZE);

  ctx->rootScope = ctx->currentScope = newScope(ctx, NULL);
}

static void releaseContext(ParserContext *ctx) {

  yylex_destroy(ctx->scanner);

  releaseArena(ctx->tokenArena);
//  releaseArena(ctx->typeArena);
//  releaseArena(ctx->astArena);
//  free(ctx->locationInfo.linesPos);
}

/**
translation_unit
    : external_declaration+
 */
AstFile* parseFile(FILE* file, const char* fileName) {
  unsigned lineNum = countLines(file);

  ParserContext context = { 0 };
  initializeContext(&context, lineNum);

  AstFile *astFile = createAstFile(&context, INITIAL_FILE_CAPACITY);
  astFile->fileName = fileName;
  context.parsedFile = astFile;

  yyset_in(file, context.scanner);
  nextToken(&context);

  while (context.token->code) {
      parseExternalDeclaration(&context, astFile);
  }

  yylex_destroy(context.scanner);
  releaseContext(&context);

  return astFile;
}
