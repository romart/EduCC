
#ifndef __SEMA_H__
#define __SEMA_H__ 1

#include "common.h"
#include "tree.h"
#include "parser.h"
#include "types.h"

enum {
  POINTER_TYPE_SIZE = 8,
};

size_t computeTypeSize(TypeRef *type);

enum TypeEqualityKind {
  TEK_UNKNOWN,
  TEK_EQUAL,
  TEK_ALMOST_EQUAL, // could be casted implicitly
  TEK_NOT_EXATCLY_EQUAL, // in case of two different enums, warning
  TEK_NOT_EQUAL
};

enum Boolean typesEquals(TypeRef *t1, TypeRef *t2);
enum TypeEqualityKind typeEquality(TypeRef *t1, TypeRef *t2);
int isTypeName(ParserContext *ctx, const char* name, struct _Scope* scope);

enum TypeCastabilityKind {
  TCK_UNKNOWN,
  TCK_NO_CAST, // int -> int
  TCK_IMPLICIT_CAST, // char -> int, could be casted implicitly
  TCK_EXPLICIT_CAST // struct S -> struct D
};

enum TypeCastabilityKind typeCastability(TypeRef *to, TypeRef *from);

enum SymbolKind {
    FunctionSymbol = 1,
    UnionSymbol,
    StructSymbol,
    TypedefSymbol,
    ValueSymbol,
    EnumSymbol, /** TODO: not sure about it */
    EnumConstSymbol
};

typedef struct _Symbol {
    int kind;
    const char* name; /** struct/union/enum is referenced via "$$name" or "|$name" or "#$enum"*/
    union {
        struct _AstFunctionDeclaration *function; // FunctionSymbol
        TypeDesc *typeDescriptor; // StructSymbol | UnionSymbol | EnumSymbol, struct S;
        TypeRef * typeref; // TypedefSymbol, typedef struct TS* ts_t;
        struct _AstValueDeclaration *variableDesc; // ValueSymbol, int a = 10 | int foo(int value)
        struct _EnumConstant *enumerator; // EnumConstSymbol
    };
} Symbol;

typedef struct _Scope {
    struct _Scope* parent;
    HashMap* symbols;
} Scope;


Symbol* findSymbol(ParserContext *ctx, const char *name);
Symbol* declareSymbol(ParserContext *ctx, int kind, const char *name);
Symbol* findOrDeclareSymbol(ParserContext* ctx, int kind, const char* name);

Symbol *declareTypeDef(ParserContext *ctx, const char *name, TypeRef *type);
Symbol *declareValueSymbol(ParserContext *ctx, const char *name, AstValueDeclaration *declaration);
Symbol *declareFunctionSymbol(ParserContext *ctx, const char *name, AstFunctionDeclaration *declaration);
Symbol *declareSUESymbol(ParserContext *ctx, int symbolKind, int typeId, const char *name, AstSUEDeclaration *declaration, Symbol **ss);
Symbol *declareEnumConstantSymbol(ParserContext *ctx, EnumConstant *enumerator);

Scope *newScope(ParserContext *ctx, Scope *parent);

TypeRef *makeBasicType(ParserContext *ctx, TypeDesc *descriptor, unsigned flags);
TypeRef* makePointedType(ParserContext *ctx, SpecifierFlags flags, TypeRef *pointedTo);
TypeRef *makeArrayType(ParserContext *ctx, int size, TypeRef *elementType);
TypeRef *makeFunctionType(ParserContext *ctx, TypeRef *returnType, FunctionParams *params);
TypeRef *makeFunctionReturnType(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator);
TypeRef *makeTypeRef(ParserContext *ctx, DeclarationSpecifiers *specifiers, Declarator *declarator);
TypeRef *makeErrorRef(ParserContext *ctx);


#endif // __SEMA_H__
