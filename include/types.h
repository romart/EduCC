
#ifndef __TYPES_H__
#define __TYPES_H__ 1

#include "common.h"

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

  T_ERROR
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

typedef struct _TypeList {
  struct _TypeRef *type;
  struct _TypeList *next;
} TypeList;

typedef struct _FunctionTypeDescriptor {
    TypeList *parameters;
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
  struct _AstSUEDeclaration *structInfo;
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

#endif // __TYPES_H__
