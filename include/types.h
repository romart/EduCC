
#ifndef __TYPES_H__
#define __TYPES_H__ 1

#include "common.h"

typedef enum _TypeId {
  T_VOID,

  T_BOOL,

  T_S1,
  T_S2,
  T_S4,
  T_S8,


  T_U1,
  T_U2,
  T_U4,
  T_U8,

  T_F4,
  T_F8,

  T_F10,

  T_BUILT_IN_TYPES,

  T_STRUCT,
  T_UNION,

  T_ENUM,

  T_ERROR
} TypeId;

typedef union {
    unsigned storage;
    struct {
        unsigned isConst : 1;
        unsigned isVolatile : 1;
        unsigned isRestrict : 1;

        unsigned isStatic : 1;
        unsigned isExternal : 1;
        unsigned isRegister : 1;
        unsigned isTypedef : 1;
        unsigned isAuto : 1;

        unsigned isInline : 1;

        unsigned isLocal : 1;
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
  TypeId typeId;
  int size;
  const char *name;
  struct _TypeDefinition *typeDefinition;
} TypeDesc;

typedef struct _BitFieldTypeDescriptor {
  struct _TypeRef *storageType;
  unsigned offset : 7;
  unsigned width  : 7;
} BitFieldTypeDescriptor;

typedef enum _TypeRefKind {
    TR_VALUE,
    TR_POINTED,
    TR_ARRAY,
    TR_FUNCTION,
    TR_BITFIELD
} TypeRefKind;

typedef struct _TypeRef {
    TypeRefKind kind;
    SpecifierFlags flags;
    union {
        TypeDesc *descriptorDesc; // aka TypeConstructor
        FunctionTypeDescriptor functionTypeDesc;
        struct _TypeRef *pointed;
        ArrayTypeDescriptor arrayTypeDesc;
        BitFieldTypeDescriptor bitFieldDesc;
    };
} TypeRef;

#endif // __TYPES_H__
