

#include "sema.h"


size_t computeTypeSize(TypeRef *type) {
  if (type->kind == TR_VALUE) {
      return type->descriptorDesc->size;
  }

  if (type->kind == TR_POINTED) {
      return POINTER_TYPE_SIZE;
  }

  if (type->kind == TR_ARRAY) {
      ArrayTypeDescriptor *atype = &type->arrayTypeDesc;
      return atype->size * computeTypeSize(atype->elementType);
  }

  return POINTER_TYPE_SIZE;
}
