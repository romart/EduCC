
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


#endif // __SEMA_H__
