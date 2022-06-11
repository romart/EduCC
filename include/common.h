#ifndef __COMMON_H__
#define __COMMON_H__

#include <stddef.h>
#include <stdint.h>

// ERROR CODES

enum {
  NO_ERRORS = 0,
  ERR_MALLOC = 3,
  ERR_MMAP = 4,
};

typedef enum _Boolean {
    FALSE = 0,
    TRUE = !FALSE
} Boolean;

#define BIT(n) (1U << (n))
#define CHECK_BIT(v, n) ((v) & BIT(n))
#define SET_BIT(v, n) ((v) =| BIT(n))
#define CLEAR_BIT(v, n) ((v) =& ~BIT(n))

#define ALIGN_SIZE(len, align) (((len)+(align - 1)) & ~((align)-1))
#define ALIGN_PTR(ptr, align) (((~((uintptr_t)(ptr))) + 1) & ((align) - 1))

#define max(a, b) ((a) < (b)) ? (b) : (a)
#define min(a, b) ((a) < (b)) ? (a) : (b)

void unreachable(const char *msg);

#endif // __COMMON_H__
