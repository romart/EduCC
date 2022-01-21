#ifndef __COMMON_H__
#define __COMMON_H__

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

void unreachable(const char *msg);

#endif // __COMMON_H__
