#ifndef __UTILS_H__
#define __UTILS_H__ 1

#include <stdio.h>
#include <unistd.h>
#include "common.h"

typedef struct _Vector {
    int size;
    int capacity;
    intptr_t* storage;
} Vector;

#define INITIAL_VECTOR_CAPACITY 20

void addToVector(Vector* vector, intptr_t value);
Vector* createVector(int capacity);
intptr_t getFromVector(Vector* vector, int idx);
void releaseVector(Vector *vector);

#define DEFAULT_MAP_CAPACITY 1024

typedef int (*hashCode_fun)(intptr_t);
typedef int (*compare_fun)(intptr_t, intptr_t);

typedef struct _HashMap HashMap;

HashMap* createHashMap(int capacity, hashCode_fun hc, compare_fun cmp);
void releaseHashMap(HashMap *map);

/** returns old value if exixtsed, NULL otherwise */
intptr_t putToHashMap(HashMap* map, intptr_t key, intptr_t value);

intptr_t getFromHashMap(HashMap* map, intptr_t key);

/** returns removed value if found, NULL otherwise */
intptr_t removeFromHashMap(HashMap* map, intptr_t key);

int isInHashMap(HashMap* map, intptr_t key);


unsigned countLines(FILE* file);

int isTerminal(FILE *stream);

extern int snprintf (char *__s, size_t __maxlen, const char *__format, ...);

#endif // __UTILS_H__
