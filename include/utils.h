#ifndef __UTILS_H__
#define __UTILS_H__ 1

#include <stdio.h>

enum {
    FALSE = 0,
    TRUE = !FALSE
};

typedef struct _Vector {
    int size;
    int capacity;
    void** storage;
} Vector;

#define INITIAL_VECTOR_CAPACITY 20

void addToVector(Vector* vector, void* value);
Vector* createVector(int capacity);
void* getFromVector(Vector* vector, int idx);
void releaseVector(Vector *vector);

extern Vector *emptyVector;

#define DEFAULT_MAP_CAPACITY 1024

typedef int (*hashCode_fun)(const void*);
typedef int (*compare_fun)(const void*, const void*);

typedef struct _HashMap HashMap;

HashMap* createHashMap(int capacity, hashCode_fun hc, compare_fun cmp);

/** returns old value if exixtsed, NULL otherwise */
const void* putToHashMap(HashMap* map, const void* key, const void* value);

const void* getFromHashMap(HashMap* map, const void* key);

/** returns removed value if found, NULL otherwise */
const void* removeFromHashMap(HashMap* map, const void* key);

int isInHashMap(HashMap* map, const void* key);


unsigned countLines(FILE* file);

#endif // __UTILS_H__
