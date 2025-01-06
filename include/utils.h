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

void foreachHashMap(HashMap *map, void (*func)(intptr_t, intptr_t, void*), void *ctx);

int isInHashMap(HashMap* map, intptr_t key);

typedef struct _LinkedListNode {
    intptr_t data;

    struct _LinkedListNode *next;
    struct _LinkedListNode *prev;
} LinkedListNode;

typedef struct _LinkedList {
    struct _LinkedListNode *head;
    struct _LinkedListNode *tail;
} LinkedList;

LinkedListNode *addNodeToListHead(LinkedList *list, LinkedListNode *node);
LinkedListNode *addNodeToListTail(LinkedList *list, LinkedListNode *node);
LinkedListNode *removeNodeFromList(LinkedList *list, LinkedListNode *node);

unsigned countLines(FILE* file);

int isTerminal(FILE *stream);

extern int snprintf (char *__s, size_t __maxlen, const char *__format, ...);
extern char *strndup (const char *__string, size_t __n);

typedef struct {
  char *ptr;
  size_t size;
  unsigned idx;
} StringBuffer;

void putSymbol(StringBuffer *b, char c);


typedef struct {
  size_t size;
  size_t wordCount;
  uintptr_t *bits;
} BitSet;

void initBitSet(BitSet *bs, size_t s);
void releaseBitSet(BitSet *bs);

void setBit(BitSet *bs, size_t idx);
void clearBit(BitSet *bs, size_t idx);
unsigned getBit(const BitSet *bs, size_t idx);

void setAll(BitSet *bs);
void clearAll(BitSet *bs);

void intersectBitSets(const BitSet *lhs, const BitSet *rhs, BitSet *rs);
void mergeBitSets(const BitSet *lhs, const BitSet *rhs, BitSet *rs);

void copyBitSet(const BitSet *src, BitSet *dst);
int compareBitSets(const BitSet *lhs, const BitSet *rhs);
Boolean isEmptyBitSet(const BitSet *bs);
size_t countBits(const BitSet *bs);

unsigned countLinesInBuffer(const char *buffer);
char *readFileToBuffer(const char *fileName, size_t *bufferSize);

int isPowerOf2(intptr_t v);
int log2Integer(intptr_t v);

size_t alignSize(size_t size, size_t alignment);

#endif // __UTILS_H__
