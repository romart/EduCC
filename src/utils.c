

#include <memory.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "mem.h"
#include "utils.h"

static void resizeVector(Vector *v, size_t newCapacity) {
  if (newCapacity <= v->capacity)
    return;

  intptr_t* newStorage = (intptr_t*)heapAllocate(sizeof(intptr_t) * newCapacity);
  memcpy(newStorage, v->storage, v->capacity * sizeof(intptr_t));
  releaseHeap(v->storage);
  v->storage = newStorage;
  v->capacity = newCapacity;
}

void addToVector(Vector* vector, intptr_t value) {
  if (vector->size == vector->capacity) {
    int newCapacity = (int)(vector->capacity * 2) ;
    resizeVector(vector, newCapacity);
  }

  vector->storage[vector->size++] = value;
}

void removeFromVector(Vector *vector, intptr_t v) {
  size_t i = 0;

  while (i < vector->size) {
    size_t n = i + 1;
    if (vector->storage[i] == v) {
      if (n < vector->size) {
        printf("Memmove vector[%lu] %p %lu -> %lu (%p -> %p)\n", vector->size, vector->storage, n, i, &vector->storage[i], &vector->storage[n]);
        memmove(&vector->storage[i], &vector->storage[n], (vector->size - n) * sizeof(intptr_t));
      }
      size_t s = vector->size;
      vector->size -= 1;
      assert(vector->size < s);
      continue;
    }

    ++i;
  }
}

size_t removeFromVectorAt(Vector *v, size_t i) {

  assert(i < v->size);

  size_t tail = v->size - i - 1;

  memmove(&v->storage[i], &v->storage[i + 1], tail * sizeof (intptr_t));
  v->size -= 1;

  return i;
}

intptr_t putAtVector(Vector *vector, size_t idx, intptr_t v) {
  resizeVector(vector, idx + 1);
  intptr_t old = vector->storage[idx];
  vector->storage[idx] = v;
  vector->size = max(vector->size, idx + 1);
  return old;
}

void initVector(Vector* vector, int capacity) {
    assert(vector->storage == NULL);
    vector->size = 0;
    vector->capacity = capacity;
    vector->storage = (intptr_t*)heapAllocate(sizeof(intptr_t) * capacity);
}

Vector* createVector(int capacity) {
    Vector* result = (Vector*)heapAllocate(sizeof(Vector));
    memset(result, 0, sizeof(Vector));
    initVector(result, capacity);
    return result;
}

void clearVector(Vector *v) {
  v->size = 0;
}

void releaseVector(Vector *vector) {
    releaseHeap(vector->storage);
//    releaseHeap(vector);
}

intptr_t getFromVector(const Vector* vector, int idx) {
    assert(idx < vector->size);
    return vector->storage[idx];
}

void pushToStack(Vector *v, intptr_t o) {
    addToVector(v, o);
}

intptr_t popFromStack(Vector *v) {
  intptr_t r =  topOfStack(v);
  v->size -= 1;
  return r;
}

intptr_t topOfStack(Vector *v) {
  assert(v->size > 0);
  return getFromVector(v, v->size - 1);
}

void popOffStack(Vector *v, size_t pops) {
  assert(v->size - pops >= 0);
  v->size -= pops;
}

// =====================================================


struct LinkedNode {
    struct LinkedNode* next;
    intptr_t key;
    intptr_t value;
};

struct _HashMap {
    int capacity;
    struct LinkedNode** storage;

    hashCode_fun hashCode;
    compare_fun compare;
};


struct _HashMap* createHashMap(int capacity, hashCode_fun hc, compare_fun cmp) {
    struct _HashMap* map = (struct _HashMap*)heapAllocate(sizeof(struct _HashMap));
    map->hashCode = hc;
    map->compare = cmp;
    map->capacity = capacity;
    map->storage = (struct LinkedNode**)heapAllocate(sizeof(struct LinkedNode*) * capacity);;

    return map;
}

intptr_t putToHashMap(HashMap* map, intptr_t key, intptr_t value) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            intptr_t oldValue = list->value;
            list->value = value;
            return oldValue;
        }
        list = list->next;
    }

    struct LinkedNode* newNode = (struct LinkedNode*)heapAllocate(sizeof(struct LinkedNode));
    newNode->key = key;
    newNode->value = value;
    newNode->next = map->storage[idx];
    map->storage[idx] = newNode;

    return value;
}


intptr_t getFromHashMap(HashMap* map, intptr_t key) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            return list->value;
        }
        list = list->next;
    }

    return 0;
}


intptr_t removeFromHashMap(HashMap* map, intptr_t key) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode** prev = &(map->storage[idx]);
    struct LinkedNode* list = *prev;

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            *prev = list->next;
            intptr_t oldValue = list->value;
            releaseHeap(list);
            return oldValue;
        }
        prev = &list->next;
        list = *prev;
    }

    return 0;
}

void foreachHashMap(HashMap *map, void (*func)(intptr_t, intptr_t, void*), void *ctx) {
    for (unsigned idx = 0; idx < map->capacity; ++idx) {
        struct LinkedNode *node = map->storage[idx];
        while (node != NULL) {
            func(node->key, node->value, ctx);
            node = node->next;
        }
    }
}

int isInHashMap(HashMap* map, intptr_t key) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            return TRUE;
        }
        list = list->next;
    }

    return FALSE;
}

void releaseHashMap(HashMap *map) {
  unsigned i;
  for (i = 0; i < map->capacity; ++i) {
      struct LinkedNode *list = map->storage[i];
      while (list) {
          struct LinkedNode *next = list->next;
          releaseHeap(list);
          list = next;
      }
      map->storage[i] = NULL;
  }
  releaseHeap(map->storage);
  releaseHeap(map);
}

LinkedListNode *addNodeToListHead(LinkedList *list, LinkedListNode *node) {
    LinkedListNode *head = node->next = list->head;

    if (head)
        head->prev = node;

    list->head = node;

    return node;
}

LinkedListNode *addNodeToListTail(LinkedList *list, LinkedListNode *node) {
    LinkedListNode *tail = node->prev = list->tail;

    if (tail)
        tail->next = node;

    list->tail = node;

    return node;
}

LinkedListNode *removeNodeFromList(LinkedList *list, LinkedListNode *node) {
    LinkedListNode *prev = node->prev;
    LinkedListNode *next = node->next;

    if (prev)
        prev->next = next;

    if (next)
        next->prev = prev;

    node->prev = node->next = NULL;

    if (list->head == node)
        list->head = next;

    if (list->tail == node)
        list->tail = prev;

    return node;
}

// =========== BitSet ===============================//

static const size_t bitsPerWord = sizeof(uintptr_t) * 8ULL;

void initBitSet(BitSet *bs, size_t size) {
  bs->size = size;
  assert(isPowerOf2(bitsPerWord));
  const size_t storageCount = (size + bitsPerWord) / bitsPerWord;
  bs->wordCount = storageCount;
  bs->bits = malloc(sizeof(uintptr_t) * storageCount);
  clearAll(bs);
}

void releaseBitSet(BitSet *bs) {
  free(bs->bits);
  bs->size = bs->wordCount = 0;
  bs->bits = NULL;
}

void setBit(BitSet *bs, size_t idx) {
  assert(idx < bs->size);
  const size_t wordIdx = idx / bitsPerWord;
  const size_t bitIdx = idx % bitsPerWord;
  const uintptr_t mask = 1ULL << bitIdx;
  bs->bits[wordIdx] |= mask;
}

void clearBit(BitSet *bs, size_t idx) {
  assert(idx < bs->size);
  const size_t wordIdx = idx / bitsPerWord;
  const size_t bitIdx = idx % bitsPerWord;
  const uintptr_t mask = 1ULL << bitIdx;
  bs->bits[wordIdx] &= ~mask;
}

unsigned getBit(const BitSet *bs, size_t idx) {
  assert(idx < bs->size);
  const size_t wordIdx = idx / bitsPerWord;
  const size_t bitIdx = idx % bitsPerWord;
  const uintptr_t mask = 1ULL << bitIdx;
  return (bs->bits[wordIdx] & mask) >> bitIdx;
}


static void adjustBitSetTail(BitSet *bs) {
    // handle tail
    const size_t lastWordIdx = bs->wordCount - 1;
    const size_t tailBits = bs->wordCount * bitsPerWord - bs->size;
    assert(tailBits <= bitsPerWord);
    const size_t valuableBits = bitsPerWord - tailBits;
    const size_t valuableMask = (~0ULL << valuableBits);
    bs->bits[lastWordIdx] &= ~valuableMask;
}

void clearAll(BitSet *bs) {
  memset(bs->bits, 0U, bs->wordCount * sizeof(uintptr_t));
}

void setAll(BitSet *bs) {
  memset(bs->bits, ~0U, bs->wordCount * sizeof(uintptr_t));
  adjustBitSetTail(bs);
}

void intersectBitSets(const BitSet *lhs, const BitSet *rhs, BitSet *rs) {
  assert(lhs->size == rhs->size);
  assert(rs->size == lhs->size);
  
  for (size_t idx = 0; idx < lhs->wordCount; ++idx) {
    rs->bits[idx] = lhs->bits[idx] & rhs->bits[idx];
  }
}

void mergeBitSets(const BitSet *lhs, const BitSet *rhs, BitSet *rs) {
  assert(lhs->size == rhs->size);
  assert(rs->size == lhs->size);
  
  for (size_t idx = 0; idx < lhs->wordCount; ++idx) {
    rs->bits[idx] = lhs->bits[idx] | rhs->bits[idx];
  }
}

void copyBitSet(const BitSet *src, BitSet *dst) {
  assert(src->size == dst->size);
  memcpy(dst->bits, src->bits, src->wordCount * sizeof(uintptr_t));
  adjustBitSetTail(dst);
}

int compareBitSets(const BitSet *lhs, const BitSet *rhs) {
    assert(lhs->size == rhs->size);

    return memcmp(lhs->bits, rhs->bits, lhs->wordCount * sizeof (uintptr_t));
}

Boolean isEmptyBitSet(const BitSet *bs) {
  for (size_t idx = 0; idx < bs->wordCount; ++idx) {
    if (bs->bits[idx] != 0)
      return FALSE;
  }
  return TRUE; 
}

static size_t countBitsWord(uintptr_t w) {
  size_t r = 0;
  for (size_t idx = 0; idx < bitsPerWord; ++idx) {
    const uintptr_t mask = 1ULL << idx;
    if ((w & mask) != 0)
      ++r;
  }
  return r;
}

size_t countBits(const BitSet *bs) {
    size_t r = 0;
    for (size_t idx = 0; idx < bs->wordCount; ++idx) {
      r += countBitsWord(bs->bits[idx]);
    }
    return r;
}

// =========== BitSet ===============================//
 

unsigned countLines(FILE* file) {
  unsigned result = 1;
  while(!feof(file)) {
    int ch = fgetc(file);
    if(ch == '\n') {
      result++;
    }
  }

  rewind(file);

  return result;
}

unsigned countLinesInBuffer(const char *buffer) {
  unsigned result = 1;
  unsigned idx = 0;

  while (buffer[idx]) {
    int ch = buffer[idx++];
    if(ch == '\n') {
      result++;
    }
  }

  return result;
}

char *readFileToBuffer(const char *fileName, size_t *bufferSize) {

  FILE* opened = fopen(fileName, "r");

  if (opened == NULL) return NULL;

  fseek(opened, 0L, SEEK_END);
  size_t size = ftell(opened);

  rewind(opened);

  char *b = heapAllocate(size + 1);

  size_t readed = fread(b, 1, size, opened);

  assert(readed == size);

  fclose(opened);

  *bufferSize = size + 1;

  return b;
}

void putSymbol(StringBuffer *b, char c) {
  if (b->idx == b->size) {
      size_t newSize = (b->size + 512) << 1;
      b->ptr = heapReallocate(b->ptr, b->size, newSize);
      b->size = newSize;
  }

  b->ptr[b->idx++] = c;
}

void unreachable(const char *msg) {
  fprintf(stderr, "Unreachable execution: %s\n", msg);
  abort();
}

void unimplemented(const char *msg) {
  fprintf(stderr, "Unimplemented: %s\n", msg);
  abort();
}

int fileno (FILE *stream);

int isTerminal(FILE *stream) {
  return isatty(fileno(stream));
}

int isPowerOf2(intptr_t v) {
    return (v & (v - 1)) == 0;
}

int log2Integer(intptr_t v) {
    assert(isPowerOf2(v));

    uintptr_t uv = *(uintptr_t *)&v;

    int result = 0;
    while (uv > 1) {
        uv >>= 1;
        result++;
    }
    return result;
}

size_t alignSize(size_t size, size_t alignment) {
    if (alignment == 0) {
        return size; // If alignment is zero, return the size as is
    }
    // Align size to the nearest higher multiple of alignment
    return (size + alignment - 1) & ~(alignment - 1);
}
