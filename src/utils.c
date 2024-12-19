

#include <memory.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "mem.h"
#include "utils.h"

void addToVector(Vector* vector, intptr_t value) {
    if (vector->size == vector->capacity) {
        int newCapacity = (int)(vector->capacity * 1.2f) ;
        intptr_t* newStorage = (intptr_t*)heapAllocate(sizeof(intptr_t) * newCapacity);
        memcpy(newStorage, vector->storage, vector->capacity);
        releaseHeap(vector->storage);
        vector->storage = newStorage;
        vector->capacity = newCapacity;
    }

    vector->storage[vector->size++] = value;
}

static void initVector(Vector* vector, int capacity) {
    vector->capacity = capacity;
    vector->storage = (intptr_t*)heapAllocate(sizeof(intptr_t) * capacity);
}

Vector* createVector(int capacity) {
    Vector* result = (Vector*)heapAllocate(sizeof(Vector));
    memset(result, 0, sizeof(Vector));
    initVector(result, capacity);
    return result;
}

void releaseVector(Vector *vector) {
    releaseHeap(vector->storage);
    releaseHeap(vector);
}

intptr_t getFromVector(Vector* vector, int idx) {
    assert(idx < vector->size);
    return vector->storage[idx];
}

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

    return 0;
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

int fileno (FILE *stream);

int isTerminal(FILE *stream) {
  return isatty(fileno(stream));
}

