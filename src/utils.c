

#include <memory.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "mem.h"
#include "utils.h"

static Vector emptyImpl = {0};

Vector *emptyVector = &emptyImpl;

void addToVector(Vector* vector, void* value) {
    if (vector->size == vector->capacity) {
        int newCapacity = (int)(vector->capacity * 1.2f) ;
        void** newStorage = (void**)heapAllocate(sizeof(void*) * newCapacity);
        memcpy(newStorage, vector->storage, vector->capacity);
        releaseHeap(vector->storage);
        vector->storage = newStorage;
        vector->capacity = newCapacity;
    }

    vector->storage[vector->size++] = value;
}

static void initVector(Vector* vector, int capacity) {
    vector->capacity = capacity;
    vector->storage = (void**)heapAllocate(sizeof(void*) * capacity);
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

void* getFromVector(Vector* vector, int idx) {
    assert(idx < vector->size);
    return vector->storage[idx];
}

struct LinkedNode {
    struct LinkedNode* next;
    const void* key;
    const void* value;
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

const void* putToHashMap(HashMap* map, const void* key, const void* value) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            const void* oldValue = list->value;
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

    return NULL;
}


const void* getFromHashMap(HashMap* map, const void* key) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            return list->value;
        }
        list = list->next;
    }

    return NULL;
}


const void* removeFromHashMap(HashMap* map, const void* key) {
    unsigned hc = map->hashCode(key);
    unsigned idx = hc % map->capacity;
    struct LinkedNode** prev = &(map->storage[idx]);
    struct LinkedNode* list = *prev;

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            *prev = list->next;
            const void* oldValue = list->value;
            releaseHeap(list);
            return oldValue;
        }
        prev = &list->next;
        list = *prev;
    }

    return NULL;
}

int isInHashMap(HashMap* map, const void* key) {
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

void unreachable(const char *msg) {
  fprintf(stderr, "Unreachable execution: %s\n", msg);
  abort();
}

int isTerminal(FILE *stream) {
  return isatty(fileno(stream));
}

