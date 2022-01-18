
#include "utils.h"

#include <malloc.h>
#include <memory.h>
#include <assert.h>

static Vector emptyImpl = {0};

Vector *emptyVector = &emptyImpl;

void addToVector(Vector* vector, void* value) {
    if (vector->size == vector->capacity) {
        int newCapacity = (int)(vector->capacity * 1.2f) ;
        void** newStorage = (void**)malloc(sizeof(void*) * newCapacity);
        memcpy(newStorage, vector->storage, vector->capacity);
        free(vector->storage);
        vector->storage = newStorage;
        vector->capacity = newCapacity;
    }

    vector->storage[vector->size++] = value;
}

static void initVector(Vector* vector, int capacity) {
    vector->capacity = capacity;
    vector->storage = (void**)malloc(sizeof(void*) * capacity);
}

Vector* createVector(int capacity) {
    Vector* result = (Vector*)malloc(sizeof(Vector));
    memset(result, 0, sizeof(Vector));
    initVector(result, capacity);
    return result;
}

void releaseVector(Vector *vector) {
    free(vector->storage);
    free(vector);
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

typedef struct _HashMap {
    int capacity;
    struct LinkedNode** storage;

    hashCode_fun hashCode;
    compare_fun compare;
} HashMap;


HashMap* createHashMap(int capacity, hashCode_fun hc, compare_fun cmp) {
    HashMap* map = (HashMap*)malloc(sizeof(HashMap));
    map->hashCode = hc;
    map->compare = cmp;
    map->capacity = capacity;
    map->storage = (struct LinkedNode**)malloc(sizeof(struct LinkedNode*) *  capacity);

    return map;
}

const void* putToHashMap(HashMap* map, const void* key, const void* value) {
    int hc = map->hashCode(key);
    int idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            const void* oldValue = list->value;
            list->value = value;
            return oldValue;
        }
        list = list->next;
    }

    struct LinkedNode* newNode = (struct LinkedNode*)malloc(sizeof(struct LinkedNode));
    newNode->key = key;
    newNode->value = value;
    newNode->next = map->storage[idx];
    map->storage[idx] = newNode;

    return NULL;
}


const void* getFromHashMap(HashMap* map, const void* key) {
    int hc = map->hashCode(key);
    int idx = hc % map->capacity;
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
    int hc = map->hashCode(key);
    int idx = hc % map->capacity;
    struct LinkedNode** prev = &(map->storage[idx]);
    struct LinkedNode* list = *prev;

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            *prev = list->next;
            const void* oldValue = list->value;
            free(list);
            return oldValue;
        }
        prev = &list->next;
        list = *prev;
    }

    return NULL;
}

int isInHashMap(HashMap* map, const void* key) {
    int hc = map->hashCode(key);
    int idx = hc % map->capacity;
    struct LinkedNode* list = map->storage[idx];

    while (list != NULL) {
        if (map->compare(list->key, key) == 0) {
            return TRUE;
        }
        list = list->next;
    }

    return FALSE;
}

unsigned countLines(FILE* file) {
  unsigned result = 0;
  while(!feof(file)) {
    int ch = fgetc(file);
    if(ch == '\n') {
      result++;
    }
  }

  rewind(file);

  return result;
}

