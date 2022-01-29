

#include <malloc.h>
#include <memory.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <errno.h>

#include "common.h"
#include "mem.h"

#define PAGE_SIZE 0x1000

#define ALIGN_SIZE_TO_PAGE(len) ALIGN_SIZE(len, PAGE_SIZE)
#define ALIGN_SIZE_TO_WORD(len) ALIGN_SIZE(len, 0x8)

size_t heapBytesAllocated = 0;

void *heapAllocate(size_t size) {
  void *result = malloc(size);

  if (result == NULL) {
      fprintf(stderr, "Cannot allocate %zu bytes, OutOfMemory, halt\n", size);
      exit(ERR_MALLOC);
  }

  heapBytesAllocated += size;

  memset(result, 0, size);

  return result;
}

void releaseHeap(void *p) {
  free(p);
}

static void *mmapAllocate(size_t size) {
  void *result = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (result == MAP_FAILED) {
      fprintf(stderr, "Cannot allocate %zu bytes via mmap, %s\n", size, strerror(errno));
      exit(ERR_MMAP);
  }
  return result;
}

typedef unsigned char byte;

typedef struct _Chunck {

  byte *start;
  byte *pnt;
  size_t size;
  size_t avaliable;

  struct _Chunck *next;
} Chunck;

typedef struct _HeapChunck {
  void *allocated;
  struct _HeapChunck *next;
} HeapChunck;

static Chunck *createChuck(size_t size) {
  Chunck *chunck = (Chunck *)heapAllocate(sizeof (Chunck));

  byte *storage = (byte *)mmapAllocate(size);
  memset(storage, 0, size);

  chunck->size = chunck->avaliable = size;
  chunck->start = chunck->pnt = storage;
  chunck->next = NULL;

  return chunck;
}

Arena *createArena(const char *name, size_t chuckSize) {
  Arena *result = (Arena *)heapAllocate(sizeof(Arena));
  memset(result, 0, sizeof(Arena));

  if (chuckSize < PAGE_SIZE)
    chuckSize = PAGE_SIZE;

  result->name = name;
  result->chuckSize = ALIGN_SIZE_TO_PAGE(chuckSize);
  result->chuncks = NULL; //createChuck(chuckSize);
  result->bigChuncks = NULL;

  return result;
}

static HeapChunck *allocateOnHeap(size_t size) {
  HeapChunck *c = (HeapChunck *)heapAllocate(sizeof(HeapChunck));

  c->next = NULL;
  c->allocated = heapAllocate(size);
  memset(c->allocated, 0, size);

  return c;

}

void *areanAllocate(Arena *arena, size_t _size) {
  size_t size = ALIGN_SIZE_TO_WORD(_size);
  if (size > arena->chuckSize) {
      HeapChunck *newChunck = allocateOnHeap(size);
      newChunck->next = arena->bigChuncks;
      arena->bigChuncks = newChunck;
      return newChunck->allocated;
  }

  Chunck *chunck = arena->chuncks;
  while (chunck) {
      if (chunck->avaliable >= size) {
          break;
      }
      chunck = chunck->next;
  }

  if (chunck == NULL) {
      chunck = createChuck(arena->chuckSize);
      chunck->next = arena->chuncks;
      arena->chuncks = chunck;
  }

  byte *result = chunck->pnt;
  chunck->pnt += size;
  chunck->avaliable -= size;

  return result;
}

void releaseArena(Arena *arena) {
  Chunck *chunck = arena->chuncks;

  while (chunck) {
      munmap(chunck->start, chunck->size);
      Chunck *next = chunck->next;
      releaseHeap(chunck);
      chunck = next;
  }

  HeapChunck *heap = arena->bigChuncks;

  while (heap) {
      releaseHeap(heap->allocated);
      HeapChunck *next = heap->next;
      releaseHeap(heap);
      heap = next;
  }

  releaseHeap(arena);
}

void printArenaStatistic(FILE *output, Arena *arena) {

  const size_t kb = 1024;
  size_t allocated = 0, used = 0;
  unsigned chuncks = 0, heapChunks = 0;

  Chunck *c = arena->chuncks;

  while (c) {
      allocated += c->size;
      used += c->pnt - c->start;
      chuncks++;
      c = c->next;
  }

  HeapChunck *hp = arena->bigChuncks;

  while (hp) {
      ++heapChunks;
      hp = hp->next;
  }

  fprintf(output, "Arena '%s': allocated = %lu bytes (%lu kb), used = %lu bytes (%lu kb), chunks = %u, heap chunks = %u\n", arena->name, allocated, allocated / kb, used, used / kb,chuncks, heapChunks);
}
