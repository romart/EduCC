

#include <malloc.h>
#include <memory.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <errno.h>

#include "mem.h"

#define PAGE_SIZE 0x1000

#define ALIGN_SIZE(len, align) ((((align)-1) & (len)) ? (((len)+(align)) & ~((align)-1)) : (len))

#define ALIGN_SIZE_TO_PAGE(len) ALIGN_SIZE(len, PAGE_SIZE)
#define ALIGN_SIZE_TO_WORD(len) ALIGN_SIZE(len, 0x8)


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
  Chunck *chunck = (Chunck *)malloc(sizeof (Chunck));
  if (chunck == NULL) exit(5);

  byte *storage = (byte *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (storage == (void*)-1) {
      fprintf(stderr, "%s\n", strerror(errno));
      exit(-5);
  }

  memset(storage, 0, size);
  chunck->size = chunck->avaliable = size;
  chunck->start = chunck->pnt = storage;
  chunck->next = NULL;

  return chunck;
}

Arena *createArena(const char *name, size_t chuckSize) {
  Arena *result = (Arena *)malloc(sizeof(Arena));

  if (result == NULL) exit(5);

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
  HeapChunck *c = (HeapChunck *)malloc(sizeof(HeapChunck));
  if (c == NULL) exit(5);

  c->next = NULL;
  c->allocated = malloc(size);
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
      free(chunck);
      chunck = next;
  }

  HeapChunck *heap = arena->bigChuncks;

  while (heap) {
      free(heap->allocated);
      HeapChunck *next = heap->next;
      free(heap);
      heap = next;
  }

  free(arena);
}
