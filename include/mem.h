
#ifndef __MEM_H__
#define __MEM_H__ 1

#include <stddef.h>
#include <stdio.h>

#define DEFAULT_CHUNCK_SIZE 0x4000

typedef struct _Arena {
  const char *name;

  size_t chuckSize;
  struct _Chunck *chuncks;
  struct _HeapChunck *bigChuncks;
} Arena;

void *heapAllocate(size_t size);
void releaseHeap(void *);

Arena *createArena(const char *name, size_t chuckSize);
void *areanAllocate(Arena *arena, size_t size);
void releaseArena(Arena *arena);

void printArenaStatistic(FILE *output, Arena *arena);

#endif // __MEM_H__
