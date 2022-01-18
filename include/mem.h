
#ifndef __MEM_H__
#define __MEM_H__ 1

#include <stddef.h>

#define DEFAULT_CHUNCK_SIZE 0x4000

typedef struct _Arena {
  const char *name;

  size_t chuckSize;
  struct _Chunck *chuncks;
  struct _HeapChunck *bigChuncks;
} Arena;


Arena *createArena(const char *name, size_t chuckSize);
void *areanAllocate(Arena *arena, size_t size);
void releaseArena(Arena *arena);

#endif // __MEM_H__
