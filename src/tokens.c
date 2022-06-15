
#include <stdio.h>
#include <assert.h>
#include "tokens.h"
#include "parser.h"

#define TOKEN_DEF(tt) #tt


static const char* strTokens[] = { TOKENS };

static char tmp_buffer[2] = { 0 };

const char* tokenNameInBuffer(int token, char* buff) {
  if (token == 0) return "<<EOF>>";
  if (token < LAST_SIMPLE_TOKEN) {
    sprintf(buff, "%c", token);
    return buff;
  }
  return strTokens[token - LAST_SIMPLE_TOKEN - 1];
}


const char* tokenName(int token) {
  return tokenNameInBuffer(token, tmp_buffer);
}

unsigned tokenRawLine(Token *t) {
  unsigned lineNum;
  unsigned lineMax = t->locInfo->fileInfo.lineno;
  unsigned *lineMap = t->locInfo->fileInfo.linesPos;
  unsigned pos = t->pos - t->locInfo->buffer;

  for (lineNum = 0; lineNum < lineMax; ++lineNum) {
      unsigned lineOffset = lineMap[lineNum];
      if (pos < lineOffset) break;
  }

  return lineNum;
}

void findFileAndLine(LocationInfo *locInfo, unsigned origLine, unsigned *linePtr, const char **filePtr) {
  LineChunk *chunk = locInfo->fileInfo.chunks;

  for (; chunk->posLineNumber > origLine; chunk = chunk->next);

  assert(chunk != NULL);

  int32_t diff = origLine - chunk->posLineNumber - 1;
  *linePtr = chunk->overrideLineNumber + diff;
  *filePtr = chunk->overrideFileName ? chunk->overrideFileName : locInfo->fileInfo.fileName;
}

void fileAndLine(Token *token, unsigned *linePtr, const char **filePtr) {
  unsigned origLine = tokenRawLine(token);

  LocationInfo *locInfo = token->locInfo;

  findFileAndLine(locInfo, origLine, linePtr, filePtr);
}
