
#include "tokens.h"
#include <stdio.h>

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


