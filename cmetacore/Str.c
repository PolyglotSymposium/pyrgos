#include "Str.h"
#include "Struct.h"

Struct* newStr(char* s) {
  return singleton_struct(STR_SYMBOL, (void*)s);
}

char* asStr(Struct* s) {
  return (char*)singleton_payload(s);
}

void printStr(FILE* stream, Struct* s) {
  fprintf(stream, "\"%s\"", asStr(s));
}
