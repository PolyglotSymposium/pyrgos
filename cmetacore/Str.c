#include "Str.h"
#include "Struct.h"

Struct* newStr(char* s) {
  return newStruct(STR_SYMBOL, 1, (void**)s);
}

char* asStr(Struct* s) {
  return (char*)get_payload(s);
}

void printStr(FILE* stream, Struct* s) {
  fprintf(stream, "%s", asStr(s));
}
