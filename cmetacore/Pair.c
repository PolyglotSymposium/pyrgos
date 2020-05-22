#include <gc.h>
#include "Pair.h"

Struct* newPair(Struct* a, Struct* b) {
  Struct** s = (Struct**)GC_MALLOC(sizeof(Struct*)*2);
  s[0] = a;
  s[1] = b;
  return new_struct(PAIR_SYMBOL, 2, (void**)s);
}

Struct* asFirst(Struct* s) {
  return (Struct*)get_field(s, 0);
}

Struct* asSecond(Struct* s) {
  return (Struct*)get_field(s, 1);
}

void printPair(FILE* stream, void(*printStruct)(FILE*, Struct*), Struct* s) {
  fprintf(stream, "(");
  printStruct(stream, asFirst(s));
  fprintf(stream, " ");
  printStruct(stream, asSecond(s));
  fprintf(stream, ")");
}
