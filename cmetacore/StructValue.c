#include "StructValue.h"
#include <assert.h>

Struct* quote(Struct* form) {
  return singleton_struct(STRUCT_SYMBOL, (void*)form);
}

Struct* structFromNameAndPairs(Symbol, Struct*) {
  int UNIMPLEMENTED_STRUCTFROMNAMEANDPAIRS = 0;
  assert(UNIMPLEMENTED_STRUCTFROMNAMEANDPAIRS);
  return NULL;
}

Struct* structFromNameAnd1(Symbol tag, Struct* x) {
  return quote(singleton_struct(tag, (void*)x));
}

Struct* structFromName(Symbol tag) {
  return quote(atomic_struct(tag));
}

Struct* dequote(Struct* form) {
  return (Struct*)singleton_payload(form);
}

void printStruct(FILE* stream, void(*print)(FILE*, Struct*), Struct* s) {
  Struct* x = dequote(s);
  fprintf(stream, "[%s", decompressSymbol(get_tag(x)));
  for (size_t i = 0; i < get_size(x); i++) {
    fprintf(stream, " ");
    print(stream, (Struct*)get_field(x, i));
  }
  fprintf(stream, "]");
}
