#include "Pair.h"
#include "StructValue.h"
#include <gc.h>
#include <assert.h>

Struct* quote(Struct* form) {
  return singleton_struct(STRUCT_SYMBOL, (void*)form);
}

Struct* structFromNameAndPairs(Symbol tag, Struct* x) {
  size_t size = 1;
  for (Struct* left = x; get_tag(left) == PAIR_SYMBOL; left = asFirst(left)) {
    size++;
  }
  Struct** payload = (Struct**)GC_MALLOC(sizeof(Struct*)*size);
  Struct* p = x;
  for (size_t i = size-1; i > 0; i--) {
    payload[i] = asSecond(p);
    p = asFirst(p);
  }
  payload[0] = p;
  return quote(new_struct(tag, size, (void**)payload));
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
