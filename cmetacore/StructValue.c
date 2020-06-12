#include "Pair.h"
#include "StructValue.h"
#include <gc.h>
#include <assert.h>

Struct* quote(Struct* form) {
  return singleton_struct(STRUCT_SYMBOL, (void*)form);
}

Struct* structFromNameAndPairs(Symbol tag, Struct* x) {
  size_t size = 1;
  for (Struct* right = x; get_tag(right) == PAIR_SYMBOL; right = asSecond(right)) {
    size++;
  }
  Struct** payload = (Struct**)GC_MALLOC(sizeof(Struct*)*size);
  Struct* p = x;
  for (size_t i = 0; i < size; i++) {
    payload[i] = asFirst(p);
    p = asSecond(p);
  }
  return quote(new_struct(tag, size, (void**)payload));
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
