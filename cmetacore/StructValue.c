#include "Pair.h"
#include "StructValue.h"
#include <gc.h>
#include <assert.h>

Struct* quote(Struct* form) {
  return singleton_struct(STRUCT_SYMBOL, (void*)form);
}

Struct* structFromNameAndPairs(Symbol tag, Struct* x) {
  Struct* s = NULL;
  size_t size = 0;
  for (Struct* right = x; right != NULL; right = asSecond(right)) {
    size++;
  }
  if (size == 1) {
    s = singleton_struct(tag, asFirst(x));
  } else {
    Struct** payload = (Struct**)GC_MALLOC(sizeof(Struct*)*size);
    Struct* p = x;
    for (size_t i = 0; i < size; i++) {
      payload[i] = asFirst(p);
      p = asSecond(p);
    }
    s = new_struct(tag, size, (void**)payload);
  }
  return quote(s);
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
