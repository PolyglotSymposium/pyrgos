#include "StructValue.h"

Struct* quote(Struct* form) {
  return singleton_struct(STRUCT_SYMBOL, (void*)form);
}

Struct* dequote(Struct* form) {
  return (Struct*)singleton_payload(form);
}

void printStruct(FILE* stream, void(*print)(FILE*, Struct*), Struct* s) {
  Struct* x = dequote(s);
  fprintf(stream, "(%s", decompressSymbol(get_tag(x)));
  for (size_t i = 0; i < get_size(x); i++) {
    fprintf(stream, " ");
    print(stream, get_field(x, i));
  }
  fprintf(stream, ")");
}
