#include "Bool.h"

void printBool(FILE* stream, Struct* s) {
  fprintf(stream, "%s", decompressSymbol(value_payload(s)));
}
