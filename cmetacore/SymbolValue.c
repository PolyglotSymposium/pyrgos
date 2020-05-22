#include "SymbolValue.h"

Struct* newSymbol(Symbol s) {
  return value_struct(SYMBOL_SYMBOL, s);
}

Symbol asSymbol(Struct* s) {
  return value_payload(s);
}

void printSymbol(FILE* stream, Struct* s) {
  fprintf(stream, "%lu", asSymbol(s));
}
