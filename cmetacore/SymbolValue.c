#include "SymbolValue.h"

Symbol SYMBOL_SYMBOL = 383824658; /* symbol */

Struct* newSymbol(Symbol s) {
  return value_struct(SYMBOL_SYMBOL, s);
}

Symbol asSymbol(Struct* s) {
  return value_payload(s);
}

void printSymbol(FILE* stream, Struct* s) {
  fprintf(stream, "%i", asSymbol(s));
}
