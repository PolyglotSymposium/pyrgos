#include "Bool.h"

Symbol BOOL_SYMBOL = 375233; /* bool */

Struct* newBool(bool n) {
  return value_struct(BOOL_SYMBOL, (bool)n);
}

bool asBool(Struct* s) {
  return (bool)value_payload(s);
}

void printBool(FILE* stream, Struct* s) {
  fprintf(stream, "%b", asBool(s));
}
