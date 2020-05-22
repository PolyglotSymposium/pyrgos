#include "Nat.h"

Struct* newNat(unsigned long n) {
  return value_struct(NAT_SYMBOL, n);
}

unsigned long asNat(Struct* s) {
  return value_payload(s);
}

void printNat(FILE* stream, Struct* s) {
  fprintf(stream, "%lu", asNat(s));
}
