#ifndef __SYMBOL_VALUE_H__
#define __SYMBOL_VALUE_H__

#include "Struct.h"

Struct* newSymbol(Symbol);
Symbol SYMBOL_SYMBOL;
Symbol asSymbol(Struct*);
void printSymbol(FILE*, Struct*);

#endif
