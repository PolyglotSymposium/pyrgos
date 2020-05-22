#ifndef __SYMBOL_VALUE_H__
#define __SYMBOL_VALUE_H__

#include <stdio.h>
#include "Struct.h"

const Symbol SYMBOL_SYMBOL = 383824658; /* symbol */

Struct* newSymbol(Symbol);
Symbol asSymbol(Struct*);
void printSymbol(FILE*, Struct*);

#endif
