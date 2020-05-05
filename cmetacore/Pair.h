#ifndef __PAIR_H__
#define __PAIR_H__

#include "Struct.h"

Struct* newPair(Struct*, Struct*);
Symbol PAIR_SYMBOL;
Struct* asFirst(Struct*);
Struct* asSecond(Struct*);
void printPair(FILE*, void(*)(FILE*, Struct*), Struct*);

#endif
