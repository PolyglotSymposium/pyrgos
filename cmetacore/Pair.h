#ifndef __PAIR_H__
#define __PAIR_H__

#include <stdio.h>
#include "Struct.h"

const Symbol PAIR_SYMBOL = 565263; /* pair */

Struct* newPair(Struct*, Struct*);
Struct* asFirst(Struct*);
Struct* asSecond(Struct*);
void printPair(FILE*, void(*)(FILE*, Struct*), Struct*);

#endif
