#ifndef __NAT_H__
#define __NAT_H__

#include "Struct.h"

Struct* newNat(unsigned long);
const Symbol NAT_SYMBOL = 19469; /* nat */
unsigned long asNat(Struct*);
void printNat(FILE*, Struct*);

#endif
