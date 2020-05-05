#ifndef __NAT_H__
#define __NAT_H__

#include "Struct.h"

Struct* newNat(unsigned long);
Symbol NAT_SYMBOL;
unsigned long asNat(Struct*);
void printNat(FILE*, Struct*);

#endif
