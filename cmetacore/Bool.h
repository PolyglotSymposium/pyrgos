#ifndef __BOOL_H__
#define __BOOL_H__

#include "Struct.h"
#include <bool.h>

Struct* newBool(bool);
Symbol BOOL_SYMBOL;
bool asBool(Struct*);
void printBool(FILE*, Struct*);

#endif
