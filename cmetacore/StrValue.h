#ifndef __STR_VALUE_H__
#define __STR_VALUE_H__

#include "Struct.h"

Struct* newStr(char*);
Symbol STR_SYMBOL;
char* asStr(Struct*);
void printStr(FILE*, Struct*);

#endif
