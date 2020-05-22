#ifndef __STR_H__
#define __STR_H__

#include "Struct.h"

Struct* newStr(char*);
const Symbol STR_SYMBOL = 18034; /* str */
char* asStr(Struct*);
void printStr(FILE*, Struct*);

#endif
