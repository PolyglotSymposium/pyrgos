#ifndef __METACORE_STR_H__
#define __METACORE_STR_H__

#include <stdio.h>
#include "Struct.h"

Struct* newStr(char*);
const Symbol STR_SYMBOL = 18034; /* str */
char* asStr(Struct*);
void printStr(FILE*, Struct*);

#endif
