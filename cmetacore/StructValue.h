#ifndef __METACORE_STRUCT_VALUE_H__
#define __METACORE_STRUCT_VALUE_H__

#include <stdio.h>
#include "Struct.h"

Struct* structFromNameAndPairs(Symbol, Struct*);
Struct* structFromNameAnd1(Symbol, Struct*);
Struct* structFromName(Symbol);
Struct* quote(Struct*);
const Symbol STRUCT_SYMBOL = 640304754 /* struct */;
Struct* dequote(Struct*);
void printStruct(FILE*, void(*)(FILE*, Struct*), Struct*);

#endif
