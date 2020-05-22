#ifndef __STRUCT_VALUE_H__
#define __STRUCT_VALUE_H__

#include "Struct.h"

Struct* quote(Struct*);
const Symbol STRUCT_SYMBOL = 640304754 /* struct */;
Struct* dequote(Struct*);
void printStruct(FILE*, void(*)(FILE*, Struct*), Struct*);

#endif
