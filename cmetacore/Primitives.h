#ifndef __METACORE_PRIMITIVES_H__
#define __METACORE_PRIMITIVES_H__

#include <stdio.h>
#include "Symbol.h"
#include "Struct.h"

void printValue(FILE*, Struct*);
Struct* matchPrim(Symbol);

#endif
