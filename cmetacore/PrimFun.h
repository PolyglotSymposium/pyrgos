#ifndef __PRIM_FUN_1_H__
#define __PRIM_FUN_1_H__

#include "Struct.h"

Symbol PRIMFUN1_SYMBOL;
Symbol PRIMFUN2_SYMBOL;
Symbol CLOHALF_SYMBOL;
Symbol PRIMFUN3_SYMBOL;
Symbol CLOTHIRD_SYMBOL;
Symbol CLOTWOTHIRD_SYMBOL;
Struct* newPrimFun1(Struct* (*) (Struct*));
Struct* newPrimFun2(Struct* (*) (Struct*, Struct*));
Struct* newPrimFun3(Struct* (*) (Struct*, Struct*, Struct*));
Struct* apply(Struct*, Struct*);
void printPrimFun(FILE*, Struct*);

#endif
