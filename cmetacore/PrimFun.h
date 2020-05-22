#ifndef __METACORE_PRIM_FUN_H__
#define __METACORE_PRIM_FUN_H__

#include <stdio.h>
#include "Struct.h"

const Symbol PRIMFUN1_SYMBOL = 1152921972953719343 /* prim-fun_1   */;
const Symbol PRIMFUN2_SYMBOL = 2305843477560566319 /* prim-fun_2   */;
const Symbol CLOHALF_SYMBOL  = 492942044167        /* half-clo     */;
const Symbol PRIMFUN3_SYMBOL = 3458764982167413295 /* prim-fun_3   */;
const Symbol CLOTHIRD_SYMBOL = 516887134896492974  /* onethird-clo */;
const Symbol TWOTHIRD_SYMBOL = 516887134896503507  /* twothird-clo */;

Struct* newPrimFun1(Struct* (*) (Struct*));
Struct* newPrimFun2(Struct* (*) (Struct*, Struct*));
Struct* newPrimFun3(Struct* (*) (Struct*, Struct*, Struct*));
Struct* apply(Struct*, Struct*);
void printPrimFun(FILE*, Struct*);

#endif
