#ifndef __BOOL_H__
#define __BOOL_H__

#include <stdio.h>
#include "Struct.h"

const Symbol BOOL_SYMBOL  = 375233 ; /* bool  */
const Symbol TRUE_SYMBOL  = 152115 ; /* true  */
const Symbol FALSE_SYMBOL = 4795397; /* false */

Struct* TRUE  = value_struct(BOOL_SYMBOL, TRUE_SYMBOL );
Struct* FALSE = value_struct(BOOL_SYMBOL, FALSE_SYMBOL);

void printBool(FILE*, Struct*);

#endif
