#ifndef __EVAL_H__
#define __EVAL_H__

#include <stdbool.h>
#include <stdio.h>
#include "Value.h"

Value* eval(Value*);

void printValue(FILE*, Value*);

#endif
