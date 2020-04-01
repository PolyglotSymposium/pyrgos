#ifndef __EVAL_H__
#define __EVAL_H__

#include <stdio.h>

typedef enum ValueTag {
  vINT, vSTRING, vERROR
} ValueTag;

typedef struct Value {
  union {
    int intValue;
    char* cString;
  };
  ValueTag type;
} Value;


/**
 * Evaluate the expression, stealing or freeing memory as you go, as needed, in
 * order to entirely consume the structure.
 */
Value* eval(SExpr*);

void printValue(FILE*, Value*);
void recDelValue(Value*);

#endif
