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


Value* eval(SExpr*);

void printValue(FILE*, Value*);

#endif
