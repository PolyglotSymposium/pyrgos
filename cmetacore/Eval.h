#ifndef __EVAL_H__
#define __EVAL_H__

#include <stdio.h>

typedef enum ValueTag {
  vINT, vSTRING, vERROR, vCLO
} ValueTag;

typedef struct Value Value;

typedef struct TypeError {
  ValueTag requiredType;
  ValueTag actualType;
} TypeError;

typedef enum ErrorTag {
  eTYPE
} ErrorTag;

typedef struct Error {
  union {
    TypeError typeError;
  };
  ErrorTag type;
} Error;

typedef struct PrimFun2 {
    Value* (*fun2) (Value*, Value*);
    /**
    * Function is partially applied if `arg1` is non-null
    */
    Value* arg1;
} Clo1;

struct Value {
  union {
    int intValue;
    char* cString;
    PrimFun2 primFun2;
    Error error;
  };
  ValueTag type;
};


Value* eval(SExpr*);

void printValue(FILE*, Value*);

#endif
