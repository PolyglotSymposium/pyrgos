#ifndef __EVAL_H__
#define __EVAL_H__

#include <stdio.h>

typedef enum ValueTag {
  vINT, vSTRING, vERROR, vFUN
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

typedef enum PrimFunTag { PRIMFUN1, PRIMFUN2 } PrimFunTag;

typedef struct PrimFun2 {
    Value* (*fun2) (Value*, Value*);
    /**
    * Function is partially applied if `arg1` is non-null
    */
    Value* arg1;
} PrimFun2;

typedef struct PrimFun {
  union {
    PrimFun2 f2;
    Value* (*f1) (Value*);
  };
  PrimFunTag type;
} PrimFun;

struct Value {
  union {
    int intValue;
    char* cString;
    PrimFun primFun;
    Error error;
  };
  ValueTag type;
};


Value* eval(Expr*);

void printValue(FILE*, Value*);

#endif
