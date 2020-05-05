#ifndef __VALUE_H__
#define __VALUE_H__

#include "Cons.h"
#include "Symbol.h"
#include <stdbool.h>

typedef enum ValueTag
{
 vBOOL, vFUN
} ValueTag;

typedef enum PrimFunTag
{
 PRIMFUN1, PRIMFUN2, PRIMFUN3
} PrimFunTag;

typedef struct PrimFun2 {
    Value* (*fun2) (Value*, Value*);
    /**
    * Function is partially applied if `arg1` is non-null
    */
    Value* arg1;
} PrimFun2;

typedef struct PrimFun3 {
  Value* (*fun3) (Value*, Value*, Value*);
  /**
   * Function is partially applied if `arg1` is non-null
   */
  Value* arg1;
  /**
   * Function is partially applied if `arg2` is non-null
   */
  Value* arg2;
} PrimFun3;

typedef struct PrimFun {
  union {
    Value* (*f1) (Value*);
    PrimFun2 f2;
    PrimFun3 f3;
  };
  PrimFunTag type;
} PrimFun;

Value* vBool(bool);
Value* primFun1(Value* (*) (Value*));
Value* primFun2(Value* (*) (Value*, Value*));
Value* primFun3(Value* (*) (Value*, Value*, Value*));

Value* buildPairs(const Cons* const);
Value* buildStruct(Symbol, Cons*);
Value* ap1PrimFun2(const PrimFun2, Value*);
Value* ap1PrimFun3(const PrimFun3, Value*);
Value* ap2PrimFun3(const PrimFun3, Value*, Value*);
Value* require(ValueTag, Value*);
Value* apply1(Value*, Value*);

#endif
