#ifndef __VALUE_H__
#define __VALUE_H__

#include "Symbol.h"

typedef enum ValueTag
{
 vINT, vSTRING, vBOOL, vERROR, vFUN, vTUPLE2, vTUPLE3, vTUPLE4, vSYMBOL
} ValueTag;

typedef struct Value Value;

typedef struct TypeError {
  ValueTag requiredType;
  ValueTag actualType;
} TypeError;

typedef enum ErrorTag
{
 eTYPE, eUNDEFINED, eNOSUCHFORM, eTOOMANYARGS, eTOOFEWARGS
} ErrorTag;

typedef struct Error {
  union {
    TypeError typeError;
    Symbol name;
  };
  ErrorTag type;
} Error;

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

typedef struct Tuple2 {
  Value* item1;
  Value* item2;
} Tuple2;

typedef struct Tuple3 {
  Value* item1;
  Value* item2;
  Value* item3;
} Tuple3;

typedef struct Tuple4 {
  Value* item1;
  Value* item2;
  Value* item3;
  Value* item4;
} Tuple4;

struct Value {
  union {
    int intValue;
    char* cString;
    bool boolValue;
    PrimFun primFun;
    Error error;
    Tuple2 t2;
    Tuple3 t3;
    Tuple4 t4;
    Symbol symbol;
  };
  ValueTag type;
};

Error typeError(const ValueTag required, const ValueTag actual);
Error undefined(const Symbol);
Error noSuchForm(const Symbol);
Error tooManyArgs(const Symbol);
Error tooFewArgs(const Symbol);
Value* vError(const Error);
Value* vInt(const int);
Value* vStr(char*);
Value* vBool(const bool);
Value* vSymbol(const Symbol);
Value* vTuple2(Value*, Value*);
Value* vTuple3(Value*, Value*, Value*);
Value* vTuple4(Value*, Value*, Value*, Value*);
Value* primFun1(Value* (*) (Value*));
Value* primFun2(Value* (*) (Value*, Value*));
Value* primFun3(Value* (*) (Value*, Value*, Value*));
Value* ap1PrimFun2(const PrimFun2, Value*);
Value* ap1PrimFun3(const PrimFun3, Value*);
Value* ap2PrimFun3(const PrimFun3, Value*, Value*);
Value* require(ValueTag, Value*);
Value* apply1(Value*, Value*);

#endif
