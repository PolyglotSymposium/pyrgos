#ifndef __VALUE_H__
#define __VALUE_H__

#include "Cons.h"
#include "Symbol.h"
#include <stdbool.h>

typedef enum ValueTag
{
 vINT, vSTRING, vBOOL, vERROR, vFUN, vSYMBOL, vPAIR, vSTRUCT
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

typedef struct Struct {
  Symbol name;
  size_t nFields;
  Value** fields;
} Struct;

typedef struct Pair {
  Value* first;
  Value* second;
} Pair;

struct Value {
  union {
    int intValue;
    char* cString;
    bool boolValue;
    PrimFun primFun;
    Struct strukt;
    Error error;
    Symbol symbol;
    Pair pair;
  };
  ValueTag type;
};

Error typeError(const ValueTag required, const ValueTag actual);
Error undefined(const Symbol);
Error noSuchForm(const Symbol);
Error tooManyArgs(const Symbol);
Error tooFewArgs(const Symbol);
Value* vError(const Error);
Value* vInt(int);
Value* vStr(char*);
Value* vBool(bool);
Value* vSymbol(const Symbol);
Value* primFun1(Value* (*) (Value*));
Value* primFun2(Value* (*) (Value*, Value*));
Value* primFun3(Value* (*) (Value*, Value*, Value*));
Pair pair(Value*, Value*);
Value* vPair(Pair);
Struct strukt(Symbol, size_t, Value**);
Value* vStruct(Struct);

Value* buildPairs(const Cons* const);
Value* buildStruct(Symbol, Cons*);
Value* ap1PrimFun2(const PrimFun2, Value*);
Value* ap1PrimFun3(const PrimFun3, Value*);
Value* ap2PrimFun3(const PrimFun3, Value*, Value*);
Value* require(ValueTag, Value*);
Value* apply1(Value*, Value*);

#endif
