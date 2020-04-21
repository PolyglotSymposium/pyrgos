#ifndef __VALUE_H__
#define __VALUE_H__

typedef enum ValueTag {
  vINT, vSTRING, vBOOL, vERROR, vFUN
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

struct Value {
  union {
    int intValue;
    char* cString;
    bool boolValue;
    PrimFun primFun;
    Error error;
  };
  ValueTag type;
};

Error typeError(const ValueTag required, const ValueTag actual);
Value* vError(const Error);
Value* vInt(int);
Value* vStr(char*);
Value* vBool(bool);
Value* primFun1(Value* (*) (Value*));
Value* primFun2(Value* (*) (Value*, Value*));
Value* primFun3(Value* (*) (Value*, Value*, Value*));
Value* ap1PrimFun2(const PrimFun2, Value*);
Value* ap1PrimFun3(const PrimFun3, Value*);
Value* ap2PrimFun3(const PrimFun3, Value*, Value*);
Value* require(ValueTag, Value*);
Value* apply1(Value*, Value*);

#endif
