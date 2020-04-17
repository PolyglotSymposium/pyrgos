#include <gc.h>
#include <assert.h>
#include "Value.h"

Error typeError(const ValueTag required, const ValueTag actual) {
  Error error;
  error.type = eTYPE;
  TypeError typeError = { .requiredType = required, .actualType = actual };
  error.typeError = typeError;
  return error;
}

Error undefined(const Symbol name) {
  Error error;
  error.type = eUNDEFINED;
  error.name = name;
  return error;
}

Error noSuchForm(const Symbol name) {
  Error error;
  error.type = eNOSUCHFORM;
  error.name = name;
  return error;
}

Error tooManyArgs(const Symbol name) {
  Error error;
  error.type = eTOOMANYARGS;
  error.name = name;
  return error;
}

Error tooFewArgs(const Symbol name) {
  Error error;
  error.type = eTOOMANYARGS;
  error.name = name;
  return error;
}

Value* vError(const Error error) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vERROR;
  x->error = error;
  return x;
}

Value* vTuple2(Value* item1, Value* item2) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vTUPLE2;
  x->t2.item1 = item1;
  x->t2.item2 = item2;
  return x;
}

Value* vTuple3(Value* item1, Value* item2, Value* item3) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vTUPLE3;
  x->t3.item1 = item1;
  x->t3.item2 = item2;
  x->t3.item3 = item3;
  return x;
}

Value* vTuple4(Value* item1, Value* item2, Value* item3, Value* item4) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vTUPLE4;
  x->t4.item1 = item1;
  x->t4.item2 = item2;
  x->t4.item3 = item3;
  x->t4.item4 = item4;
  return x;
}

Value* vInt(const int value) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vINT;
  x->intValue = value;
  return x;
}

Value* vStr(char* s) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vSTRING;
  x->cString = s;
  return x;
}

Value* vBool(const bool value) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vBOOL;
  x->boolValue = value;
  return x;
}

Value* vSymbol(const Symbol symbol) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vSYMBOL;
  x->symbol = symbol;
  return x;
}

Value* primFun1(Value* (*fun1) (Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN1;
  x->primFun.f1 = fun1;
  return x;
}

Value* primFun2(Value* (*fun2) (Value*, Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  PrimFun2 pfun2 = { .fun2 = fun2, .arg1 = NULL };
  x->type = vFUN;
  x->primFun.type = PRIMFUN2;
  x->primFun.f2 = pfun2;
  return x;
}

Value* primFun3(Value* (*fun3) (Value*, Value*, Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  PrimFun3 pfun3 = { .fun3 = fun3, .arg1 = NULL };
  x->type = vFUN;
  x->primFun.type = PRIMFUN3;
  x->primFun.f3 = pfun3;
  return x;
}

Value* ap1PrimFun2(const PrimFun2 fun2, Value* arg1) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN2;
  x->primFun.f2 = fun2;
  x->primFun.f2.arg1 = arg1;
  return x;
}

Value* ap1PrimFun3(const PrimFun3 fun3, Value* arg1) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN3;
  x->primFun.f3 = fun3;
  x->primFun.f3.arg1 = arg1;
  return x;
}

Value* ap2PrimFun3(const PrimFun3 fun3, Value* arg1, Value* arg2) {
  // TODO would this all be easier with memcpy?
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN3;
  x->primFun.f3 = fun3;
  x->primFun.f3.arg1 = arg1;
  x->primFun.f3.arg2 = arg2;
  return x;
}

Value* require(ValueTag type, Value* x) {
  Value* v = NULL;
  if (x->type != type) {
    if (x->type == vERROR) {
      v = x;
    } else {
      v = vError(typeError(type, x->type));
    }
  }
  return v;
}

Value* apply1(Value* f, Value* arg) {
  Value* v = NULL;
  v = require(vFUN, f);
  if (v == NULL) {
    switch (f->primFun.type) {
    case PRIMFUN1:
      v = f->primFun.f1(arg);
      break;
    case PRIMFUN2:
      if (f->primFun.f2.arg1 == NULL) {
        v = ap1PrimFun2(f->primFun.f2, arg);
      } else {
        v = f->primFun.f2.fun2(f->primFun.f2.arg1, arg);
      }
      break;
    case PRIMFUN3:
      if (f->primFun.f3.arg1 == NULL && f->primFun.f3.arg2 == NULL) {
        v = ap1PrimFun3(f->primFun.f3, arg);
      } else if (f->primFun.f3.arg1 != NULL && f->primFun.f3.arg2 == NULL) {
        v = ap2PrimFun3(f->primFun.f3, f->primFun.f3.arg1, arg);
      } else if (f->primFun.f3.arg1 != NULL && f->primFun.f3.arg2 != NULL) {
        v = f->primFun.f3.fun3(f->primFun.f3.arg1, f->primFun.f3.arg2, arg);
      } else {
        int UNEXPECTED_PRIMFUN3_STATE = 0;
        assert(UNEXPECTED_PRIMFUN3_STATE);
      }
      break;
    default:
      int UNHANDLED_PRIMFUN_TAG = 0;
      assert(UNHANDLED_PRIMFUN_TAG);
    }
  }
  return v;
}
