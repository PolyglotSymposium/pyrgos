#include "Expr.h"
#include "Eval.h"

#include <gc.h>
#include <string.h>
#include <assert.h>

static Error typeError(const ValueTag required, const ValueTag actual) {
  TypeError typeError = { .requiredType = required, .actualType = actual };
  Error error = { typeError, eTYPE };
  return error;
}

static Value* vError(const Error error) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vERROR;
  x->error = error;
  return x;
}

static Value* vInt(int value) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vINT;
  x->intValue = value;
  return x;
}

static Value* vStr(char* s) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vSTRING;
  x->cString = s;
  return x;
}

static Value* primFun1(Value* (*fun1) (Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN1;
  x->primFun.f1 = fun1;
  return x;
}

static Value* primFun2(Value* (*fun2) (Value*, Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  PrimFun2 pfun2 = { .fun2 = fun2, .arg1 = NULL };
  x->type = vFUN;
  x->primFun.type = PRIMFUN2;
  x->primFun.f2 = pfun2;
  return x;
}

static Value* primFun3(Value* (*fun3) (Value*, Value*, Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  PrimFun3 pfun3 = { .fun3 = fun3, .arg1 = NULL };
  x->type = vFUN;
  x->primFun.type = PRIMFUN3;
  x->primFun.f3 = pfun3;
  return x;
}

static Value* ap1PrimFun2(const PrimFun2 fun2, Value* arg1) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN2;
  x->primFun.f2 = fun2;
  x->primFun.f2.arg1 = arg1;
  return x;
}

static Value* ap1PrimFun3(const PrimFun3 fun3, Value* arg1) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vFUN;
  x->primFun.type = PRIMFUN3;
  x->primFun.f3 = fun3;
  x->primFun.f3.arg1 = arg1;
  return x;
}

static Value* ap2PrimFun3(const PrimFun3 fun3, Value* arg1, Value* arg2) {
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
    v = vError(typeError(type, x->type));
  }
  return v;
}

static Value* add(Value* x, Value* y) {
  Value* v = NULL;
  v = require(vINT, x);
  if (v == NULL) {
    v = require(vINT, y);
    if (v == NULL) {
      v = vInt(x->intValue + y->intValue);
    }
  }
  return v;
}

static Value* mult(Value* x, Value* y) {
  Value* v = NULL;
  v = require(vINT, x);
  if (v == NULL) {
    v = require(vINT, y);
    if (v == NULL) {
      v = vInt(x->intValue * y->intValue);
    }
  }
  return v;
}

static Value* kcomb(Value* x, Value* _) {
  return x;
}

static Value* icomb(Value* x) { return x; }

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

static Value* bcomb(Value* f, Value* g, Value* x) {
  Value* v = apply1(g, x);
  if (v->type != vERROR) {
    v = apply1(f, v);
  }
  return v;
}

static Value* scomb(Value* f, Value* g, Value* x) {
  Value* v = apply1(f, x);
  if (v->type != vERROR) {
    Value* v2 = apply1(g, x);
    if (v2->type != vERROR) {
      v = apply1(v, v2);
    }
  }
  return v;
}

Value* toClosable(Func func) {
  Value* v = NULL;
  switch (func) {
  case fADD:
    v = primFun2(add);
    break;
  case fMULT:
    v = primFun2(mult);
    break;
  case fKCOMB:
    v = primFun2(kcomb);
    break;
  case fICOMB:
    v = primFun1(icomb);
    break;
  case fBCOMB:
    v = primFun3(bcomb);
    break;
  case fSCOMB:
    v = primFun3(scomb);
    break;
  default:
    int UNHANDLED_FUNC_TAG = 0;
    assert(UNHANDLED_FUNC_TAG);
  }
  return v;
}

Value* apply1By1(Value* f, Expr* exprArg, Cons* args) {
  Value* v = NULL;
  v = apply1(f, eval(exprArg));
  if (v->type != vERROR) {
    if (args != NULL) {
      v = apply1By1(v, (Expr*)args->head, args->tail);
    }
  }
  return v;
}

Value* apply(Value* f, Cons* args) {
  Value* v = NULL;
  if (args == NULL) {
    v = f;
  } else {
    v = apply1By1(f, (Expr*)args->head, args->tail);
  }
  return v;
}

Value* eval(Expr* e) {
  Value* v = NULL;
  switch (e->type) {
  case eINT:
    v = vInt(e->intValue);
    break;
  case eSTRING:
    v = vStr(e->cString);
    break;
  case eAPPLY:
    v = apply(eval(e->applyF), e->args);
    break;
  case eFUN:
    v = toClosable(e->func);
    break;
  default:
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
  return v;
}

static void printType(FILE* stream, ValueTag type) {
  switch (type) {
  case vINT: fprintf(stream, "integer"); break;
  case vSTRING: fprintf(stream, "string"); break;
  case vERROR: fprintf(stream, "error"); break;
  case vFUN: fprintf(stream, "closure"); break;
  default: fprintf(stream, "unrecognized (%i)", type); break;
  }
}

static void printError(FILE* stream, const Error error) {
  switch (error.type) {
  case eTYPE:
    fprintf(stream, "required type ");
    printType(stream, error.typeError.requiredType);
    fprintf(stream, " but was type ");
    printType(stream, error.typeError.actualType);
    break;
  default:
    int UNHANDLED_ERROR_TAG = 0;
    assert(UNHANDLED_ERROR_TAG);
  }
}

void printValue(FILE* stream, Value* v) {
  switch (v->type) {
  case vINT: fprintf(stream, "%i", v->intValue); break;
  case vERROR: printError(stream, v->error); break;
  case vSTRING: fprintf(stream, "\"%s\"", v->cString); break;
  case vFUN: fprintf(stream, "[closure]"); break;
  default:
    int UNHANDLED_VALUE_TAG = 0;
    assert(UNHANDLED_VALUE_TAG);
  }
}
