#include "Expr.h"
#include "Eval.h"

#include <gc.h>
#include <string.h>
#include <assert.h>

char* concat(char* s1, char* s2) {
  char* x = (char*)GC_MALLOC(strlen(s1) + strlen(s2) + 1);
  assert(x != NULL);
  strcpy(x, s1);
  strcat(x, s2);
  return x;
}

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

static Value* primFun2(Value* (*fun2) (Value*, Value*)) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  PrimFun2 pfun2 = { .fun2 = fun2, .arg1 = NULL };
  x->type = vCLO;
  x->primFun2 = pfun2;
  return x;
}

static Value* ap1PrimFun2(const PrimFun2 fun2, Value* arg1) {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  assert(x != NULL);
  x->type = vCLO;
  x->primFun2 = fun2;
  x->primFun2.arg1 = arg1;
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

Value* funcToClo(Func func) {
  Value* v = NULL;
  switch (func) {
  case fADD:
    v = primFun2(add);
    break;
  case fMULT:
    v = primFun2(mult);
    break;
  default:
    int UNHANDLED_FUNC_TAG = 0;
    assert(UNHANDLED_FUNC_TAG);
  }
  return v;
}

Value* apply1(Value* f, Value* arg, Cons* args) {
  Value* v = NULL;
  v = require(vCLO, f);
  if (v == NULL) {
    if (f->primFun2.arg1 == NULL) {
      v = ap1PrimFun2(f->primFun2, arg);
    } else {
      v = f->primFun2.fun2(f->primFun2.arg1, arg);
    }
    if (args != NULL) {
      v = apply1(v, (Value*)args->head, args->tail);
    }
  }
  return v;
}

Value* apply(Func func, Cons* args) {
  Value* v = NULL;
  Value* f = funcToClo(func);
  if (args == NULL) {
    v = f;
  } else {
    v = apply1(f, (Value*)args->head, args->tail);
  }
  return v;
}

Value* eval(SExpr* e) {
  Value* v = NULL;
  switch (e->type) {
  case eINT:
    v = vInt(e->intValue);
    break;
  case eSTRING:
    v = vStr(e->cString);
    break;
  case eAPPLY:
    v = apply(e->func, e->args);
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
  case vCLO: fprintf(stream, "closure"); break;
  default:
    int UNHANDLED_VALUE_TAG = 0;
    assert(UNHANDLED_VALUE_TAG);
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
  case vCLO: fprintf(stream, "[closure]"); break;
  default:
    int UNHANDLED_VALUE_TAG = 0;
    assert(UNHANDLED_VALUE_TAG);
  }
}
