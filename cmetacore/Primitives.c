#include <gc.h>
#include <assert.h>
#include "Value.h"
#include "Symbol.h"

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

static Value* sub(Value* x, Value* y) {
  Value* v = NULL;
  v = require(vINT, x);
  if (v == NULL) {
    v = require(vINT, y);
    if (v == NULL) {
      v = vInt(x->intValue - y->intValue);
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

static Value* mod(Value* x, Value* y) {
  Value* v = NULL;
  v = require(vINT, x);
  if (v == NULL) {
    v = require(vINT, y);
    if (v == NULL) {
      v = vInt(x->intValue % y->intValue);
    }
  }
  return v;
}

static Value* eq(Value* x, Value* y) {
  Value* v = NULL;
  v = require(vINT, x);
  if (v == NULL) {
    v = require(vINT, y);
    if (v == NULL) {
      v = vBool(x->intValue == y->intValue);
    }
  }
  return v;
}

static Value* kcomb(Value* x, Value* _) {
  return x;
}

static Value* icomb(Value* x) { return x; }

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

static Value* ccomb(Value* f, Value* x, Value* y) {
  Value* v = apply1(f, y);
  if (v->type != vERROR) {
    v = apply1(v, x);
  }
  return v;
}

Value* matchPrim(Symbol name) {
  Value* p = NULL;
  switch (name) {
  case 1     /* b   */: p = primFun3(bcomb); break;
  case 2     /* c   */: p = primFun3(ccomb); break;
  case 8     /* i   */: p = primFun1(icomb); break;
  case 10    /* k   */: p = primFun2(kcomb); break;
  case 18    /* s   */: p = primFun3(scomb); break;
  case 26    /* -   */: p = primFun2(sub  ); break;
  case 27    /* +   */: p = primFun2(add  ); break;
  case 29    /* *   */: p = primFun2(mult ); break;
  case 3532  /* mod */: p = primFun2(mod  ); break;
  case 31236 /* eq? */: p = primFun2(eq   ); break;
  default: break;
  }
  return p;
}
