#include <gc.h>
#include <assert.h>
#include "Struct.h"
#include "Error.h"
#include "Symbol.h"
#include "Nat.h"
#include "PrimFun.h"

static Struct* require(Symbol tag, Struct* x) {
  Struct* v = NULL;
  Symbol actualTag = get_tag(x);
  if (actualTag != tag) {
    if (actualTag == ERROR_SYMBOL) {
      v = x;
    } else {
      v = typeError(tag, actualTag);
    }
  }
  return v;
}

static Struct* add(Struct* x, Struct* y) {
  Struct* v = NULL;
  v = require(NAT_SYMBOL, x);
  if (v == NULL) {
    v = require(NAT_SYMBOL, y);
    if (v == NULL) {
      v = newNat(asNat(x) + asNat(y));
    }
  }
  return v;
}

static Struct* monus(Struct* x, Struct* y) {
  Struct* v = NULL;
  v = require(NAT_SYMBOL, x);
  if (v == NULL) {
    v = require(NAT_SYMBOL, y);
    if (v == NULL) {
      unsigned long xNat = asNat(x);
      unsigned long yNat = asNat(y);
      if (xNat < yNat) {
        v = newNat(0);
      } else {
        v = newNat(xNat - yNat);
      }
    }
  }
  return v;
}

static Struct* mult(Struct* x, Struct* y) {
  Struct* v = NULL;
  v = require(NAT_SYMBOL, x);
  if (v == NULL) {
    v = require(NAT_SYMBOL, y);
    if (v == NULL) {
      v = newNat(asNat(x) * asNat(y));
    }
  }
  return v;
}

static Struct* natEq(Struct* x, Struct* y) {
  Struct* v = NULL;
  v = require(NAT_SYMBOL, x);
  if (v == NULL) {
    v = require(NAT_SYMBOL, y);
    if (v == NULL) {
      v = newBool(asNat(x) == asNat(y));
    }
  }
  return v;
}

static Struct* kcomb(Struct* x, Struct* _) {
  return x;
}

static Struct* icomb(Struct* x) { return x; }

static Struct* bcomb(Struct* f, Struct* g, Struct* x) {
  Struct* v = apply(g, x);
  if (get_tag(v) != ERROR_SYMBOL) {
    v = apply1(f, v);
  }
  return v;
}

static Struct* scomb(Struct* f, Struct* g, Struct* x) {
  Struct* v = apply(f, x);
  if (get_tag(v) != ERROR_SYMBOL) {
    Struct* v2 = apply(g, x);
    if (get_tag(v2) != ERROR_SYMBOL) {
      v = apply(v, v2);
    }
  }
  return v;
}

static Struct* ccomb(Struct* f, Struct* x, Struct* y) {
  Struct* v = apply(f, y);
  if (get_tag(v) != ERROR_SYMBOL) {
    v = apply(v, x);
  }
  return v;
}

Struct* matchPrim(Symbol name) {
  Struct* p = NULL;
  switch (name) {
  case 1           /* b       */: p = primFun3(bcomb  ); break;
  case 2           /* c       */: p = primFun3(ccomb  ); break;
  case 8           /* i       */: p = primFun1(icomb  ); break;
  case 10          /* k       */: p = primFun2(kcomb  ); break;
  case 18          /* s       */: p = primFun3(scomb  ); break;
  case 27          /* +       */: p = primFun2(add    ); break;
  case 29          /* *       */: p = primFun2(mult   ); break;
  case 19543500    /* monus   */: p = primFun2(monus  ); break;
  case 4874453137  /* require */: p = primFun2(require); break;
  case 32754191373 /* nat-eq? */: p = primFun2(natEq  ); break;
  default: break;
  }
  return p;
}
