#include "Eval.h"
#include "Primitives.h"
#include "Str.h"
#include "Nat.h"
#include "Bool.h"
#include "Error.h"
#include "PrimFun.h"
#include "Pair.h"
#include "StructValue.h"
#include "SymbolValue.h"
#include <gc.h>
#include <assert.h>
#include <stdio.h>

struct Struct* matchForm(Struct* form) {
  Struct* x = NULL;
  Symbol tag = get_tag(form);
  switch (tag) {
  case 4831888 /* quote */:
    x = (Struct*)get_field(form, 0);
    assert(x != NULL);
    break;
  default:
    x = noSuchForm(tag);
    break;
  }
  return x;
}

Struct* eval(Struct* e) {
  assert(e != NULL);
  Struct* v = NULL;
  switch (get_tag(e)) {
  case NAT_SYMBOL     :
  case STR_SYMBOL     :
  case BOOL_SYMBOL    :
  case PRIMFUN1_SYMBOL:
  case PRIMFUN2_SYMBOL:
  case CLOHALF_SYMBOL :
  case PRIMFUN3_SYMBOL:
  case CLOTHIRD_SYMBOL:
  case TWOTHIRD_SYMBOL:
  case ERROR_SYMBOL   :
    v = e;
    break;
  case PAIR_SYMBOL    :
    v = apply(eval(asFirst(e)), eval(asSecond(e)));
    assert(v != NULL);
    break;
  case STRUCT_SYMBOL  :
    v = matchForm(dequote(e));
    break;
  case SYMBOL_SYMBOL  :
    v = matchPrim(asSymbol(e));
    if (v == NULL) { v = undefined(asSymbol(e)); }
    assert(v != NULL);
    break;
  default             :
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
  return v;
}

