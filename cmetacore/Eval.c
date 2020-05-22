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
#include "Forms.h"
#include <gc.h>
#include <assert.h>
#include <stdio.h>

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
    fprintf(stderr, "Unknown expression type: %s\n", decompressSymbol(get_tag(e)));
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
  return v;
}

