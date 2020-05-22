#include "Eval.h"
#include "Primitives.h"
#include "Str.h"
#include "Nat.h"
#include "Bool.h"
#include "Error.h"
#include <gc.h>
#include <assert.h>
#include <stdio.h>

struct Struct* matchForm(Struct* form) {
  Struct* x = NULL;
  Symbol tag = get_tag(form);
  switch (tag) {
  case 4831888 /* quote */: x = get_field(form, 1); break;
  default: x = noSuchForm(tag); break;
  }
}

Struct* eval(Struct* e) {
  Struct* v = NULL;
  switch (get_tag(e)) {
  case NAT_SYMBOL        :
  case STR_SYMBOL        :
  case BOOL_SYMBOL       :
  case PRIMFUN1_SYMBOL   :
  case PRIMFUN2_SYMBOL   :
  case CLOHALF_SYMBOL    :
  case PRIMFUN3_SYMBOL   :
  case CLOTHIRD_SYMBOL   :
  case CLOTWOTHIRD_SYMBOL:
  case ERROR_SYMBOL      : v = e; break;
  case PAIR_SYMBOL       :
    v = apply(asFirst(e), asSecond(e));
    break;
  case STRUCT_SYMBOL     : v = matchForm(dequote(e)); break;
  case SYMBOL_SYMBOL:
    v = matchPrim(e->symbol);
    if (v == NULL) {
      v = vError(undefined(e->symbol));
    }
    break;
  default:
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
  return v;
}

void printValues(FILE* stream, size_t nValues, Value** values) {
  for (size_t i = 0; i < nValues; i++) {
    fprintf(stream, " ");
    printValue(stream, values[i]);
  }
}

void printValue(FILE* stream, Value* v) {
  switch (get_tag(e)) {
  case NAT_SYMBOL        : printNat(e)    ; break;
  case STR_SYMBOL        : printStr(e)    ; break;
  case BOOL_SYMBOL       : printBool(e)   ; break;
  case ERROR_SYMBOL      : printError(e)  ; break;
  case PRIMFUN1_SYMBOL   :
  case PRIMFUN2_SYMBOL   :
  case CLOHALF_SYMBOL    :
  case PRIMFUN3_SYMBOL   :
  case CLOTHIRD_SYMBOL   :
  case CLOTWOTHIRD_SYMBOL: printPrimFun(e); break;
  case SYMBOL_SYMBOL     : printSymbol(e) ; break;
  case PAIR_SYMBOL       : printPair(e)   ; break;
  case STRUCT_SYMBOL     : printStruct(e) ; break;
  default:
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
}
