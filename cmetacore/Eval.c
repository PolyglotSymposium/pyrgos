#include "Eval.h"
#include "Primitives.h"

#include <gc.h>
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

Value* matchForm(Value* isForm) {
  Value* v = NULL;
  switch (isForm->strukt.name) {
  case 4831888 /* quote */: v = isForm; break; /* quote is the identity macro */
  default: break;
  }
  return v;
}

Struct* eval(Struct* e) {
  Struct* v = NULL;
  switch (get_tag(e)) {
  case NAT_SYMBOL  : v = e; break;
  case STR_SYMBOL  : v = e; break;
  case BOOL_SYMBOL : v = e; break;
  case ERROR_SYMBOL: v = e; break;
  case vFUN:
    v = e;
    break;
  case vPAIR:
    v = apply1(eval(e->pair.first), eval(e->pair.second));
    break;
  case SYMBOL_SYMBOL:
    v = matchPrim(e->symbol);
    if (v == NULL) {
      v = vError(undefined(e->symbol));
    }
    break;
  case vSTRUCT:
    v = matchForm(e);
    if (v == NULL) {
      v = vError(noSuchForm(e->strukt.name));
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
  case vPAIR:
    v = apply1(eval(e->pair.first), eval(e->pair.second));
    break;
  case SYMBOL_SYMBOL:
    v = matchPrim(e->symbol);
    if (v == NULL) {
      v = vError(undefined(e->symbol));
    }
    break;
  case vSTRUCT:
    v = matchForm(e);
    if (v == NULL) {
      v = vError(noSuchForm(e->strukt.name));
    }
    break;
  default:
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
  }
}
