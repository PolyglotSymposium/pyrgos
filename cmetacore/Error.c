#include <gc.h>
#include <assert.h>
#include "Error.h"

const Symbol TYPE_ERROR_SYMBOL         = 614130019090195   ; /* type-error   */
const Symbol TOO_SHORT_ERROR_SYMBOL    = 617929432747555283; /* too-short-er */
const Symbol UNDEFINED_ERROR_SYMBOL    = 617911392830361012; /* undefined-er */
const Symbol MALFORMED_ERROR_SYMBOL    = 617911392068086796; /* malformed-er */
const Symbol NO_SUCH_FORM_ERROR_SYMBOL = 173829976459200973; /* nosuchform-e */
const Symbol INAPPLICABLE_ERROR_SYMBOL = 628869139782926760; /* inappl-error */

Struct* typeError(Symbol required, Symbol actual) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*3);
  s[0] = TYPE_ERROR_SYMBOL;
  s[1] = required;
  s[2] = actual;
  return new_struct(ERROR_SYMBOL, 3, (void**)s);
}

Struct* too_short(Symbol tag, size_t len, size_t n) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*4);
  s[0] = TOO_SHORT_ERROR_SYMBOL;
  s[1] = tag;
  s[2] = len;
  s[3] = n;
  return new_struct(ERROR_SYMBOL, 4, (void**)s);
}

Struct* undefined(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = UNDEFINED_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* malformed(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = MALFORMED_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* noSuchForm(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = NO_SUCH_FORM_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* inapplicable(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = INAPPLICABLE_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Symbol asErrorCode(Struct* s) {
  return (Symbol)get_field(s, 0);
}

void printError(FILE* stream, Struct* error) {
  switch (asErrorCode(error)) {
  case TYPE_ERROR_SYMBOL:
    fprintf(
      stream,
      "required type %s but was type %s",
      decompressSymbol((Symbol)get_field(error, 1)),
      decompressSymbol((Symbol)get_field(error, 2))
    );
    break;
  case TOO_SHORT_ERROR_SYMBOL:
    fprintf(
      stream,
      "index %lu beyond end of struct %s with size %lu",
      (size_t)get_field(error, 3),
      decompressSymbol((Symbol)get_field(error, 1)),
      (size_t)get_field(error, 2)
    );
    break;
  case UNDEFINED_ERROR_SYMBOL:
    fprintf(
      stream,
      "undefined identifier: %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  case MALFORMED_ERROR_SYMBOL:
    fprintf(
      stream,
      "malformed %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  case NO_SUCH_FORM_ERROR_SYMBOL:
    fprintf(
      stream,
      "no special form defined by: %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  case INAPPLICABLE_ERROR_SYMBOL:
    fprintf(
      stream,
      "cannot apply value which is not a function but a %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  default:
    int UNHANDLED_ERROR_TAG = 0;
    assert(UNHANDLED_ERROR_TAG);
  }
}
