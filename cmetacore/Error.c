#include <gc.h>
#include <assert.h>
#include "Error.h"

const Symbol TYPE_ERROR_SYMBOL          = 614130019090195   ; /* type-error   */
const Symbol UNDEFINED_ERROR_SYMBOL     = 617911392830361012; /* undefined-er */
const Symbol NO_SUCH_FORM_ERROR_SYMBOL  = 173829976459200973; /* nosuchform-e */
const Symbol TOO_MANY_ARGS_ERROR_SYMBOL = 174029086159669988; /* excessargs-e */
const Symbol TOO_FEW_ARGS_ERROR_SYMBOL  = 174029086279252435; /* toofewargs-e */

Struct* typeError(Symbol required, Symbol actual) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*3);
  s[0] = TYPE_ERROR_SYMBOL;
  s[1] = required;
  s[2] = actual;
  return new_struct(ERROR_SYMBOL, 3, (void**)s);
}

Struct* undefined(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = UNDEFINED_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* noSuchForm(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = NO_SUCH_FORM_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* tooManyArgs(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = TOO_MANY_ARGS_ERROR_SYMBOL;
  s[1] = x;
  return new_struct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* tooFewArgs(Symbol x) {
  Symbol* s = (Symbol*)GC_MALLOC(sizeof(Symbol)*2);
  s[0] = TOO_FEW_ARGS_ERROR_SYMBOL;
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
  case UNDEFINED_ERROR_SYMBOL:
    fprintf(
      stream,
      "undefined identifier: %s",
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
  case TOO_MANY_ARGS_ERROR_SYMBOL:
    fprintf(
      stream,
      "too many arguments for: %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  case TOO_FEW_ARGS_ERROR_SYMBOL:
    fprintf(
      stream,
      "too few arguments for: %s",
      decompressSymbol((Symbol)get_field(error, 1))
    );
    break;
  default:
    int UNHANDLED_ERROR_TAG = 0;
    assert(UNHANDLED_ERROR_TAG);
  }
}
