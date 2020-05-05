#include "Error.h"

// eTYPE, eUNDEFINED, eNOSUCHFORM, eTOOMANYARGS, eTOOFEWARGS

Symbol ERROR_SYMBOL               = 18302500          ; /* error        */
Symbol TYPE_ERROR_SYMBOL          = 614130019090195   ; /* type-error   */
Symbol UNDEFINED_ERROR_SYMBOL     = 617911392830361012; /* undefined-er */
Symbol NO_SUCH_FORM_ERROR_SYMBOL  = 173829976459200973; /* nosuchform-e */
Symbol TOO_MANY_ARGS_ERROR_SYMBOL = 174029086159669988; /* excessargs-e */
Symbol TOO_FEW_ARGS_ERROR_SYMBOL  = 174029086279252435; /* toofewargs-e */

Struct* typeError(Symbol required, Symbol actual) {
  Struct* s = (Struct*)GC_MALLOC(sizeof(Struct)*3);
  s[0] = TYPE_ERROR_SYMBOL;
  s[1] = required;
  s[2] = actual;
  return newStruct(ERROR_SYMBOL, 3, (void**)s);
}

Struct* undefined(Symbol x) {
  Struct* s = (Struct*)GC_MALLOC(sizeof(Struct)*2);
  s[0] = UNDEFINED_ERROR_SYMBOL;
  s[1] = x;
  return newStruct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* noSuchForm(Symbol x) {
  Struct* s = (Struct*)GC_MALLOC(sizeof(Struct)*2);
  s[0] = NO_SUCH_FORM_ERROR_SYMBOL;
  s[1] = x;
  return newStruct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* tooManyArgs(Symbol x) {
  Struct* s = (Struct*)GC_MALLOC(sizeof(Struct)*2);
  s[0] = TOO_MANY_ARGS_ERROR_SYMBOL;
  s[1] = x;
  return newStruct(ERROR_SYMBOL, 2, (void**)s);
}

Struct* tooFewArgs(Symbol x) {
  Struct* s = (Struct*)GC_MALLOC(sizeof(Struct)*2);
  s[0] = TOO_FEW_ARGS_ERROR_SYMBOL;
  s[1] = x;
  return newStruct(ERROR_SYMBOL, 2, (void**)s);
}

Symbol asErrorCode(Struct* s) {
  return (Symbol)get_field(s, 0);
}

void printError(FILE* stream, Struct* s) {
  fprintf(stream, "%i", asError(s));
}
