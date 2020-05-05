#ifndef __ERROR_H__
#define __ERROR_H__

#include "Struct.h"

Struct* typeError(Symbol required, Symbol actual);
Struct* undefined(Symbol);
Struct* noSuchForm(Symbol);
Struct* tooManyArgs(Symbol);
Struct* tooFewArgs(Symbol);
Symbol ERROR_SYMBOL;
Symbol asErrorCode(Struct*);
void printError(FILE*, Struct*);

#endif
