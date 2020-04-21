#include "Eval.h"
#include "Primitives.h"

#include <gc.h>
#include <assert.h>
#include <stdbool.h>

Value* matchForm(Value* isForm) {
  Value* v = NULL;
  switch (isForm->strukt.name) {
  case 4831888 /* quote */: v = isForm; break; /* quote is the identity macro */
  default: break;
  }
  return v;
}

Value* eval(Value* e) {
  Value* v = NULL;
  switch (e->type) {
  case vINT:
    v = e;
    break;
  case vSTRING:
    v = e;
    break;
  case vBOOL:
    v = e;
    break;
  case vERROR:
    v = e;
    break;
  case vFUN:
    v = e;
    break;
  case vPAIR:
    v = apply1(eval(e->pair.first), eval(e->pair.second));
    break;
  case vSYMBOL:
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

static void printType(FILE* stream, ValueTag type) {
  switch (type) {
  case vINT: fprintf(stream, "integer"); break;
  case vSTRING: fprintf(stream, "string"); break;
  case vBOOL: fprintf(stream, "boolean"); break;
  case vERROR: fprintf(stream, "error"); break;
  case vFUN: fprintf(stream, "closure"); break;
  case vSYMBOL: fprintf(stream, "symbol"); break;
  case vPAIR: fprintf(stream, "pair"); break;
  case vSTRUCT: fprintf(stream, "struct"); break;
  default: fprintf(stream, "unrecognized (%i)", type); break;
  }
}

static void printError(FILE* stream, const Error error) {
  switch (error.type) {
  case eTYPE:
    fprintf(stream, "required type ");
    printType(stream, error.typeError.requiredType);
    fprintf(stream, " but was type ");
    printType(stream, error.typeError.actualType);
    break;
  case eUNDEFINED:
    fprintf(stream, "undefined identifier: %s", decompressSymbol(error.name));
    break;
  case eNOSUCHFORM:
    fprintf(stream, "no special form defined by: %s", decompressSymbol(error.name));
    break;
  case eTOOMANYARGS:
    fprintf(stream, "too many arguments for: %s", decompressSymbol(error.name));
    break;
  case eTOOFEWARGS:
    fprintf(stream, "too few arguments for: %s", decompressSymbol(error.name));
    break;
  default:
    int UNHANDLED_ERROR_TAG = 0;
    assert(UNHANDLED_ERROR_TAG);
  }
}

void printValues(FILE* stream, size_t nValues, Value** values) {
  for (size_t i = 0; i < nValues; i++) {
    fprintf(stream, " ");
    printValue(stream, values[i]);
  }
}

void printValue(FILE* stream, Value* v) {
  switch (v->type) {
  case vINT: fprintf(stream, "%i", v->intValue); break;
  case vSTRING: fprintf(stream, "\"%s\"", v->cString); break;
  case vBOOL: fprintf(stream, "%i", v->boolValue); break;
  case vERROR: printError(stream, v->error); break;
  case vFUN: fprintf(stream, "[closure]"); break;
  case vSYMBOL: fprintf(stream, "%s", decompressSymbol(v->symbol)); break;
  case vPAIR:
    fprintf(stream, "(");
    printValue(stream, v->pair.first);
    fprintf(stream, " ");
    printValue(stream, v->pair.second);
    fprintf(stream, ")");
    break;
  case vSTRUCT:
    fprintf(stream, "[");
    fprintf(stream, "%s", decompressSymbol(v->strukt.name));
    printValues(stream, v->strukt.nFields, v->strukt.fields);
    fprintf(stream, "]");
    break;
  default:
    int UNHANDLED_VALUE_TAG = 0;
    assert(UNHANDLED_VALUE_TAG);
  }
}
