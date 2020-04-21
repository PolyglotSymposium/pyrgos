#include "Expr.h"
#include "Eval.h"
#include "Primitives.h"

#include <gc.h>
#include <assert.h>
#include <stdbool.h>

static Value* apply1By1(Value* f, Expr* exprArg, Cons* args) {
  Value* v = NULL;
  v = apply1(f, eval(exprArg));
  if (v->type != vERROR) {
    if (args != NULL) {
      v = apply1By1(v, (Expr*)args->head, args->tail);
    }
  }
  return v;
}

static Value* apply(Value* f, Cons* args) {
  Value* v = NULL;
  if (args == NULL) {
    v = f;
  } else {
    v = apply1By1(f, (Expr*)args->head, args->tail);
  }
  return v;
}

Value* quoteExpr(Expr* e) {
  Value* v = NULL;
  switch (e->type) {
  case eINT:
    v = vTuple2(vSymbol(19880 /* int */), vInt(e->intValue));
    break;
  case eSTRING:
    v = vTuple2(vSymbol(215238258 /* string */), vStr(e->cString));
    break;
  case eAPPLY:
    // 25542112 /* apply */
    {
      int QUOTE_APPLY_NOT_IMPLEMENTED_YET = 0;
      assert(QUOTE_APPLY_NOT_IMPLEMENTED_YET);
    }
    break;
  case eNAME:
    v = vSymbol(e->name);
    break;
  case eFORM:
    {
      // 411077 /* form */
      int QUOTE_FORM_NOT_IMPLEMENTED_YET = 0;
      assert(QUOTE_FORM_NOT_IMPLEMENTED_YET);
    }
    break;
  default:
    int UNHANDLED_EXPR_TAG = 0;
    assert(UNHANDLED_EXPR_TAG);
    break;
  }
  return v;
}

Value* quote(Form form) {
  Value* v = NULL;
  if (form.args == NULL) {
    v = vError(tooFewArgs(form.name));
  } else if (form.args->tail == NULL) {
    v = quoteExpr((Expr*)(form.args->head));
  } else {
    v = vError(tooManyArgs(form.name));
  }
  return v;
}

Value* matchForm(const Form form) {
  Value* v = NULL;
  switch (form.name) {
  case 4831888 /* quote */: v = quote(form); break;
  default: break;
  }
  return v;
}

Value* eval(Expr* e) {
  Value* v = NULL;
  switch (e->type) {
  case eINT:
    v = vInt(e->intValue);
    break;
  case eSTRING:
    v = vStr(e->cString);
    break;
  case eAPPLY:
    v = apply(eval(e->applyF), e->args);
    break;
  case eNAME:
    v = matchPrim(e->name);
    if (v == NULL) {
      v = vError(undefined(e->name));
    }
    break;
  case eFORM:
    v = matchForm(e->form);
    if (v == NULL) {
      v = vError(noSuchForm(e->form.name));
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

void printValue(FILE* stream, Value* v) {
  switch (v->type) {
  case vINT: fprintf(stream, "%i", v->intValue); break;
  case vSTRING: fprintf(stream, "\"%s\"", v->cString); break;
  case vBOOL: fprintf(stream, "%i", v->boolValue); break;
  case vERROR: printError(stream, v->error); break;
  case vFUN: fprintf(stream, "[closure]"); break;
  default:
    int UNHANDLED_VALUE_TAG = 0;
    assert(UNHANDLED_VALUE_TAG);
  }
}
