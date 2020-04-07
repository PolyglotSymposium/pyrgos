#include "Cons.h"
#include "Expr.h"
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>


SExpr* num(int value)
{
  SExpr* x = (SExpr*)GC_MALLOC(sizeof(SExpr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eINT;
  x->intValue = value;
  return x;
}

SExpr* str(char* s)
{
  SExpr* x = (SExpr*)GC_MALLOC(sizeof(SExpr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eSTRING;
  x->cString = s;
  return x;
}

SExpr* ap(Func func, Cons* args)
{
  SExpr* x = (SExpr*)GC_MALLOC(sizeof(SExpr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eAPPLY;
  x->func = func;
  x->args = args;
  return x;
}
