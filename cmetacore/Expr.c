#include <stdlib.h>
#include <stdio.h>
#include "Cons.h"
#include "Expr.h"


SExpr* num(int value)
{
  SExpr* x = (SExpr*)malloc(sizeof(SExpr));
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
  SExpr* x = (SExpr*)malloc(sizeof(SExpr));
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
  SExpr* x = (SExpr*)malloc(sizeof(SExpr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eAPPLY;
  x->func = func;
  x->args = args;
  return x;
}
