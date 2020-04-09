#include "Cons.h"
#include "Expr.h"
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>


Expr* num(int value)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eINT;
  x->intValue = value;
  return x;
}

Expr* str(char* s)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eSTRING;
  x->cString = s;
  return x;
}

Expr* ap(Func func, Cons* args)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  if (x == NULL) {
    exit(3);
    return NULL;
  }
  x->type = eAPPLY;
  x->func = func;
  x->args = args;
  return x;
}
