#include "Cons.h"
#include "Expr.h"
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

Expr* num(int value)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eINT;
  x->intValue = value;
  return x;
}

Expr* str(char* s)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eSTRING;
  x->cString = s;
  return x;
}

Expr* ap(Cons* apply)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eAPPLY;
  x->applyF = (Expr*)apply->head;
  x->args = apply->tail;
  return x;
}

Expr* fun(Func f)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eFUN;
  x->func = f;
  return x;
}
