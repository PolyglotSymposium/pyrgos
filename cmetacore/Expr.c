#include "Cons.h"
#include "Expr.h"
#include <gc.h>
#include <stdbool.h>
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

Expr* truth(bool value)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eBOOL;
  x->boolValue = value;
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

Expr* name(Symbol name)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eNAME;
  x->name = name;
  return x;
}

Expr* form(Symbol name, Cons* args)
{
  Expr* x = (Expr*)GC_MALLOC(sizeof(Expr));
  assert(x != NULL);
  x->type = eFORM;
  x->form.name = name;
  x->form.args = args;
  return x;
}
