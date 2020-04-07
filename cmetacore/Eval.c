#include "Expr.h"
#include "Eval.h"

#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* concat(char* s1, char* s2) {
  char* x = (char*)GC_MALLOC(strlen(s1) + strlen(s2) + 1);
  if (x == NULL) exit(3);
  strcpy(x, s1);
  strcat(x, s2);
  return x;
}

static Value* vError() {
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vERROR;
  return x;
}

static Value* vInt(int value)
{
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vINT;
  x->intValue = value;
  return x;
}

static Value* vStr(char* s)
{
  Value* x = (Value*)GC_MALLOC(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vSTRING;
  x->cString = s;
  return x;
}

static Value* add(Value* x, Value* y) {
  Value* v = NULL;
  switch (x->type) {
  case vERROR:
    v = x;
    break;
  case vINT:
    switch (y->type) {
    case vERROR:
      v = y;
      break;
    case vINT:
      v = vInt(x->intValue + y->intValue);
      break;
    default:
      v = vError();
    }
    break;
  case vSTRING:
    switch (y->type) {
    case vERROR:
      v = y;
      break;
    case vSTRING:
      v = vStr(concat(x->cString, y->cString));
      break;
    default:
      v = vError();
    }
    break;
  default:
    v = vError();
  }
  return v;
}

static Value* mult(Value* x, Value* y) {
  Value* v = NULL;
  switch (x->type) {
  case vERROR:
    v = x;
    break;
  case vINT:
    switch (y->type) {
    case vERROR:
      v = y;
      break;
    case vINT:
      v = vInt(x->intValue * y->intValue);
      break;
    default:
      v = vError();
    }
    break;
  default:
    v = vError();
  }
  return v;
}


Value* eval(SExpr* e) {
  Value* v = NULL;
  switch (e->type) {
  case eINT:
    v = vInt(e->intValue);
    break;
  case eSTRING:
    v = vStr(e->cString);
    break;
  case eAPPLY:
    switch (e->func) {
    case fADD:
      // TODO unsafe to assume linked list is of length 2
      v = add(eval((SExpr*)(e->args->head)), eval((SExpr*)(e->args->tail->head)));
      break;
    case fMULT:
      // TODO unsafe to assume linked list is of length 2
      v = mult(eval((SExpr*)(e->args->head)), eval((SExpr*)(e->args->tail->head)));
      break;
    default:
      fprintf(stderr, "BUG: unhandled func");
      exit(2);
    }
    break;
  default:
    fprintf(stderr, "BUG: unhandled expr type");
    exit(2);
  }
  return v;
}

void printValue(FILE* stream, Value* v) {
  switch (v->type) {
  case vINT: fprintf(stream, "%i", v->intValue); break;
  case vERROR: fprintf(stream, "[error]"); break;
  case vSTRING: fprintf(stream, "\"%s\"", v->cString); break;
  default:
    fprintf(stderr, "BUG: unhandled value type");
    exit(2);
  }
}
