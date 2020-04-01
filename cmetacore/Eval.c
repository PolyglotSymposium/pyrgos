#include "Expr.h"
#include "Eval.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* concat(char* s1, char* s2) {
  char* x = (char*)malloc(strlen(s1) + strlen(s2) + 1);
  if (x == NULL) exit(3);
  strcpy(x, s1); free(s1); s1 = NULL;
  strcat(x, s2); free(s2); s2 = NULL;
  return x;
}

static Value* vError() {
  Value* x = (Value*)malloc(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vERROR;
  return x;
}

static Value* vInt(int value)
{
  Value* x = (Value*)malloc(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vINT;
  x->intValue = value;
  return x;
}

/**
 * Steals the memory
 */
static Value* vStr(char* s)
{
  Value* x = (Value*)malloc(sizeof(Value));
  if (x == NULL) exit(3);
  x->type = vSTRING;
  x->cString = s;
  s = NULL;
  return x;
}

/**
 * Steals or frees memory from the arguments.
 */
static Value* add(Value* x, Value* y) {
  Value* v = NULL;
  switch (x->type) {
  case vERROR:
    v = x;
    x = NULL;
    recDelValue(y);
    break;
  case vINT:
    switch (y->type) {
    case vERROR:
      v = y;
      y = NULL;
      break;
    case vINT:
      v = vInt(x->intValue + y->intValue);
      free(y);
      y = NULL;
      break;
    default:
      recDelValue(y);
      v = vError();
    }
    free(x);
    x = NULL;
    break;
  case vSTRING:
    switch (y->type) {
    case vERROR:
      v = y;
      y = NULL;
      break;
    case vSTRING:
      v = vStr(concat(x->cString, y->cString));
      free(y);
      y = NULL;
      break;
    default:
      recDelValue(y);
      v = vError();
    }
    free(x);
    x = NULL;
    break;
  default:
    recDelValue(x);
    recDelValue(y);
    v = vError();
  }
  return v;
}

/**
 * Steals or frees memory from the arguments.
 */
static Value* mult(Value* x, Value* y) {
  Value* v = NULL;
  switch (x->type) {
  case vERROR:
    v = x;
    x = NULL;
    recDelValue(y);
    break;
  case vINT:
    switch (y->type) {
    case vERROR:
      v = y;
      y = NULL;
      break;
    case vINT:
      v = vInt(x->intValue * y->intValue);
      free(y);
      y = NULL;
      break;
    default:
      recDelValue(y);
      v = vError();
    }
    free(x);
    x = NULL;
    break;
  default:
    recDelValue(x);
    recDelValue(y);
    v = vError();
  }
  return v;
}


static void recDelSExpr(SExpr* e) {
  if (e == NULL) return;
  switch (e->type) {
  case eINT: break;
  case eSTRING: free(e->cString); e->cString = NULL; break;
  case eAPPLY: recDelConsWith((void(*)(void*))recDelSExpr, e->args); break;
  default:
    fprintf(stderr, "BUG: unhandled expr type");
    exit(2);
  }
  free(e);
  e = NULL;
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
    recDelConsWith((void(*)(void*))recDelSExpr, e->args->tail->tail);
    free(e->args->tail);
    free(e->args);
    break;
  default:
    fprintf(stderr, "BUG: unhandled expr type");
    exit(2);
  }
  free(e);
  e = NULL;
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

void recDelValue(Value* v) {
  switch (v->type) {
  case vINT: break;
  case vERROR: break;
  case vSTRING: free(v->cString); free(v); v = NULL; break;
  default:
    fprintf(stderr, "BUG: unhandled value type");
    exit(2);
  }
  free(v);
  v = NULL;
}
