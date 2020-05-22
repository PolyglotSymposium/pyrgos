#include <gc.h>
#include "Error.h"
#include "PrimFun.h"

Struct* newPrimFun1(Struct* (*f) (Struct*)) {
  return singleton_struct(PRIMFUN1_SYMBOL, (void*)f);
}

Struct* newPrimFun2(Struct* (*f) (Struct*, Struct*)) {
  return singleton_struct(PRIMFUN2_SYMBOL, (void*)f);
}

static Struct* newCloHalf(Struct* (*f) (Struct*, Struct*), Struct* x) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*2);
  s[0] = (void*)f;
  s[1] = (void*)x;
  return new_struct(CLOHALF_SYMBOL, 2, s);
}

Struct* newPrimFun3(Struct* (*f) (Struct*, Struct*, Struct*)) {
  return singleton_struct(PRIMFUN3_SYMBOL, (void*)f);
}

static Struct* newCloOneThird(Struct* (*f) (Struct*, Struct*, Struct*), Struct* x) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*2);
  s[0] = (void*)f;
  s[1] = (void*)x;
  return new_struct(CLOTHIRD_SYMBOL, 2, s);
}

static Struct* newCloTwoThird(Struct* (*f) (Struct*, Struct*, Struct*), Struct* x, Struct* y) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*3);
  s[0] = (void*)f;
  s[1] = (void*)x;
  s[2] = (void*)y;
  return new_struct(CLOTHIRD_SYMBOL, 3, s);
}

static Struct* apPrimFun1(Struct* closure, Struct* x) {
  Struct* (*f) (Struct*) = (Struct* (*) (Struct*))singleton_payload(closure);
  return f(x);
}

static Struct* apPrimFun2(Struct* closure, Struct* x) {
  Struct* (*f) (Struct*, Struct*);
  f = (Struct* (*) (Struct*, Struct*))singleton_payload(closure);
  return newCloHalf(f, x);
}

static Struct* apPrimFun3(Struct* closure, Struct* x) {
  Struct* (*f) (Struct*, Struct*, Struct*);
  f = (Struct* (*) (Struct*, Struct*, Struct*))singleton_payload(closure);
  return newCloOneThird(f, x);
}

static Struct* apCloHalf(Struct* closure, Struct* y) {
  Struct* (*f) (Struct*, Struct*);
  f = (Struct* (*) (Struct*, Struct*))get_field(closure, 0);
  Struct* x = (Struct*)get_field(closure, 1);
  return f(x, y);
}

static Struct* apOneThird(Struct* closure, Struct* y) {
  Struct* (*f) (Struct*, Struct*, Struct*);
  f = (Struct* (*) (Struct*, Struct*, Struct*))get_field(closure, 0);
  Struct* x = (Struct*)get_field(closure, 1);
  return newCloTwoThird(f, x, y);
}

static Struct* apTwoThird(Struct* closure, Struct* z) {
  Struct* (*f) (Struct*, Struct*, Struct*);
  f = (Struct* (*) (Struct*, Struct*, Struct*))get_field(closure, 0);
  Struct* x = (Struct*)get_field(closure, 1);
  Struct* y = (Struct*)get_field(closure, 2);
  return f(x, y, z);
}

Struct* apply(Struct* f, Struct* arg) {
  Struct* x = NULL;
  Symbol tag = get_tag(f);
  switch (tag) {
  case PRIMFUN1_SYMBOL: x = apPrimFun1(f, arg); break;
  case PRIMFUN2_SYMBOL: x = apPrimFun2(f, arg); break;
  case CLOHALF_SYMBOL : x = apCloHalf(f, arg) ; break;
  case PRIMFUN3_SYMBOL: x = apPrimFun3(f, arg); break;
  case CLOTHIRD_SYMBOL: x = apOneThird(f, arg); break;
  case TWOTHIRD_SYMBOL: x = apTwoThird(f, arg); break;
  default             : x = tooManyArgs(tag)  ; break;
  }
  return x;
}

void printPrimFun(FILE* stream, Struct* f) {
  fprintf(stream, "<#closure>");
}
