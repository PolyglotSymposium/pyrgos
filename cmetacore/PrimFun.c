#include <gc.h>
#include "PrimFun1.h"

Symbol PRIMFUN1_SYMBOL    = 1152921972953719343 /* prim-fun_1   */;
Symbol PRIMFUN2_SYMBOL    = 2305843477560566319 /* prim-fun_2   */;
Symbol CLOHALF_SYMBOL     = 492942044167        /* half-clo     */;
Symbol PRIMFUN3_SYMBOL    = 3458764982167413295 /* prim-fun_3   */;
Symbol CLOTHIRD_SYMBOL    = 516887134896492974  /* onethird-clo */;
Symbol CLOTWOTHIRD_SYMBOL = 516887134896503507  /* twothird-clo */;

Struct* newPrimFun1(Struct* (*f) (Struct*)) {
  return singleton_struct(PRIMFUN1_SYMBOL, (void*)f);
}

Struct* newPrimFun2(Struct* (*f) (Struct*, Struct*)) {
  return singleton_struct(PRIMFUN2_SYMBOL, (void*)f);
}

//Value* ap1PrimFun2(const PrimFun2 fun2, Value* arg1) {
//  Value* x = (Value*)GC_MALLOC(sizeof(Value));
//  assert(x != NULL);
//  x->type = vFUN;
//  x->primFun.type = PRIMFUN2;
//  x->primFun.f2 = fun2;
//  x->primFun.f2.arg1 = arg1;
//  return x;
//}

static Struct* newCloHalf(Struct* (*f) (Struct*, Struct*), Struct* x) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*2);
  s[0] = (void*)f;
  s[1] = (void*)x;
  return new_struct(CLOHALF_SYMBOL, 2, s);
}

Struct* newPrimFun3(Struct* (*f) (Struct*, Struct*, Struct*)) {
  return singleton_struct(PRIMFUN3_SYMBOL, (void*)f);
}

static Struct* newCloOneThird(Struct* (*f) (Struct*, Struct*), Struct* x) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*2);
  s[0] = (void*)f;
  s[1] = (void*)x;
  return new_struct(CLOTHIRD_SYMBOL, 2, s);
}

//Value* ap1PrimFun3(const PrimFun3 fun3, Value* arg1) {
//  Value* x = (Value*)GC_MALLOC(sizeof(Value));
//  assert(x != NULL);
//  x->type = vFUN;
//  x->primFun.type = PRIMFUN3;
//  x->primFun.f3 = fun3;
//  x->primFun.f3.arg1 = arg1;
//  return x;
//}

static Struct* newCloTwoThird(Struct* (*f) (Struct*, Struct*), Struct* x, Struct* y) {
  void** s = (void**)GC_MALLOC(sizeof(void*)*3);
  s[0] = (void*)f;
  s[1] = (void*)x;
  s[2] = (void*)y;
  return new_struct(CLOTHIRD_SYMBOL, 3, s);
}

// Value* ap2PrimFun3(const PrimFun3 fun3, Value* arg1, Value* arg2) {
//   Value* x = (Value*)GC_MALLOC(sizeof(Value));
//   assert(x != NULL);
//   x->type = vFUN;
//   x->primFun.type = PRIMFUN3;
//   x->primFun.f3 = fun3;
//   x->primFun.f3.arg1 = arg1;
//   x->primFun.f3.arg2 = arg2;
//   return x;
// }

Struct* apply(Struct* maybeF, Struct* maybeX) {
  return NULL;
}

void printPrimFun(FILE* stream, Struct* f) {
  return NULL;
}
