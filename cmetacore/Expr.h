#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"

typedef enum Func {
  fADD, fMULT
} Func;

typedef enum ExprTag {
  eINT, eAPPLY, eSTRING
} ExprTag;

typedef struct SExpr {
  union {
    struct {
      Func func;
      Cons* args;
    };
    int intValue;
    char* cString;
  };
  ExprTag type;
} SExpr;

SExpr* num(int);
SExpr* str(char*);
SExpr* ap(Func, Cons*);

#endif /* __EXPR_H__ */
