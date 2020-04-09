#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"

typedef enum Func {
  fADD, fMULT, fKCOMB, fICOMB
} Func;

typedef enum ExprTag {
  eINT, eAPPLY, eSTRING
} ExprTag;

typedef struct Expr {
  union {
    struct {
      Func func;
      Cons* args;
    };
    int intValue;
    char* cString;
  };
  ExprTag type;
} Expr;

Expr* num(int);
Expr* str(char*);
Expr* ap(Func, Cons*);

#endif /* __EXPR_H__ */
