#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"

typedef enum Func
{
 fADD, fSUB, fMULT, fMOD, fKCOMB, fICOMB, fBCOMB, fSCOMB, fCCOMB
} Func;

typedef enum ExprTag
{
 eINT, eAPPLY, eSTRING, eFUN
} ExprTag;

typedef struct Expr {
  union {
    struct {
      struct Expr* applyF;
      Cons* args;
    };
    Func func;
    int intValue;
    char* cString;
  };
  ExprTag type;
} Expr;

Expr* num(int);
Expr* str(char*);
Expr* ap(Cons*);
Expr* fun(Func);

#endif /* __EXPR_H__ */
