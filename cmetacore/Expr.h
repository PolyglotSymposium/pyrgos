#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"
#include <stdbool.h>

typedef enum Func
{
 fADD, fSUB, fMULT, fMOD, fEQ, fKCOMB, fICOMB, fBCOMB, fSCOMB, fCCOMB
} Func;

typedef enum ExprTag
{
 eINT, eAPPLY, eSTRING, eBOOL, eFUN
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
    bool boolValue;
  };
  ExprTag type;
} Expr;

Expr* num(int);
Expr* str(char*);
Expr* truth(bool);
Expr* ap(Cons*);
Expr* fun(Func);

#endif /* __EXPR_H__ */
