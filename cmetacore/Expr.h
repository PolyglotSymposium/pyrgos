#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"
#include "Symbol.h"
#include <stdbool.h>

typedef enum ExprTag
{
 eINT, eAPPLY, eSTRING, eBOOL, eNAME
} ExprTag;

typedef struct Expr {
  union {
    struct {
      struct Expr* applyF;
      Cons* args;
    };
    int intValue;
    char* cString;
    bool boolValue;
    Symbol name;
  };
  ExprTag type;
} Expr;

Expr* num(int);
Expr* str(char*);
Expr* truth(bool);
Expr* ap(Cons*);
Expr* name(Symbol);

#endif /* __EXPR_H__ */
