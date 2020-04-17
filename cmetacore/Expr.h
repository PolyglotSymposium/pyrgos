#ifndef __EXPR_H__
#define __EXPR_H__

#include "Cons.h"
#include "Symbol.h"
#include <stdbool.h>

typedef enum ExprTag
{
 eINT, eAPPLY, eSTRING, eBOOL, eNAME, eFORM
} ExprTag;

typedef struct Form {
  Symbol name;
  Cons* args;
} Form;

typedef struct Expr {
  union {
    struct {
      struct Expr* applyF;
      Cons* args;
    };
    Form form;
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
Expr* form(Symbol, Cons*);

#endif /* __EXPR_H__ */
