%{

#include "Expr.h"
#include "Parser.h"
#include "Lexer.h"

int yyerror(Expr **expr, yyscan_t scanner, const char *msg) {
  fprintf(stderr, "Parser: %s\n", msg);
  exit(1);
  return 0;
}

%}

%code requires {
  typedef void* yyscan_t;
}

%output  "Parser.c"
%defines "Parser.h"

%define api.pure
%lex-param   { yyscan_t scanner }
%parse-param { Expr **expr }
%parse-param { yyscan_t scanner }

%union {
  int value;
  char* string;
  Expr* expr;
  Symbol name;
  Cons* apply;
}

%token TOKEN_LPAREN "("
%token TOKEN_RPAREN ")"
%token <value> TOKEN_NUMBER "number"
%token <string> TOKEN_STRING "string"
%token <name> TOKEN_NAME "name"

%type <expr> expr
%type <apply> apply

%%

input
: expr { *expr = $1; }
;

expr
: TOKEN_LPAREN apply[A] TOKEN_RPAREN {
  $$ = ap($A);
}
| TOKEN_NUMBER { $$ = num($1); }
| TOKEN_STRING { $$ = str($1); }
| TOKEN_NAME { $$ = name($1); }
;

apply
: expr[H] apply[T] { $$ = cons($H, $T); }
| expr { $$ = cons($1, NULL); }
;

%%
