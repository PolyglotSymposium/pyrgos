%{

#include "Expr.h"
#include "Parser.h"
#include "Lexer.h"

int yyerror(SExpr **expr, yyscan_t scanner, const char *msg) {
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
%parse-param { SExpr **expr }
%parse-param { yyscan_t scanner }

%union {
  int value;
  char* string;
  SExpr* expr;
  Func func;
  Cons* args;
}

%token TOKEN_LPAREN "("
%token TOKEN_RPAREN ")"
%token <func> TOKEN_FUNC "func"
%token <value> TOKEN_NUMBER "number"
%token <string> TOKEN_STRING "string"

%type <expr> expr
%type <args> args

%%

input
: expr { *expr = $1; }
;

expr
: TOKEN_LPAREN TOKEN_FUNC[F] args[A] TOKEN_RPAREN {
  $$ = ap($F, $A);
}
| TOKEN_NUMBER { $$ = num($1); }
| TOKEN_STRING { $$ = str($1); }
;

args
: expr[H] args[T] { $$ = cons($H, $T); }
| expr { $$ = cons($1, NULL); }
;

%%
