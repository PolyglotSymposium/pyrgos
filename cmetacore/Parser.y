%{

#include "Value.h"
#include "Parser.h"
#include "Lexer.h"

int yyerror(Value** expr, yyscan_t scanner, const char* msg) {
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
%parse-param { Value** expr }
%parse-param { yyscan_t scanner }

%union {
  int value;
  char* string;
  Value* expr;
  Symbol name;
  Cons* apply;
}

%token          TOKEN_LPAREN "("
%token          TOKEN_RPAREN ")"
%token          TOKEN_LSQBRK "["
%token          TOKEN_RSQBRK "]"
%token <value>  TOKEN_NUMBER "number"
%token <string> TOKEN_STRING "string"
%token <name>   TOKEN_NAME   "name"

%type <expr>  expr
%type <apply> apply

%%

input
: expr { *expr = $1; }
;

expr
: TOKEN_LPAREN apply[A] TOKEN_RPAREN                  { $$ = buildPairs($A); }
| TOKEN_LSQBRK TOKEN_NAME[N] apply[A] TOKEN_RSQBRK    { $$ = buildStruct($N, $A); }
| TOKEN_LSQBRK TOKEN_NAME[N] TOKEN_RSQBRK             { $$ = buildStruct($N, NULL); }
| TOKEN_NUMBER                                        { $$ = vInt($1); }
| TOKEN_STRING                                        { $$ = vStr($1); }
| TOKEN_NAME                                          { $$ = vSymbol($1); }
;

apply
: expr[H] apply[T] { $$ = cons($H, $T); }
| expr { $$ = cons($1, NULL); }
;

%%
