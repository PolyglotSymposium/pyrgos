%token <Syntax.expr> DATUM
%token LPAREN RPAREN QUOTE EOF
%type <Syntax.expr> sxp
%start sxp

%%

sxp:
| DATUM { $1 }
| LPAREN RPAREN { Syntax.Unit }
| LPAREN list_body { $2 }
| QUOTE sxp { Syntax.Quote $2 }

list_body:
| sxp sxp RPAREN { Syntax.Appl($1, $2) }
| sxp list_body { Syntax.Appl($1, $2) }
