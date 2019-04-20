%token <Syntax.symbol> ATOM
%token <Syntax.symbol> SYMBOL
%token LPAREN RPAREN QUOTE ARROW FATARROW EOF COLON
%type <Syntax.expr> sxp
%start sxp

%%

txp:
| SYMBOL { Syntax.TVar $1 }
| SYMBOL ARROW txp { Syntax.Func (Syntax.TVar $1, $3) }
| LPAREN txp RPAREN ARROW txp { Syntax.Func ($2, $5) }

sxp:
| ATOM { Syntax.Atom $1 }
| SYMBOL { Syntax.Symbol $1 }
| LPAREN RPAREN { Syntax.Unit }
| LPAREN SYMBOL FATARROW sxp RPAREN { Syntax.Lambda($2, $4) }
| LPAREN sxp COLON txp RPAREN { Syntax.Annotate ($2, $4) }
| LPAREN list_body { $2 }
| QUOTE sxp { Syntax.Quote $2 }

list_body:
| sxp sxp RPAREN { Syntax.Appl ($1, $2) }
| sxp list_body { Syntax.Appl ($1, $2) }
